/*
	MIT License

	Copyright (c) 2023 RealTimeChris

	Permission is hereby granted, free of charge, to any person obtaining a copy of this
	software and associated documentation files (the "Software"), to deal in the Software
	without restriction, including without limitation the rights to use, copy, modify, merge,
	publish, distribute, sublicense, and/or sell copies of the Software, and to permit
	persons to whom the Software is furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all copies or
	substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
	INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
	PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
	FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
	OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
	DEALINGS IN THE SOFTWARE.
*/
/// https://github.com/RealTimeChris/jsonifier
/// Feb 20, 2023
#pragma once

#include <jsonifier/NumberUtils.hpp>
#include <jsonifier/StringUtils.hpp>
#include <jsonifier/Parser.hpp>

#include <memory>

namespace jsonifier_internal {

	static constexpr auto falseV{ "false" };
	static constexpr auto trueV{ "true" };
	static constexpr auto nullV{ "null" };
	static constexpr char newline{ '\n' };
	static constexpr char lBracket{ '[' };
	static constexpr char rBracket{ ']' };
	static constexpr char lBrace{ '{' };
	static constexpr char rBrace{ '}' };
	static constexpr char colon{ ':' };
	static constexpr char comma{ ',' };
	static constexpr char quote{ '"' };
	static constexpr char n{ 'n' };

	template<jsonifier::parse_options options, bool minified> struct index_processor {
		template<size_t index, typename value_type, typename parse_context_type>
		JSONIFIER_ALWAYS_INLINE static bool processIndex(value_type& value, parse_context_type& context) noexcept {
			if constexpr (index < tuple_size_v<core_tuple_t<value_type>>) {
				static constexpr auto ptr			= get<index>(coreTupleV<value_type>).ptr();
				static constexpr auto key			= get<index>(coreTupleV<value_type>).view();
				static constexpr auto stringLiteral = stringLiteralFromView<key.size()>(key);
				static constexpr auto keySize		= key.size();
				static constexpr auto keySizeNew	= keySize + 1;
				if JSONIFIER_LIKELY (string_literal_comparitor<stringLiteral>::impl(context.iter)) {
					context.iter += keySizeNew;
					if constexpr (!minified) {
						JSONIFIER_SKIP_WS();
					}
					if JSONIFIER_LIKELY (*context.iter == colon) {
						++context.iter;
						if constexpr (!minified) {
							JSONIFIER_SKIP_WS();
						}
						parse<minified, options>::impl(value.*ptr, context);
						return true;
					}
					JSONIFIER_ELSE_UNLIKELY(else) {
						static constexpr auto sourceLocation{ std::source_location::current() };
						context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
						derailleur<options, parse_context_type>::skipToNextValue(context);
					}
				} else {
					derailleur<options, parse_context_type>::skipKey(context);
					++context.iter;
					derailleur<options, parse_context_type>::skipToNextValue(context);
				}
			}
			return false;
		}
	};

	template<bool minified, jsonifier::parse_options options, typename value_type, typename parse_context_type, size_t... indices>
	constexpr auto generateFunctionPtrsImpl(std::index_sequence<indices...>) noexcept {
		using function_type = decltype(&index_processor<options, minified>::template processIndex<0, value_type, parse_context_type>);
		return array<function_type, sizeof...(indices)>{ { &index_processor<options, minified>::template processIndex<indices, value_type, parse_context_type>... } };
	}

	template<bool minified, jsonifier::parse_options options, typename value_type, typename parse_context_type> constexpr auto generateFunctionPtrs() noexcept {
		constexpr auto tupleSize = tuple_size_v<core_tuple_t<value_type>>;
		return generateFunctionPtrsImpl<minified, options, value_type, parse_context_type>(std::make_index_sequence<tupleSize>{});
	}

	template<bool minified, jsonifier::parse_options options, typename value_type, typename parse_context_type>
	static constexpr auto functionPtrsParse{ generateFunctionPtrs<minified, options, value_type, parse_context_type>() };

	template<jsonifier::parse_options options, jsonifier::concepts::jsonifier_value_t value_type, typename parse_context_type>
	struct parse_impl<false, options, value_type, parse_context_type> {
		JSONIFIER_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			static constexpr auto memberCount = tuple_size_v<core_tuple_t<value_type>>;

			if JSONIFIER_LIKELY (*context.iter == lBrace) {
				++context.iter;
				++context.currentObjectDepth;
				if JSONIFIER_LIKELY (*context.iter != rBrace) {
					if constexpr (memberCount > 0) {
						const auto wsStart = context.iter;
						JSONIFIER_SKIP_WS();
						size_t wsSize{ static_cast<size_t>(context.iter - wsStart) };
						bool antihash{ true };
						if constexpr (jsonifier::concepts::has_excluded_keys<value_type>) {
							const auto keySize = getKeyLength<options>(context);
							jsonifier::string_view key{ static_cast<const char*>(context.iter) + 1, keySize };
							auto& keys = value.jsonifierExcludedKeys;
							if JSONIFIER_LIKELY (keys.find(static_cast<typename std::remove_cvref_t<decltype(keys)>::key_type>(key)) != keys.end()) {
								derailleur<options, parse_context_type>::skipToNextValue(context);
							}
						}

						static constexpr auto parseLambda01 = [](auto& antihashNew, auto& value, auto& context) {
							antihashNew = false;
							auto index	= hash_map<value_type, std::remove_cvref_t<decltype(context.iter)>>::findIndex(context.iter, context.endIter);
							if JSONIFIER_LIKELY (index < memberCount) {
								if JSONIFIER_LIKELY (functionPtrsParse<false, options, value_type, parse_context_type>[index](value, context)) {
								}
								JSONIFIER_ELSE_UNLIKELY(else) {
									derailleur<options, parse_context_type>::skipKey(context);
									++context.iter;
									if JSONIFIER_LIKELY (*context.iter == ':') {
										++context.iter;
									}
									JSONIFIER_ELSE_UNLIKELY(else) {
										static constexpr auto sourceLocation{ std::source_location::current() };
										context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
										derailleur<options, parse_context_type>::skipToNextValue(context);
										return;
									}
									derailleur<options, parse_context_type>::skipToNextValue(context);
								}
							} else {
								derailleur<options, parse_context_type>::skipKey(context);
								++context.iter;
								if JSONIFIER_LIKELY (*context.iter == ':') {
									++context.iter;
								}
								JSONIFIER_ELSE_UNLIKELY(else) {
									static constexpr auto sourceLocation{ std::source_location::current() };
									context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
									derailleur<options, parse_context_type>::skipToNextValue(context);
									return;
								}
								derailleur<options, parse_context_type>::skipToNextValue(context);
							}
						};

						if JSONIFIER_LIKELY (*context.iter == quote) {
							++context.iter;
							if constexpr (options.knownOrder) {
								static constexpr auto ptr			= get<0>(jsonifier::concepts::coreV<value_type>).ptr();
								static constexpr auto key			= get<0>(jsonifier::concepts::coreV<value_type>).view();
								static constexpr auto stringLiteral = stringLiteralFromView<key.size()>(key);
								static constexpr auto keySize		= key.size();
								static constexpr auto keySizeNew	= keySize + 1;
								if JSONIFIER_LIKELY (*(context.iter + key.size()) == quote && string_literal_comparitor<stringLiteral>::impl(context.iter)) {
									context.iter += keySizeNew;
									JSONIFIER_SKIP_WS();
									if JSONIFIER_LIKELY (*context.iter == colon) {
										++context.iter;
										JSONIFIER_SKIP_WS();
										parse<false, options>::impl(value.*ptr, context);
									}
									JSONIFIER_ELSE_UNLIKELY(else) {
										static constexpr auto sourceLocation{ std::source_location::current() };
										context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
										derailleur<options, parse_context_type>::skipToNextValue(context);
									}
								} else {
									parseLambda01(antihash, value, context);
								}
							} else {
								parseLambda01(antihash, value, context);
							}
						}
						JSONIFIER_ELSE_UNLIKELY(else) {
							static constexpr auto sourceLocation{ std::source_location::current() };
							context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_String_Start>(context);
							derailleur<options, parse_context_type>::skipToNextValue(context);
						}
						static constexpr auto parseLambda = [](const auto index, const auto newLines, const auto antiHashNew, auto&& parseLambda, value_type& value,
																parse_context_type& context, const auto wsStart = {}, size_t wsSize = {}) {
							if constexpr (index < memberCount) {
								if JSONIFIER_LIKELY (*context.iter != rBrace) {
									if JSONIFIER_LIKELY (*context.iter == comma) {
										++context.iter;
										JSONIFIER_SKIP_MATCHING_WS();
									}
									JSONIFIER_ELSE_UNLIKELY(else) {
										static constexpr auto sourceLocation{ std::source_location::current() };
										context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Comma_Or_Object_End>(context);
										derailleur<options, parse_context_type>::skipToNextValue(context);
										return;
									}

									if constexpr (jsonifier::concepts::has_excluded_keys<value_type>) {
										const auto keySize = getKeyLength<options>(context);
										jsonifier::string_view key{ static_cast<const char*>(context.iter) + 1, keySize };
										auto& keys = value.jsonifierExcludedKeys;
										if JSONIFIER_LIKELY (keys.find(static_cast<typename std::remove_cvref_t<decltype(keys)>::key_type>(key)) != keys.end()) {
											derailleur<options, parse_context_type>::skipToNextValue(context);
											return;
										}
									}

									if JSONIFIER_LIKELY (*context.iter == quote) {
										++context.iter;
										if constexpr (antiHashNew && options.knownOrder) {
											static constexpr auto ptr			= get<index>(jsonifier::concepts::coreV<value_type>).ptr();
											static constexpr auto key			= get<index>(jsonifier::concepts::coreV<value_type>).view();
											static constexpr auto stringLiteral = stringLiteralFromView<key.size()>(key);
											static constexpr auto keySize		= key.size();
											static constexpr auto keySizeNew	= keySize + 1;
											if JSONIFIER_LIKELY (*(context.iter + key.size()) == quote && string_literal_comparitor<stringLiteral>::impl(context.iter)) {
												context.iter += keySizeNew;
												JSONIFIER_SKIP_WS();
												if JSONIFIER_LIKELY (*context.iter == colon) {
													++context.iter;
													JSONIFIER_SKIP_WS();
													parse<false, options>::impl(value.*ptr, context);
													return parseLambda(std::integral_constant<size_t, index + 1>{}, std::integral_constant<bool, newLines>{},
														std::integral_constant<bool, true>{}, parseLambda, value, context, wsStart, wsSize);
												}
												JSONIFIER_ELSE_UNLIKELY(else) {
													static constexpr auto sourceLocation{ std::source_location::current() };
													context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
													derailleur<options, parse_context_type>::skipToNextValue(context);
													return;
												}
											}
										}
										if JSONIFIER_LIKELY (auto indexNew =
																 hash_map<value_type, std::remove_cvref_t<decltype(context.iter)>>::findIndex(context.iter, context.endIter);
															 indexNew < memberCount) {
											if JSONIFIER_LIKELY (functionPtrsParse<false, options, value_type, parse_context_type>[indexNew](value, context)) {
												return parseLambda(std::integral_constant<size_t, index>{}, std::integral_constant<bool, newLines>{},
													std::integral_constant<bool, false>{}, parseLambda, value, context, wsStart, wsSize);
											}
											JSONIFIER_ELSE_UNLIKELY(else) {
												derailleur<options, parse_context_type>::skipKey(context);
												++context.iter;
												if JSONIFIER_LIKELY (*context.iter == ':') {
													++context.iter;
												}
												JSONIFIER_ELSE_UNLIKELY(else) {
													static constexpr auto sourceLocation{ std::source_location::current() };
													context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
													derailleur<options, parse_context_type>::skipToNextValue(context);
													return;
												}
												derailleur<options, parse_context_type>::skipToNextValue(context);
												return parseLambda(std::integral_constant<size_t, index>{}, std::integral_constant<bool, newLines>{},
													std::integral_constant<bool, false>{}, parseLambda, value, context, wsStart, wsSize);
											}
										} else {
											derailleur<options, parse_context_type>::skipKey(context);
											++context.iter;
											if JSONIFIER_LIKELY (*context.iter == ':') {
												++context.iter;
											}
											JSONIFIER_ELSE_UNLIKELY(else) {
												static constexpr auto sourceLocation{ std::source_location::current() };
												context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
												derailleur<options, parse_context_type>::skipToNextValue(context);
												return;
											}
											derailleur<options, parse_context_type>::skipToNextValue(context);
											return parseLambda(std::integral_constant<size_t, index>{}, std::integral_constant<bool, newLines>{},
												std::integral_constant<bool, false>{}, parseLambda, value, context, wsStart, wsSize);
										}
									}
									JSONIFIER_ELSE_UNLIKELY(else) {
										static constexpr auto sourceLocation{ std::source_location::current() };
										context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_String_Start>(context);
										derailleur<options, parse_context_type>::skipKey(context);
										++context.iter;
										derailleur<options, parse_context_type>::skipToNextValue(context);
										return;
									}
								}
								return;
							}
							return;
						};
						if (whitespaceTable[static_cast<uint8_t>(*(context.iter + wsSize))]) {
							if constexpr (options.knownOrder) {
								if (antihash) {
									parseLambda(std::integral_constant<size_t, 1>{}, std::integral_constant<bool, true>{}, std::integral_constant<bool, true>{}, parseLambda, value,
										context, wsStart, wsSize);
								} else {
									parseLambda(std::integral_constant<size_t, 1>{}, std::integral_constant<bool, true>{}, std::integral_constant<bool, false>{}, parseLambda,
										value, context, wsStart, wsSize);
								}
							} else {
								parseLambda(std::integral_constant<size_t, 1>{}, std::integral_constant<bool, true>{}, std::integral_constant<bool, false>{}, parseLambda, value,
									context, wsStart, wsSize);
							}
						} else {
							parseLambda(std::integral_constant<size_t, 1>{}, std::integral_constant<bool, false>{}, std::integral_constant<bool, false>{}, parseLambda, value,
								context, wsStart, wsSize);
						}
					}
				}
				++context.iter;
				--context.currentObjectDepth;
				JSONIFIER_SKIP_WS();
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Object_Start>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
				return;
			}
		}
	};

	template<jsonifier::parse_options options, jsonifier::concepts::jsonifier_value_t value_type, typename parse_context_type>
	struct parse_impl<true, options, value_type, parse_context_type> {
		JSONIFIER_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			static constexpr auto memberCount = tuple_size_v<core_tuple_t<value_type>>;

			if JSONIFIER_LIKELY (*context.iter == lBrace) {
				++context.iter;
				++context.currentObjectDepth;
				if JSONIFIER_LIKELY (*context.iter != rBrace) {
					if constexpr (memberCount > 0) {
						bool antihash{ true };

						if constexpr (jsonifier::concepts::has_excluded_keys<value_type>) {
							const auto keySize = getKeyLength<options>(context);
							jsonifier::string_view key{ static_cast<const char*>(context.iter) + 1, keySize };
							auto& keys = value.jsonifierExcludedKeys;
							if JSONIFIER_LIKELY (keys.find(static_cast<typename std::remove_cvref_t<decltype(keys)>::key_type>(key)) != keys.end()) {
								derailleur<options, parse_context_type>::skipToNextValue(context);
							}
						}

						if JSONIFIER_LIKELY (*context.iter == quote) {
							++context.iter;
							static constexpr auto ptr			= get<0>(jsonifier::concepts::coreV<value_type>).ptr();
							static constexpr auto key			= get<0>(jsonifier::concepts::coreV<value_type>).view();
							static constexpr auto stringLiteral = stringLiteralFromView<key.size()>(key);
							static constexpr auto keySize		= key.size();
							static constexpr auto keySizeNew	= keySize + 1;
							if JSONIFIER_LIKELY (*(context.iter + key.size()) == quote && string_literal_comparitor<stringLiteral>::impl(context.iter)) {
								context.iter += keySizeNew;
								if JSONIFIER_LIKELY (*context.iter == colon) {
									++context.iter;
									parse<true, options>::impl(value.*ptr, context);
								}
								JSONIFIER_ELSE_UNLIKELY(else) {
									static constexpr auto sourceLocation{ std::source_location::current() };
									context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
									derailleur<options, parse_context_type>::skipToNextValue(context);
								}
							}
							JSONIFIER_ELSE_UNLIKELY(else) {
								antihash   = false;
								auto index = hash_map<value_type, std::remove_cvref_t<decltype(context.iter)>>::findIndex(context.iter, context.endIter);
								if JSONIFIER_LIKELY (index < memberCount) {
									if JSONIFIER_LIKELY (functionPtrsParse<true, options, value_type, parse_context_type>[index](value, context)) {
									}
									JSONIFIER_ELSE_UNLIKELY(else) {
										derailleur<options, parse_context_type>::skipToNextValue(context);
									}
								} else {
									derailleur<options, parse_context_type>::skipKey(context);
									++context.iter;
									derailleur<options, parse_context_type>::skipToNextValue(context);
								}
							}
						}
						JSONIFIER_ELSE_UNLIKELY(else) {
							static constexpr auto sourceLocation{ std::source_location::current() };
							context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_String_Start>(context);
							derailleur<options, parse_context_type>::skipToNextValue(context);
						}

						static constexpr auto parseLambda = [](const auto index, const auto antiHashNew, auto&& parseLambda, value_type& value, parse_context_type& context) {
							if constexpr (index < memberCount) {
								if JSONIFIER_LIKELY (*context.iter != rBrace) {
									if JSONIFIER_LIKELY (*context.iter == comma) {
										++context.iter;
									}
									JSONIFIER_ELSE_UNLIKELY(else) {
										static constexpr auto sourceLocation{ std::source_location::current() };
										context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Comma_Or_Object_End>(context);
										derailleur<options, parse_context_type>::skipToNextValue(context);
										return;
									}

									if constexpr (jsonifier::concepts::has_excluded_keys<value_type>) {
										const auto keySize = getKeyLength<options>(context);
										jsonifier::string_view key{ static_cast<const char*>(context.iter) + 1, keySize };
										auto& keys = value.jsonifierExcludedKeys;
										if JSONIFIER_LIKELY (keys.find(static_cast<typename std::remove_cvref_t<decltype(keys)>::key_type>(key)) != keys.end()) {
											derailleur<options, parse_context_type>::skipToNextValue(context);
											return;
										}
									}

									if JSONIFIER_LIKELY (*context.iter == quote) {
										++context.iter;
										if constexpr (antiHashNew && options.knownOrder) {
											static constexpr auto ptr			= get<index>(jsonifier::concepts::coreV<value_type>).ptr();
											static constexpr auto key			= get<index>(jsonifier::concepts::coreV<value_type>).view();
											static constexpr auto stringLiteral = stringLiteralFromView<key.size()>(key);
											static constexpr auto keySize		= key.size();
											static constexpr auto keySizeNew	= keySize + 1;
											if JSONIFIER_LIKELY (*(context.iter + key.size()) == quote && string_literal_comparitor<stringLiteral>::impl(context.iter)) {
												context.iter += keySizeNew;
												if JSONIFIER_LIKELY (*context.iter == colon) {
													++context.iter;
													parse<true, options>::impl(value.*ptr, context);
													return parseLambda(std::integral_constant<size_t, index + 1>{}, std::integral_constant<bool, true>{}, parseLambda, value,
														context);
												}
												JSONIFIER_ELSE_UNLIKELY(else) {
													static constexpr auto sourceLocation{ std::source_location::current() };
													context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
													derailleur<options, parse_context_type>::skipToNextValue(context);
													return;
												}
											}
										}
										if JSONIFIER_LIKELY (auto indexNew =
																 hash_map<value_type, std::remove_cvref_t<decltype(context.iter)>>::findIndex(context.iter, context.endIter);
															 indexNew < memberCount) {
											if JSONIFIER_LIKELY (functionPtrsParse<true, options, value_type, parse_context_type>[indexNew](value, context)) {
												return parseLambda(std::integral_constant<size_t, index>{}, std::integral_constant<bool, false>{}, parseLambda, value, context);
											}
											JSONIFIER_ELSE_UNLIKELY(else) {
												derailleur<options, parse_context_type>::skipKey(context);
												++context.iter;
												if JSONIFIER_LIKELY (*context.iter == ':') {
													++context.iter;
												}
												JSONIFIER_ELSE_UNLIKELY(else) {
													static constexpr auto sourceLocation{ std::source_location::current() };
													context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
													derailleur<options, parse_context_type>::skipToNextValue(context);
													return;
												}
												derailleur<options, parse_context_type>::skipToNextValue(context);
												return parseLambda(std::integral_constant<size_t, index>{}, std::integral_constant<bool, false>{}, parseLambda, value, context);
											}
										} else {
											derailleur<options, parse_context_type>::skipKey(context);
											++context.iter;
											if JSONIFIER_LIKELY (*context.iter == ':') {
												++context.iter;
											}
											JSONIFIER_ELSE_UNLIKELY(else) {
												static constexpr auto sourceLocation{ std::source_location::current() };
												context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
												derailleur<options, parse_context_type>::skipToNextValue(context);
												return;
											}
											derailleur<options, parse_context_type>::skipToNextValue(context);
											return parseLambda(std::integral_constant<size_t, index>{}, std::integral_constant<bool, false>{}, parseLambda, value, context);
										}
									}
									JSONIFIER_ELSE_UNLIKELY(else) {
										static constexpr auto sourceLocation{ std::source_location::current() };
										context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_String_Start>(context);
										derailleur<options, parse_context_type>::skipToNextValue(context);
										return;
									}
								}
							}
							return;
						};
						if (antihash) {
							parseLambda(std::integral_constant<size_t, 1>{}, std::integral_constant<bool, true>{}, parseLambda, value, context);
						} else {
							parseLambda(std::integral_constant<size_t, 1>{}, std::integral_constant<bool, false>{}, parseLambda, value, context);
						}
					}
				}
				++context.iter;
				--context.currentObjectDepth;
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Object_Start>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
				return;
			}
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::jsonifier_scalar_value_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			static constexpr auto size{ tuple_size_v<core_tuple_t<value_type>> };
			if constexpr (size == 1) {
				static constexpr auto newPtr = get<0>(coreTupleV<value_type>);
				parse<minified, options>::impl(getMember<newPtr>(value), context);
			}
		}
	};

	template<jsonifier::parse_options options, jsonifier::concepts::tuple_t value_type, typename parse_context_type>
	struct parse_impl<false, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			static constexpr auto memberCount = tuple_size_v<std::remove_cvref_t<value_type>>;
			if JSONIFIER_LIKELY (*context.iter == lBracket) {
				++context.iter;
				++context.currentArrayDepth;
				if JSONIFIER_LIKELY (*context.iter != rBracket) {
					if constexpr (memberCount > 0) {
						const auto wsStart = context.iter;
						JSONIFIER_SKIP_WS();
						size_t wsSize{ static_cast<size_t>(context.iter - wsStart) };
						parse<false, options>::impl(get<0>(value), context);

						if (whitespaceTable[static_cast<uint8_t>(*(context.iter + wsSize))]) {
							parseObjects<memberCount, 1, true>(value, context, wsStart, wsSize);
						} else {
							parseObjects<memberCount, 1, false>(value, context, wsStart, wsSize);
						}
					}
				}
				++context.iter;
				JSONIFIER_SKIP_WS();
				--context.currentArrayDepth;
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Array_Start>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
				return;
			}
		}

		template<size_t maxIndex, size_t currentIndex, bool newLines>
		JSONIFIER_ALWAYS_INLINE static void parseObjects(value_type& value, parse_context_type& context, const auto wsStart = {}, size_t wsSize = {}) {
			if constexpr (currentIndex < maxIndex) {
				if JSONIFIER_LIKELY (*context.iter != rBracket) {
					if JSONIFIER_LIKELY (*context.iter == comma) {
						++context.iter;
						JSONIFIER_SKIP_MATCHING_WS();
						parse<false, options>::impl(get<currentIndex>(value), context);
						return parseObjects<maxIndex, currentIndex + 1, newLines>(value, context, wsStart, wsSize);
					} else {
						static constexpr auto sourceLocation{ std::source_location::current() };
						context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Comma_Or_Object_End>(context);
						derailleur<options, parse_context_type>::skipToNextValue(context);
						return;
					}
				} else {
					++context.iter;
					JSONIFIER_SKIP_WS();
				}
			} else {
				++context.iter;
				JSONIFIER_SKIP_WS();
			}
		}
	};

	template<jsonifier::parse_options options, jsonifier::concepts::tuple_t value_type, typename parse_context_type>
	struct parse_impl<true, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			static constexpr auto memberCount = tuple_size_v<std::remove_cvref_t<value_type>>;
			if JSONIFIER_LIKELY (*context.iter == lBracket) {
				++context.iter;
				++context.currentArrayDepth;
				if JSONIFIER_LIKELY (*context.iter != rBracket) {
					if constexpr (memberCount > 0) {
						parse<true, options>::impl(get<0>(value), context);
						parseObjects<memberCount, 1>(value, context);
					}
				}
				++context.iter;
				--context.currentArrayDepth;
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Array_Start>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
				return;
			}
		}

		template<size_t maxIndex, size_t currentIndex> JSONIFIER_ALWAYS_INLINE static void parseObjects(value_type& value, parse_context_type& context) {
			if constexpr (currentIndex < maxIndex) {
				if JSONIFIER_LIKELY (*context.iter != rBracket) {
					if JSONIFIER_LIKELY (*context.iter == comma) {
						++context.iter;
						parse<true, options>::impl(get<currentIndex>(value), context);
						return parseObjects<maxIndex, currentIndex + 1>(value, context);
					} else {
						static constexpr auto sourceLocation{ std::source_location::current() };
						context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Comma_Or_Object_End>(context);
						derailleur<options, parse_context_type>::skipToNextValue(context);
						return;
					}
				} else {
					++context.iter;
				}
			} else {
				++context.iter;
			}
		}
	};

	template<jsonifier::parse_options options, jsonifier::concepts::map_t value_type, typename parse_context_type>
	struct parse_impl<false, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (*context.iter == lBrace) {
				++context.iter;
				++context.currentObjectDepth;
				if JSONIFIER_LIKELY (*context.iter != rBrace) {
					const auto wsStart = context.iter;
					JSONIFIER_SKIP_WS();
					size_t wsSize{ static_cast<size_t>(context.iter - wsStart) };
					static thread_local typename std::remove_cvref_t<value_type>::key_type key{};
					parse<false, options>::impl(key, context);

					if JSONIFIER_LIKELY (*context.iter == colon) {
						++context.iter;
						JSONIFIER_SKIP_WS();
						parse<false, options>::impl(value[key], context);
					}
					JSONIFIER_ELSE_UNLIKELY(else) {
						static constexpr auto sourceLocation{ std::source_location::current() };
						context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
						derailleur<options, parse_context_type>::skipToNextValue(context);
						return;
					}
					if (whitespaceTable[static_cast<uint8_t>(*(context.iter + wsSize))]) {
						return parseObjects<true>(value, context, wsStart, wsSize);
					} else {
						return parseObjects<false>(value, context, wsStart, wsSize);
					}
				} else {
					++context.iter;
					JSONIFIER_SKIP_WS();
					--context.currentObjectDepth;
				}
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Object_Start>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
				return;
			}
		}

		template<bool newLines> JSONIFIER_ALWAYS_INLINE static void parseObjects(value_type& value, parse_context_type& context, const auto wsStart = {}, size_t wsSize = {}) {
			while
				JSONIFIER_LIKELY(*context.iter != rBrace) {
					if ((*context.iter == comma)) {
						++context.iter;
						JSONIFIER_SKIP_MATCHING_WS();
						static thread_local typename std::remove_cvref_t<value_type>::key_type key{};
						parse<false, options>::impl(key, context);

						if JSONIFIER_LIKELY (*context.iter == colon) {
							++context.iter;
							JSONIFIER_SKIP_WS();
							value.emplace(key, typename value_type::mapped_type{});
							parse<false, options>::impl(value[key], context);
						} else {
							static constexpr auto sourceLocation{ std::source_location::current() };
							context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
							derailleur<options, parse_context_type>::skipToNextValue(context);
							return;
						}
					} else {
						static constexpr auto sourceLocation{ std::source_location::current() };
						context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Comma_Or_Object_End>(context);
						derailleur<options, parse_context_type>::skipToNextValue(context);
						return;
					}
				}
			++context.iter;
			--context.currentObjectDepth;
			JSONIFIER_SKIP_WS();
		}
	};

	template<jsonifier::parse_options options, jsonifier::concepts::map_t value_type, typename parse_context_type>
	struct parse_impl<true, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (*context.iter == lBrace) {
				++context.iter;
				++context.currentObjectDepth;
				if JSONIFIER_LIKELY (*context.iter != rBrace) {
					static thread_local typename std::remove_cvref_t<value_type>::key_type key{};
					parse<true, options>::impl(key, context);

					if JSONIFIER_LIKELY (*context.iter == colon) {
						++context.iter;
						parse<true, options>::impl(value[key], context);
					}
					JSONIFIER_ELSE_UNLIKELY(else) {
						static constexpr auto sourceLocation{ std::source_location::current() };
						context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
						derailleur<options, parse_context_type>::skipToNextValue(context);
						return;
					}

					while
						JSONIFIER_LIKELY(*context.iter != rBrace) {
							if JSONIFIER_LIKELY (*context.iter == comma) {
								++context.iter;
								parse<true, options>::impl(key, context);

								if JSONIFIER_LIKELY (*context.iter == colon) {
									++context.iter;
									parse<true, options>::impl(value[key], context);
								} else {
									static constexpr auto sourceLocation{ std::source_location::current() };
									context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Colon>(context);
									derailleur<options, parse_context_type>::skipToNextValue(context);
									return;
								}
							} else {
								static constexpr auto sourceLocation{ std::source_location::current() };
								context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Comma_Or_Object_End>(context);
								derailleur<options, parse_context_type>::skipToNextValue(context);
								return;
							}
						}
					++context.iter;
					--context.currentObjectDepth;
				} else {
					++context.iter;
					--context.currentObjectDepth;
				}
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				context.parserPtr->template reportError<sourceLocation, parse_errors::Missing_Object_Start>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
				return;
			}
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::variant_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			static constexpr auto lambda = [](auto&& valueNew, auto&& value, auto&& context) {
				return parse<minified, options>::impl(value, context);
			};
			visit<lambda>(value, value, context);
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::optional_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (*context.iter != n) {
				parse<minified, options>::impl(value.emplace(), context);
			} else {
				if JSONIFIER_LIKELY (parseNull(context.iter)) {
					if constexpr (!minified) {
						JSONIFIER_SKIP_WS();
					}
					return;
				}
				JSONIFIER_ELSE_UNLIKELY(else) {
					static constexpr auto sourceLocation{ std::source_location::current() };
					context.parserPtr->template reportError<sourceLocation, parse_errors::Invalid_Null_Value>(context);
					derailleur<options, parse_context_type>::skipToNextValue(context);
					return;
				}
			}
		}
	};

	JSONIFIER_ALWAYS_INLINE void noop() noexcept {};

	template<jsonifier::parse_options options, jsonifier::concepts::vector_t value_type, typename parse_context_type>
	struct parse_impl<false, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (*context.iter == lBracket) {
				++context.currentArrayDepth;
				++context.iter;
				if JSONIFIER_LIKELY (*context.iter != rBracket) {
					const auto wsStart = context.iter;
					JSONIFIER_SKIP_WS();
					size_t wsSize{ static_cast<size_t>(context.iter - wsStart) };
					parseObjects<true>(value, context, wsStart, wsSize);
				} else {
					++context.iter;
					JSONIFIER_SKIP_WS();
					--context.currentArrayDepth;
				}
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				reportError<sourceLocation, parse_errors::Missing_Array_Start>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
			}
		}

		template<bool newLines> JSONIFIER_ALWAYS_INLINE static void parseObjects(value_type& value, parse_context_type& context, const auto wsStart = {}, size_t wsSize = {}) {
			if JSONIFIER_LIKELY (const size_t size = value.size(); size > 0) {
				auto iterNew = value.begin();

				for (size_t i = 0; i < size; ++i) {
					parse<false, options>::impl(*(iterNew++), context);

					if JSONIFIER_LIKELY (*context.iter == comma) {
						++context.iter;
						JSONIFIER_SKIP_MATCHING_WS();
					}
					JSONIFIER_ELSE_UNLIKELY(else) {
						if JSONIFIER_LIKELY (*context.iter == rBracket) {
							++context.iter;
							JSONIFIER_SKIP_WS()
							--context.currentArrayDepth;
							return (value.size() == i + 1) ? noop() : value.resize(i + 1);
						}
						JSONIFIER_ELSE_UNLIKELY(else) {
							static constexpr auto sourceLocation{ std::source_location::current() };
							reportError<sourceLocation, parse_errors::Imbalanced_Array_Brackets>(context);
							derailleur<options, parse_context_type>::skipToNextValue(context);
							return;
						}
					}
				}
			}

			while (true) {
				parse<false, options>::impl(value.emplace_back(), context);

				if JSONIFIER_LIKELY (*context.iter == comma) {
					++context.iter;
					JSONIFIER_SKIP_MATCHING_WS();
				}
				JSONIFIER_ELSE_UNLIKELY(else) {
					if JSONIFIER_LIKELY (*context.iter == rBracket) {
						++context.iter;
						JSONIFIER_SKIP_WS();
						--context.currentArrayDepth;
						return;
					}
					JSONIFIER_ELSE_UNLIKELY(else) {
						static constexpr auto sourceLocation{ std::source_location::current() };
						reportError<sourceLocation, parse_errors::Imbalanced_Array_Brackets>(context);
						derailleur<options, parse_context_type>::skipToNextValue(context);
						return;
					}
				}
			}
		}

	  protected:
		template<const auto& sourceLocation, auto parseError> JSONIFIER_ALWAYS_INLINE static void reportError(parse_context_type context) {
			context.parserPtr->template reportError<sourceLocation, parseError>(context);
		}
	};

	template<jsonifier::parse_options options, jsonifier::concepts::vector_t value_type, typename parse_context_type>
	struct parse_impl<true, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (*context.iter == lBracket) {
				++context.currentArrayDepth;
				++context.iter;
				if JSONIFIER_LIKELY (*context.iter != rBracket) {
					if JSONIFIER_LIKELY (const size_t size = value.size(); size > 0) {
						auto iterNew = value.begin();

						for (size_t i = 0; i < size; ++i) {
							parse<true, options>::impl(*(iterNew++), context);

							if JSONIFIER_LIKELY (*context.iter == comma) {
								++context.iter;
							}
							JSONIFIER_ELSE_UNLIKELY(else) {
								if JSONIFIER_LIKELY (*context.iter == rBracket) {
									++context.iter;
									--context.currentArrayDepth;
									return (value.size() == i + 1) ? noop() : value.resize(i + 1);
								}
								JSONIFIER_ELSE_UNLIKELY(else) {
									static constexpr auto sourceLocation{ std::source_location::current() };
									reportError<sourceLocation, parse_errors::Imbalanced_Array_Brackets>(context);
									derailleur<options, parse_context_type>::skipToNextValue(context);
									return;
								}
							}
						}
					}

					while (true) {
						parse<true, options>::impl(value.emplace_back(), context);

						if JSONIFIER_LIKELY (*context.iter == comma) {
							++context.iter;
						}
						JSONIFIER_ELSE_UNLIKELY(else) {
							if JSONIFIER_LIKELY (*context.iter == rBracket) {
								++context.iter;
								--context.currentArrayDepth;
								return;
							}
							JSONIFIER_ELSE_UNLIKELY(else) {
								static constexpr auto sourceLocation{ std::source_location::current() };
								reportError<sourceLocation, parse_errors::Imbalanced_Array_Brackets>(context);
								derailleur<options, parse_context_type>::skipToNextValue(context);
								return;
							}
						}
					}
				} else {
					++context.iter;
					--context.currentArrayDepth;
				}
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				reportError<sourceLocation, parse_errors::Missing_Array_Start>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
			}
		}

	  protected:
		template<const auto& sourceLocation, auto parseError> JSONIFIER_ALWAYS_INLINE static void reportError(parse_context_type context) {
			context.parserPtr->template reportError<sourceLocation, parseError>(context);
		}
	};

	template<jsonifier::parse_options options, jsonifier::concepts::raw_array_t value_type, typename parse_context_type>
	struct parse_impl<false, options, value_type, parse_context_type> {
		static constexpr char rBracket{ ']' };
		static constexpr char lBracket{ '[' };
		static constexpr char comma{ ',' };
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (*context.iter == lBracket) {
				++context.currentArrayDepth;
				++context.iter;
				if JSONIFIER_LIKELY (*context.iter != rBracket) {
					const auto wsStart = context.iter;
					JSONIFIER_SKIP_WS();
					size_t wsSize{ static_cast<size_t>(context.iter - wsStart) };
					if (whitespaceTable[static_cast<uint8_t>(*(context.iter + wsSize))]) {
						return parseObjects<true>(value, context, wsStart, wsSize);
					} else {
						return parseObjects<false>(value, context, wsStart, wsSize);
					}
				} else {
					++context.iter;
					JSONIFIER_SKIP_WS();
					--context.currentArrayDepth;
				}
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				reportError<sourceLocation, parse_errors::Missing_Array_Start>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
			}
		}

		template<bool newLines> JSONIFIER_ALWAYS_INLINE static void parseObjects(value_type& value, parse_context_type& context, const auto wsStart = {}, size_t wsSize = {}) {
			if JSONIFIER_LIKELY (const size_t n = std::size(value); n > 0) {
				auto iterNew = std::begin(value);

				for (size_t i = 0; i < n; ++i) {
					parse<false, options>::impl(*(iterNew++), context);

					if JSONIFIER_LIKELY (*context.iter == comma) {
						++context.iter;
						JSONIFIER_SKIP_MATCHING_WS();
					}
					JSONIFIER_ELSE_UNLIKELY(else) {
						if JSONIFIER_LIKELY (*context.iter == rBracket) {
							++context.iter;
							JSONIFIER_SKIP_WS()
							--context.currentArrayDepth;
							return;
						}
						JSONIFIER_ELSE_UNLIKELY(else) {
							static constexpr auto sourceLocation{ std::source_location::current() };
							reportError<sourceLocation, parse_errors::Imbalanced_Array_Brackets>(context);
							derailleur<options, parse_context_type>::skipToNextValue(context);
							return;
						}
					}
				}
			}
		}

	  protected:
		template<const auto& sourceLocation, auto parseError> JSONIFIER_ALWAYS_INLINE static void reportError(parse_context_type context) {
			context.parserPtr->template reportError<sourceLocation, parseError>(context);
		}
	};

	template<jsonifier::parse_options options, jsonifier::concepts::raw_array_t value_type, typename parse_context_type>
	struct parse_impl<true, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (*context.iter == lBracket) {
				++context.currentArrayDepth;
				++context.iter;
				if JSONIFIER_LIKELY (*context.iter != rBracket) {
					auto iterNew = std::begin(value);

					for (size_t i = 0; i < value.size(); ++i) {
						parse<true, options>::impl(*(iterNew++), context);

						if JSONIFIER_LIKELY (*context.iter == comma) {
							++context.iter;
						}
						JSONIFIER_ELSE_UNLIKELY(else) {
							if JSONIFIER_LIKELY (*context.iter == rBracket) {
								++context.iter;
								--context.currentArrayDepth;
								return;
							}
							JSONIFIER_ELSE_UNLIKELY(else) {
								static constexpr auto sourceLocation{ std::source_location::current() };
								reportError<sourceLocation, parse_errors::Imbalanced_Array_Brackets>(context);
								derailleur<options, parse_context_type>::skipToNextValue(context);
								return;
							}
						}
					}
				} else {
					++context.iter;
					--context.currentArrayDepth;
				}
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				reportError<sourceLocation, parse_errors::Missing_Array_Start>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
			}
		}

	  protected:
		template<const auto& sourceLocation, auto parseError> JSONIFIER_ALWAYS_INLINE static void reportError(parse_context_type context) {
			context.parserPtr->template reportError<sourceLocation, parseError>(context);
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::string_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			derailleur<options, parse_context_type>::parseString(value, context);
			if constexpr (!minified) {
				JSONIFIER_SKIP_WS();
			}
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::char_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			value = static_cast<value_type>(*(static_cast<const char*>(context.iter) + 1));
			++context.iter;
			if constexpr (!minified) {
				JSONIFIER_SKIP_WS();
			}
		}
	};

	template<jsonifier::parse_options options, jsonifier::concepts::char_t value_type, typename parse_context_type>
	struct parse_impl<true, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			value = static_cast<value_type>(*(static_cast<const char*>(context.iter) + 1));
			++context.iter;
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::raw_json_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			auto newPtr = static_cast<const char*>(context.iter);
			derailleur<options, parse_context_type>::skipToNextValue(context);
			int64_t newSize = static_cast<const char*>(context.iter) - newPtr;
			if JSONIFIER_LIKELY (newSize > 0) {
				jsonifier::string newString{};
				newString.resize(static_cast<size_t>(newSize));
				std::memcpy(newString.data(), newPtr, static_cast<size_t>(newSize));
				value = newString;
			}
			return;
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::shared_ptr_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (*context.iter != n) {
				using member_type = decltype(*value);
				if JSONIFIER_UNLIKELY (!value) {
					value = std::make_shared<std::remove_pointer_t<std::remove_cvref_t<member_type>>>();
				}
				parse<minified, options>::impl(*value, context);
			} else {
				if JSONIFIER_LIKELY (parseNull(context.iter)) {
					if constexpr (!minified) {
						JSONIFIER_SKIP_WS();
					}
					return;
				}
				JSONIFIER_ELSE_UNLIKELY(else) {
					static constexpr auto sourceLocation{ std::source_location::current() };
					context.parserPtr->template reportError<sourceLocation, parse_errors::Invalid_Null_Value>(context);
					derailleur<options, parse_context_type>::skipToNextValue(context);
					return;
				}
			}
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::unique_ptr_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (*context.iter != n) {
				using member_type = decltype(*value);
				if JSONIFIER_UNLIKELY (!value) {
					value = std::make_unique<std::remove_pointer_t<std::remove_cvref_t<member_type>>>();
				}
				parse<minified, options>::impl(*value, context);
			} else {
				if JSONIFIER_LIKELY (parseNull(context.iter)) {
					if constexpr (!minified) {
						JSONIFIER_SKIP_WS();
					}
					return;
				}
				JSONIFIER_ELSE_UNLIKELY(else) {
					static constexpr auto sourceLocation{ std::source_location::current() };
					context.parserPtr->template reportError<sourceLocation, parse_errors::Invalid_Null_Value>(context);
					derailleur<options, parse_context_type>::skipToNextValue(context);
					return;
				}
			}
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::pointer_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (*context.iter != n) {
				if JSONIFIER_UNLIKELY (!value) {
					value = new std::remove_pointer_t<std::remove_cvref_t<value_type>>{};
				}
				parse<minified, options>::impl(*value, context);
			} else {
				if JSONIFIER_LIKELY (parseNull(context.iter)) {
					if constexpr (!minified) {
						JSONIFIER_SKIP_WS();
					}
					return;
				}
				JSONIFIER_ELSE_UNLIKELY(else) {
					static constexpr auto sourceLocation{ std::source_location::current() };
					context.parserPtr->template reportError<sourceLocation, parse_errors::Invalid_Null_Value>(context);
					derailleur<options, parse_context_type>::skipToNextValue(context);
					return;
				}
			}
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::always_null_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type&, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (parseNull(context.iter)) {
				if constexpr (!minified) {
					JSONIFIER_SKIP_WS();
				}
				return;
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				context.parserPtr->template reportError<sourceLocation, parse_errors::Invalid_Null_Value>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
				return;
			}
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::enum_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			size_t newValue{};
			if JSONIFIER_LIKELY (parseNumber(newValue, context.iter, context.endIter)) {
				value = static_cast<value_type>(newValue);
				if constexpr (!minified) {
					JSONIFIER_SKIP_WS();
				}
				return;
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				context.parserPtr->template reportError<sourceLocation, parse_errors::Invalid_Number_Value>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
				return;
			}
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::num_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (parseNumber(value, context.iter, context.endIter)) {
				if constexpr (!minified) {
					JSONIFIER_SKIP_WS();
				}
				return;
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				context.parserPtr->template reportError<sourceLocation, parse_errors::Invalid_Number_Value>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
				return;
			}
		}
	};

	template<bool minified, jsonifier::parse_options options, jsonifier::concepts::bool_t value_type, typename parse_context_type>
	struct parse_impl<minified, options, value_type, parse_context_type> {
		JSONIFIER_ALWAYS_INLINE static void impl(value_type& value, parse_context_type& context) noexcept {
			if JSONIFIER_LIKELY (parseBool(value, context.iter)) {
				if constexpr (!minified) {
					JSONIFIER_SKIP_WS();
				}
				return;
			}
			JSONIFIER_ELSE_UNLIKELY(else) {
				static constexpr auto sourceLocation{ std::source_location::current() };
				context.parserPtr->template reportError<sourceLocation, parse_errors::Invalid_Bool_Value>(context);
				derailleur<options, parse_context_type>::skipToNextValue(context);
				return;
			}
		}
	};

}