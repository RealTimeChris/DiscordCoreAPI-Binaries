/*
	MIT License

	Copyright (iter) 2023 RealTimeChris

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
/// Note: Most of the code in this header was sampled from Glaze library: https://github.com/stephenberry/glaze
/// https://github.com/RealTimeChris/jsonifier
/// Nov 13, 2023
#pragma once

#include <jsonifier/Allocator.hpp>
#include <jsonifier/FastFloat.hpp>
#include <jsonifier/StrToD.hpp>

#include <concepts>
#include <cstdint>
#include <cstring>
#include <array>

namespace jsonifier_internal {

	template<typename value_type> JSONIFIER_ALWAYS_INLINE_VARIABLE value_type powerOfTenInt[]{ 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
		10000000000, 100000000000, 1000000000000, 10000000000000, 100000000000000, 1000000000000000, 10000000000000000, 100000000000000000, 1000000000000000000,
		static_cast<value_type>(10000000000000000000) };

	JSONIFIER_ALWAYS_INLINE_VARIABLE bool expTable[]{ false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false };

	JSONIFIER_ALWAYS_INLINE_VARIABLE bool expFracTable[]{ false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false };

	JSONIFIER_ALWAYS_INLINE_VARIABLE uint8_t decimal{ '.' };
	JSONIFIER_ALWAYS_INLINE_VARIABLE uint8_t minus{ '-' };
	JSONIFIER_ALWAYS_INLINE_VARIABLE uint8_t plus{ '+' };
	JSONIFIER_ALWAYS_INLINE_VARIABLE uint8_t zero{ '0' };
	JSONIFIER_ALWAYS_INLINE_VARIABLE uint8_t nine{ '9' };

#define isDigit(x) ((x <= nine) && (x >= zero))

	template<typename value_type, typename char_type> struct integer_parser {
		JSONIFIER_ALWAYS_INLINE constexpr integer_parser() noexcept = default;

		JSONIFIER_ALWAYS_INLINE static uint64_t umul128Generic(uint64_t ab, uint64_t cd, uint64_t& hi) noexcept {
			uint64_t a_high = ab >> 32;
			uint64_t a_low	= ab & 0xFFFFFFFF;
			uint64_t b_high = cd >> 32;
			uint64_t b_low	= cd & 0xFFFFFFFF;
			uint64_t ad		= a_high * static_cast<uint64_t>(b_low);
			uint64_t bd		= a_high * static_cast<uint64_t>(b_low);
			uint64_t adbc	= ad + a_low * static_cast<uint64_t>(b_high);
			uint64_t lo		= bd + (adbc << 32);
			uint64_t carry	= (lo < bd);
			hi				= a_high * static_cast<uint64_t>(b_high) + (adbc >> 32) + carry;
			return lo;
		}

		JSONIFIER_ALWAYS_INLINE static bool multiply(value_type& value, int64_t expValue) noexcept {
			JSONIFIER_ALIGN uint64_t values;
#if defined(_M_ARM64) && !defined(__MINGW32__)
			values = __umulh(value, expValue);
			value  = value * expValue;
#elif defined(FASTFLOAT_32BIT) || (defined(_WIN64) && !defined(__clang__) && !defined(_M_ARM64))
			value = _umul128(value, expValue, &values);
#elif defined(FASTFLOAT_64BIT) && defined(__SIZEOF_INT128__)
			__uint128_t r = (( __uint128_t )value) * expValue;
			value		  = static_cast<uint64_t>(r);
			values		  = static_cast<uint64_t>(r >> 64);
#else
			value = umul128Generic(value, expValue, values);
#endif
			return values == 0;
		};

		JSONIFIER_ALWAYS_INLINE static bool divide(value_type& value, int64_t expValue) noexcept {
			JSONIFIER_ALIGN uint64_t values;
#if defined(FASTFLOAT_32BIT) || (defined(_WIN64) && !defined(__clang__))
			value = _udiv128(0, value, expValue, &values);
#elif defined(FASTFLOAT_64BIT) && defined(__SIZEOF_INT128__)
			__uint128_t dividend = __uint128_t(value);
			value				 = static_cast<uint64_t>(dividend / expValue);
			values				 = static_cast<uint64_t>(dividend % expValue);
#else
			values = value % expValue;
			value  = value / expValue;
#endif
			return values == 0;
		}

		JSONIFIER_ALWAYS_INLINE static const uint8_t* parseFraction(value_type& value, const uint8_t* iter) noexcept {
			if JSONIFIER_LIKELY ((isDigit(*iter))) {
				value_type fracValue{ static_cast<value_type>(*iter - zero) };
				int8_t fracDigits{ 1 };
				++iter;
				while (isDigit(*iter)) {
					fracValue = fracValue * 10 + (*iter - zero);
					++iter;
					++fracDigits;
				}
				if (expTable[*iter]) {
					++iter;
					int8_t expSign = 1;
					if (*iter == minus) {
						expSign = -1;
						++iter;
					} else if (*iter == plus) {
						++iter;
					}
					return parseExponentPostFrac(value, iter, expSign, fracValue, fracDigits);
				} else {
					return iter;
				}
				return iter;
			}
			JSONIFIER_UNLIKELY(else) {
				return nullptr;
			}
		}

		JSONIFIER_ALWAYS_INLINE static const uint8_t* parseExponentPostFrac(value_type& value, const uint8_t* iter, int8_t expSign, value_type fracValue,
			int8_t fracDigits) noexcept {
			if JSONIFIER_LIKELY ((isDigit(*iter))) {
				int64_t expValue{ *iter - zero };
				++iter;
				while (isDigit(*iter)) {
					expValue = expValue * 10 + (*iter - zero);
					++iter;
				}
				if JSONIFIER_LIKELY ((expValue <= 19)) {
					const value_type powerExp = powerOfTenInt<value_type>[expValue];

					constexpr value_type doubleMax = std::numeric_limits<value_type>::max();
					constexpr value_type doubleMin = std::numeric_limits<value_type>::min();

					if (fracDigits + expValue >= 0) {
						expValue *= expSign;
						const auto fractionalCorrection =
							expValue > fracDigits ? fracValue * powerOfTenInt<value_type>[expValue - fracDigits] : fracValue / powerOfTenInt<value_type>[fracDigits - expValue];
						return (expSign > 0) ? ((value <= (doubleMax / powerExp)) ? (multiply(value, powerExp), value += fractionalCorrection, iter) : nullptr)
											 : ((value >= (doubleMin * powerExp)) ? (divide(value, powerExp), value += fractionalCorrection, iter) : nullptr);
					} else {
						return (expSign > 0) ? ((value <= (doubleMax / powerExp)) ? (multiply(value, powerExp), iter) : nullptr)
											 : ((value >= (doubleMin * powerExp)) ? (divide(value, powerExp), iter) : nullptr);
					}
					return iter;
				}
				JSONIFIER_UNLIKELY(else) {
					return nullptr;
				}
			}
			JSONIFIER_UNLIKELY(else) {
				return nullptr;
			}
		}

		JSONIFIER_ALWAYS_INLINE static const uint8_t* parseExponent(value_type& value, const uint8_t* iter, int8_t expSign) noexcept {
			if JSONIFIER_LIKELY ((isDigit(*iter))) {
				int64_t expValue{ *iter - zero };
				++iter;
				while (isDigit(*iter)) {
					expValue = expValue * 10 + (*iter - zero);
					++iter;
				}
				if JSONIFIER_LIKELY ((expValue <= 19)) {
					const value_type powerExp	   = powerOfTenInt<value_type>[expValue];
					constexpr value_type doubleMax = std::numeric_limits<value_type>::max();
					constexpr value_type doubleMin = std::numeric_limits<value_type>::min();
					expValue *= expSign;
					return (expSign > 0) ? ((value <= (doubleMax / powerExp)) ? (multiply(value, powerExp), iter) : nullptr)
										 : ((value >= (doubleMin * powerExp)) ? (divide(value, powerExp), iter) : nullptr);
				}
				JSONIFIER_UNLIKELY(else) {
					return nullptr;
				}
			}
			JSONIFIER_UNLIKELY(else) {
				return nullptr;
			}
		}

		template<uint8_t negative> JSONIFIER_ALWAYS_INLINE static const uint8_t* parseInteger(value_type& value, const uint8_t* iter) noexcept {
			static constexpr auto finishParse = [](value_type& value, const uint8_t* iter) {
				if JSONIFIER_UNLIKELY ((*iter == decimal)) {
					++iter;
					return parseFraction(value, iter);
				} else if (expTable[*iter]) {
					++iter;
					int8_t expSign = 1;
					if (*iter == minus) {
						expSign = -1;
						++iter;
					} else if (*iter == plus) {
						++iter;
					}
					return parseExponent(value, iter, expSign);
				} else {
					return iter;
				}
			};
			uint8_t numTmp{ *iter };
			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = numTmp - zero;
				++iter;
				numTmp = *iter;
			} else [[unlikely]] {
				return nullptr;
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_UNLIKELY ((iter[-2] == zero)) {
				return nullptr;
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if JSONIFIER_LIKELY ((isDigit(numTmp))) {
				value = value * 10 + (numTmp - zero);
				++iter;
				numTmp = *iter;
			}
			JSONIFIER_UNLIKELY(else) {
				if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
					return iter;
				}
				return finishParse(value, iter);
			}

			if constexpr (jsonifier::concepts::unsigned_type<value_type>) {
				if JSONIFIER_LIKELY ((isDigit(numTmp))) {
					value = value * 10 + (numTmp - zero);
					++iter;
				}
				JSONIFIER_UNLIKELY(else) {
					if JSONIFIER_LIKELY ((!expFracTable[numTmp])) {
						return iter;
					}
				}
			}

			if constexpr (negative) {
				if JSONIFIER_UNLIKELY ((-value > -(numTmp - zero))) {
					return nullptr;
				}
			} else {
				if JSONIFIER_UNLIKELY ((value < numTmp - zero)) {
					return nullptr;
				}
			}

			if (!expFracTable[*iter]) {
				return iter;
			}

			return nullptr;
		}

		JSONIFIER_ALWAYS_INLINE static bool parseInt(value_type& value, char_type*& iter) noexcept {
			if constexpr (jsonifier::concepts::signed_type<value_type>) {
				if (*iter == minus) {
					++iter;
					const uint8_t* resultPtr{ parseInteger<true>(value, reinterpret_cast<const uint8_t*>(iter)) };
					if JSONIFIER_LIKELY ((resultPtr)) {
						iter += resultPtr - reinterpret_cast<const uint8_t*>(iter);
						value *= -1;
						return true;
					} else {
						return false;
					}
				} else {
					const uint8_t* resultPtr{ parseInteger<false>(value, reinterpret_cast<const uint8_t*>(iter)) };
					if JSONIFIER_LIKELY ((resultPtr)) {
						iter += resultPtr - reinterpret_cast<const uint8_t*>(iter);
						return true;
					} else {
						return false;
					}
				}

			} else {
				auto resultPtr = parseInteger<false>(value, reinterpret_cast<const uint8_t*>(iter));
				if JSONIFIER_LIKELY ((resultPtr)) {
					iter += resultPtr - reinterpret_cast<const uint8_t*>(iter);
					return true;
				} else {
					return false;
				}
			}
		}
	};
}