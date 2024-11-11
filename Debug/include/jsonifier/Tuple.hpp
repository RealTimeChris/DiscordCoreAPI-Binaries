/*
	MIT License	

	Copyright (c) 2024 RealTimeChris

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
/// Feb 3, 2023
#pragma once

#include <type_traits>
#include <cstdint>
#include <utility>

namespace jsonifier_internal {

	template<typename value_type> using identity_t = value_type;

	template<typename value_type> using type_t = typename value_type::type;

	template<size_t I> using tag = std::integral_constant<size_t, I>;

	template<size_t I> constexpr tag<I> tag_v{};

	template<size_t N> using tag_range = std::make_index_sequence<N>;

	template<typename tup> using base_list_t	 = typename std::decay_t<tup>::base_list;

	template<typename tuple>
	concept base_list_tuple = requires() { typename std::decay_t<tuple>::base_list; };

	template<typename value_type>
	concept stateless = std::is_empty_v<std::decay_t<value_type>>;

	template<typename value_type>
	concept indexable = stateless<value_type> || requires(value_type t) { t[tag<0>()]; };

	template<typename... value_type> struct tuple;

	template<typename... value_type> struct type_list {};

	template<typename... types> struct tuple_size;

	template<typename... types> struct tuple_size<tuple<types...>> {
		static constexpr size_t value = sizeof...(types);
	};

	template<typename... types> struct tuple_size<std::tuple<types...>> {
		static constexpr size_t value = sizeof...(types);
	};

	template<typename value_type> constexpr size_t tuple_size_v = tuple_size<std::remove_cvref_t<value_type>>::value;

	template<typename... Ls, typename... Rs> constexpr auto operator+(type_list<Ls...>, type_list<Rs...>) {
		return type_list<Ls..., Rs...>{};
	}

	template<typename... bases> struct type_map : bases... {
		using base_list = type_list<bases...>;
		using bases::operator[]...;
	};

	template<typename A, typename... value_type> struct get_tuple_base;

	template<typename first, typename> using first_t = first;

	template<typename value_type, typename... Q> constexpr auto repeatType(type_list<Q...>) {
		return type_list<first_t<value_type, Q>...>{};
	}

	template<typename... outer> constexpr auto getOuterBases(type_list<outer...>) {
		return (repeatType<outer>(base_list_t<type_t<outer>>{}) + ...);
	}

	template<typename... outer> constexpr auto getInnerBases(type_list<outer...>) {
		return (base_list_t<type_t<outer>>{} + ...);
	}

	template<typename value_type, typename... outer, typename... inner> constexpr auto catImpl([[maybe_unused]] value_type tup, type_list<outer...>, type_list<inner...>) -> tuple<type_t<inner>...> {
		return { { { static_cast<type_t<outer>&&>(tup.identity_t<outer>::value).identity_t<inner>::value }... } };
	}

	template<size_t I, typename value_type> struct tuple_elem {
		using type = value_type;

		value_type value;

		constexpr decltype(auto) operator[](tag<I>) & {
			return (value);
		}
		constexpr decltype(auto) operator[](tag<I>) const& {
			return (value);
		}
		constexpr decltype(auto) operator[](tag<I>) && {
			return (std::move(*this).value);
		}
	};

	template<size_t... I, typename... value_type> struct get_tuple_base<std::index_sequence<I...>, value_type...> {
		using type = type_map<tuple_elem<I, value_type>...>;
	};

	template<typename... value_type> using tuple_base_t = typename get_tuple_base<tag_range<sizeof...(value_type)>, value_type...>::type;

	template<typename... value_type> struct tuple : tuple_base_t<value_type...> {
		static constexpr size_t N = sizeof...(value_type);
		using super							= tuple_base_t<value_type...>;
		using super::operator[];
		using base_list	   = typename super::base_list;
	};

	template<> struct tuple<> : tuple_base_t<> {
		static constexpr size_t N = 0;
		using super				  = tuple_base_t<>;
		using base_list			  = type_list<>;
	};
	template<typename... types> tuple(types...) -> tuple<std::unwrap_ref_decay_t<types>...>; 

	template<size_t I, indexable tup> JSONIFIER_ALWAYS_INLINE constexpr decltype(auto) get(tup&& tuple) {
		return static_cast<tup&&>(tuple)[tag<I>()];
	}

	template<base_list_tuple... value_type> constexpr auto tupleCat(value_type&&... ts) {
		if constexpr (sizeof...(value_type) == 0) {
			return tuple<>();
		} else {
#if !defined(TUPLET_CAT_BY_FORWARDING_TUPLE)
	#if defined(__clang__)
		#define TUPLET_CAT_BY_FORWARDING_TUPLE 0
	#else
		#define TUPLET_CAT_BY_FORWARDING_TUPLE 1
	#endif
#endif
#if TUPLET_CAT_BY_FORWARDING_TUPLE
			using big_tuple = tuple<value_type&&...>;
#else
			using big_tuple = tuple<std::decay_t<value_type>...>;
#endif
			using outer_bases	 = base_list_t<big_tuple>;
			constexpr auto outer = getOuterBases(outer_bases{});
			constexpr auto inner = getInnerBases(outer_bases{});
			return catImpl(big_tuple{ { { std::forward<value_type>(ts) }... } }, outer, inner);
		}
	}

	template<typename... types> constexpr auto makeTuple(types&&... args) {
		return tuple<std::unwrap_ref_decay_t<types>...>{ { { std::forward<types>(args) }... } };
	}

}