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

	template<class T> using identity_t = T;

	// Obtains T::type
	template<class T> using type_t = typename T::type;

	template<size_t I> using tag = std::integral_constant<size_t, I>;

	template<size_t I> constexpr tag<I> tag_v{};

	template<size_t N> using tag_range = std::make_index_sequence<N>;

	template<class T, class U>
	concept same_as = std::is_same_v<T, U> && std::is_same_v<U, T>;

	template<class T, class U>
	concept other_than = !std::is_same_v<std::decay_t<T>, U>;

	template<class Tup> using base_list_t	 = typename std::decay_t<Tup>::base_list;
	template<class Tup> using element_list_t = typename std::decay_t<Tup>::element_list;

	template<class Tuple>
	concept base_list_tuple = requires() { typename std::decay_t<Tuple>::base_list; };

	template<class T>
	concept stateless = std::is_empty_v<std::decay_t<T>>;

	template<class T>
	concept indexable = stateless<T> || requires(T t) { t[tag<0>()]; };

	template<class U, class T>
	concept assignable_to = requires(U u, T t) { t = u; };

	template<class T>
	concept ordered = requires(const T& t) {
		{ t <=> t };
	};
	template<class T>
	concept equality_comparable = requires(const T& t) {
		{ t == t } -> same_as<bool>;
	};

	template<class... T> struct tuple;

	template<class... T> struct type_list {};

	template<typename... types> struct tuple_size;

	template<typename... types> struct tuple_size<tuple<types...>> {
		static constexpr size_t value = sizeof...(types);
	};

	template<typename... types> struct tuple_size<std::tuple<types...>> {
		static constexpr size_t value = sizeof...(types);
	};

	template<typename value_type> constexpr size_t tuple_size_v = tuple_size<std::remove_cvref_t<value_type>>::value;

	template<class... Ls, class... Rs> constexpr auto operator+(type_list<Ls...>, type_list<Rs...>) {
		return type_list<Ls..., Rs...>{};
	}

	template<class... Bases> struct type_map : Bases... {
		using base_list = type_list<Bases...>;
		using Bases::operator[]...;
		using Bases::decl_elem...;
		auto operator<=>(const type_map&) const = default;
		bool operator==(const type_map&) const	= default;
	};

	template<size_t I, class T> struct tuple_elem {
		// Like declval, but with the element
		static T decl_elem(tag<I>);
		using type = T;

		T value;

		constexpr decltype(auto) operator[](tag<I>) & {
			return (value);
		}
		constexpr decltype(auto) operator[](tag<I>) const& {
			return (value);
		}
		constexpr decltype(auto) operator[](tag<I>) && {
			return (std::move(*this).value);
		}
		auto operator<=>(const tuple_elem&) const = default;
		bool operator==(const tuple_elem&) const  = default;
		// Implements comparison for tuples containing reference types
		constexpr auto operator<=>(const tuple_elem& other) const noexcept(noexcept(value <=> other.value))
			requires(std::is_reference_v<T> && ordered<T>)
		{
			return value <=> other.value;
		}
		constexpr bool operator==(const tuple_elem& other) const noexcept(noexcept(value == other.value))
			requires(std::is_reference_v<T> && equality_comparable<T>)
		{
			return value == other.value;
		}
	};
	template<class T> using unwrap_ref_decay_t = typename std::unwrap_ref_decay<T>::type;

	template<class A, class... T> struct get_tuple_base;

	template<size_t... I, class... T> struct get_tuple_base<std::index_sequence<I...>, T...> {
		using type = type_map<tuple_elem<I, T>...>;
	};

	template<class F, class T, class... Bases> constexpr decltype(auto) apply_impl(F&& f, T&& t, type_list<Bases...>) {
		return static_cast<F&&>(f)(static_cast<T&&>(t).identity_t<Bases>::value...);
	}

	template<class First, class> using first_t = First;

	template<class T, class... Q> constexpr auto repeat_type(type_list<Q...>) {
		return type_list<first_t<T, Q>...>{};
	}
	template<class... Outer> constexpr auto get_outer_bases(type_list<Outer...>) {
		return (repeat_type<Outer>(base_list_t<type_t<Outer>>{}) + ...);
	}
	template<class... Outer> constexpr auto get_inner_bases(type_list<Outer...>) {
		return (base_list_t<type_t<Outer>>{} + ...);
	}

	// This takes a forwarding tuple as a parameter. The forwarding tuple only
	// contains references, so it should just be taken by value.
	template<class T, class... Outer, class... Inner> constexpr auto cat_impl([[maybe_unused]] T tup, type_list<Outer...>, type_list<Inner...>) -> tuple<type_t<Inner>...> {
		return { { { static_cast<type_t<Outer>&&>(tup.identity_t<Outer>::value).identity_t<Inner>::value }... } };
	}

	template<class... T> using tuple_base_t = typename get_tuple_base<tag_range<sizeof...(T)>, T...>::type;

	template<class... T> struct tuple : tuple_base_t<T...> {
		static constexpr auto glaze_reflect = false;
		constexpr static size_t N			= sizeof...(T);
		using super							= tuple_base_t<T...>;
		using super::operator[];
		using base_list	   = typename super::base_list;
		using element_list = type_list<T...>;
		using super::decl_elem;

		template<other_than<tuple> U>// Preserves default assignments
		constexpr auto& operator=(U&& tup) {
			using tuple2 = std::decay_t<U>;
			if (base_list_tuple<tuple2>) {
				eq_impl(static_cast<U&&>(tup), base_list(), typename tuple2::base_list());
			} else {
				eq_impl(static_cast<U&&>(tup), tag_range<N>());
			}
			return *this;
		}

		// TODO: currently segfaults clang
		/*template <assignable_to<T>... U>
         constexpr auto& assign(U&&... values) {
            assign_impl(base_list(), static_cast<U&&>(values)...);
            return *this;
         }*/

		auto operator<=>(const tuple&) const = default;
		bool operator==(const tuple&) const	 = default;

		// Applies a function to every element of the tuple. The order is the
		// declaration order, so first the function will be applied to element 0,
		// then element 1, then element 2, and so on, where element N is identified
		// by get<N>
		template<class F> constexpr void for_each(F&& func) & {
			for_each_impl(base_list(), static_cast<F&&>(func));
		}
		template<class F> constexpr void for_each(F&& func) const& {
			for_each_impl(base_list(), static_cast<F&&>(func));
		}
		template<class F> constexpr void for_each(F&& func) && {
			static_cast<tuple&&>(*this).for_each_impl(base_list(), static_cast<F&&>(func));
		}

		// Applies a function to each element successively, until one returns a
		// truthy value. Returns true if any application returned a truthy value,
		// and false otherwise
		template<class F> constexpr bool any(F&& func) & {
			return any_impl(base_list(), static_cast<F&&>(func));
		}
		template<class F> constexpr bool any(F&& func) const& {
			return any_impl(base_list(), static_cast<F&&>(func));
		}
		template<class F> constexpr bool any(F&& func) && {
			return static_cast<tuple&&>(*this).any_impl(base_list(), static_cast<F&&>(func));
		}

		// Applies a function to each element successively, until one returns a
		// falsy value. Returns true if every application returned a truthy value,
		// and false otherwise
		template<class F> constexpr bool all(F&& func) & {
			return all_impl(base_list(), static_cast<F&&>(func));
		}
		template<class F> constexpr bool all(F&& func) const& {
			return all_impl(base_list(), static_cast<F&&>(func));
		}
		template<class F> constexpr bool all(F&& func) && {
			return static_cast<tuple&&>(*this).all_impl(base_list(), static_cast<F&&>(func));
		}

		// Map a function over every element in the tuple, using the values to
		// construct a new tuple
		template<class F> constexpr auto map(F&& func) & {
			return map_impl(base_list(), static_cast<F&&>(func));
		}
		template<class F> constexpr auto map(F&& func) const& {
			return map_impl(base_list(), static_cast<F&&>(func));
		}
		template<class F> constexpr auto map(F&& func) && {
			return static_cast<tuple&&>(*this).map_impl(base_list(), static_cast<F&&>(func));
		}

	  private:
		template<class U, class... B1, class... B2> constexpr void eq_impl(U&& u, type_list<B1...>, type_list<B2...>) {
			// See:
			// https://developercommunity.visualstudio.com/t/fold-expressions-unreliable-in-171-with-c20/1676476
			(void(B1::value = static_cast<U&&>(u).B2::value), ...);
		}
		template<class U, size_t... I> constexpr void eq_impl(U&& u, std::index_sequence<I...>) {
			(void(tuple_elem<I, T>::value = get<I>(static_cast<U&&>(u))), ...);
		}
		template<class... U, class... B> constexpr void assign_impl(type_list<B...>, U&&... u) {
			(void(B::value = static_cast<U&&>(u)), ...);
		}

		template<class F, class... B> constexpr void for_each_impl(type_list<B...>, F&& func) & {
			(void(func(B::value)), ...);
		}
		template<class F, class... B> constexpr void for_each_impl(type_list<B...>, F&& func) const& {
			(void(func(B::value)), ...);
		}
		template<class F, class... B> constexpr void for_each_impl(type_list<B...>, F&& func) && {
			(void(func(static_cast<T&&>(B::value))), ...);
		}

		template<class F, class... B> constexpr bool any_impl(type_list<B...>, F&& func) & {
			return (bool(func(B::value)) || ...);
		}
		template<class F, class... B> constexpr bool any_impl(type_list<B...>, F&& func) const& {
			return (bool(func(B::value)) || ...);
		}
		template<class F, class... B> constexpr bool any_impl(type_list<B...>, F&& func) && {
			return (bool(func(static_cast<T&&>(B::value))) || ...);
		}

		template<class F, class... B> constexpr bool all_impl(type_list<B...>, F&& func) & {
			return (bool(func(B::value)) && ...);
		}
		template<class F, class... B> constexpr bool all_impl(type_list<B...>, F&& func) const& {
			return (bool(func(B::value)) && ...);
		}
		template<class F, class... B> constexpr bool all_impl(type_list<B...>, F&& func) && {
			return (bool(func(static_cast<T&&>(B::value))) && ...);
		}

		template<class F, class... B> constexpr auto map_impl(type_list<B...>, F&& func) & -> tuple<unwrap_ref_decay_t<decltype(func(B::value))>...> {
			return { func(B::value)... };
		}
		template<class F, class... B> constexpr auto map_impl(type_list<B...>, F&& func) const& -> tuple<unwrap_ref_decay_t<decltype(func(B::value))>...> {
			return { func(B::value)... };
		}
		template<class F, class... B> constexpr auto map_impl(type_list<B...>, F&& func) && -> tuple<unwrap_ref_decay_t<decltype(func(static_cast<T&&>(B::value)))>...> {
			return { func(static_cast<T&&>(B::value))... };
		}
	};

	template<> struct tuple<> : tuple_base_t<> {
		constexpr static size_t N = 0;
		using super				  = tuple_base_t<>;
		using base_list			  = type_list<>;
		using element_list		  = type_list<>;

		template<other_than<tuple> U>// Preserves default assignments
			requires stateless<U>// Check that U is similarly stateless
		constexpr auto& operator=(U&&) noexcept {
			return *this;
		}

		constexpr auto& assign() noexcept {
			return *this;
		}
		auto operator<=>(const tuple&) const = default;
		bool operator==(const tuple&) const	 = default;

		// Applies a function to every element of the tuple. The order is the
		// declaration order, so first the function will be applied to element 0,
		// then element 1, then element 2, and so on, where element N is identified
		// by get<N>
		//
		// (Does nothing when invoked on empty tuple)
		template<class F> constexpr void for_each(F&&) const noexcept {
		}

		// Applies a function to each element successively, until one returns a
		// truthy value. Returns true if any application returned a truthy value,
		// and false otherwise
		//
		// (Returns false for empty tuple)
		template<class F> constexpr bool any(F&&) const noexcept {
			return false;
		}

		// Applies a function to each element successively, until one returns a
		// falsy value. Returns true if every application returned a truthy value,
		// and false otherwise
		//
		// (Returns true for empty tuple)
		template<class F> constexpr bool all(F&&) const noexcept {
			return true;
		}

		// Map a function over every element in the tuple, using the values to
		// construct a new tuple
		//
		// (Returns empty tuple when invoked on empty tuple)
		template<class F> constexpr auto map(F&&) const noexcept {
			return tuple{};
		}
	};
	template<class... Ts> tuple(Ts...) -> tuple<unwrap_ref_decay_t<Ts>...>;

	// glz::get implementation
	// glz::tie implementation
	// glz::apply implementation
	template<size_t I, indexable Tup> JSONIFIER_ALWAYS_INLINE constexpr decltype(auto) get(Tup&& tup) {
		return static_cast<Tup&&>(tup)[tag<I>()];
	}

	template<base_list_tuple... T> constexpr auto tupleCat(T&&... ts) {
		if constexpr (sizeof...(T) == 0) {
			return tuple<>();
		} else {
			/**
             * It appears that Clang produces better assembly when
             * TUPLET_CAT_BY_FORWARDING_TUPLE == 0, while GCC produces better assembly when
             * TUPLET_CAT_BY_FORWARDING_TUPLE == 1. MSVC always produces terrible assembly
             * in either case. This will set TUPLET_CAT_BY_FORWARDING_TUPLE to the correct
             * value (0 for clang, 1 for everyone else)
             *
             * See: https://github.com/codeinred/tuplet/discussions/14
             */
#if !defined(TUPLET_CAT_BY_FORWARDING_TUPLE)
	#if defined(__clang__)
		#define TUPLET_CAT_BY_FORWARDING_TUPLE 0
	#else
		#define TUPLET_CAT_BY_FORWARDING_TUPLE 1
	#endif
#endif
#if TUPLET_CAT_BY_FORWARDING_TUPLE
			using big_tuple = tuple<T&&...>;
#else
			using big_tuple = tuple<std::decay_t<T>...>;
#endif
			using outer_bases	 = base_list_t<big_tuple>;
			constexpr auto outer = get_outer_bases(outer_bases{});
			constexpr auto inner = get_inner_bases(outer_bases{});
			return cat_impl(big_tuple{ { { std::forward<T>(ts) }... } }, outer, inner);
		}
	}

	template<typename... Ts> constexpr auto makeTuple(Ts&&... args) {
		return tuple<unwrap_ref_decay_t<Ts>...>{ { { std::forward<Ts>(args) }... } };
	}

}