#pragma once
/*
 *  Copyright (C) 2024  Brett Terpstra
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef BLT_META_FILTER_VOID_H
#define BLT_META_FILTER_VOID_H

#include <tuple>
#include <type_traits>

namespace blt::meta
{
	template <template<typename...> typename Func, typename Tuple>
	struct apply_tuple;

	template <template<typename...> typename Func, typename... Values>
	struct apply_tuple<Func, std::tuple<Values...>>
	{
		using type = Func<Values...>;
	};

	template <template<typename...> typename Func, typename Tuple>
	using apply_tuple_t = typename apply_tuple<Func, Tuple>::type;

	template <typename... Ts>
	struct filter_void;

	template <>
	struct filter_void<>
	{
		using type = std::tuple<>;
	};

	template <typename T, typename... Ts>
	struct filter_void<T, Ts...>
	{
		using type = std::conditional_t<std::is_void_v<T>, typename filter_void<Ts...>::type, decltype(std::tuple_cat(
											std::declval<std::tuple<T>>(), std::declval<typename filter_void<Ts...>::type>()))>;
	};

	template <typename... Ts>
	struct filter_void<std::tuple<Ts...>>
	{
		using type = typename filter_void<Ts...>::type;
	};

	template <typename... Ts>
	using filter_void_t = typename filter_void<Ts...>::type;

	template <template<typename...> typename Func, typename ArgsTuple, typename... Ts>
	struct filter_func;

	template <template<typename...> typename Func, typename ArgsTuple>
	struct filter_func<Func, ArgsTuple>
	{
		using type = std::tuple<>;
	};

	template <template<typename...> typename Func, typename ArgsTuple, typename T, typename... Ts>
	struct filter_func<Func, ArgsTuple, T, Ts...>
	{
		using ArgsTupleWithT = decltype(std::tuple_cat(std::declval<ArgsTuple>(), std::declval<std::tuple<T>>()));
		using type = std::conditional_t<apply_tuple_t<Func, ArgsTupleWithT>::value, decltype( std::tuple_cat(
											std::declval<std::tuple<T>>(),
											std::declval<typename filter_func<Func, ArgsTuple, Ts...>::type>())), typename filter_func<
											Func, ArgsTuple, Ts...>::type>;
	};

	template <template<typename...> typename Func, typename ArgsTuple, typename... Ts>
	struct filter_func<Func, ArgsTuple, std::tuple<Ts...>>
	{
		using type = typename filter_func<Func, ArgsTuple, Ts...>::type;
	};

	template <template<typename...> typename Func, typename ArgsTuple, typename... Ts>
	using filter_func_t = typename filter_func<Func, ArgsTuple, Ts...>::type;

	template <typename...>
	struct tuple_contains;

	template <typename T>
	struct tuple_contains<T, std::tuple<>> : std::false_type
	{};

	template <typename T, typename T1, typename... Ts>
	struct tuple_contains<T, std::tuple<T1, Ts...>> : std::conditional_t<
		std::is_same_v<T, T1> || tuple_contains<T, std::tuple<Ts...>>::value, std::true_type, std::false_type>
	{};

	template <typename T, typename Tuple>
	constexpr static bool tuple_contains_v = tuple_contains<T, Tuple>::value;

	template <typename... Ts>
	struct unique_types;

	template <>
	struct unique_types<>
	{
		using type = std::tuple<>;
	};

	template <typename T>
	struct unique_types<T>
	{
		using type = std::tuple<T>;
	};

	template <typename T, typename... Rest>
	struct unique_types<T, Rest...>
	{
		using rest_unique = typename unique_types<Rest...>::type;
		using type = std::conditional_t<tuple_contains_v<T, rest_unique>, rest_unique, decltype(std::tuple_cat(
											std::declval<rest_unique>(), std::declval<std::tuple<T>>()))>;
	};

	template <typename Tuple>
	struct unique_tuple;

	template <typename... Ts>
	struct unique_tuple<std::tuple<Ts...>>
	{
		using type = typename unique_types<Ts...>::type;
	};

	template <typename Tuple>
	using unique_tuple_t = typename unique_tuple<Tuple>::type;
}

#endif //BLT_META_FILTER_VOID_H
