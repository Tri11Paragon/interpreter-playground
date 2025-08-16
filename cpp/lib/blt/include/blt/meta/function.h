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

#ifndef BLT_META_FUNCTION_H
#define BLT_META_FUNCTION_H

#include <functional>
#include <tuple>

namespace blt::meta
{
	struct std_function_tag
	{};

	struct function_ptr_tag
	{};

	struct lambda_tag
	{};

	struct member_function_ptr_tag
	{};

	template <typename Func>
	struct function_like;

	template <typename Return, typename... Args>
	struct function_like<std::function<Return(Args...)>>
	{
		using return_type = Return;
		using args_tuple = std::tuple<Args...>;
		using tag = std_function_tag;
	};

	template <typename Return, typename... Args>
	struct function_like<Return (*)(Args...)>
	{
		using return_type = Return;
		using args_tuple = std::tuple<Args...>;
		using tag = function_ptr_tag;
	};

	template <typename Return, typename... Args>
	struct function_like<Return (*)(Args...) noexcept>
	{
		using return_type = Return;
		using args_tuple = std::tuple<Args...>;
		using tag = function_ptr_tag;
	};

	template <typename Return, typename Class, typename... Args>
	struct function_like<Return (Class::*)(Args...)>
	{
		using return_type = Return;
		using args_tuple = std::tuple<Args...>;
		using tag = member_function_ptr_tag;
		using class_type = Class;
	};

	template <typename Return, typename Class, typename... Args>
	struct function_like<Return (Class::*)(Args...) const>
	{
		using return_type = Return;
		using args_tuple = std::tuple<Args...>;
		using tag = member_function_ptr_tag;
		using class_type = Class;
	};

	template <typename Return, typename Class, typename... Args>
	struct function_like<Return (Class::*)(Args...) noexcept>
	{
		using return_type = Return;
		using args_tuple = std::tuple<Args...>;
		using tag = member_function_ptr_tag;
		using class_type = Class;
	};

	template <typename Return, typename Class, typename... Args>
	struct function_like<Return (Class::*)(Args...) const noexcept>
	{
		using return_type = Return;
		using args_tuple = std::tuple<Args...>;
		using tag = member_function_ptr_tag;
		using class_type = Class;
	};

	template <typename>
	struct lambda_traits
	{};

	template <typename Return, typename Class, typename... Args>
	struct lambda_traits<Return (Class::*)(Args...) const>
	{
		using return_type = Return;
		using args_tuple = std::tuple<Args...>;
		using class_type = Class;
	};

	template <typename Return, typename Class, typename... Args>
	struct lambda_traits<Return (Class::*)(Args...) noexcept>
	{
		using return_type = Return;
		using args_tuple = std::tuple<Args...>;
		using class_type = Class;
	};

	template <typename Return, typename Class, typename... Args>
	struct lambda_traits<Return (Class::*)(Args...) const noexcept>
	{
		using return_type = Return;
		using args_tuple = std::tuple<Args...>;
		using class_type = Class;
	};

	template <typename Return, typename Class, typename... Args>
	struct lambda_traits<Return (Class::*)(Args...)>
	{
		using return_type = Return;
		using args_tuple = std::tuple<Args...>;
		using class_type = Class;
	};

	template <typename Func>
	struct function_like
	{
	private:
		using lambda_trait = lambda_traits<decltype(&std::decay_t<Func>::operator())>;
	public:
		using tag_type = lambda_tag;
		using return_type = typename lambda_trait::return_type;
		using args_tuple = typename lambda_trait::args_tuple;
		using class_type = typename lambda_trait::class_type;
	};
}

#endif //BLT_META_FUNCTION_H
