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

#ifndef BLT_META_H
#define BLT_META_H

#include <blt/std/types.h>
#include <utility>
#include <type_traits>
#include <ostream>
#include <blt/meta/is_streamable.h>

namespace blt::meta
{
    template<typename IFirst = std::void_t<void>, typename... IArgs>
    struct arg_helper
    {
        using First = IFirst;
        using Next = arg_helper<IArgs...>;
    };
    
    template<typename IFirst>
    struct arg_helper<IFirst>
    {
        using First = IFirst;
        using Next = void;
    };
    
    template<>
    struct arg_helper<>
    {
        using First = void;
        using Next = void;
    };
    
    template<typename TheLambda, typename>
    struct lambda_helper
    {
        using Lambda = TheLambda;
    };
    
    template<typename TheLambda, typename IReturn, typename IClass, typename... LArgs>
    struct lambda_helper<TheLambda, IReturn (IClass::*)(LArgs...) const>
    {
        using Lambda = TheLambda;
        using Return = IReturn;
        using Class = IClass;
        using Args = arg_helper<LArgs...>;
        
        template<typename T>
        explicit lambda_helper(T)
        {}
        
        lambda_helper() = default;
    };
    
    template<typename Lambda>
    lambda_helper(Lambda) -> lambda_helper<Lambda, decltype(&Lambda::operator())>;
    
    template<typename T>
    struct arrow_return
    {
        using type = typename std::invoke_result_t<decltype(&T::operator->), T*>;
    };
    
    template<typename T>
    struct arrow_return<T*>
    {
        using type = T*;
    };
    
    // gets the return type for arrow operator
    template<typename T>
    using arrow_return_t = typename arrow_return<T>::type;
    
    template<typename T>
    struct deref_return
    {
        using type = std::invoke_result_t<decltype(&T::operator*), T&>;
    };
    
    template<typename T>
    struct deref_return<T*>
    {
        using type = T&;
    };
    
    // gets the return type for the reference operator
    template<typename T>
    using deref_return_t = typename deref_return<T>::type;

#define BLT_META_MAKE_FUNCTION_CHECK(FUNC, ...)\
    template<typename T, typename = void> \
    class has_func_##FUNC : public std::false_type \
    {}; \
    template<typename T> \
    class has_func_##FUNC<T, std::void_t<decltype(std::declval<T>().FUNC(,##__VA_ARGS__))>> : public std::true_type \
    {}; \
    template<typename T> \
    inline constexpr bool has_func_##FUNC##_v = has_func_##FUNC<T>::value;

#define BLT_META_MAKE_STATIC_FUNCTION_CHECK(FUNC, ...)\
    template<typename T, typename = void> \
    class has_static_func_##FUNC : public std::false_type \
    {}; \
    template<typename T> \
    class has_static_func_##FUNC<T, std::void_t<decltype(T::FUNC(,##__VA_ARGS__))>> : public std::true_type \
    {}; \
    template<typename T> \
    inline constexpr bool has_static_func_##FUNC##_v = has_static_func_##FUNC<T>::value;

#define BLT_META_MAKE_MEMBER_CHECK(MEMBER)\
    template<typename T, typename = void> \
    class has_member_##MEMBER : public std::false_type \
    {}; \
    template<typename T> \
    class has_member_##MEMBER<T, std::void_t<decltype(T::MEMBER)>> : public std::true_type \
    {}; \
    template<typename T> \
    inline constexpr bool has_member_##MEMBER##_v = has_member_##MEMBER<T>::value;
    
    
}

#endif //BLT_GP_META_H
