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

#ifndef BLT_META_TYPE_TRAITS_H
#define BLT_META_TYPE_TRAITS_H

#include <type_traits>
#include <tuple>

namespace blt::meta
{
    namespace detail
    {
        template<typename... Args>
        void empty_apply_function(Args...)
        {

        }

        inline auto lambda = [](auto...)
        {
        };
    }

    template <typename T>
    using remove_cvref_t = std::remove_volatile_t<std::remove_const_t<std::remove_reference_t<T>>>;

    template <typename U>
    using add_const_ref_t = std::conditional_t<std::is_reference_v<U>, const std::remove_reference_t<U>&, const U>;

    template <typename>
    struct is_tuple : std::false_type
    {
    };

    template <typename... T>
    struct is_tuple<std::tuple<T...>> : std::true_type
    {
    };

    template <typename>
    struct is_pair : std::false_type
    {
    };

    template <typename T, typename G>
    struct is_pair<std::pair<T, G>> : std::true_type
    {
    };

    template <typename T>
    static constexpr bool is_tuple_v = is_tuple<T>::value;

    template <typename T>
    static constexpr bool is_pair_v = is_pair<T>::value;
}

#endif // BLT_META_TYPE_TRAITS_H
