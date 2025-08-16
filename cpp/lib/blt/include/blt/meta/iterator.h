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

#ifndef BLT_META_ITERATOR_H
#define BLT_META_ITERATOR_H

#include <type_traits>
#include <iterator>

namespace blt::meta
{
    template<typename Category>
    struct is_input_iterator_category
    {
        constexpr static bool value = std::is_same_v<Category, std::input_iterator_tag>;
    };
    
    template<typename Category>
    struct is_forward_iterator_category
    {
        constexpr static bool value = std::is_same_v<Category, std::forward_iterator_tag>;
    };
    
    template<typename Category>
    struct is_bidirectional_iterator_category
    {
        constexpr static bool value = std::is_same_v<Category, std::bidirectional_iterator_tag>;
    };
    
    template<typename Category>
    struct is_random_access_iterator_category
    {
        constexpr static bool value = std::is_same_v<Category, std::random_access_iterator_tag>;
    };
    
    template<typename Iter>
    inline constexpr bool is_input_iterator_category_v = is_input_iterator_category<Iter>::value;
    
    template<typename Iter>
    inline constexpr bool is_forward_iterator_category_v = is_forward_iterator_category<Iter>::value;
    
    template<typename Iter>
    inline constexpr bool is_bidirectional_iterator_category_v = is_bidirectional_iterator_category<Iter>::value;
    
    template<typename Iter>
    inline constexpr bool is_random_access_iterator_category_v = is_random_access_iterator_category<Iter>::value;
    
    template<typename Iter>
    inline constexpr bool is_bidirectional_or_better_category_v =
            is_bidirectional_iterator_category_v<Iter> || is_random_access_iterator_category_v<Iter>;
    
    // this is required! :/
    
    
    template<typename Iter>
    struct is_input_iterator
    {
        constexpr static bool value = is_input_iterator_category_v<typename std::iterator_traits<Iter>::iterator_category>;
    };
    
    template<typename Iter>
    struct is_forward_iterator
    {
        constexpr static bool value = is_forward_iterator_category_v<typename std::iterator_traits<Iter>::iterator_category>;
    };
    
    template<typename Iter>
    struct is_bidirectional_iterator
    {
        constexpr static bool value = is_bidirectional_iterator_category_v<typename std::iterator_traits<Iter>::iterator_category>;
    };
    
    template<typename Iter>
    struct is_random_access_iterator
    {
        constexpr static bool value = is_random_access_iterator_category_v<typename std::iterator_traits<Iter>::iterator_category>;
    };
    
    template<typename Iter>
    inline constexpr bool is_input_iterator_v = is_input_iterator<Iter>::value;
    
    template<typename Iter>
    inline constexpr bool is_forward_iterator_v = is_forward_iterator<Iter>::value;
    
    template<typename Iter>
    inline constexpr bool is_bidirectional_iterator_v = is_bidirectional_iterator<Iter>::value;
    
    template<typename Iter>
    inline constexpr bool is_random_access_iterator_v = is_random_access_iterator<Iter>::value;
    
    template<typename Iter>
    inline constexpr bool is_bidirectional_or_better_v = is_bidirectional_iterator_v<Iter> || is_random_access_iterator_v<Iter>;
    
    template<typename... Iter>
    struct lowest_iterator_category
    {
        using type = std::common_type_t<typename std::iterator_traits<Iter>::iterator_category...>;
    };
    
    template<typename... Iter>
    using lowest_iterator_category_t = typename lowest_iterator_category<Iter...>::type;
    
    template<typename Iter>
    struct is_reverse_iterator : std::false_type
    {
    };
    
    template<typename Iter>
    struct is_reverse_iterator<std::reverse_iterator<Iter>> : std::true_type
    {
    };
    
    template<typename Iter>
    inline constexpr bool is_reverse_iterator_v = is_reverse_iterator<Iter>::value;
}

#endif //BLT_META_ITERATOR_H
