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

#ifndef BLT_ITERATOR_H
#define BLT_ITERATOR_H

#include <blt/iterator/common.h>
#include <blt/iterator/zip.h>
#include <blt/iterator/enumerate.h>
#include <blt/iterator/flatten.h>
#include <type_traits>
#include <iterator>
#include <tuple>

namespace blt
{
    
    template<typename T>
    static inline auto iterate(T begin, T end)
    {
        return iterator::iterator_container<T>{std::move(begin), std::move(end)};
    }
    
    template<typename T>
    static inline auto iterate(T& container)
    {
        return iterator::iterator_container<decltype(container.begin())>{container.begin(), container.end()};
    }
    
    template<typename T>
    static inline auto iterate(const T& container)
    {
        return iterator::iterator_container<decltype(container.begin())>{container.begin(), container.end()};
    }
    
    template<typename T, blt::size_t size>
    static inline auto iterate(const T(& container)[size])
    {
        return iterator::iterator_container<decltype(container.begin())>{&container[0], &container[size]};
    }
    
    template<typename T, blt::size_t size>
    static inline auto iterate(T(& container)[size])
    {
        return iterator::iterator_container<decltype(container.begin())>{&container[0], &container[size]};
    }

}

#endif //BLT_ITERATOR_H
