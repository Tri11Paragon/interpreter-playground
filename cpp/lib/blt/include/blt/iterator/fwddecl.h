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

#ifndef BLT_ITERATOR_FWDDECL_H
#define BLT_ITERATOR_FWDDECL_H

#include <utility>

namespace blt
{
    template<typename... Iter>
    class zip_iterator_container;
    
    template<typename Iter>
    class enumerate_iterator_container;
    
    namespace iterator
    {
        template<typename Iter>
        struct iterator_pair;
        
        template<typename T>
        using enumerate_item = std::pair<size_t, T>;
        
        template<typename Iter>
        class enumerate_wrapper;
        
        template<typename... Iter>
        struct zip_wrapper;
        
        template<typename Iter, typename Func>
        class map_wrapper;
        
        template<typename Iter, typename Pred>
        class filter_wrapper;

        template<typename Iter, bool Recursive>
        class flatten_wrapper;

        template<typename Iter>
        class const_wrapper;
        
        namespace impl
        {
            template<typename Derived>
            class skip_t;
            
            template<typename Derived>
            class take_t;
        }
    }
}

#endif //BLT_ITERATOR_FWDDECL_H
