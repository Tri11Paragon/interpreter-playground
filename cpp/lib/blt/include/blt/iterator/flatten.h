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

#ifndef BLT_ITERATOR_FLATTEN_H
#define BLT_ITERATOR_FLATTEN_H

#include <blt/iterator/common.h>
#include <blt/meta/type_traits.h>
#include <tuple>

namespace blt::iterator
{
    namespace detail
    {
        template <typename Tuple>
        static auto flatten_recursive(Tuple&& tuple) -> decltype(auto)
        {
            using Decay = std::decay_t<Tuple>;
            if constexpr (meta::is_tuple_v<Decay> || meta::is_pair_v<Decay>)
            {
                return std::apply([](auto&&... args)
                {
                    return std::tuple_cat(flatten_recursive(std::forward<decltype(args)>(args))...);
                }, std::forward<Tuple>(tuple));
            }
            else
            {
                return forward_as_tuple(std::forward<Tuple>(tuple));
            }
        }

        template <typename Tuple>
        static auto flatten(Tuple&& tuple) -> decltype(auto)
        {
            return std::apply([](auto&&... args)
            {
                return std::tuple_cat(ensure_tuple(std::forward<decltype(args)>(args))...);
            }, std::forward<Tuple>(tuple));
        }
    }

    template <typename Iter, bool Recursive>
    class flatten_wrapper : public deref_only_wrapper<Iter, flatten_wrapper<Iter, Recursive>>
    {
    public:
        using iterator_category = typename std::iterator_traits<Iter>::iterator_category;
        using value_type = std::conditional_t<Recursive,
                                              std::remove_reference_t<decltype(detail::flatten_recursive(*std::declval<Iter>()))>,
                                              std::remove_reference_t<decltype(detail::flatten(*std::declval<Iter>()))>>;
        using difference_type = ptrdiff_t;
        using pointer = value_type*;
        using reference = value_type&;

        explicit flatten_wrapper(Iter iter):
            deref_only_wrapper<Iter, flatten_wrapper>(std::move(iter))
        {
        }

        auto operator*() const -> decltype(auto)
        {
            if constexpr (Recursive)
            {
                return detail::flatten_recursive(*this->iter);
            }
            else
            {
                return detail::flatten(*this->iter);
            }
        }
    };
}


#endif //BLT_ITERATOR_FLATTEN_H
