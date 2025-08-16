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

#ifndef BLT_ITERATOR_ENUMERATE_H
#define BLT_ITERATOR_ENUMERATE_H

#include <blt/iterator/common.h>

namespace blt
{
    namespace iterator
    {
        template <typename Iter>
        class enumerate_wrapper : public passthrough_wrapper<Iter, enumerate_wrapper<Iter>>
        {
        public:
            enumerate_wrapper(const size_t index, Iter iter): passthrough_wrapper<Iter, enumerate_wrapper<Iter>>(std::move(iter)), index(index)
            {
            }

            using iterator_category = typename std::iterator_traits<Iter>::iterator_category;
            using value_type = enumerate_item<meta::deref_return_t<Iter>>;
            using difference_type = blt::ptrdiff_t;
            using pointer = value_type;
            using reference = value_type;

            enumerate_item<meta::deref_return_t<Iter>> operator*() const
            {
                return {index, *this->iter};
            }

            enumerate_wrapper& operator++()
            {
                ++index;
                ++this->iter;
                return *this;
            }

            enumerate_wrapper& operator--()
            {
                --index;
                --this->iter;
                return *this;
            }

            friend enumerate_wrapper operator+(const enumerate_wrapper& a, blt::ptrdiff_t n)
            {
                static_assert(meta::is_random_access_iterator_v<Iter>, "Iterator must allow random access");
                auto copy = a;
                copy.index += n;
                copy.iter = copy.iter + n;
                return copy;
            }

            friend enumerate_wrapper operator-(const enumerate_wrapper& a, blt::ptrdiff_t n)
            {
                static_assert(meta::is_random_access_iterator_v<Iter>, "Iterator must allow random access");
                auto copy = a;
                copy.index -= n;
                copy.iter = copy.iter - n;
                return copy;
            }

        private:
            blt::size_t index;
        };
    }

    template <typename Iter>
    class enumerate_iterator_container : public blt::iterator::iterator_container<blt::iterator::enumerate_wrapper<Iter>>
    {
    public:
        using blt::iterator::iterator_container<blt::iterator::enumerate_wrapper<Iter>>::iterator_container;

        enumerate_iterator_container(Iter begin, Iter end, blt::size_t size):
            blt::iterator::iterator_container<blt::iterator::enumerate_wrapper<Iter>>(
                blt::iterator::enumerate_wrapper<Iter>{0, std::move(begin)}, blt::iterator::enumerate_wrapper<Iter>{size, std::move(end)})
        {
        }
    };

    template <typename Iter>
    enumerate_iterator_container(Iter, Iter, blt::size_t) -> enumerate_iterator_container<Iter>;

    template <typename T>
    static inline auto enumerate(T& container)
    {
        return enumerate_iterator_container{
            container.begin(), container.end(),
            static_cast<blt::size_t>(std::distance(container.begin(), container.end()))
        };
    }

    template <typename T>
    static inline auto enumerate(const T& container)
    {
        return enumerate_iterator_container{
            container.begin(), container.end(),
            static_cast<blt::size_t>(std::distance(container.begin(), container.end()))
        };
    }

    template <typename T, blt::size_t size>
    static inline auto enumerate(const T (&container)[size])
    {
        return enumerate_iterator_container{&container[0], &container[size], size};
    }

    template <typename T, blt::size_t size>
    static inline auto enumerate(T (&container)[size])
    {
        return enumerate_iterator_container{&container[0], &container[size], size};
    }
}

#endif //BLT_ITERATOR_ENUMERATE_H
