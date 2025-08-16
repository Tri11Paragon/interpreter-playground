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

#ifndef BLT_META_IS_STREAMABLE_H
#define BLT_META_IS_STREAMABLE_H

#include <ostream>

namespace blt::meta
{
    // https://stackoverflow.com/questions/66397071/is-it-possible-to-check-if-overloaded-operator-for-type-or-class-exists
    template<typename T>
    class is_streamable
    {
    private:
        template<typename Subs>
        static auto test(int) -> decltype(std::declval<std::ostream&>() << std::declval<Subs>(), std::true_type())
        {
            return std::declval<std::true_type>();
        }

        template<typename>
        static auto test(...) -> std::false_type
        {
            return std::declval<std::false_type>();
        }

    public:
        static constexpr bool value = decltype(test<T>(0))::value;
    };

    template<class T>
    inline constexpr bool is_streamable_v = is_streamable<T>::value;
}

#endif //BLT_META_IS_STREAMABLE_H
