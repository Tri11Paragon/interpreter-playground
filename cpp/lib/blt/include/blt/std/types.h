/*
 *  <Short Description>
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

#ifndef BLT_TYPES_H
#define BLT_TYPES_H

#include <cstdint>
#include <cstddef>
#include <blt/std/defines.h>

#ifndef NO_BLT_NAMESPACE_ON_TYPES
namespace blt
{
#endif
    using i8 = std::int8_t;
    using i16 = std::int16_t;
    using i32 = std::int32_t;
    using i64 = std::int64_t;
    
    using u8 = std::uint8_t;
    using u16 = std::uint16_t;
    using u32 = std::uint32_t;
    using u64 = std::uint64_t;
    
    using size_t = std::size_t;
    using ptrdiff_t = std::ptrdiff_t;
    using f32 = float;
    using f64 = double;
#ifndef NO_BLT_NAMESPACE_ON_TYPES
}
#endif

namespace blt
{
    template<typename T>
    struct integer_type
    {
        using value_type = T;
        
        T id;
        
        integer_type() = default;
        
        integer_type(T id): id(id) // NOLINT
        {}
        
        inline operator T() const // NOLINT
        {
            return id;
        }
        
        friend bool operator==(const integer_type<T>& a, const integer_type<T>& b)
        {
            return a.id == b.id;
        }
        
        friend bool operator!=(const integer_type<T>& a, const integer_type<T>& b)
        {
            return a.id != b.id;
        }
        
        friend bool operator<(const integer_type<T>& a, const integer_type<T>& b)
        {
            return a.id < b.id;
        }
        
        friend bool operator>(const integer_type<T>& a, const integer_type<T>& b)
        {
            return a.id > b.id;
        }
    };
}

#endif //BLT_TYPES_H
