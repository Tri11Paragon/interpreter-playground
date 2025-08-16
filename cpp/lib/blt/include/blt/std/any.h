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

#include <cstring>

#ifndef BLT_ANY_H
#define BLT_ANY_H

#include <any>
#include <blt/std/types.h>

namespace blt::unsafe
{
    class any_t_union
    {
        private:
            static constexpr auto SIZE = sizeof(std::any);
            
            union variant_t
            {
                constexpr variant_t()
                {}
                
                blt::u8 data[SIZE]{};
                std::any any;
                
                ~variant_t()
                {}
            };
            
            variant_t variant;
            bool has_any = false;
        public:
            any_t_union() = default;
            
            any_t_union(const any_t_union& copy)
            {
                if (copy.has_any)
                {
                    variant.any = copy.variant.any;
                    has_any = true;
                } else
                {
                    std::memcpy(variant.data, copy.variant.data, SIZE);
                }
            }
            
            any_t_union(any_t_union&& move) noexcept
            {
                if (move.has_any)
                {
                    variant.any = std::move(move.variant.any);
                    has_any = true;
                } else
                {
                    std::memcpy(variant.data, move.variant.data, SIZE);
                }
            }
            
            ~any_t_union()
            {
                if (has_any)
                    variant.any.~any();
            }
            
            template<typename T>
            any_t_union(T t)
            {
                if constexpr (sizeof(T) <= SIZE && std::is_trivially_copyable_v<T>)
                {
                    std::memcpy(variant.data, &t, sizeof(t));
                } else
                {
                    variant.any = t;
                    has_any = true;
                }
            }
            
            any_t_union& operator=(const any_t_union& copy)
            {
                if (has_any)
                    variant.any.~any();
                if (copy.has_any)
                {
                    std::memset(variant.data, 0, SIZE);
                    variant.any = copy.variant.any;
                    has_any = true;
                } else
                {
                    std::memcpy(variant.data, copy.variant.data, SIZE);
                    has_any = false;
                }
                return *this;
            }
            
            any_t_union& operator=(any_t_union&& move) noexcept
            {
                if (has_any)
                    variant.any.~any();
                if (move.has_any)
                {
                    std::memset(variant.data, 0, SIZE);
                    variant.any = std::move(move.variant.any);
                    has_any = true;
                } else
                {
                    std::memcpy(variant.data, move.variant.data, SIZE);
                    has_any = false;
                }
                return *this;
            }
            
            template<typename T>
            any_t_union& operator=(T t)
            {
                if (has_any)
                    variant.any.~any();
                if constexpr (sizeof(T) <= SIZE && std::is_trivially_copyable_v<T>)
                {
                    std::memcpy(variant.data, &t, sizeof(t));
                    has_any = false;
                } else
                {
                    std::memset(variant.data, 0, SIZE);
                    variant.any = std::move(t);
                    has_any = true;
                }
                return *this;
            }
            
            template<typename T>
            T any_cast()
            {
                if constexpr (sizeof(T) <= SIZE && std::is_trivially_copyable_v<T>)
                {
                    if (!has_any)
                    {
                        T t;
                        std::memcpy(&t, variant.data, sizeof(T));
                        return t;
                    }
                }
                return std::any_cast<T>(variant.any);
            }
    };
    
    class buffer_any_t
    {
        private:
            blt::u8* _data;
        public:
            explicit buffer_any_t(blt::u8* data): _data(data)
            {}
            
            template<typename T>
            void set(const T& t) const
            {
                static_assert(std::is_trivially_copyable_v<T> && "Type must be trivially copyable");
                std::memcpy(_data, &t, sizeof(t));
            }
            
            template<typename T>
            T any_cast() const
            {
                static_assert(std::is_trivially_copyable_v<T> && "Type must be trivially copyable");
                T t;
                std::memcpy(&t, _data, sizeof(T));
                return t;
            }
    };
    
    template<blt::size_t SIZE>
    class any_t_base
    {
        private:
            blt::u8 data[SIZE]{};
        public:
            any_t_base() = default;
            
            template<typename T>
            any_t_base(T t)
            {
                static_assert(std::is_trivially_copyable_v<T> && "Type must be byte copyable");
                static_assert(sizeof(T) <= SIZE && "Size must be less than or equal to internal buffer");
                std::memcpy(data, &t, sizeof(t));
            }
            
            template<typename T>
            any_t_base& operator=(T t)
            {
                static_assert(std::is_trivially_copyable_v<T> && "Type must be byte copyable");
                static_assert(sizeof(T) <= SIZE && "Size must be less than or equal to internal buffer");
                std::memcpy(data, &t, sizeof(t));
                return *this;
            }
            
            template<typename T>
            T any_cast()
            {
                static_assert(std::is_trivially_copyable_v<T> && "Type must be byte copyable");
                static_assert(sizeof(T) <= SIZE && "Size must be less than or equal to internal buffer");
                T t;
                std::memcpy(&t, data, sizeof(T));
                return t;
            }
    };
    
    using any_t = any_t_base<8>;
}

#endif //BLT_ANY_H
