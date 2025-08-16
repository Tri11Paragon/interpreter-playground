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

#ifndef BLT_ARRAY_H
#define BLT_ARRAY_H

#include <type_traits>
#include <blt/std/types.h>
#include <blt/std/memory_util.h>
#include <stdexcept>
#include <iterator>
#include <memory>
#include "logging.h"

namespace blt
{
    template<typename MetaExtra>
    struct metadata_template_t;
    
    template<>
    struct metadata_template_t<void>
    {
        // size in number of elements!
        blt::size_t size;
        
        explicit metadata_template_t(blt::size_t size): size(size)
        {}
    };
    
    template<typename Extra>
    struct metadata_template_t
    {
        static_assert(std::is_trivially_copyable_v<Extra> && "Must be raw type!");
        Extra extra;
        // size in number of elements!
        blt::size_t size;
        
        explicit metadata_template_t(blt::size_t size): size(size)
        {}
    };
    
    /**
     * @tparam T type to store inside
     * @tparam Extra any extra data to store. void will result in zero size increase.
     */
    template<typename T = void, typename Extra = void>
    class array
    {
        public:
            using iterator = blt::ptr_iterator<T>;
            using const_iterator = blt::ptr_iterator<const T>;
            using reverse_iterator = std::reverse_iterator<iterator>;
            using const_reverse_iterator = std::reverse_iterator<const_iterator>;
        private:
            using metadata_t = metadata_template_t<Extra>;
            metadata_t metadata;
            
            static constexpr blt::size_t ALIGNMENT = std::max(sizeof(metadata_t), alignof(T));
            
            inline T* _data()
            {
                return reinterpret_cast<T*>(reinterpret_cast<blt::u8*>(this) + ALIGNMENT);
            }
            
            /**
             * constructs an array out of a block of memory of size bytes
             * @param size number of bytes available in the memory allocated to this array.
             */
            explicit array(blt::size_t size): metadata((size - sizeof(metadata)) / sizeof(T))
            {}
        
        public:
            inline static array* construct(void* ptr, blt::size_t size)
            {
                auto aligned_ptr = std::align(alignof(array), sizeof(array), ptr, size);
                return new(aligned_ptr) array<T>{size};
            }
            
            array(const array&) = delete;
            
            array(array&&) = delete;
            
            array& operator=(const array&) = delete;
            
            array& operator=(array&&) = delete;
            
            inline T& operator[](blt::size_t index)
            {
                return _data()[index];
            }
            
            inline const T& operator[](blt::size_t index) const
            {
                return _data()[index];
            }
            
            [[nodiscard]] inline T& at(blt::size_t index)
            {
                if (index > size())
                    throw std::runtime_error("Index " + std::to_string(index) += " is outside the bounds of this array!");
                return _data()[index];
            }
            
            [[nodiscard]] inline const T& at(blt::size_t index) const
            {
                if (index > size())
                    throw std::runtime_error("Index " + std::to_string(index) += " is outside the bounds of this array!");
                return _data()[index];
            }
            
            [[nodiscard]] inline T* data()
            {
                return _data();
            }
            
            [[nodiscard]] inline T* data() const
            {
                return _data();
            }
            
            [[nodiscard]] inline blt::size_t size() const
            {
                return metadata.size;
            }
            
            [[nodiscard]] inline blt::size_t size_bytes() const
            {
                return (metadata.size * sizeof(T)) + sizeof(metadata);
            }
            
            constexpr inline T* operator*()
            {
                return data();
            }
            
            constexpr inline T& front()
            {
                return *_data();
            }
            
            constexpr inline const T& front() const
            {
                return *data();
            }
            
            constexpr inline T& back()
            {
                return data()[size() - 1];
            }
            
            constexpr inline const T& back() const
            {
                return data()[size() - 1];
            }
            
            constexpr inline iterator begin() const noexcept
            {
                return data();
            }
            
            constexpr inline iterator end() const noexcept
            {
                return data() + size();
            }
            
            constexpr inline const_iterator cbegin() const noexcept
            {
                return data();
            }
            
            constexpr inline const_iterator cend() const noexcept
            {
                return data() + size();
            }
            
            constexpr inline reverse_iterator rbegin() const noexcept
            {
                return reverse_iterator{end()};
            }
            
            constexpr inline reverse_iterator rend() const noexcept
            {
                return reverse_iterator{begin()};
            }
            
            constexpr inline const_iterator crbegin() const noexcept
            {
                return const_reverse_iterator{cend()};
            }
            
            constexpr inline reverse_iterator crend() const noexcept
            {
                return reverse_iterator{cbegin()};
            }
            
            constexpr inline metadata_t& get_metadata()
            {
                return metadata;
            }
            
            constexpr inline const metadata_t& get_metadata() const
            {
                return metadata;
            }
            
            ~array() = default;
    };
    
}

#endif //BLT_ARRAY_H
