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

#ifndef BLT_MMAP_H
#define BLT_MMAP_H

#include <blt/std/types.h>
#include <cstdlib>
#include <exception>
#include <string>
#include <string_view>

// size of 2mb in bytes
inline constexpr blt::size_t BLT_2MB_SIZE = 2048 * 1024;
inline constexpr blt::size_t BLT_1GB_SIZE = 1048576 * 1024;

namespace blt
{
    
    enum class huge_page_t : blt::u64
    {
        BLT_2MB_PAGE,
        BLT_1GB_PAGE
    };
    
    class bad_alloc_t : public std::exception
    {
        public:
            bad_alloc_t() = default;
            
            explicit bad_alloc_t(const std::string_view str): str(str)
            {}
            
            explicit bad_alloc_t(std::string str): str(std::move(str))
            {}
            
            [[nodiscard]] const char* what() const noexcept override
            {
                return str.c_str();
            }
        
        private:
            std::string str;
    };
    
    /**
    * Logging function used for handling mmap errors. call after a failed mmap call.
    */
    std::string handle_mmap_error();
    
    inline static constexpr blt::size_t align_size_to(blt::size_t size, blt::size_t align)
    {
        const blt::size_t MASK = ~(align - 1);
        return (size & MASK) + align;
    }
    
    void* allocate_huge_pages(huge_page_t page_type, blt::size_t bytes);
    
    void mmap_free(void* ptr, blt::size_t bytes);
    
    class mmap_huge_allocator
    {
        public:
            explicit mmap_huge_allocator(huge_page_t page_type): page_type(page_type)
            {}
            
            void* allocate(blt::size_t bytes) // NOLINT
            {
                return allocate_huge_pages(page_type, bytes);
            }
            
            void deallocate(void* ptr, blt::size_t bytes) // NOLINT
            {
                mmap_free(ptr, bytes);
            }
        private:
            huge_page_t page_type;
    };
    
    class aligned_huge_allocator
    {
        public:
            void* allocate(blt::size_t bytes) // NOLINT
            {
#ifdef WIN32
                return _aligned_malloc(bytes, BLT_2MB_SIZE);
#else
                return std::aligned_alloc(BLT_2MB_SIZE, bytes);
#endif
            }
            
            void deallocate(void* ptr, blt::size_t) // NOLINT
            {
                std::free(ptr);
            }
    };
    
}

#endif //BLT_MMAP_H
