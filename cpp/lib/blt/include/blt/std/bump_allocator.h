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

#ifndef BLT_ATOMIC_ALLOCATOR_H
#define BLT_ATOMIC_ALLOCATOR_H

#include <optional>
#include <limits>
#include <vector>
#include <blt/std/ranges.h>
#include <blt/std/utility.h>
#include <blt/std/types.h>
#include <blt/std/assert.h>
#include <blt/std/mmap.h>
#include <blt/compatibility.h>
#include <stdexcept>
#include "blt/logging/logging.h"
#include <cstdlib>
#include <atomic>

namespace blt
{

//    template<typename Alloc = blt::aligned_huge_allocator>
//    class atomic_bump_allocator
//    {
//        private:
//            struct block
//            {
//                struct block_metadata_t
//                {
//                    const blt::size_t size = 0;
//                    std::atomic<blt::size_t> allocated_objects = 0;
//                    std::atomic<block*> next = nullptr;
//                    std::atomic<block*> prev = nullptr;
//                    std::atomic<blt::u8*> offset = nullptr;
//                } metadata;
//                blt::u8 buffer[8]{};
//
//                explicit block(blt::size_t size)
//                {
//                    metadata.size = size;
//                    metadata.offset = buffer;
//                }
//
//                [[nodiscard]] blt::ptrdiff_t storage_size() const noexcept
//                {
//                    return static_cast<blt::ptrdiff_t>(metadata.size - sizeof(typename block::block_metadata_t));
//                }
//
//                [[nodiscard]] blt::ptrdiff_t used_bytes_in_block() const noexcept
//                {
//                    return static_cast<blt::ptrdiff_t>(metadata.offset - buffer);
//                }
//
//                [[nodiscard]] blt::ptrdiff_t remaining_bytes_in_block() const noexcept
//                {
//                    return storage_size() - used_bytes_in_block();
//                }
//            };
//
//        public:
//            void* allocate(blt::size_t bytes)
//            {
//                auto head_ptr = head.load(std::memory_order_relaxed);
//                auto new_head = head_ptr;
//                do
//                {
//                    if (head_ptr->remaining_bytes_in_block() < bytes)
//                    {
//
//                    }
//                } while (!head.compare_exchange_weak(head_ptr, new_head, std::memory_order_release, std::memory_order_acquire));
//            }
//
//            void deallocate(void* ptr)
//            {
//                auto blk = to_block(ptr);
//                --blk.metadata.allocated_objects;
//                if (blk.metadata.allocated_objects == 0)
//                {
//                    if (blk->metadata.prev != nullptr)
//                        blk->metadata.prev->metadata.next = blk->metadata.next;
//                    if (blk->metadata.next != nullptr)
//                        blk->metadata.next->metadata.prev = blk->metadata.prev;
//                    alloc.deallocate(blk, blk.metadata.size);
//                }
//            }
//
//        private:
//            static inline block* to_block(void* p)
//            {
//                return reinterpret_cast<block*>(reinterpret_cast<std::uintptr_t>(p) & static_cast<std::uintptr_t>(~(BLT_2MB_SIZE - 1)));
//            }
//
//            inline block* allocate_block(blt::size_t bytes)
//            {
//                auto size = align_size_to(bytes, BLT_2MB_SIZE);
//                auto ptr = static_cast<block*>(alloc.allocate(size));
//                new(ptr) block{size};
//                return ptr;
//            }
//
//            std::atomic<block*> head = nullptr;
//            Alloc alloc;
//    };



}

#endif //BLT_ATOMIC_ALLOCATOR_H
