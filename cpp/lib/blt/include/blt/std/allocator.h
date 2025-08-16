/*
 *  <Short Description>
 *  Copyright (C) 2023  Brett Terpstra
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

#ifndef BLT_ALLOCATOR_H

#include <optional>
#include <limits>
#include <vector>
#include <blt/std/ranges.h>
#include <blt/iterator/iterator.h>
#include <blt/std/utility.h>
#include <blt/std/types.h>
#include <blt/std/assert.h>
#include <blt/std/mmap.h>
#include <blt/compatibility.h>
#include <stdexcept>
#include "blt/logging/logging.h"
#include <cstdlib>

#ifdef __unix__

#include <sys/mman.h>

#endif

namespace blt
{
	template <typename value_type, typename pointer, typename const_pointer>
	class allocator_base
	{
	public:
		template <class U, class... Args>
		inline void construct(U* p, Args&&... args)
		{
			::new((void*) p) U(std::forward<Args>(args)...);
		}

		template <class U>
		inline void destroy(U* p)
		{
			if (p != nullptr)
				p->~U();
		}

		[[nodiscard]] inline size_t max_size() const
		{
			return std::numeric_limits<size_t>::max();
		}

		inline const_pointer address(const value_type& val)
		{
			return std::addressof(val);
		}

		inline pointer address(value_type& val)
		{
			return std::addressof(val);
		}
	};

	template <typename T, size_t BLOCK_SIZE = 8192>
	class area_allocator : public allocator_base<T, T*, const T*>
	{
	public:
		using value = T;
		using type = T;
		using value_type = type;
		using pointer = type*;
		using const_pointer = const type*;
		using void_pointer = void*;
		using const_void_pointer = const void*;
		using reference = value_type&;
		using const_reference = const value_type&;
		using size_type = size_t;
		using difference_type = size_t;
		using propagate_on_container_move_assignment = std::false_type;

		template <class U>
		struct rebind
		{
			typedef blt::area_allocator<U, BLOCK_SIZE> other;
		};

		using allocator_base<value_type, pointer, const_pointer>::allocator_base;

	private:
		/**
		* Stores a view to a region of memory that has been deallocated
		* This is a non-owning reference to the memory block
		*
		* pointer p is the pointer to the beginning of the block of memory
		* size_t n is the number of elements that this block can hold
		*/
		struct pointer_view
		{
			pointer p;
			size_t n;
		};

		/**
		* Stores the actual data for allocated blocks. Since we would like to be able to allocate an arbitrary number of items
		* we need a way of storing that data. The block storage holds an owning pointer to a region of memory with used elements
		* Only up to used has to have their destructors called, which should be handled by the deallocate function
		* it is UB to not deallocate memory allocated by this allocator
		*
		* an internal vector is used to store the regions of memory which have been deallocated. the allocate function will search for
		* free blocks with sufficient size in order to maximize memory usage. In the future more advanced methods should be used
		* for both faster access to deallocated blocks of sufficient size and to ensure coherent memory.
		*/
		struct block_storage
		{
			pointer data;
			size_t used = 0;
			// TODO: b-tree?
			std::vector<pointer_view> unallocated_blocks;
		};

		/**
		* Stores an index to a pointer_view along with the amount of memory leftover after the allocation
		* it also stores the block being allocated to in question. The new inserted leftover should start at old_ptr + size
		*/
		struct block_view
		{
			block_storage* blk;
			size_t index;
			size_t leftover;

			block_view(block_storage* blk, size_t index, size_t leftover): blk(blk), index(index), leftover(leftover)
			{}
		};

		/**
		* Allocate a new block of memory and push it to the back of blocks.
		*/
		inline void allocate_block()
		{
			//BLT_INFO("Allocating a new block of size {:d}", BLOCK_SIZE);
			auto* blk = new block_storage();
			blk->data = static_cast<pointer>(malloc(sizeof(T) * BLOCK_SIZE));
			blocks.push_back(blk);
		}

		/**
		* Searches for a free block inside the block storage with sufficient space and returns an optional view to it
		* The optional will be empty if no open block can be found.
		*/
		inline std::optional<block_view> search_for_block(block_storage* blk, size_t n)
		{
			for (auto [index, item] : blt::enumerate(blk->unallocated_blocks))
			{
				if (item.n >= n)
					return block_view{blk, index, item.n - n};
			}
			return {};
		}

		/**
		* removes the block of memory from the unallocated_blocks storage in the underlying block, inserting a new unallocated block if
		* there was any leftover. Returns a pointer to the beginning of the new block.
		*/
		inline pointer swap_pop_resize_if(const block_view& view, size_t n)
		{
			pointer_view ptr = view.blk->unallocated_blocks[view.index];
			std::iter_swap(view.blk->unallocated_blocks.begin() + view.index, view.blk->unallocated_blocks.end() - 1);
			view.blk->unallocated_blocks.pop_back();
			if (view.leftover > 0)
				view.blk->unallocated_blocks.push_back({ptr.p + n, view.leftover});
			return ptr.p;
		}

		/**
		* Finds the next available unallocated block of memory, or empty if there is none which meet size requirements
		*/
		inline std::optional<pointer> find_available_block(size_t n)
		{
			for (auto* blk : blocks)
			{
				if (auto view = search_for_block(blk, n))
					return swap_pop_resize_if(view.value(), n);
			}
			return {};
		}

		/**
		* returns a pointer to a block of memory along with an offset into that pointer that the requested block can be found at
		*/
		inline std::pair<pointer, size_t> getBlock(size_t n)
		{
			if (auto blk = find_available_block(n))
				return {blk.value(), 0};

			if (blocks.back()->used + n > BLOCK_SIZE)
				allocate_block();

			auto ptr = std::pair<pointer, size_t>{blocks.back()->data, blocks.back()->used};
			blocks.back()->used += n;
			return ptr;
		}

		/**
		* Calls the constructor on elements if they require construction, otherwise constructor will not be called and this function is useless
		*
		* ALLOCATORS RETURN UNINIT STORAGE!! THIS HAS BEEN DISABLED.
		*/
		inline void allocate_in_block(pointer, size_t)
		{
			//                if constexpr (std::is_default_constructible_v<T> && !std::is_trivially_default_constructible_v<T>)
			//                {
			//                    for (size_t i = 0; i < n; i++)
			//                        new(&begin[i]) T();
			//                }
		}

	public:
		area_allocator()
		{
			allocate_block();
		}

		area_allocator(const area_allocator& copy) = delete;

		area_allocator(area_allocator&& move) noexcept
		{
			blocks = move.blocks;
		}

		area_allocator& operator=(const area_allocator& copy) = delete;

		area_allocator& operator=(area_allocator&& move) noexcept
		{
			std::swap(move.blocks, blocks);
		}

		[[nodiscard]] pointer allocate(size_t n)
		{
			if (n > BLOCK_SIZE)
				throw std::runtime_error("Requested allocation is too large!");

			auto block_info = getBlock(n);

			auto* ptr = &block_info.first[block_info.second];
			// call constructors on the objects if they require it
			allocate_in_block(ptr, n);

			return ptr;
		}

		void deallocate(pointer p, size_t n) noexcept
		{
			if (p == nullptr)
				return;
			//                for (size_t i = 0; i < n; i++)
			//                    p[i].~T();
			for (auto*& blk : blocks)
			{
				if (p >= blk->data && p <= (blk->data + BLOCK_SIZE))
				{
					blk->unallocated_blocks.push_back(pointer_view{p, n});
					break;
				}
			}
		}

		~area_allocator()
		{
			for (auto*& blk : blocks)
			{
				free(blk->data);
				delete blk;
			}
		}

	private:
		std::vector<block_storage*> blocks;
	};

	//    template<typename T>
	//    class bump_allocator : public allocator_base<T, T*, const T*>
	//    {
	//        public:
	//            using value = T;
	//            using type = T;
	//            using value_type = type;
	//            using pointer = type*;
	//            using const_pointer = const type*;
	//            using void_pointer = void*;
	//            using const_void_pointer = const void*;
	//            using reference = value_type&;
	//            using const_reference = const value_type&;
	//            using size_type = size_t;
	//            using difference_type = size_t;
	//            using propagate_on_container_move_assignment = std::false_type;
	//            template<class U>
	//            struct rebind
	//            {
	//                typedef blt::bump_allocator<U> other;
	//            };
	//            using allocator_base<value_type, pointer, const_pointer>::allocator_base;
	//        private:
	//            pointer buffer_;
	//            blt::size_t offset_;
	//            blt::size_t size_;
	//        public:
	//            explicit bump_allocator(blt::size_t size): buffer_(static_cast<pointer>(malloc(size * sizeof(T)))), offset_(0), size_(size)
	//            {}
	//
	//            template<typename... Args>
	//            explicit bump_allocator(blt::size_t size, Args&& ... defaults):
	//                    buffer_(static_cast<pointer>(malloc(size * sizeof(type)))), offset_(0), size_(size)
	//            {
	//                for (blt::size_t i = 0; i < size_; i++)
	//                    ::new(&buffer_[i]) T(std::forward<Args>(defaults)...);
	//            }
	//
	//            bump_allocator(pointer buffer, blt::size_t size): buffer_(buffer), offset_(0), size_(size)
	//            {}
	//
	//            bump_allocator(const bump_allocator& copy) = delete;
	//
	//            bump_allocator(bump_allocator&& move) noexcept
	//            {
	//                buffer_ = move.buffer_;
	//                size_ = move.size_;
	//                offset_ = move.offset_;
	//            }
	//
	//            bump_allocator& operator=(const bump_allocator& copy) = delete;
	//
	//            bump_allocator& operator=(bump_allocator&& move) noexcept
	//            {
	//                std::swap(move.buffer_, buffer_);
	//                std::swap(move.size_, size_);
	//                std::swap(move.offset_, offset_);
	//            }
	//
	//            pointer allocate(blt::size_t n)
	//            {
	//                auto nv = offset_ + n;
	//                if (nv > size_)
	//                    throw std::bad_alloc();
	//                pointer b = &buffer_[offset_];
	//                offset_ = nv;
	//                return b;
	//            }
	//
	//            void deallocate(pointer, blt::size_t)
	//            {}
	//
	//            ~bump_allocator()
	//            {
	//                free(buffer_);
	//            }
	//    };

	/**
	* The bump allocator is meant to be a faster area allocator which will only allocate forward through either a supplied buffer or size
	* or will create a linked list type data structure of buffered blocks.
	* @tparam ALLOC allocator to use for any allocations. In the case of the non-linked variant, this will be used if a size is supplied. The supplied buffer must be allocated with this allocator!
	* @tparam linked use a linked list to allocate with the allocator or just use the supplied buffer and throw an exception of we cannot allocate
	*/
	template <bool linked, template<typename> typename ALLOC = std::allocator>
	class bump_allocator_old;

	template <template<typename> typename ALLOC>
	class bump_allocator_old<false, ALLOC>
	{
	private:
		ALLOC<blt::u8> allocator;
		blt::u8* buffer_;
		blt::u8* offset_;
		blt::size_t size_;

	public:
		explicit bump_allocator_old(blt::size_t size): buffer_(static_cast<blt::u8*>(allocator.allocate(size))), offset_(buffer_), size_(size)
		{}

		explicit bump_allocator_old(blt::u8* buffer, blt::size_t size): buffer_(buffer), offset_(buffer), size_(size)
		{}

		template <typename T>
		[[nodiscard]] T* allocate()
		{
			size_t remaining_num_bytes = size_ - static_cast<size_t>(buffer_ - offset_);
			auto pointer = static_cast<void*>(offset_);
			const auto aligned_address = std::align(alignof(T), sizeof(T), pointer, remaining_num_bytes);
			if (aligned_address == nullptr)
				throw std::bad_alloc{};
			offset_ = static_cast<blt::u8*>(aligned_address) + sizeof(T);
			return static_cast<T*>(aligned_address);
		}

		template <typename T, typename... Args>
		[[nodiscard]] T* emplace(Args&&... args)
		{
			const auto allocated_memory = allocate<T>();
			return new(allocated_memory) T{std::forward<Args>(args)...};
		}

		template <class U, class... Args>
		inline void construct(U* p, Args&&... args)
		{
			::new((void*) p) U(std::forward<Args>(args)...);
		}

		template <class U>
		inline void destroy(U* p)
		{
			if (p != nullptr)
				p->~U();
		}

		~bump_allocator_old()
		{
			allocator.deallocate(buffer_, size_);
		}
	};

	template <template<typename> typename ALLOC>
	class bump_allocator_old<true, ALLOC>
	{
	private:
		struct block
		{
			blt::size_t allocated_objects = 0;
			blt::u8* buffer = nullptr;
			blt::u8* offset = nullptr;

			explicit block(blt::u8* buffer): buffer(buffer), offset(buffer)
			{}
		};

		ALLOC<blt::u8> allocator;
		std::vector<block, ALLOC<block>> blocks;
		blt::size_t size_;
		blt::size_t allocations = 0;
		blt::size_t deallocations = 0;

		void expand()
		{
			auto ptr = static_cast<blt::u8*>(allocator.allocate(size_));
			blocks.push_back(block{ptr});
			allocations++;
		}

		template <typename T>
		T* allocate_back()
		{
			auto& back = blocks.back();
			size_t remaining_bytes = size_ - static_cast<size_t>(back.offset - back.buffer);
			auto pointer = static_cast<void*>(back.offset);
			const auto aligned_address = std::align(alignof(T), sizeof(T), pointer, remaining_bytes);
			if (aligned_address != nullptr)
			{
				back.offset = static_cast<blt::u8*>(aligned_address) + sizeof(T);
				back.allocated_objects++;
			}

			return static_cast<T*>(aligned_address);
		}

	public:
		/**
		* @param size of the list blocks
		*/
		explicit bump_allocator_old(blt::size_t size): size_(size)
		{
			expand();
		}

		template <typename T>
		[[nodiscard]] T* allocate()
		{
			if (auto ptr = allocate_back<T>(); ptr == nullptr)
				expand();
			else
				return ptr;
			if (auto ptr = allocate_back<T>(); ptr == nullptr)
				throw std::bad_alloc();
			else
				return ptr;
		}

		template <typename T>
		void deallocate(T* p)
		{
			auto* ptr = reinterpret_cast<blt::u8*>(p);
			for (auto e : blt::enumerate(blocks))
			{
				auto& block = e.second;
				if (ptr >= block.buffer && ptr <= block.offset)
				{
					block.allocated_objects--;
					if (block.allocated_objects == 0)
					{
						std::iter_swap(blocks.begin() + e.first, blocks.end() - 1);
						allocator.deallocate(blocks.back().buffer, size_);
						blocks.pop_back();
						deallocations++;
					}
					return;
				}
			}
		}

		template <typename T, typename... Args>
		[[nodiscard]] T* emplace(Args&&... args)
		{
			const auto allocated_memory = allocate<T>();
			return new(allocated_memory) T{std::forward<Args>(args)...};
		}

		template <class U, class... Args>
		inline void construct(U* p, Args&&... args)
		{
			::new((void*) p) U(std::forward<Args>(args)...);
		}

		template <class U>
		inline void destroy(U* p)
		{
			if (p != nullptr)
				p->~U();
		}

		~bump_allocator_old()
		{
			if (allocations != deallocations)
				BLT_WARN("Allocator has blocks which have not been deallocated! Destructors might not have been called!");
			for (auto& v : blocks)
				allocator.deallocate(v.buffer, size_);
		}
	};

	template <typename T, bool WARN_ON_FAIL = false>
	static inline T* allocate_huge_page(blt::size_t BLOCK_SIZE, blt::size_t HUGE_PAGE_SIZE = BLT_2MB_SIZE)
	{
		#ifdef __unix__
		BLT_ASSERT((BLOCK_SIZE & (HUGE_PAGE_SIZE - 1)) == 0 && "Must be multiple of the huge page size!");
		T* buffer = static_cast<T*>(mmap(nullptr, BLOCK_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_HUGETLB | MAP_POPULATE, -1,
										0));
		// if we fail to allocate a huge page we can try to allocate normally
		if (buffer == MAP_FAILED)
		{
			if constexpr (WARN_ON_FAIL)
			{
				BLT_WARN("We failed to allocate huge pages\n{}{}", handle_mmap_error(),
						"\033[1;31mYou should attempt to enable "
						"huge pages as this will allocate normal pages and double the memory usage!\033[22m\n");
			}
			blt::size_t bytes = BLOCK_SIZE * 2;
			buffer = static_cast<T*>(mmap(nullptr, bytes, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_POPULATE, -1, 0));
			if (buffer == MAP_FAILED)
			{
				BLT_ERROR("Failed to allocate normal pages");
				throw bad_alloc_t(handle_mmap_error());
			}
			if constexpr (WARN_ON_FAIL)
			{
				if (((size_t) buffer & (HUGE_PAGE_SIZE - 1)) != 0)
					BLT_ERROR("Pointer is not aligned! {:#x}", buffer);
			}
			auto* ptr = static_cast<void*>(buffer);
			auto ptr_size = reinterpret_cast<blt::size_t>(ptr);
			buffer = static_cast<T*>(std::align(BLOCK_SIZE, BLOCK_SIZE, ptr, bytes));
			if constexpr (WARN_ON_FAIL)
				BLT_ERROR("Offset by {} pages, resulting: {:#x}", (reinterpret_cast<blt::size_t>(buffer) - ptr_size) / 4096, buffer);
		}
		return buffer;
		#endif
		return malloc(BLOCK_SIZE);
	}

	/**
	* blt::bump_allocator. Allocates blocks of BLOCK_SIZE with zero reuse. When all objects from a block are fully deallocated the block will be freed
	* @tparam BLOCK_SIZE size of block to use. recommended to be multiple of page size or huge page size.
	* @tparam USE_HUGE allocate using mmap and huge pages. If this fails it will use mmap to allocate normally. defaults to off because linux has parent huge pages.
	* @tparam HUGE_PAGE_SIZE size the system allows huge pages to be. defaults to 2mb
	* @tparam WARN_ON_FAIL print warning messages if allocating huge pages fail
	*/
	template <blt::size_t BLOCK_SIZE = BLT_2MB_SIZE, bool USE_HUGE = false, blt::size_t HUGE_PAGE_SIZE = BLT_2MB_SIZE, bool WARN_ON_FAIL = false>
	class bump_allocator
	{
		// ensure power of two
		static_assert(((BLOCK_SIZE & (BLOCK_SIZE - 1)) == 0) && "Must be a power of two!");

	public:
		/**
		* convert any pointer back into a pointer its block
		*/
		template <typename T>
		static inline auto to_block(T* p)
		{
			return reinterpret_cast<block*>(reinterpret_cast<std::uintptr_t>(p) & static_cast<std::uintptr_t>(~(BLOCK_SIZE - 1)));
		}

		class stats_t
		{
			friend bump_allocator;

		private:
			blt::size_t allocated_blocks = 0;
			blt::size_t allocated_bytes = 0;
			blt::size_t peak_blocks = 0;
			blt::size_t peak_bytes = 0;

		protected:
			inline void incrementBlocks()
			{
				allocated_blocks++;
				if (allocated_blocks > peak_blocks)
					peak_blocks = allocated_blocks;
			}

			inline void decrementBlocks()
			{
				allocated_blocks--;
			}

			inline void incrementBytes(blt::size_t bytes)
			{
				allocated_bytes += bytes;
				if (allocated_bytes > peak_bytes)
					peak_bytes = allocated_bytes;
			}

			inline void decrementBytes(blt::size_t bytes)
			{
				allocated_bytes -= bytes;
			}

		public:
			inline auto getAllocatedBlocks() const
			{
				return allocated_blocks;
			}

			inline auto getAllocatedBytes() const
			{
				return allocated_bytes;
			}

			inline auto getPeakBlocks() const
			{
				return peak_blocks;
			}

			inline auto getPeakBytes() const
			{
				return peak_bytes;
			}
		};

	private:
		stats_t stats;
		//blt::hashset_t<void*> deletes;

		struct block
		{
			struct block_metadata_t
			{
				blt::size_t allocated_objects = 0;
				block* next = nullptr;
				block* prev = nullptr;
				blt::u8* offset = nullptr;
			} metadata;

			blt::u8 buffer[BLOCK_SIZE - sizeof(block_metadata_t)]{};

			block()
			{
				metadata.offset = buffer;
			}
		};

		// remaining space inside the block after accounting for the metadata
		static constexpr blt::size_t BLOCK_REMAINDER = BLOCK_SIZE - sizeof(typename block::block_metadata_t);

		block* base = nullptr;
		block* head = nullptr;

		/**
		* Handles the allocation of the bytes for the block.
		* This function will either use mmap to allocate huge pages if requested
		* or use std::align_alloc to create an aligned allocation
		* @return pointer to a constructed block
		*/
		block* allocate_block()
		{
			block* buffer;
			#ifdef __unix__
			if constexpr (USE_HUGE)
			{
				buffer = allocate_huge_page<block, WARN_ON_FAIL>(BLOCK_SIZE, HUGE_PAGE_SIZE);
			} else
				buffer = reinterpret_cast<block*>(std::aligned_alloc(BLOCK_SIZE, BLOCK_SIZE));
			#else
                buffer = static_cast<block*>(_aligned_malloc(BLOCK_SIZE, BLOCK_SIZE));
			#endif
			construct(buffer);
			#ifndef BLT_DISABLE_STATS
			stats.incrementBlocks();
			#endif
			return buffer;
		}

		/**
		* Allocates a new block and pushes it to the front of the linked listed
		*/
		void allocate_forward()
		{
			auto* block = allocate_block();
			if (head == nullptr)
			{
				base = head = block;
				return;
			}
			block->metadata.prev = head;
			head->metadata.next = block;
			head = block;
		}

		/**
		* handles the actual allocation and alignment of memory
		* @param bytes number of bytes to allocate
		* @param alignment alignment required
		* @return aligned pointer
		*/
		void* allocate_bytes(blt::size_t bytes, blt::size_t alignment)
		{
			if (head == nullptr)
				return nullptr;
			blt::size_t remaining_bytes = BLOCK_REMAINDER - static_cast<blt::ptrdiff_t>(head->metadata.offset - head->buffer);
			auto pointer = static_cast<void*>(head->metadata.offset);
			return std::align(alignment, bytes, pointer, remaining_bytes);
		}

		/**
		* allocate an object starting from the next available address
		* @tparam T type to allocate for
		* @param count number of elements to allocate
		* @return nullptr if the object could not be allocated, pointer to the object if it could, pointer to the start if count != 1
		*/
		template <typename T>
		T* allocate_object(blt::size_t count)
		{
			blt::size_t bytes = sizeof(T) * count;
			const auto aligned_address = allocate_bytes(bytes, alignof(T));
			if (aligned_address != nullptr)
			{
				head->metadata.allocated_objects++;
				head->metadata.offset = static_cast<blt::u8*>(aligned_address) + bytes;
			}
			return static_cast<T*>(aligned_address);
		}

		/**
		* Frees a block
		* @param p pointer to the block to free
		*/
		inline void delete_block(block* p)
		{
			#ifndef BLT_DISABLE_STATS
			stats.decrementBlocks();
			#endif
			if constexpr (USE_HUGE)
			{
				if (munmap(p, BLOCK_SIZE))
				{
					BLT_ERROR("FAILED TO DEALLOCATE BLOCK");
					throw bad_alloc_t(handle_mmap_error());
				}
			} else
				free(p);
		}

	public:
		bump_allocator() = default;

		/**
		* Takes an unused size parameter. Purely used for compatibility with the old bump_allocator
		*/
		explicit bump_allocator(blt::size_t)
		{}

		/**
		* Allocate bytes for a type
		* @tparam T type to allocate
		* @param count number of elements to allocate for
		* @throws std::bad_alloc
		* @return aligned pointer to the beginning of the allocated memory
		*/
		template <typename T>
		[[nodiscard]] T* allocate(blt::size_t count = 1)
		{
			if constexpr (sizeof(T) > BLOCK_REMAINDER)
				throw std::bad_alloc();

			#ifndef BLT_DISABLE_STATS
			stats.incrementBytes(sizeof(T) * count);
			#endif

			T* ptr = allocate_object<T>(count);
			if (ptr != nullptr)
				return ptr;
			allocate_forward();
			ptr = allocate_object<T>(count);
			if (ptr == nullptr)
				throw std::bad_alloc();
			return ptr;
		}

		/**
		* Deallocate a pointer, does not call the destructor
		* @tparam T type of pointer
		* @param p pointer to deallocate
		*/
		template <typename T>
		void deallocate(T* p, blt::size_t count = 1)
		{
			if (p == nullptr)
				return;
			#ifndef BLT_DISABLE_STATS
			stats.decrementBytes(sizeof(T) * count);
			#endif
			//                if (deletes.contains(p))
			//                {
			//                    BLT_FATAL("pointer {:#x} has already been freed", p);
			//                    throw std::bad_alloc();
			//                }else
			//                    deletes.insert(static_cast<void*>(p));

			auto blk = to_block(p);
			blk->metadata.allocated_objects--;
			if (blk->metadata.allocated_objects == 0)
			{
				//BLT_INFO("Deallocating block from {:#x} in (1) {:#x} current head {:#x}, based: {:#x}", p, blk, head, base);
				if (blk == base)
				{
					base = base->metadata.next;
					// if they were equal (single allocated block) we also need to move the head forward
					if (blk == head)
						head = base;
				} else if (blk == head) // else, need to make sure the head ptr gets moved back, otherwise we will use a head that has been freed
					head = blk->metadata.prev;
				else if (blk->metadata.prev != nullptr) // finally if it wasn't the head we need to bridge the gap in the list
					blk->metadata.prev->metadata.next = blk->metadata.next;

				//BLT_INFO("Deallocating block from {:#x} in (2) {:#x} current head {:#x}, based: {:#x}", p, blk, head, base);
				delete_block(blk);
			}
		}

		/**
		* allocate a type then call its constructor with arguments
		* @tparam T type to construct
		* @tparam Args type of args to construct with
		* @param args args to construct with
		* @return aligned pointer to the constructed type
		*/
		template <typename T, typename... Args>
		[[nodiscard]] T* emplace(Args&&... args)
		{
			const auto allocated_memory = allocate<T>();
			return new(allocated_memory) T{std::forward<Args>(args)...};
		}

		/**
		* allocate an array of count T with argument(s) args and call T's constructor
		* @tparam T class to construct
		* @tparam Args argument types to supply to construction
		* @param count size of the array to allocate in number of elements. Note calling this with count = 0 is equivalent to calling emplace
		* @param args the args to supply to construction
		* @return aligned pointer to the beginning of the array of T
		*/
		template <typename T, typename... Args>
		[[nodiscard]] T* emplace_many(blt::size_t count, Args&&... args)
		{
			if (count == 0)
				return nullptr;
			const auto allocated_memory = allocate<T>(count);
			for (blt::size_t i = 0; i < count; i++)
				new(allocated_memory + i) T{std::forward<Args>(args)...};
			return allocated_memory;
		}

		/**
		* Used to construct a class U with parameters Args
		* @tparam U class to construct
		* @tparam Args args to use
		* @param p pointer to non-constructed memory
		* @param args list of arguments to build the class with
		*/
		template <class U, class... Args>
		inline void construct(U* p, Args&&... args)
		{
			::new((void*) p) U(std::forward<Args>(args)...);
		}

		/**
		* Call the destructor for class U with pointer p
		* @tparam U class to call destructor on, this will not do anything if the type is std::trivially_destructible
		* @param p
		*/
		template <class U>
		inline void destroy(U* p)
		{
			if constexpr (!std::is_trivially_destructible_v<U>)
			{
				if (p != nullptr)
					p->~U();
			}
		}

		/**
		* Calls destroy on pointer p
		* Then calls deallocate on p
		* @tparam U class to destroy
		* @param p pointer to deallocate
		*/
		template <class U>
		inline void destruct(U* p)
		{
			destroy(p);
			deallocate(p);
		}

		inline void resetStats()
		{
			stats = {};
		}

		inline const auto& getStats() const
		{
			return stats;
		}

		~bump_allocator()
		{
			block* next = base;
			while (next != nullptr)
			{
				auto* after = next->metadata.next;
				delete_block(next);
				next = after;
			}
		}
	};
}

#define BLT_ALLOCATOR_H

#endif //BLT_ALLOCATOR_H
