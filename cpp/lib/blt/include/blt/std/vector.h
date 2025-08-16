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

#ifndef BLT_VECTOR_H
#define BLT_VECTOR_H

#include <array>
#include <cstring>
#include <iterator>
#include <stdexcept>
#include <blt/compatibility.h>
#include <blt/meta/iterator.h>
#include <blt/std/variant.h>

namespace blt
{
	template <typename T, size_t MAX_SIZE>
	class static_vector
	{
	public:
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using const_reference = const T&;
		using const_pointer = const T*;
		using iterator = T*;
		using const_iterator = const T*;
		using reverse_iterator = std::reverse_iterator<iterator>;
		using const_reverse_iterator = std::reverse_iterator<const_iterator>;

		constexpr static_vector() = default;

		constexpr explicit static_vector(const size_t size)
		{
			resize(size);
		}

		constexpr explicit static_vector(const size_t size, const T& t): size_(size)
		{
			assign(size, t);
		}

		template <typename InputIt, std::enable_if_t<meta::is_forward_iterator_v<InputIt> || meta::is_bidirectional_or_better_v<InputIt>, bool>  =
				true>
		constexpr static_vector(InputIt begin, InputIt end)
		{
			assign(begin, end);
		}

		constexpr static_vector(const static_vector&) = default;
		constexpr static_vector(static_vector&&) noexcept = default;
		static_vector& operator=(const static_vector&) = default;
		static_vector& operator=(static_vector&&) noexcept = default;

		static_vector& operator=(std::initializer_list<T> list)
		{
			assign(list);
			return *this;
		}

		constexpr static_vector(std::initializer_list<T> list)
		{
			assign(list);
		}

		constexpr void assign(std::initializer_list<T> list)
		{
			auto begin = list.begin();
			if (std::distance(begin, list.end()) > MAX_SIZE)
				throw std::out_of_range(
					"Requested initializer list cannot be fit inside this vector! Max size is " + std::to_string(MAX_SIZE) + " got " + std::to_string(
						std::distance(begin, list.end())) + " elements to be inserted.");
			for (size_t i = 0; begin != list.end(); ++i, ++begin)
				buffer_[i] = *begin;
		}

		constexpr void assign(const size_t size, const T& t)
		{
			if (size > MAX_SIZE)
				throw std::out_of_range("Vector cannot be initialized to size " + std::to_string(size) + " max size is " + std::to_string(MAX_SIZE));
			for (size_t i = 0; i < size; i++)
				buffer_[i] = t;
		}

		template <typename InputIt, std::enable_if_t<meta::is_forward_iterator_v<InputIt> || meta::is_bidirectional_or_better_v<InputIt>, bool>  =
				true>
		constexpr void assign(InputIt begin, InputIt end)
		{
			if (std::distance(begin, end) > MAX_SIZE)
				throw std::out_of_range(
					"Requested range cannot be fit inside this vector! Max size is " + std::to_string(MAX_SIZE) + " got " + std::to_string(
						std::distance(begin, end)) + " elements to be inserted.");
			for (size_t i = 0; begin != end; ++i, ++begin)
				buffer_[i] = *begin;
		}

		constexpr reference at(size_t index)
		{
			if (index >= MAX_SIZE)
				throw std::runtime_error("Array index " + std::to_string(index) + " out of bounds! (Max size: " + std::to_string(MAX_SIZE) + ')');
			return buffer_[index];
		}

		constexpr const_reference at(size_t index) const
		{
			if (index >= MAX_SIZE)
				throw std::runtime_error("Array index " + std::to_string(index) + " out of bounds! (Max size: " + std::to_string(MAX_SIZE) + ')');
			return buffer_[index];
		}

		constexpr reference operator[](size_t index)
		{
			return buffer_[index];
		}

		constexpr const_reference operator[](size_t index) const
		{
			return buffer_[index];
		}

		constexpr reference front()
		{
			return data()[0];
		}

		constexpr const_reference front() const
		{
			return data()[0];
		}

		constexpr reference back()
		{
			return data()[size() - 1];
		}

		constexpr const_reference back() const
		{
			return data()[size() - 1];
		}

		constexpr pointer data()
		{
			return buffer_.data();
		}

		constexpr const_pointer data() const
		{
			return buffer_.data();
		}

		[[nodiscard]] constexpr bool empty() const
		{
			return size() == 0;
		}

		[[nodiscard]] constexpr size_t size() const
		{
			return size_;
		}

		[[nodiscard]] constexpr size_t max_size() const
		{
			return MAX_SIZE;
		}

		constexpr void reserve(const size_t size)
		{
			if (size > MAX_SIZE)
				throw std::out_of_range("Requested size cannot be reserved in this vecotor. Max size is " + std::to_string(MAX_SIZE));
			size_ = size;
		}

		constexpr void resize(const size_t size)
		{
			resize(size, T{});
		}

		constexpr void resize(const size_t size, const T& t)
		{
			if (size > MAX_SIZE)
				throw std::out_of_range("Vector cannot be initialized to size " + std::to_string(size) + " max size is " + std::to_string(MAX_SIZE));
			if (size > size_)
			{
				for (size_t i = size_; i < size; i++)
					buffer_[i] = t;
			} else if (size < size_)
			{
				for (size_t i = size; i < size_; i++)
					buffer_[i].~T();
			}
			size_ = size;
		}

		[[nodiscard]] constexpr size_t capacity() const
		{
			return MAX_SIZE;
		}

		constexpr void clear()
		{
			for (auto& v : *this)
				v.~T();
			size_ = 0;
		}

		constexpr bool push_back(const T& copy)
		{
			if (size_ >= MAX_SIZE)
				return false;
			buffer_[size_++] = copy;
			return true;
		}

		constexpr bool push_back(T&& move)
		{
			if (size_ >= MAX_SIZE)
				return false;
			buffer_[size_++] = std::move(move);
			return true;
		}

		//TODO: replace with placement new / byte data
		template <typename... Args>
		constexpr bool emplace_back(Args&&... args)
		{
			if (size_ >= MAX_SIZE)
				return false;
			new (&buffer_[size_]) T{std::forward<Args>(args)...};
			return true;
		}

		constexpr void pop_back()
		{
			if (empty())
				throw std::out_of_range("No elements contained in vector. Cannot pop_back()");
			size_--;
		}

		constexpr inline iterator begin() noexcept
		{
			return data();
		}

		constexpr inline iterator end() noexcept
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

		constexpr iterator insert(const_iterator pos, const T& ref)
		{
			if (size_ + 1 >= capacity())
				throw std::out_of_range("Inserting exceeds size of internal buffer!");
			ptrdiff_t loc = pos - buffer_;
			for (auto insert = end() - 1; (insert - buffer_) != loc - 1; --insert)
			{
				auto new_pos = insert + 1;
				if constexpr(std::is_move_assignable_v<T>)
				{
					*new_pos = std::move(*insert);
				} else
				{
					*new_pos = *insert;
				}
			}
			buffer_[loc] = ref;
			size_++;
			return buffer_ + loc;
		}

		constexpr iterator insert(const_iterator pos, T&& ref)
		{
			if (size_ + 1 >= capacity())
				throw std::out_of_range("Inserting exceeds size of internal buffer!");
			ptrdiff_t loc = pos - buffer_;
			for (auto insert = end() - 1; (insert - buffer_) != loc - 1; --insert)
			{
				auto new_pos = insert + 1;
				if constexpr(std::is_move_assignable_v<T>)
				{
					*new_pos = std::move(*insert);
				} else
				{
					*new_pos = *insert;
				}
			}
			buffer_[loc] = std::move(ref);
			size_++;
			return buffer_ + loc;
		}

		constexpr iterator insert(const_iterator pos, const size_t count, const T& ref)
		{
			if (size_ + count >= capacity())
				throw std::out_of_range("Inserting exceeds size of internal buffer!");
			auto bgn = pos - 1;
			auto ed = bgn + count;
			auto shift = end() - 1;
			auto to = shift + count;
			for (; shift != ed - 1; --to, --shift)
			{
				if constexpr(std::is_move_assignable_v<T>)
				{
					*to = std::move(*shift);
				} else
				{
					*to = *shift;
				}
			}
			for (; bgn != ed; ++bgn)
				*bgn = ref;
			size_ += count;
			return bgn;
		}

		template<typename InputIt >
		iterator insert(const_iterator pos, InputIt first, InputIt last)
		{
			auto count = std::distance(first, last);
			if (size_ + count >= capacity())
				throw std::out_of_range("Inserting exceeds size of internal buffer!");
			auto bgn = pos - 1;
			auto ist = bgn;
			auto shift = end() - 1;
			auto to = shift + count;
			for (auto ed = bgn + count; shift != ed - 1; --to, --shift)
			{
				if constexpr(std::is_move_assignable_v<T>)
				{
					*to = std::move(*shift);
				} else
				{
					*to = *shift;
				}
			}
			for (; first != last; ++first, ++bgn)
			{
				*bgn = *first;
			}
			size_ += count;
			return ist;
		}

		iterator insert(const const_iterator pos, std::initializer_list<T> list)
		{
			return insert(pos, list.begin(), list.end());
		}

		constexpr iterator erase(const_iterator pos)
		{
			blt::ptrdiff_t loc = pos - buffer_;

			for (auto fetch = begin() + loc + 1; fetch != end(); ++fetch)
			{
				auto insert = fetch - 1;
				*insert = *fetch;
			}

			size_--;
			return buffer_ + loc + 1;
		}

		constexpr iterator erase(const_iterator first, const_iterator last)
		{
			blt::ptrdiff_t first_pos = first - buffer_;
			blt::ptrdiff_t last_pos = last - buffer_;
			blt::ptrdiff_t remove_amount = last_pos - first_pos;

			for (auto fetch = begin() + last_pos, insert = begin() + first_pos; fetch != end(); ++fetch, ++insert)
			{
				*insert = *fetch;
			}

			size_ -= remove_amount;
			return buffer_ + first_pos + 1;
		}

		template <typename T1, size_t size1, typename T2, size_t size2>
		constexpr friend bool operator==(const static_vector<T1, size1>& v1, const static_vector<T2, size2>& v2)
		{
			if (v1.size() != v2.size())
				return false;
			for (size_t i = 0; i < v1.size(); i++)
			{
				if (v1[i] != v2[i])
					return false;
			}
			return true;
		}
	private:
		std::array<T, MAX_SIZE> buffer_;
		size_t size_ = 0;
	};

	template <typename T, size_t BUFFER_SIZE = sizeof(std::vector<T>) / sizeof(T), typename ALLOC = std::allocator<T>>
	class svo_vector
	{
	public:
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using const_reference = const T&;
		using const_pointer = const T*;
		using iterator = T*;
		using const_iterator = const T*;
		using reverse_iterator = std::reverse_iterator<iterator>;
		using const_reverse_iterator = std::reverse_iterator<const_iterator>;

		svo_vector() noexcept = default;

		explicit svo_vector(const ALLOC &alloc) noexcept: m_allocator(alloc)
		{
		}

		explicit svo_vector(const size_t size, const ALLOC& alloc = ALLOC()): m_allocator{alloc}
		{
			if (size > BUFFER_SIZE)
				m_storage = std::vector<T, ALLOC>(size, alloc);
			else
				m_storage = static_vector<T, BUFFER_SIZE>(size);
		}

		explicit svo_vector(const size_t size, const T& t, const ALLOC& alloc = ALLOC()): m_allocator{alloc}
		{
			if (size > BUFFER_SIZE)
				m_storage = std::vector<T, ALLOC>(size, t, alloc);
			else
				m_storage = static_vector<T, BUFFER_SIZE>(size, t);
		}

		template <typename InputIt, std::enable_if_t<meta::is_forward_iterator_v<InputIt> || meta::is_bidirectional_or_better_v<InputIt>, bool> = true>
		explicit svo_vector(InputIt begin, InputIt end, const ALLOC& alloc = ALLOC()): m_allocator{alloc}
		{
			if (std::distance(begin, end) > BUFFER_SIZE)
				m_storage = std::vector<T, ALLOC>(begin, end, alloc);
			else
				m_storage = static_vector<T, BUFFER_SIZE>(begin, end);
		}

		svo_vector(std::initializer_list<T> list, const ALLOC& alloc = ALLOC()): m_allocator{alloc}
		{
			if (list.size() > BUFFER_SIZE)
				m_storage = std::vector<T, ALLOC>(list, alloc);
			else
				m_storage = static_vector<T, BUFFER_SIZE>(list);
		}

		svo_vector(const svo_vector&) = default;
		svo_vector(svo_vector&&) noexcept = default;
		svo_vector& operator=(const svo_vector&) = default;
		svo_vector& operator=(svo_vector&&) noexcept = default;

		svo_vector& operator=(std::initializer_list<T> list)
		{
			assign(list);
			return *this;
		}

		void assign(std::initializer_list<T> list)
		{
			m_storage.visit([&list](auto& vec) {
				vec.assign(list);
			});
		}

		void assign(const size_t size, const T& t)
		{
			m_storage.visit([size, &t](auto& vec) {
				vec.assign(size, t);
			});
		}

		template<typename InputIt, std::enable_if_t<meta::is_forward_iterator_v<InputIt> || meta::is_bidirectional_or_better_v<InputIt>, bool> = true>
		void assign(InputIt begin, InputIt end)
		{
			m_storage.visit([&begin, &end](auto& vec) {
				vec.assign(begin, end);
			});
		}

		reference at(const size_t index)
		{
			return m_storage.visit([index](auto& vec) {
				return vec.at(index);
			});
		}

		const_reference at(const size_t index) const
		{
			return m_storage.visit([index](auto& vec) {
				return vec.at(index);
			});
		}

		reference operator[](const size_t index)
		{
			return m_storage.visit([index](auto& vec) {
				return vec[index];
			});
		}

		const_reference operator[](const size_t index) const
		{
			return m_storage.visit([index](auto& vec) {
				return vec[index];
			});
		}

		reference front()
		{
			return m_storage.visit([](auto& vec) {
				return vec.front();
			});
		}

		const_reference front() const
		{
			return m_storage.visit([](auto& vec) {
				return vec.front();
			});
		}

		reference back()
		{
			return m_storage.visit([](auto& vec) {
				return vec.back();
			});
		}

		const_reference back() const
		{
			return m_storage.visit([](auto& vec) {
				return vec.back();
			});
		}

		pointer data()
		{
			return m_storage.visit([](auto& vec) {
				return vec.data();
			});
		}

		const_pointer data() const
		{
			return m_storage.visit([](auto& vec) {
				return vec.data();
			});
		}

		[[nodiscard]] bool empty() const
		{
			return m_storage.visit([](auto& vec) {
				return vec.empty();
			});
		}

		[[nodiscard]] size_t size() const
		{
			return m_storage.visit([](auto& vec) {
				return vec.size();
			});
		}

		[[nodiscard]] size_t max_size() const
		{
			return m_storage.visit([](auto& vec) {
				return vec.max_size();
			});
		}

		void reserve(const size_t size)
		{
			m_storage.visit([size](auto& vec) {
				vec.reserve(size);
			});
		}

		void resize(const size_t size)
		{
			m_storage.visit([size](auto& vec) {
				vec.resize(size);
			});
		}

		void resize(const size_t size, const T& t)
		{
			m_storage.visit([size, &t](auto& vec) {
				vec.resize(size, t);
			});
		}

		[[nodiscard]] size_t capacity() const
		{
			return m_storage.visit([](auto& vec) {
				return vec.capacity();
			});
		}

		void clear()
		{
			m_storage.visit([](auto& vec) {
				vec.clear();
			});
		}

		void push_back(const T& copy)
		{
			if (m_storage.template has_index<0>() && size() >= BUFFER_SIZE)
				swap_to_vec();
			m_storage.visit([&copy](auto& vec) {
				vec.push_back(copy);
			});
		}

		void push_back(T&& move)
		{
			if (m_storage.template has_index<0>() && size() >= BUFFER_SIZE)
				swap_to_vec();
			m_storage.visit([&move](auto& vec) {
				vec.push_back(std::move(move));
			});
		}

		template<typename... Args>
		void emplace_back(Args&&... args)
		{
			if (m_storage.template has_index<0>() && size() >= BUFFER_SIZE)
				swap_to_vec();
			if (m_storage.template has_index<0>())
				m_storage.template get<0>().emplace_back(std::forward<Args>(args)...);
			else
				m_storage.template get<1>().emplace_back(std::forward<Args>(args)...);
		}

		void pop_back()
		{
			m_storage.visit([](auto& vec) {
				vec.pop_back();
			});
		}

		auto begin() noexcept
		{
			return m_storage.visit([](auto& vec) {
				return vec.begin();
			});
		}

		auto end() noexcept
		{
			return m_storage.visit([](auto& vec) {
				return vec.end();
			});
		}

		auto cbegin() const noexcept
		{
			return m_storage.visit([](auto& vec) {
				return vec.cbegin();
			});
		}

		auto cend() const noexcept
		{
			return m_storage.visit([](auto& vec) {
				return vec.cend();
			});
		}

		auto rbegin() const noexcept
		{
			return m_storage.visit([](auto& vec) {
				return vec.rbegin();
			});
		}

		auto rend() const noexcept
		{
			return m_storage.visit([](auto& vec) {
				return vec.rend();
			});
		}

		auto crbegin() const noexcept
		{
			return m_storage.visit([](auto& vec) {
				return vec.crbegin();
			});
		}

		auto crend() const noexcept
		{
			return m_storage.visit([](auto& vec) {
				return vec.crend();
			});
		}

		iterator insert(const_iterator pos, const T& ref)
		{
			if (size() == BUFFER_SIZE)
				swap_to_vec();
			return m_storage.visit([&pos, &ref](auto& vec) {
				return vec.insert(pos, ref);
			});
		}

		iterator insert(const_iterator pos, T&& ref)
		{
			if (size() == BUFFER_SIZE)
				swap_to_vec();
			return m_storage.visit([&pos, ref=std::move(ref)](auto& vec) {
				return vec.insert(pos, std::move(ref));
			});
		}

		iterator insert(const_iterator pos, size_t count, const T& ref)
		{
			if (size() + count >= BUFFER_SIZE)
				swap_to_vec();
			return m_storage.visit([&pos, count, &ref](auto& vec) {
				return vec.insert(pos, count, ref);
			});
		}

		template<typename InputIt>
		iterator insert(const_iterator pos, InputIt begin, InputIt end)
		{
			if (size() + std::distance(begin, end) >= BUFFER_SIZE)
				swap_to_vec();
			return m_storage.visit([&pos, &begin, &end](auto& vec) {
				return vec.insert(pos, begin, end);
			});
		}

		iterator insert(const_iterator pos, std::initializer_list<T> list)
		{
			if (size() + list.size() >= BUFFER_SIZE)
				swap_to_vec();
			return m_storage.visit([&pos, &list](auto& vec) {
				return vec.insert(pos, list);
			});
		}

		iterator erase(iterator pos)
		{
			return m_storage.visit([&pos](auto& vec) {
				return vec.erase(pos);
			});
		}

		iterator erase(const_iterator pos)
		{
			return m_storage.visit([&pos](auto& vec) {
				return vec.erase(pos);
			});
		}

		iterator erase(iterator first, iterator last)
		{
			return m_storage.visit([&first, &last](auto& vec) {
				return vec.erase(first, last);
			});
		}

		iterator erase(const_iterator first, const_iterator last)
		{
			return m_storage.visit([&first, &last](auto& vec) {
				return vec.erase(first, last);
			});
		}

		template<typename A, size_t SIZE_A, typename B, size_t SIZE_B>
		friend bool operator==(const svo_vector<A, SIZE_A>& a, const svo_vector<B, SIZE_B>& b)
		{
			if (a.size() != b.size())
				return false;
			return std::visit(lambda_visitor{
				[](auto& a1, auto& b1) {
					for (const auto [e1, e2] : in_pairs(a1, b1))
					{
						if (e1 != e2)
							return false;
					}
					return true;
				}
			}, a.m_storage.variant(), b.m_storage.variant());
		}

		template<typename A, size_t SIZE_A, typename B, size_t SIZE_B>
		friend bool operator!=(const svo_vector<A, SIZE_A>& a, const svo_vector<B, SIZE_B>& b)
		{
			return !(a == b);
		}

	private:
		void swap_to_vec()
		{
			if (m_storage.template has_index<1>())
				return;
			std::vector<T, ALLOC> vec{m_allocator.value_or(ALLOC())};
			auto& vec_storage = m_storage.template get<0>();
			vec.reserve(vec_storage.size());
			if constexpr (std::is_trivially_copyable_v<T>)
			{
				vec.resize(vec_storage.size());
				std::memcpy(vec.data(), vec_storage.data(), vec_storage.size() * sizeof(T));
			} else if constexpr (std::is_move_constructible_v<T>)
			{
				for (auto& v : vec_storage)
					vec.emplace_back(std::move(v));
			} else
			{
				for (auto& v : vec_storage)
					vec.emplace_back(v);
			}
			m_storage = std::move(vec);
		}
		std::optional<ALLOC> m_allocator;
		variant_t<static_vector<T, BUFFER_SIZE>, std::vector<T, ALLOC>> m_storage;
	};
}

#endif //BLT_VECTOR_H
