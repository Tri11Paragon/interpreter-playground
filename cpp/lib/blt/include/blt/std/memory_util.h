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

#ifndef BLT_MEMORY_UTIL_H
#define BLT_MEMORY_UTIL_H

#include <type_traits>
#include <array>
#include <string>
#include <cstring>
#include <algorithm>
#include <cstdint>
#include <climits>
#include <cmath>
#include <iostream>

#if defined(__clang__) || defined(__llvm__) || defined(__GNUC__) || defined(__GNUG__)

#if (defined(__GNUC__) || defined(__GNUG__)) && !defined(WIN32)

#include <byteswap.h>

#define SWAP16(val) bswap_16(val)
#define SWAP32(val) bswap_32(val)
#define SWAP64(val) bswap_64(val)
#else
        #define SWAP16(val) __builtin_bswap16(val)
        #define SWAP32(val) __builtin_bswap32(val)
        #define SWAP64(val) __builtin_bswap64(val)
#endif


#if __cplusplus >= 202002L

        #include <bit>

        #define ENDIAN_LOOKUP(little_endian) (std::endian::native == std::endian::little && !little_endian) || \
                                             (std::endian::native == std::endian::big && little_endian)
#else
#define ENDIAN_LOOKUP(little_endian) !little_endian
#endif
#elif defined(_MSC_VER)
    #include <intrin.h>
    #define SWAP16(val) _byteswap_ushort(val)
    #define SWAP32(val) _byteswap_ulong(val)
    #define SWAP64(val) _byteswap_uint64(val)
    #define ENDIAN_LOOKUP(little_endian) !little_endian
#endif

namespace blt::mem
{
    template <typename R, typename T>
    static R type_cast(T type)
    {
        static_assert(std::is_trivially_copyable_v<T>, "Type must be trivially copyable to be type casted!");
        static_assert(sizeof(T) == sizeof(R));
        R r;
        std::memcpy(&r, &type, sizeof(type));
        return r;
    }

    template <typename T>
    void reverse(T& out)
    {
        static_assert(std::is_trivially_copyable_v<T>, "Type must be trivially copyable to be reversible!");
        // if we need to swap find the best way to do so
        if constexpr (std::is_same_v<T, std::int16_t> || std::is_same_v<T, std::uint16_t>)
            out = type_cast<T>(SWAP16(type_cast<std::uint16_t>(out)));
        else if constexpr (std::is_same_v<T, std::int32_t> || std::is_same_v<T, std::uint32_t> || std::is_same_v<T, float>)
            out = type_cast<T>(SWAP32(type_cast<std::uint32_t>(out)));
        else if constexpr (std::is_same_v<T, std::int64_t> || std::is_same_v<T, std::uint64_t> || std::is_same_v<T, double>)
            out = type_cast<T>(SWAP64(type_cast<std::uint64_t>(out)));
        else
        {
            std::array<std::byte, sizeof(T)> data;
            std::memcpy(data.data(), &out, sizeof(T));
            std::reverse(data.begin(), data.end());
            std::memcpy(&out, data.data(), sizeof(T));
        }
    }

    // Used to grab the byte-data of any T element. Defaults to Big Endian, however can be configured to use little endian
    template <bool little_endian = false, typename BYTE_TYPE, typename T>
    int toBytes(const T& in, BYTE_TYPE* out)
    {
        if constexpr (!(std::is_same_v<BYTE_TYPE, std::int8_t> || std::is_same_v<BYTE_TYPE, std::uint8_t>))
            static_assert("Must provide a signed/unsigned int8 type");
        std::memcpy(out, &in, sizeof(T));

        if constexpr (ENDIAN_LOOKUP(little_endian))
        {
            // TODO: this but better.
            for (size_t i = 0; i < sizeof(T) / 2; i++)
                std::swap(out[i], out[sizeof(T) - 1 - i]);
        }

        return 0;
    }

    // Used to cast the binary data of any T object, into a T object. Assumes data is in big ending (configurable)
    template <bool little_endian = false, typename BYTE_TYPE, typename T>
    int fromBytes(const BYTE_TYPE* in, T& out)
    {
        if constexpr (!(std::is_same_v<BYTE_TYPE, std::int8_t> || std::is_same_v<BYTE_TYPE, std::uint8_t>))
            static_assert("Must provide a signed/unsigned int8 type");

        std::memcpy(&out, in, sizeof(T));
        if constexpr (ENDIAN_LOOKUP(little_endian))
        {
            reverse(out);
        }

        return 0;
    }

    template <bool little_endian = false, typename BYTE_TYPE, typename T>
    static int fromBytes(const BYTE_TYPE* in, T* out)
    {
        return fromBytes<little_endian>(in, *out);
    }

    inline std::size_t next_byte_allocation(std::size_t prev_size, std::size_t default_allocation_block = 8192, std::size_t default_size = 16)
    {
        if (prev_size < default_size)
            return default_size;
        if (prev_size < default_allocation_block)
            return prev_size * 2;
        return prev_size + default_allocation_block;
    }

    template <bool bits = true, typename OStream, typename Value>
    void print_bytes(OStream& stream, const Value& value)
    {
        constexpr auto size = sizeof(Value);

        std::string line;
        for (std::size_t i = 0; i < size; i++)
        {
            std::uint8_t byte;
            std::memcpy(&byte, reinterpret_cast<const char*>(&value) + i, 1);
            if constexpr (bits)
            {
                for (std::ptrdiff_t j = CHAR_BIT - 1; j >= 0; j--)
                {
                    const auto bit = (byte >> j) & 1;
                    line += std::to_string(bit);
                }
            }
            else
            {
                const auto byte_str = std::to_string(byte);
                const auto amount = CHAR_BIT - byte_str.size();
                for (std::size_t j = 0; j < static_cast<std::size_t>(std::ceil(static_cast<double>(amount) / 2.0)); j++)
                    line += ' ';
                line += byte_str;
                for (std::size_t j = 0; j < static_cast<std::size_t>(std::floor(static_cast<double>(amount) / 2.0)); j++)
                    line += ' ';
            }
            if (i != size - 1)
                line += " : ";
        }
        for (std::size_t i = 0; i < size; i++)
        {
            auto index = std::to_string(i);
            const auto amount = CHAR_BIT - index.size();
            for (std::size_t j = 0; j < static_cast<std::size_t>(std::ceil(static_cast<double>(amount) / 2.0)); j++)
                stream << ' ';
            stream << index;
            for (std::size_t j = 0; j < static_cast<std::size_t>(std::floor(static_cast<double>(amount) / 2.0)); j++)
                stream << ' ';
            if (i != size - 1)
                stream << " | ";
        }
        stream << '\n';
        stream << line;
        stream << '\n';
    }

    // TODO: check if the platform actually has enough room in their pointers for this. plus endianness

    struct bit_storage
    {
        static constexpr std::size_t START_BIT = 48;
        static constexpr std::size_t END_BIT = sizeof(std::uintptr_t) * CHAR_BIT;
        static constexpr std::size_t AVAILABLE_BITS = END_BIT - START_BIT;

        static constexpr std::size_t make_storage_ones()
        {
            #ifdef __EMSCRIPTEN__
            return ~static_cast<size_t>(0);
            #else
            std::size_t result = 0;
            for (std::size_t i = START_BIT; i < END_BIT; i++)
                result |= 1ul << i;
            return result;
            #endif
        }

        static constexpr std::size_t make_ptr_ones()
        {
            #ifdef __EMSCRIPTEN__
            return ~static_cast<size_t>(0);
            #else
            std::size_t result = 0;
            for (std::size_t i = 0; i < START_BIT; i++)
                result |= 1ul << i;
            return result;
            #endif
        }

        bit_storage(): bits(0)
        {
        }

        explicit bit_storage(const std::uint16_t bits): bits(bits)
        {
        }

        std::uint16_t bits : AVAILABLE_BITS;
    };

    template <typename Ptr>
    struct pointer_storage
    {
        static constexpr std::size_t STORAGE_ALL_ONES = bit_storage::make_storage_ones();
        static constexpr std::size_t PTR_ALL_ONES = bit_storage::make_ptr_ones();

        explicit pointer_storage(Ptr* ptr): ptr_bits(reinterpret_cast<std::uintptr_t>(ptr))
        {
        }

        explicit pointer_storage(Ptr* ptr, const bit_storage bits): ptr_bits(reinterpret_cast<std::uintptr_t>(ptr))
        {
            storage(bits);
        }

        template <typename T, std::enable_if_t<!std::is_same_v<T, bit_storage>, bool> = false>
        explicit pointer_storage(Ptr* ptr, const T& type): ptr_bits(reinterpret_cast<std::uintptr_t>(ptr))
        {
            storage(type);
        }

        [[nodiscard]] bit_storage storage() const noexcept
        {
            bit_storage storage{};
            storage.bits = (ptr_bits & STORAGE_ALL_ONES) >> bit_storage::START_BIT;
            return storage;
        }

        [[nodiscard]] bool bit(const std::size_t index) const noexcept
        {
            if (index >= bit_storage::END_BIT)
                return false;
            return (ptr_bits >> (bit_storage::START_BIT + index)) & 1;
        }

        pointer_storage& bit(const std::size_t index, const bool b) noexcept
        {
            #ifndef __EMSCRIPTEN__
            if (index >= bit_storage::END_BIT)
                return *this;
            ptr_bits &= ~(1ul << (bit_storage::START_BIT + index));
            ptr_bits |= (static_cast<std::uintptr_t>(b) << (bit_storage::START_BIT + index));
            #endif
            return *this;
        }

        template<typename T, std::enable_if_t<!std::is_same_v<T, bit_storage>, bool> = false>
        pointer_storage& storage(const T& type)
        {
            // TODO!! Emscript support!
            #ifndef __EMSCRIPTEN__
            static_assert(sizeof(T) <= sizeof(std::uintptr_t), "Type takes too many bits to be stored!");
            static constexpr std::uintptr_t store_bits = (2 << (bit_storage::AVAILABLE_BITS - 1)) - 1;
            std::uintptr_t bit_store = 0;
            bit_storage store{};
            std::memcpy(&bit_store, &type, sizeof(T));
            store.bits |= bit_store & store_bits;
            storage(store);
            #endif
            return *this;
        }

        pointer_storage& storage(const bit_storage bits) noexcept
        {
            #ifndef __EMSCRIPTEN__
            ptr_bits = ((ptr_bits & PTR_ALL_ONES) | (static_cast<std::uintptr_t>(bits.bits) << bit_storage::START_BIT));
            #endif
            return *this;
        }

        pointer_storage& operator=(const bit_storage bits) noexcept
        {
            storage(bits);
            return *this;
        }

        template<typename T, std::enable_if_t<!std::is_same_v<T, bit_storage>, bool> = false>
        pointer_storage& operator=(const T& type)
        {
            storage(type);
            return *this;
        }

        pointer_storage& clear_storage() noexcept
        {
            ptr_bits &= PTR_ALL_ONES;
            return *this;
        }

        // changes pointer without changing the tag bits
        pointer_storage& pointer(Ptr* ptr) noexcept
        {
            const bit_storage old_storage = storage();
            ptr_bits = (reinterpret_cast<std::uintptr_t>(ptr) & PTR_ALL_ONES) | old_storage.bits;
            return *this;
        }

        pointer_storage& operator=(Ptr* ptr) noexcept
        {
            pointer(ptr);
            return *this;
        }

        pointer_storage& clear_pointer() noexcept
        {
            ptr_bits &= STORAGE_ALL_ONES;
            return *this;
        }

        Ptr* get() const noexcept
        {
            return reinterpret_cast<Ptr*>(ptr_bits & PTR_ALL_ONES);
        }

        Ptr& operator*() const noexcept
        {
            return *get();
        }

        Ptr* operator->() const noexcept
        {
            return get();
        }

    private:
        std::uintptr_t ptr_bits;
    };
}

namespace blt
{
    template <typename V>
    struct ptr_iterator
    {
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using value_type = V;
        using pointer = value_type*;
        using reference = value_type&;
        using iter_reference = ptr_iterator&;

        explicit ptr_iterator(V* v): _v(v)
        {
        }

        reference operator*() const
        {
            return *_v;
        }

        pointer operator->()
        {
            return _v;
        }

        ptr_iterator& operator++()
        {
            _v++;
            return *this;
        }

        ptr_iterator& operator--()
        {
            _v--;
            return *this;
        }

        ptr_iterator operator++(int)
        {
            auto tmp = *this;
            ++(*this);
            return tmp;
        }

        ptr_iterator operator--(int)
        {
            auto tmp = *this;
            --(*this);
            return tmp;
        }

        iter_reference operator+=(difference_type amount)
        {
            _v += amount;
            return *this;
        }

        iter_reference operator-=(difference_type amount)
        {
            _v -= amount;
            return *this;
        }

        reference operator[](difference_type index)
        {
            return *(_v + index);
        }

        reference operator[](std::size_t index)
        {
            return *(_v + index);
        }

        friend bool operator<(const ptr_iterator& a, const ptr_iterator& b)
        {
            return b._v - a._v > 0;
        }

        friend bool operator>(const ptr_iterator& a, const ptr_iterator& b)
        {
            return a._v - b._v > 0;
        }

        friend bool operator<=(const ptr_iterator& a, const ptr_iterator& b)
        {
            return b._v - a._v >= 0;
        }

        friend bool operator>=(const ptr_iterator& a, const ptr_iterator& b)
        {
            return a._v - b._v >= 0;
        }

        friend difference_type operator-(const ptr_iterator& a, const ptr_iterator& b)
        {
            return a._v - b._v;
        }

        friend ptr_iterator operator+(const ptr_iterator& a, difference_type n)
        {
            return ptr_iterator(a._v + n);
        }

        friend ptr_iterator operator+(difference_type n, const ptr_iterator& a)
        {
            return ptr_iterator(a._v + n);
        }

        friend bool operator==(const ptr_iterator& a, const ptr_iterator& b)
        {
            return a._v == b._v;
        }

        friend bool operator!=(const ptr_iterator& a, const ptr_iterator& b)
        {
            return a._v != b._v;
        }

    private:
        V* _v;
    };
}

#endif //BLT_MEMORY_UTIL_H
