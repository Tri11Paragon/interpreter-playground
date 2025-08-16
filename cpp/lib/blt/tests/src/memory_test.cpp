/*
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
#include <blt/std/allocator.h>
#include <memory_test.h>

#include <blt/std/logging.h>
#include <blt/std/memory.h>
#include <blt/std/assert.h>
#include <blt/std/random.h>
#include <type_traits>
#include "blt/std/utility.h"
#include "blt/std/vector.h"
#include <unordered_set>
#include <blt/compatibility.h>

template<typename T>
blt::scoped_buffer<T> create_scoped_buffer(size_t size)
{
    static std::random_device dev;
    static std::mt19937_64 engine(dev());
    blt::scoped_buffer<T> data(size);
    if constexpr (std::is_floating_point_v<T>)
    {
        static std::uniform_real_distribution<T> dist(std::numeric_limits<T>::min(), std::numeric_limits<T>::max());
        
        for (auto& v : data)
            v = dist(engine);
    } else if (std::is_integral_v<T>)
    {
        static std::uniform_int_distribution<T> dist(std::numeric_limits<T>::min(), std::numeric_limits<T>::max());
        
        for (auto& v : data)
            v = dist(engine);
    }
    return data;
}

template<typename T>
blt::scoped_buffer<T> modify_copy(blt::scoped_buffer<T> fill)
{
    for (size_t i = 0; i < size_t(fill.size() / 2); i++)
    {
        std::swap(fill[i], fill[fill.size() - i - 1]);
    }
    return fill;
}

template<typename T>
T collect(blt::scoped_buffer<T> buff)
{
    T val = 0;
    for (auto v : buff)
        val = std::max(v, val);
    return val;
}

void blt::test::memory::copy()
{
    BLT_INFO("Running memory copy tests");
    
    auto int_buffer_small = create_scoped_buffer<int32_t>(16);
    auto int_buffer_medium = create_scoped_buffer<int32_t>(512);
    auto int_buffer_large = create_scoped_buffer<int32_t>(8192);
    
    auto float_buffer_small = create_scoped_buffer<float>(16);
    auto float_buffer_medium = create_scoped_buffer<float>(512);
    auto float_buffer_large = create_scoped_buffer<float>(8192);
    
    auto int_small = collect(modify_copy(int_buffer_small));
    auto int_medium = collect(modify_copy(int_buffer_medium));
    auto int_large = collect(modify_copy(int_buffer_large));
    
    auto float_small = collect(modify_copy(float_buffer_small));
    auto float_medium = collect(modify_copy(float_buffer_medium));
    auto float_large = collect(modify_copy(float_buffer_large));
    
    BLT_TRACE("We collected values [%d, %d, %d]; [%f.0, %f.0, %f.0]", int_small, int_medium, int_large, float_small, float_medium, float_large);
}

void blt::test::memory::move()
{
    BLT_INFO("Running memory move tests");
    
    
}

void blt::test::memory::access()
{
    BLT_INFO("Running memory construction tests");
    
}

void blt::test::memory::static_vector_test()
{
    blt::static_vector<int, 16> vec;
    
    for (size_t i = 0; i < 16; i++)
        vec[i] = static_cast<int>(i * 2);
    
    for (size_t i = 0; i < 16; i++)
        BLT_DEBUG_STREAM << vec[i] << ' ';
    BLT_DEBUG_STREAM << '\n';
    
    vec[3] = 120;
    vec[7] = 230;
    
    vec.reserve(vec.capacity());
    
    for (auto v : vec)
        BLT_DEBUG_STREAM << v << ' ';
    BLT_DEBUG_STREAM << '\n';
    
    vec.reserve(0);
    
    for (size_t i = 0; i < vec.capacity(); i++)
    {
        if (!vec.push_back(static_cast<int>(i)))
            BLT_INFO("Failed to insert on %d", i);
    }
    
    if (!vec.push_back(10))
        BLT_INFO("Vector unable to push, current size vs capacity: %d vs %d", vec.size(), vec.capacity());
    
    for (auto v : vec)
        BLT_DEBUG_STREAM << v << ' ';
    BLT_DEBUG_STREAM << '\n';
}

struct fucked_type2
{
    public:
        static constexpr size_t initial_value = 50;
        int T = 0;
    public:
        fucked_type2()
        {
            T = initial_value;
            //BLT_DEBUG("I HAVE BEEN CONSTRUCTED");
        }
        
        void set(int t)
        {
            T = t;
        }
        
        ~fucked_type2()
        {
            //BLT_DEBUG("I HAVE BEEN DESTRUCTED!");
        }
};

#define ALLOC(alloc, amount) alloc.allocate(amount), amount

/**
 * run tests to make sure that we can actually allocate blocks of memory.
 * we are using a custom type to ensure that the state is known and the example is complex enough
 * if this work then it should work for any generic type
 */
template<size_t allocator_size = 20>
void test_allocations_1()
{
    std::vector<std::pair<fucked_type2*, size_t>> types;
    blt::area_allocator<fucked_type2, allocator_size> int_test{};
    
    types.emplace_back(ALLOC(int_test, static_cast<size_t>(allocator_size * 0.75)));
    for (size_t i = 0; i < static_cast<size_t>(allocator_size * 0.30); i++)
    {
        types.emplace_back(ALLOC(int_test, 1));
        auto v = std::pair{ALLOC(int_test, 1)};
        v.first->set(120);
        int_test.deallocate(v.first, 1);
        types.emplace_back(ALLOC(int_test, 1));
        types.emplace_back(ALLOC(int_test, 1));
        types.emplace_back(ALLOC(int_test, 1));
    }
    
    types.emplace_back(ALLOC(int_test, 1));
    types.emplace_back(ALLOC(int_test, 1));
    types.emplace_back(ALLOC(int_test, 1));
    types.emplace_back(ALLOC(int_test, 1));
    types.emplace_back(ALLOC(int_test, 1));
    
    bool passed = true;
    
    std::unordered_set<fucked_type2*> used_pointers;
    
    for (const auto& pair : types)
    {
        for (size_t i = 0; i < pair.second; i++)
        {
            // every value should be the initial value assigned in the constructor
            // if this isn't the case there was an error.
            if (pair.first[i].T != fucked_type2::initial_value)
            {
                BLT_WARN("We have an allocated value that isn't initial at index %d (allocated in a block of size %d at pointer %p)", i, pair.second,
                         pair.first);
                passed = false;
                break;
            }
            // every allocation here should be unique.
            // if we have a pointer in our list which is not unique,
            // we know we have an error
            if (BLT_CONTAINS(used_pointers, &pair.first[i]))
            {
                BLT_WARN(
                        "We have found another pointer which was allocated as a unique block but isn't (in block %d with size %d; pointer in question: %p)",
                        i, pair.second, pair.first);
                passed = false;
                break;
            }
            used_pointers.insert(&pair.first[i]);
        }
        int_test.deallocate(pair.first, pair.second);
    }
    if (passed)
        BLT_INFO("Test (1) with size %d passed!", allocator_size);
    else
        BLT_ERROR("Test (1) with size %d failed!", allocator_size);
}

template<size_t allocator_size = 20>
void test_allocations_2()
{
    std::vector<int, blt::area_allocator<int, allocator_size>> vec;
    for (size_t i = 0; i < allocator_size * 2; i++)
    {
        vec.push_back(10);
        vec.push_back(42);
    }
    bool passed = true;
    for (size_t i = 0; i < vec.size(); i += 2)
    {
        if (vec[i] != 10 && vec[i] != 42)
            passed = false;
    }
    if (passed)
        BLT_INFO("Test (2) with size %d passed!", allocator_size);
    else
        BLT_ERROR("Test (2) with size %d failed!", allocator_size);
    blt::black_box(vec);
}

void blt::test::memory::test()
{
    test_allocations_1();
    test_allocations_1<1024 * 4>();
    test_allocations_1<1024 * 8>();
    test_allocations_1<1024 * 16>();
    test_allocations_2();
    test_allocations_2<1024 * 4>();
    test_allocations_2<1024 * 8>();
    test_allocations_2<1024 * 16>();
    
    std::vector<std::pair<fucked_type2*, size_t>> types;
    area_allocator<fucked_type2, 20> int_test{};
    //auto arr = int_test.allocate(10);
    types.emplace_back(ALLOC(int_test, 15));
    types.emplace_back(ALLOC(int_test, 1));
    auto v = std::pair{ALLOC(int_test, 1)};
    v.first->set(120);
    int_test.deallocate(v.first, 1);
    types.emplace_back(ALLOC(int_test, 1));
    types.emplace_back(ALLOC(int_test, 1));
    types.emplace_back(ALLOC(int_test, 1));
    
    types.emplace_back(ALLOC(int_test, 1));
    types.emplace_back(ALLOC(int_test, 1));
    types.emplace_back(ALLOC(int_test, 1));
    types.emplace_back(ALLOC(int_test, 1));
    types.emplace_back(ALLOC(int_test, 1));
    //blt::black_box(arr4);
    BLT_INFO("CUM");
    
    for (const auto& pair : types)
    {
        BLT_TRACE("Pointer: %p", pair.first);
        for (size_t i = 0; i < pair.second; i++)
        {
            BLT_TRACE_STREAM << pair.first[i].T << ' ';
        }
        BLT_TRACE_STREAM << '\n';
        int_test.deallocate(pair.first, pair.second);
        BLT_INFO("-----------------");
    }
}
