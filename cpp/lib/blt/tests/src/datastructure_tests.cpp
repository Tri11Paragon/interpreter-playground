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
#include <blt_tests.h>
#include <blt/std/queue.h>
#include <blt/std/binary_tree.h>
#include <queue>
#include <stack>
#include <vector>
#include <blt/profiling/profiler_v2.h>
#include <random>
#include <cmath>

namespace blt
{
    
    constexpr size_t max_size = 100000000;
    constexpr size_t min_size = 10000;
    
    size_t exp(size_t base, size_t e)
    {
        size_t collect = 1;
        for (size_t i = 0; i < e; i++)
            collect *= base;
        return collect;
    }
    
    void run_size(size_t size)
    {
        std::vector<int> random_data;
        std::random_device dev;
        std::mt19937_64 engine(dev());
        std::uniform_int_distribution gen(std::numeric_limits<int>::min(), std::numeric_limits<int>::max());
        
        for (size_t i = 0; i < size; i++)
            random_data.push_back(gen(engine));
        
        auto insertProfile = "Insert (" + std::to_string(size) += ')';
        auto readProfile = "Read (" + std::to_string(size) += ')';
        
        std::stack<int> std_stack;
        BLT_START_INTERVAL(insertProfile, "std::stack");
        for (size_t i = 0; i < size; i++)
            std_stack.push(random_data[i]);
        BLT_END_INTERVAL(insertProfile, "std::stack");
        
        blt::flat_stack<int> blt_flat_stack;
        BLT_START_INTERVAL(insertProfile, "blt::flat_stack");
        for (size_t i = 0; i < size; i++)
            blt_flat_stack.push(random_data[i]);
        BLT_END_INTERVAL(insertProfile, "blt::flat_stack");
        
        blt::linked_stack<int> blt_linked_stack;
        BLT_START_INTERVAL(insertProfile, "blt::linked_stack");
        for (size_t i = 0; i < size; i++)
            blt_linked_stack.push(random_data[i]);
        BLT_END_INTERVAL(insertProfile, "blt::linked_stack");
        
        std::vector<int> vector_stack;
        BLT_START_INTERVAL(insertProfile, "std::vector");
        for (size_t i = 0; i < size; i++)
            vector_stack.push_back(random_data[i]);
        BLT_END_INTERVAL(insertProfile, "std::vector");
        
        BLT_START_INTERVAL(readProfile, "std::stack");
        for (size_t i = 0; i < size; i++)
        {
            blt::black_box(std_stack.top());
            std_stack.pop();
        }
        BLT_END_INTERVAL(readProfile, "std::stack");
        
        BLT_START_INTERVAL(readProfile, "blt::flat_stack");
        for (size_t i = 0; i < size; i++)
        {
            blt::black_box(blt_flat_stack.top());
            blt_flat_stack.pop();
        }
        BLT_END_INTERVAL(readProfile, "blt::flat_stack");
        
        BLT_START_INTERVAL(readProfile, "blt::linked_stack");
        for (size_t i = 0; i < size; i++)
        {
            blt::black_box(blt_linked_stack.top());
            blt_linked_stack.pop();
        }
        BLT_END_INTERVAL(readProfile, "blt::linked_stack");
        
        BLT_START_INTERVAL(readProfile, "std::vector");
        for (size_t i = 0; i < size; i++)
        {
            blt::black_box(vector_stack.back());
            vector_stack.pop_back();
        }
        BLT_END_INTERVAL(readProfile, "std::vector");
        
        BLT_PRINT_PROFILE(insertProfile);
        BLT_PRINT_PROFILE(readProfile);
    }
    
    void test::data::run()
    {
//        auto max = static_cast<size_t>(std::log10(max_size));
//        auto min = static_cast<size_t>(std::log10(min_size));
//        for (size_t i = min; i <= max; i++)
//            run_size(exp(10, i));
        
        double d = -1;
        char data[sizeof(d)]{};
        long v = 0;
        std::memcpy(data, &d, sizeof(d));
        blt::mem::fromBytes(data, v);
        for (int i = 0; i < 64; i++)
        {
            if (i == 1 || i == 12)
                std::cout << ' ';
            std::cout << ((v >> i) & 0x1);
        }
        std::cout << std::endl;
        
        BLT_TRACE("%s", std::to_string(1.0/27.0).c_str());
    }
    
}