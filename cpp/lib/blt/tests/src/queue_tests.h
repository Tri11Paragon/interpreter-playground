/*
 * Created by Brett on 28/02/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_TESTS_QUEUE_TESTS_H
#define BLT_TESTS_QUEUE_TESTS_H

#include <queue>
#include <blt/std/queue.h>
#include <blt/std/logging.h>
#include <blt/profiling/profiler.h>
#include <array>
#include <blt/std/random.h>

std::array<int, 15000> values{};

std::queue<int> base_queue{};
blt::flat_queue<int> blt_flat_queue{};
blt::flat_stack<int> blt_flat_stack{};

static inline void fill_queues(){
    BLT_START_INTERVAL("Insert", "std::queue");
    for (const auto& value : values)
        base_queue.push(value);
    BLT_END_INTERVAL("Insert", "std::queue");
    
    BLT_START_INTERVAL("Insert", "blt::flat_queue");
    for (const auto& value : values)
        blt_flat_queue.push(value);
    BLT_END_INTERVAL("Insert", "blt::flat_queue");
    
    BLT_START_INTERVAL("Insert", "blt::flat_stack");
    for (const auto& value : values)
        blt_flat_stack.push(value);
    BLT_END_INTERVAL("Insert", "blt::flat_stack");
}

static inline void validate(){
    bool std_valid = true;
    bool flat_valid = true;
    bool stack_valid = true;
    BLT_START_INTERVAL("Access", "std::queue");
    for (const auto& value : values) {
        auto front = base_queue.front();
        
        if (front != value)
            std_valid = false;
        base_queue.pop();
    }
    BLT_END_INTERVAL("Access", "std::queue");
        
    BLT_START_INTERVAL("Access", "blt::flat_queue");
    for (const auto& value : values) {
        auto front = blt_flat_queue.front();
        
        if (front != value)
            flat_valid = false;
        blt_flat_queue.pop();
    }
    BLT_END_INTERVAL("Access", "blt::flat_queue");
    
    BLT_START_INTERVAL("Access", "blt::flat_stack");
    for (int i = values.size()-1; i > 0; i--) {
        const auto& value = values[i];
        auto front = blt_flat_stack.top();
    
        if (front != value)
            stack_valid = false;

        blt_flat_stack.pop();
    }
    BLT_END_INTERVAL("Access", "blt::flat_stack");
    
    if (!std_valid)
        BLT_ERROR("std::queue invalid!");
    if (!flat_valid)
        BLT_ERROR("blt::flat_queue invalid!");
    if (!stack_valid)
        BLT_ERROR("blt::stack invalid!");
}

static inline void random_access() {
    bool flat_valid = true;
    bool stack_valid = true;
    
    BLT_START_INTERVAL("Random", "blt::flat_queue");
    for (int i = 0; i < 500; i++) {
        auto front = blt_flat_queue.front();
        auto next = base_queue.front();
        
        if (front != next)
            flat_valid = false;
        blt_flat_queue.pop();
        base_queue.pop();
    }
    for (int value : values){
        blt_flat_queue.push(value);
        base_queue.push(value);
    }
    for (unsigned int i = 0; i < values.size(); i++) {
        auto front = blt_flat_queue.front();
        auto next = base_queue.front();
        
        if (front != next)
            flat_valid = false;
        blt_flat_queue.pop();
        base_queue.pop();
    }
    BLT_END_INTERVAL("Random", "blt::flat_queue");
    
    BLT_START_INTERVAL("Random", "blt::flat_stack");
    for (int i = values.size()-1; i > 0; i--) {
        const auto& value = values[i];
        auto front = blt_flat_stack.top();
        
        if (front != value)
            stack_valid = false;
        
        blt_flat_stack.pop();
    }
    BLT_END_INTERVAL("Random", "blt::flat_stack");
    
    if (!flat_valid)
        BLT_ERROR("blt::flat_queue invalid!");
    if (!stack_valid)
        BLT_ERROR("blt::stack invalid!");
}

static inline void test_queues() {
    blt::random::random<int, std::uniform_int_distribution> rand{1, 100};
    
    for (int& value : values){
        value = rand.get();
    }
    
    fill_queues();
    validate();
    fill_queues();
    random_access();
    
    BLT_PRINT_PROFILE("Insert", blt::logging::log_level::INFO, true);
    BLT_PRINT_PROFILE("Access", blt::logging::log_level::INFO, true);
    BLT_PRINT_PROFILE("Random", blt::logging::log_level::INFO, true);
}

#endif //BLT_TESTS_QUEUE_TESTS_H
