/*
 * Created by Brett on 29/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_TESTS_NBT_TESTS_H
#define BLT_TESTS_NBT_TESTS_H

#include <blt/std/random.h>
#include <cstring>

namespace blt::tests {
    
    template<typename T>
    T* generateRandomData(T* arr, size_t size, uint32_t seed = 0) {
        for (size_t i = 0; i < size; i++)
            arr[i] = blt::random::random_t(i * size + seed).get(std::numeric_limits<T>::min(), std::numeric_limits<T>::max());
        
        return arr;
    }
    
    // test block fs vs std::ios
    void nbtFSBlockRead(size_t buff_size, size_t arr_size, size_t num_array, unsigned char** arr);
    void nbtFSBlockWrite(size_t buff_size, size_t arr_size, size_t num_array, unsigned char** arr);
    void nbtFSRead(size_t buff_size, size_t arr_size, size_t num_array, unsigned char** arr);
    void nbtFSWrite(size_t buff_size, size_t arr_size, size_t num_array, unsigned char** arr);
    void nbtFSTest(size_t total_size);
    
    // test raw ios r/w speeds
    void nbtRawRead();
    void nbtRawWrite();
    void nbtRawTest();
    
    // test nbt r/w speeds
    void nbtRead();
    void nbtWrite();
    void nbtTest();
}

#endif //BLT_TESTS_NBT_TESTS_H
