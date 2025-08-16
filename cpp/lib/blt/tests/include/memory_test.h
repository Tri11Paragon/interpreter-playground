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

#ifndef BLT_TESTS_MEMORY_TEST_H
#define BLT_TESTS_MEMORY_TEST_H

namespace blt::test::memory
{
    void copy();
    
    void move();
    
    void access();
    
    void static_vector_test();
    
    void test();
    
    static inline void run()
    {
        copy();
        move();
        access();
        static_vector_test();
        test();
    }
}

#endif //BLT_TESTS_MEMORY_TEST_H
