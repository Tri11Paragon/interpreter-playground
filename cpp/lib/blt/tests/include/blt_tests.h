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

#ifndef BLT_BLT_TESTS_H
#define BLT_BLT_TESTS_H

#include <memory_test.h>
#include <blt/parse/argparse.h>
#include <utility_test.h>
#include <blt/std/utility.h>
#include <templating_test.h>

namespace blt::test
{
    namespace data
    {
        void run();
    }
    
    namespace sys
    {
    
    }
    
    void vector_run();
    void allocator();
    void fixed_point();
}

#endif //BLT_BLT_TESTS_H
