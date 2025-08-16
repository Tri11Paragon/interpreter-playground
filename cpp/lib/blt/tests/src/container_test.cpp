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

#include <blt_tests.h>
#include <blt/std/vector.h>
#include <blt/std/logging.h>
#include "blt/std/assert.h"

namespace blt::test
{
    
    template<typename T>
    void print(const T& ref)
    {
        BLT_TRACE_STREAM << "(" << ref.size() << ") [";
        for (auto [index, item] : blt::enumerate(ref))
            BLT_TRACE_STREAM << item << ((index != ref.size()-1) ? ", " : "]\n");
    }
    
    void vector_run()
    {
        
        blt::vector<int> vec;
        
        vec.push_back(10);
        vec.push_back(20);
        vec.push_back(30);
        vec.push_back(40);
        vec.push_back(50);
        vec.push_back(60);
        vec.push_back(70);
        vec.push_back(80);
        vec.push_back(90);
        
        print(vec);
        BLT_ASSERT(vec.size() == 9 && "Failed at push_back");
        
        vec.insert(vec.cbegin() + 2, 25);
        BLT_ASSERT(vec.size() == 10 && "Failed at insert single");
        
        print(vec);
        
        for (int i = 0; i < 128; i++)
            vec.insert(vec.begin() + 2, i);
        BLT_ASSERT(vec.size() == 138 && "Failed at insert 128");
        
        print(vec);
        
        vec.erase(vec.begin() + 3, vec.begin() + 8);
        BLT_ASSERT(vec.size() == 133 && "Failed at erase range (non end)");
        
        print(vec);
        
        vec.erase(vec.begin() + 5);
        
        print(vec);
        BLT_ASSERT(vec.size() == 132 && "Failed at erase single");
        
        vec.erase(vec.begin() + 10, vec.end());
        
        print(vec);
        BLT_ASSERT(vec.size() == 10 && "Failed at erase range (end)");
    }
}
