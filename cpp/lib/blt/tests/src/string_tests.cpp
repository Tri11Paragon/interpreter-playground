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
#include <string_tests.h>
#include <blt/std/utility.h>
#include <blt/std/string.h>
#include <blt/std/logging.h>
#include <blt/iterator/iterator.h>

namespace blt::test
{
    void run_string_test()
    {
        std::string str = "I HAVE A LOT OF\tTABULAR\tFORMED MEMORIES";
        
        auto s_splits_c = blt::string::split(str, ' ');
        auto s_splits_s = blt::string::split(str, "LOT");
        
        auto sv_splits_c = blt::string::split_sv(str, ' ');
        auto sv_splits_s = blt::string::split_sv(str, "LOT");
        
        for (auto [index, item] : blt::enumerate(s_splits_c))
        {
            if (item != sv_splits_c[index])
            {
                BLT_WARN("THEY DO NOT MATCH!!! '%s' vs '%s'", item.c_str(), std::string(sv_splits_c[index]).c_str());
            } else
            {
                BLT_DEBUG(item);
            }
        }
        
        BLT_INFO("");
        
        for (auto [index, item] : blt::enumerate(s_splits_s))
        {
            if (item != sv_splits_s[index])
            {
                BLT_WARN("THEY DO NOT MATCH!!! '%s' vs '%s'", item.c_str(), std::string(sv_splits_s[index]).c_str());
            } else
            {
                BLT_DEBUG(item);
            }
        }
    }
}