/*
 * Created by Brett on 08/02/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_TESTS_LOADER_H
#define BLT_TESTS_LOADER_H

#include <fstream>
#include <ios>
#include <vector>
#include <unordered_map>
#include <string>
#include <blt/std/string.h>
#include <blt/logging/logging.h>
#include <unordered_set>

namespace blt::fs
{
    struct include_guard
    {
        char open = '<';
        char close = '>';
    };
    
    std::string getFile(std::string_view path);
    
    std::vector<std::string> getLinesFromFile(std::string_view path);
    
    /**
     * Recursively include files
     * @param path initial file to load
     * @param include_header the beginning of the line that should be used to recognize when a line is to be treated as an include
     * @param guards characters used to identify the parts that specify the file path. if empty it will assume everything after the include header
     * @return a list of lines in all files. added together in order.
     */
    std::vector<std::string> recursiveInclude(std::string_view path, const std::string& include_header = "#include",
                                              const std::vector<include_guard>& guards = {{'<',  '>'}, {'"', '"'}});
    
    static inline std::string loadBrainFuckFile(const std::string& path)
    {
        std::string buffer;
        
        auto lines = recursiveInclude(path, "~", {});
        
        for (auto& line : lines)
        {
            buffer += line;
            buffer += '\n';
        }
        
        return buffer;
    }
}

#endif //BLT_TESTS_LOADER_H
