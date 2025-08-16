/*
 * Created by Brett on 08/02/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */
#include <blt/fs/loader.h>
#include <blt/std/assert.h>

std::vector<std::string> blt::fs::getLinesFromFile(std::string_view path)
{
    std::string file = getFile(path);
    // we only use unix line endings here...
    string::replaceAll(file, "\r", "");
    // split the file into the lines, this way we can get out the #include statements.
    return string::split(file, "\n");
}

std::vector<std::string> blt::fs::recursiveInclude(std::string_view path, const std::string& include_header,
                                                   const std::vector<include_guard>& guards)
{
    auto mainLines = getLinesFromFile(path);
    std::vector<std::string> return_lines;
    
    for (auto& line : mainLines)
    {
        // if the line is an include statement then we want to add lines recursively.
        auto include_pos = line.find(include_header);
        if (include_pos != std::string::npos)
        {
            auto past_include = include_pos + include_header.size();
            std::string file_to_include;
            
            if (guards.empty())
            {
                file_to_include = line.substr(past_include);
            } else
            {
                size_t index = past_include;
                while (std::find_if(guards.begin(), guards.end(), [&](const include_guard& item) {
                    return index < line.size() && line[index] == item.open;
                }) == guards.end())
                    index++;
                index++;
                BLT_ASSERT(index < line.size() && "Include found but no file was provided!");
                
                while (std::find_if(guards.begin(), guards.end(), [&](const include_guard& item) {
                    return index < line.size() && line[index] == item.close;
                }) == guards.end())
                    file_to_include += line[index++];
            }
            // ignore absolute paths TODO: path lib
            //if (!blt::string::starts_with(blt::string::trim(file_to_include), '/'))
            
            auto lines = recursiveInclude(file_to_include, include_header, guards);
            for (const auto& i_line : lines)
                return_lines.push_back(i_line);
        } else
            return_lines.push_back(line);
    }
    
    return return_lines;
}

std::string blt::fs::getFile(std::string_view path)
{
    std::string file_contents;
    std::ifstream the_file;
    if (!the_file.good())
        BLT_ERROR("Input stream not good!\n");
    // ensure ifstream objects can throw exceptions:
    the_file.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    try
    {
        // open file
        the_file.open(std::string(path));
        std::stringstream file_stream;
        // read file's buffer contents into streams
        file_stream << the_file.rdbuf();
        // close file handlers
        the_file.close();
        // convert stream into std::string
        file_contents = file_stream.str();
    } catch (std::ifstream::failure& e)
    {
        BLT_WARN("Unable to read file '{}'!\n", std::string(path).c_str());
        BLT_WARN("Exception: {}", e.what());
        BLT_THROW(std::runtime_error("Failed to read file!\n"));
    }
    return file_contents;
}
