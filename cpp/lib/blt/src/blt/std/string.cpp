//
// Created by brett on 7/9/23.
//
#include <blt/std/string.h>

namespace blt
{
    
    void blt::string::StringBuffer::expand()
    {
        size_t multiplier = size / BLOCK_SIZE;
        auto newSize = BLOCK_SIZE * (multiplier * 2);
        characterBuffer = static_cast<char*>(realloc(characterBuffer, newSize));
        size = newSize;
    }
    
    void blt::string::StringBuffer::trim()
    {
        characterBuffer = static_cast<char*>(realloc(characterBuffer, front + 1));
        size = front + 1;
        characterBuffer[front] = '\0';
    }
    
    blt::string::StringBuffer& blt::string::StringBuffer::operator<<(char c)
    {
        characterBuffer[front++] = c;
        if (front > size)
            expand();
        return *this;
    }
    
    std::string blt::string::StringBuffer::str()
    {
        trim();
        return std::string{characterBuffer};
    }
    
    
    bool string::contains(std::string_view string, std::string_view search)
    {
        if (search.length() > string.length())
            return false;
        for (unsigned int i = 0; i < string.length(); i++)
        {
            if (string[i] == search[0])
            {
                bool correct = true;
                for (unsigned int j = 0; j < search.length(); j++)
                {
                    if (string[i + j] != search[j])
                    {
                        correct = false;
                        break;
                    }
                }
                if (correct)
                    return true;
            }
        }
        return false;
    }
    
    std::string string::toLowerCase(std::string_view s)
    {
        std::string str;
        std::for_each(
                s.begin(), s.end(), [&str](unsigned char ch) {
                    str += (char) std::tolower(ch);
                }
        );
        return str;
    }
    
    bool string::contains(std::string_view string, const char search)
    {
#if __cplusplus >= 202002L
        return std::ranges::any_of(string, [search](const char c) -> bool {
            return c == search;
        });
#else
        for (const char c : string)
        {
            if (c == search)
                return true;
        }
        return false;
#endif
    }
    
    size_t string::contains(std::string_view string, const std::unordered_set<char>& search)
    {
        for (size_t i = 0; i < string.length(); i++)
        {
            if (BLT_CONTAINS(search, string[i]))
                return i;
        }
        return false;
    }
    
    std::optional<std::vector<size_t>> string::containsAll(std::string_view string, const std::unordered_set<char>& search)
    {
        std::vector<size_t> pos;
        for (size_t i = 0; i < string.length(); i++)
        {
            if (BLT_CONTAINS(search, string[i]))
                pos.push_back(i);
        }
        if (!pos.empty())
            return pos;
        return {};
    }
    
    std::string string::toUpperCase(std::string_view s)
    {
        std::string str;
        std::for_each(
                s.begin(), s.end(), [&str](unsigned char ch) {
                    str += (char) std::toupper(ch);
                }
        );
        return str;
    }
    
    std::vector<std::string> string::split(std::string_view s, std::string_view delim)
    {
        size_t pos = 0;
        size_t from = 0;
        std::vector<std::string> tokens;
        while ((pos = s.find(delim, from)) != std::string::npos)
        {
            auto size = pos - from;
            auto token = s.substr(from, size);
            if (!token.empty())
                tokens.emplace_back(token);
            from += size + delim.length();
        }
        auto str = s.substr(from);
        if (!str.empty())
            tokens.emplace_back(str);
        return tokens;
    }
    
    std::vector<std::string> string::split(std::string_view s, char delim)
    {
        size_t pos = 0;
        size_t from = 0;
        std::vector<std::string> tokens;
        while ((pos = s.find(delim, from)) != std::string::npos)
        {
            auto size = pos - from;
            auto token = s.substr(from, size);
            if (!token.empty())
                tokens.emplace_back(token);
            from += size + 1;
        }
        auto str = s.substr(from);
        if (!str.empty())
            tokens.emplace_back(str);
        return tokens;
    }
    
    std::vector<std::string_view> string::split_sv(std::string_view s, std::string_view delim)
    {
        size_t pos = 0;
        size_t from = 0;
        std::vector<std::string_view> tokens;
        while ((pos = s.find(delim, from)) != std::string::npos)
        {
            auto size = pos - from;
            auto token = s.substr(from, size);
            if (!token.empty())
                tokens.push_back(token);
            from += size + delim.length();
        }
        auto str = s.substr(from);
        if (!str.empty())
            tokens.push_back(str);
        return tokens;
    }
    
    std::vector<std::string_view> string::split_sv(std::string_view s, char delim)
    {
        size_t pos = 0;
        size_t from = 0;
        std::vector<std::string_view> tokens;
        while ((pos = s.find(delim, from)) != std::string::npos)
        {
            auto size = pos - from;
            auto token = s.substr(from, size);
            if (!token.empty())
                tokens.push_back(token);
            from += size + 1;
        }
        auto str = s.substr(from);
        if (!str.empty())
            tokens.push_back(str);
        return tokens;
    }
    
    void string::replaceAll(std::string& str, std::string_view from, std::string_view to)
    {
        if (from.empty())
            return;
        size_t start_pos = 0;
        while ((start_pos = str.find(from, start_pos)) != std::string::npos)
        {
            str.replace(start_pos, from.length(), to);
            start_pos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
        }
    }
    
    
}
