/*
 * Created by Brett on 26/12/22.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_STRING_H
#define BLT_STRING_H

#include <iostream>
#include <string>
#include <string_view>
#include <sstream>
#include <algorithm>
#include <vector>
#include <optional>
#include <cctype>
#include <unordered_set>
#include <blt/compatibility.h>

namespace blt::string
{
    
    class StringBuffer
    {
        private:
            const size_t BLOCK_SIZE = 4096;
            size_t front = 0;
            size_t size = 0;
            char* characterBuffer = nullptr;
            
            void expand();
        
        public:
            void trim();
            
            std::string str();
            
            StringBuffer()
            {
                characterBuffer = static_cast<char*>(malloc(BLOCK_SIZE));
                size = BLOCK_SIZE;
            }
            
            StringBuffer& operator<<(char c);
            
            StringBuffer& operator<<(const std::string& str)
            {
                for (char c : str)
                    *this << c;
                return *this;
            }
            
            template<typename T>
            inline StringBuffer& operator<<(T t)
            {
                *this << std::to_string(t);
                return *this;
            }
            
            ~StringBuffer()
            {
                free(characterBuffer);
            }
    };
    
    static inline BLT_CPP20_CONSTEXPR bool starts_with(std::string_view string, std::string_view search)
    {
#ifdef BLT_USE_CPP20
        return string.starts_with(search);
#else
        if (search.length() > string.length())
            return false;
        for (unsigned int i = 0; i < search.length(); i++)
        {
            if (string[i] != search[i])
                return false;
        }
        return true;
#endif
    }
    
    static inline BLT_CPP20_CONSTEXPR bool starts_with(std::string_view string, char search)
    {
#ifdef BLT_USE_CPP20
        return string.starts_with(search);
#else
        if (string.empty())
            return false;
        return string.front() == search;
#endif
    }
    
    inline BLT_CPP20_CONSTEXPR bool ends_with(std::string_view string, std::string_view search)
    {
#ifdef BLT_USE_CPP20
        return string.ends_with(search);
#else
        if (string.empty())
            return false;
        return string.substr(string.size() - search.size()) == search;
#endif
    }
    
    static inline BLT_CPP20_CONSTEXPR bool ends_with(std::string_view string, char search)
    {
#ifdef BLT_USE_CPP20
        return string.ends_with(search);
#else
        if (string.empty())
            return false;
        return string.back() == search;
#endif
    }
    
    std::optional<std::vector<size_t>> containsAll(std::string_view string, const std::unordered_set<char>& search);
    
    size_t contains(std::string_view string, const std::unordered_set<char>& search);
    
    
    BLT_CPP20_CONSTEXPR bool contains(std::string_view string, char search);
    
    BLT_CPP20_CONSTEXPR bool contains(std::string_view string, std::string_view search);
    
    /**
     * Converts the string into lower case
     * @param s string to lower case
     * @return a string copy that is all lower case
     */
    BLT_CPP20_CONSTEXPR std::string toLowerCase(std::string_view s);
    
    /**
     * Converts the string into upper case
     * @param s string to upper case
     * @return a string copy that is all upper case
     */
    BLT_CPP20_CONSTEXPR std::string toUpperCase(std::string_view s);
    
    // taken from https://stackoverflow.com/questions/14265581/parse-split-a-string-in-c-using-string-delimiter-standard-c
    // extended to return a vector
    BLT_CPP20_CONSTEXPR std::vector<std::string> split(std::string_view s, std::string_view delim);
    
    BLT_CPP20_CONSTEXPR std::vector<std::string> split(std::string_view s, char delim);
    
    BLT_CPP20_CONSTEXPR std::vector<std::string_view> split_sv(std::string_view s, std::string_view delim);
    
    BLT_CPP20_CONSTEXPR std::vector<std::string_view> split_sv(std::string_view s, char delim);
    
    // https://stackoverflow.com/questions/3418231/replace-part-of-a-string-with-another-string
    static inline BLT_CPP20_CONSTEXPR bool replace(std::string& str, std::string_view from, std::string_view to)
    {
        size_t start_pos = str.find(from);
        if (start_pos == std::string::npos)
            return false;
        str.replace(start_pos, from.length(), to);
        return true;
    }
    
    BLT_CPP20_CONSTEXPR void replaceAll(std::string& str, std::string_view from, std::string_view to);
    
    // taken from https://stackoverflow.com/questions/216823/how-to-trim-an-stdstring
    // would've preferred to use boost lib but instructions said to avoid external libs
    // trim from start (in place)
    static inline BLT_CPP20_CONSTEXPR std::string& ltrim(std::string& s)
    {
        s.erase(
                s.begin(), std::find_if(
                        s.begin(), s.end(), [](unsigned char ch) {
                            return !std::isblank(ch);
                        }
                ));
        return s;
    }
    
    // trim from end (in place)
    static inline BLT_CPP20_CONSTEXPR std::string& rtrim(std::string& s)
    {
        s.erase(
                std::find_if(
                        s.rbegin(), s.rend(), [](unsigned char ch) {
                            return !std::isblank(ch);
                        }
                ).base(), s.end());
        return s;
    }
    
    static inline BLT_CPP20_CONSTEXPR std::string_view ltrim(std::string_view s)
    {
        size_t start_pos = 0;
        for (auto c = s.begin(); c != s.end() && std::isblank(*c); ++c, start_pos++);
        return s.substr(start_pos);
    }
    
    static inline BLT_CPP20_CONSTEXPR std::string_view rtrim(std::string_view s)
    {
        size_t end_pos = 0;
        for (auto c = s.rbegin(); c != s.rend() && std::isblank(*c); ++c, end_pos++);
        return s.substr(0, s.size() - end_pos);
    }
    
    static inline BLT_CPP20_CONSTEXPR std::string_view trim(std::string_view s)
    {
        size_t start_pos = 0;
        for (auto c = s.begin(); c != s.end() && std::isblank(*c); ++c, start_pos++);
        size_t end_pos = s.size();
        for (auto c = s.rbegin(); c != s.rend() && std::isblank(*c); ++c, end_pos--);
        return s.substr(start_pos, end_pos - start_pos);
    }
    
    // trim from both ends (in place)
    static inline BLT_CPP20_CONSTEXPR std::string& trim(std::string& s)
    {
        ltrim(s);
        rtrim(s);
        return s;
    }
    
    // trim from start (copying)
    static inline BLT_CPP20_CONSTEXPR std::string ltrim_copy(std::string s)
    {
        ltrim(s);
        return s;
    }
    
    // trim from end (copying)
    static inline BLT_CPP20_CONSTEXPR std::string rtrim_copy(std::string s)
    {
        rtrim(s);
        return s;
    }
    
    // trim from both ends (copying)
    static inline BLT_CPP20_CONSTEXPR std::string trim_copy(std::string s)
    {
        trim(s);
        return s;
    }
    
    static inline BLT_CPP20_CONSTEXPR bool is_numeric(const std::string& s)
    {
#if __cplusplus >= 202002L
        return std::ranges::all_of(s, [](char c) -> bool {
            return std::isdigit(c);
        });
#else
        for (const char c : s)
        {
            if (!std::isdigit(c))
                return false;
        }
        return true;
#endif
    }
    
    inline std::string ensure_ends_with_path_separator(std::string_view string)
    {
        if (ends_with(string, '/'))
            return std::string(string);
        else
            return std::string(string) += '/';
    }
    
    inline std::string ensure_ends_with_path_separator(std::string&& string)
    {
        if (ends_with(string, '/'))
            return string;
        else
            return (std::move(string) + '/');
    }
    
    class match
    {
        private:
        
        public:
        
    };
    
}

#endif //BLT_STRING_H
