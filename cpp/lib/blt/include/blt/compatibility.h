/*
 *  BLT C++ 20 / C++ 17 Compatability helper file
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

#ifndef BLT_COMPATIBILITY_H
#define BLT_COMPATIBILITY_H

#define BLT_CPP23 202302L
#define BLT_CPP20 202002L
#define BLT_CPP17 201703L
#define BLT_CPP14 201402L
#define BLT_CPP11 201103L

#if __cplusplus >= BLT_CPP23

#endif

#if __cplusplus >= BLT_CPP20
    #define BLT_CONTAINS(container, value) container.contains(value)
    #define BLT_CPP20_CONSTEXPR constexpr
    #define BLT_USE_CPP20
#else
    #include <algorithm>
    #define BLT_CONTAINS(container, value) std::find(container.begin(), container.end(), value) != container.end()
    #define BLT_CPP20_CONSTEXPR
    #undef BLT_USE_CPP20
#endif

#define BLT_CONTAINS_IF(container, value) std::find_if(container.begin(), container.end(), value) != container.end()

#define INCLUDE_FS \
    #if defined(CXX17_FILESYSTEM) || defined (CXX17_FILESYSTEM_LIBFS) \
        #include <filesystem>\
    #elif defined(CXX11_EXP_FILESYSTEM) || defined (CXX11_EXP_FILESYSTEM_LIBFS)\
        #include <experimental/filesystem>\
    #else\
            #error Filesystem ops not supported!\
    #endif

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) && !defined(__CYGWIN__)
    #define BLT_OS_WINDOWS
#elif  defined(__linux__) || defined(__unix__)
    #define BLT_OS_LINUX
#else
    #define BLT_OS_UNKNOWN
#endif

#endif //BLT_COMPATIBILITY_H
