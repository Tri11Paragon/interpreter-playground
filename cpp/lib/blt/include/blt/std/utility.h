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

#ifndef BLT_UTILITY_H
#define BLT_UTILITY_H

#include <string>
#include <utility>

#if defined(__GNUC__)

#include <cxxabi.h>

namespace blt
{
	static std::string demangle(const std::string& str)
	{
		int status;
		// only defined for GNU C++11?
		char* demangled_name = abi::__cxa_demangle(str.c_str(), nullptr, nullptr, &status);
		if (demangled_name == nullptr)
			return str;
		std::string ret_name = demangled_name;
		std::free(demangled_name);
		return ret_name;
	}
}
#else
namespace blt
{
    static inline std::string demangle(const std::string& str)
    {
        return str;
    }
}
#endif

namespace blt
{
	template <typename RefType>
	using ref = std::reference_wrapper<RefType>;

	template <typename T>
	static std::string type_string()
	{
		return demangle(typeid(T).name());
	}

	template <typename T>
	static std::string type_string_raw()
	{
		return typeid(T).name();
	}

	#if defined(__GNUC__) || defined(__llvm__)
	#define BLT_UNREACHABLE __builtin_unreachable()
	#define BLT_ATTRIB_NO_INLINE __attribute__ ((noinline))
	/**
	* means that the return value is solely a function of the arguments,
	* and if any of the arguments are pointers, then the pointers must not be dereferenced.
	* Calls to functions whose return value is not affected by changes to the observable state of the program
	* and that have no observable effects on such state other than
	* to return a value may lend themselves to optimizations such as common subexpression elimination.
	*/
	#define BLT_ATTRIB_NO_SIDE_EFFECTS __attribute__((const))
	/**
	* the function has no side effects and the value returned depends on the arguments and the state of global variables.
	* Therefore it is safe for the optimizer to elide some calls to it, if the arguments are the same,
	* and the caller did not do anything to change the state of the globals in between the calls.
	*/
	#define BLT_ATTRIB_GLOBAL_READ_ONLY __attribute__((pure))
	#else
	#if defined(_MSC_VER) && !defined(__INTEL_COMPILER)
        #define BLT_ATTRIB_NO_INLINE __declspec(noinline)
        #define BLT_UNREACHABLE __assume(false)
	#else
        #define BLT_ATTRIB_NO_INLINE
        #define BLT_UNREACHABLE
	#endif
    #define BLT_ATTRIB_CONST
    #define BLT_ATTRIB_PURE
	#endif

	#if __cplusplus > 202002L
    #undef BLT_UNREACHABLE
    #define BLT_UNREACHABLE std::unreachable()
	#endif

	template <typename T>
	BLT_ATTRIB_NO_INLINE void black_box(const T& val)
	{
		static volatile void* hell;
		hell = (void*) &val;
		(void) hell;
	}

	template <typename T>
	BLT_ATTRIB_NO_INLINE T& black_box_ret(T& val)
	{
		static volatile void* hell;
		hell = (void*) &val;
		(void) hell;
		return val;
	}

	template <typename T>
	BLT_ATTRIB_NO_INLINE const T& black_box_ret(const T& val)
	{
		static volatile void* hell;
		hell = (void*) &val;
		(void) hell;
		return val;
	}
}

#endif //BLT_UTILITY_H
