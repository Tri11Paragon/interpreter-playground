#pragma once
/*
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

#ifndef BLT_DEBUG_H
#define BLT_DEBUG_H

// Flag List:
// note: this list contains flags for other BLT projects. I'm not a fan of how centralized this is, or how difficult it is to add more flags.
// it is an open TODO to find a better solution to this problem

// current idea; as blt-gp needs a lot of flags, some which range from a quick if statement to a full walking of a tree, it might be useful to define
// general computational flags rather than directly trying to create flags for every use case.
// For example, we could replace the following initial list:
// #define BLT_DEBUG_GP_TREE 1
// #define BLT_DEBUG_GP_STACK 2
// #define BLT_DEBUG_OPENGL_FLAG 8
// could be better replaced by this list:

// note: the performance examples are not strict requirements but helpful guidelines for future implementations
// *I will be very annoyed if turning on BLT_DEBUG_CHEAP significantly slows down a program*

// should be used by checks which only consume a single comparison, with at most a single indirection
#define BLT_DEBUG_CHEAP			(1 << 0)
// should be used to enable the use of asserts
#define BLT_DEBUG_ASSERTS		(1 << 1)
// should be used to enable the use of programmer contracts which validate the use of an API
#define BLT_DEBUG_CONTRACTS		(1 << 2)
// should be used to enable simple atomic counters
#define BLT_DEBUG_COUNTERS		(1 << 3)
// should be used to enable allocation tracking measures
#define BLT_DEBUG_ALLOCATIONS	(1 << 4)
// should be used to enable basic statistic measurements that do not consume more than a few dozen instructions.
#define BLT_DEBUG_STATS			(1 << 5)
// should be used to enable the profiler
#define BLT_DEBUG_PROFILER		(1 << 6)
// should be used to enable the use of catch and throw debugging
#define BLT_DEBUG_EXCEPTIONS	(1 << 7)
// should be used to enable more detailed monitoring of a program that may require a few hundred if not more instructions, such as OTel or otherwise equivalent toolset.
#define BLT_DEBUG_MONITOR		(1 << 8)
// should be used to enable deep checks which validate the program state
#define BLT_DEBUG_SANITY		(1 << 9)

#if (BLT_DEBUG_LEVEL + 0) == 0
#undef BLT_DEBUG_LEVEL
#define BLT_DEBUG_LEVEL 0
#endif

// if the compiler hasn't defined our debug level, we should default to only contracts, asserts, and inexpensive checks
#if !defined(BLT_DEBUG_LEVEL)
#define BLT_DEBUG_LEVEL 7
#endif

#define blt_has_flag(obj, flag) ((obj & flag) == flag)
#define blt_debug_has_flag(flag) blt_has_flag(BLT_DEBUG_LEVEL, flag)
#define blt_should_debug() (BLT_DEBUG_LEVEL != 0)

#endif //BLT_DEBUG_H
