# **BLT v5.1**
A C++17 common utilities library to make thing easy! 

![Icon](icon_large.png)

---

# Features

## BLT Format

This module provides general string formatting utilities. *Note: this folder contains mostly outdated library files and will be updated in the future.*

### Files

- **boxing.h**  
  Simple utility for drawing boxes around blocks of text.

- **format.h**  
  Legacy library file containing various utilities for creating formatted output. Also includes methods for writing Java UTF8 strings.

## BLT Filesystem
This module provides helper classes for filesystem objects. It seeks to offer an interface that is simpler than the one provided by the standard library. 
Specifically, the number of functions required to implement is significantly lower, 
and the interface is generally cleaner. Eventually, this module aims to support various file formats, 
such as Minecraft's NBT system. Currently, there is an existing NBT file, but it was written when I was first learning C++.

### Files

- **filesystem.h**  
  This is the base file which includes all other files. You should use the other options as this can be a heavy file to include

- **path_helper.h**  
  This file provides functions for interfacing with paths. Specifically, as of this moment it only provides an interface for getting the base name of a file path.

- **loader.h**  
  - `std::string blt::fs::getFile(std::string_view path)`
    - Gets the entire file as a string.  
  - `std::vector\<std::string> blt::fs::getLinesFromFile(std::string_view path)`
    - Gets the entire file as a string, then splits on the new line character. Then returns a vector of those lines  
  - `std::vector\<std::string> blt::fs::recursiveInclude(std::string_view path, std::string include_header, std::vector<include_guard> guards)`
    - Recursively include in order based on the include string (include_header) marked by the include guards  
    - Defaults to C/C++/GLSL preprocessor style (Was designed for GLSL)
  - `std::string blt::fs::loadBrainFuckFile(const std::string& path)`
    - Load a brainfuck file recursively, uses ~ to mark the include path
- ### nbt.h
  - probably needs to be remade (TODO)
- ## blt/math
  - ### averages.h
    - blt::averagizer_o_matic
      - Computes a rolling average in a dynamically allocated array.
      - Useful for average FPS over a period of time
  - ### fixed_point.h
    - Provides a 64bit fixed point number with 2^32 bits of precision on each side of the decimal
    - Mostly optimal assembly, avoids sign extension by using 64 bit numbers
    - Requires your compiler to support 128 bit numbers
    - Docs: TODO
  - ### fixed_point_vector.h
    - Not finished due to complications in the vector type, this is a TODO
  - ### log_util.h
    - provides overloads for logging math types
  - ### math.h
    - blt::toRadians 
      - to radians function (from deg)
    - blt::f_randi
      - fast random integer from seed
    - blt::fsqrt
      - fast inverse square root algorithm, floats only
    - blt::pow
      - power function
    - blt::round_up
      - rounds up to a variable number of decimal places

# Specialties
## blt::logging.v2 
Found in the header file `include/std/logging.h`, blt::logging is a fast and simple
single header logging library which can be used independently of BLT. Just include
the header file anywhere in your project and in ONE cpp file define `#define BLT_LOGGING_IMPLEMENTATION`.
If you are using BLT as a CMake library (as you should!) this is done for you.
- ### Compile Time Config
  - Using CMake
    - Several options are provided which disable various logging contexts, as such global logging can be disabled by passing `-DBLT_DISABLE_LOGGING:BOOL=ON`
    - Options follow the pattern of `BLT_DISABLE_LEVEL` where level is one of `TRACE`,`DEBUG`,`INFO`,`WARN`,`ERROR`, or `FATAL`.
      - This allows for individual_t logging levels to be disabled while leaving the others functional. These options can be combined.
      - See CMakeLists.txt for a complete list of options.
  - Standalone
    - The CMake options define global variables in a config file. If you are using logging standalone you will need to remove the config include.
    - Macros can be used in the same pattern of `BLT_DISABLE_LEVEL` to disable the various logging levels
## blt::arg_parse
blt::arg_parse found in `blt/parse/argparse.h` is an attempt at a near 1:1 replication of Python's argparse in C++. 
Python's argparse is one of the best command line argument parsing solutions with some of the best documentation I've seen. 
The goal was to create an API which would function exactly as the Python docs described, simplifying how much I needed to remember and document for this library.
## blt::profile_t (v2)
The newest version of my profiler solution now features a CPU cycle counter, CPU thread execution time, and wall time. It has more options for
sorting and general printing while featuring a nicer codebase. It is an improvement over blt::profiling while maintaining (almost) complete backwards
compatability. Due to changes in the API the `BLT_PRINT_PROFILE` macro deletes the internal profiles and intervals AND takes arguments which match
the new formatting options for profiling v2. However, `BLT_START_INTERVAL`, `BLT_WRITE_PROFILE`, and `BLT_END_INTERVAL` are still the same.
Documentation for this is coming soon, along with more profiling features. the `BLT_*` macros can be disabled by the standard `BLT_DISABLE_PROFILING`
from the v1 profiler. It is encouraged to use the new blt::* profile functions over the macros however these currently cannot be disabled. (TODO)
---

# **Features / Examples**
- ## Data Structures
  - Queue / Stack 
    - faster than std::queue / std::stack
    - backed by a contiguous array
- ## Utility
  - Simple Random Wrapper Interface
  - Simple random functions based on the PCG Hash
  - ### String Functions
    - starts_with
    - ends_with
    - contains
    - toLowerCase
    - toUpperCase
    - split
    - trim
  - Logging
    - See blt::logging section above
  - Time
    - Current time in nanoseconds (without all the c++ gobbledygook)
      - Java's currentTimeMilliseconds
      - nanoTime as well
      - `std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now().time_since_epoch()).count()` becomes `blt::system::nanoTime()`
    - Formatted time string with year/month/date + current time
- ## Profiling
  - Basic profiler with history and formatted output
