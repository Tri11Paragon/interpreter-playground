cmake_minimum_required(VERSION 3.20)
include(cmake/color.cmake)
set(BLT_VERSION 5.4.32)

set(BLT_TARGET BLT)

project(BLT VERSION ${BLT_VERSION})

set(CMAKE_CXX_STANDARD 17)

option(ENABLE_ADDRSAN "Enable the address sanitizer" OFF)
option(ENABLE_UBSAN "Enable the ub sanitizer" OFF)
option(ENABLE_TSAN "Enable the thread data race sanitizer" OFF)

option(BLT_LOGGING_THREAD_SAFE "Make sure logging is thread synced" ON)
option(BLT_DEBUG_OTEL "Use Open Telemetry for debugging" OFF)

option(BUILD_STD "Build the BLT standard utilities." ON)
option(BUILD_PROFILING "Build the BLT profiler extension" ON)
option(BUILD_FS "Build the BLT FS utilities including the NBT + eNBT extension" ON)
option(BUILD_PARSE "Build the BLT parsers" ON)
option(BUILD_FORMAT "Build the BLT formatters" ON)
option(BUILD_LOGGING "Build the BLT logging utilities" ON)

option(BUILD_TESTS "Build the BLT test set" OFF)

option(BLT_DISABLE_STATS "Disable tracking stats in certain objects. Enabling this will cause stat functions to return 0" OFF)
option(BLT_DISABLE_LOGGING "Disable blt::logging (all macros and will safely disable logging function!)" OFF)
option(BLT_DISABLE_TRACE "Disable blt::logging BLT_TRACE macro" OFF)
option(BLT_DISABLE_DEBUG "Disable blt::logging BLT_DEBUG macro" OFF)
option(BLT_DISABLE_INFO "Disable blt::logging BLT_INFO macro" OFF)
option(BLT_DISABLE_WARN "Disable blt::logging BLT_WARN macro" OFF)
option(BLT_DISABLE_ERROR "Disable blt::logging BLT_ERROR macro" OFF)
option(BLT_DISABLE_FATAL "Disable blt::logging BLT_FATAL macro" OFF)

if (BLT_DEBUG_OTEL)
    message(STATUS "Searching in path '${CMAKE_PREFIX_PATH}'")
    find_package(absl REQUIRED)
    find_package(protobuf REQUIRED)
    find_package(OpenSSL REQUIRED)
    find_package(gRPC REQUIRED)
    find_package(nlohmann_json CONFIG REQUIRED)
    find_package(prometheus-cpp CONFIG REQUIRED)
    find_package(opentelemetry-cpp CONFIG REQUIRED)
endif ()

if (BLT_LOGGING_THREAD_SAFE)
    add_compile_definitions(BLT_LOGGING_THREAD_SAFE)
endif ()

if (${BLT_DISABLE_STATS})
    add_compile_definitions(BLT_DISABLE_STATS)
endif ()

find_program(MOLD "mold")

configure_file(include/blt/config.h.in config/blt/config.h @ONLY)

message("Enabling library compilation")
if (${BUILD_STD} OR ${BUILD_PROFILING})
    message(STATUS "Building ${Yellow}standard${ColourReset} cxx files")
    file(GLOB_RECURSE STD_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/blt/std/*.cpp")
else ()
    set(STD_FILES "")
endif ()

if (${BUILD_PROFILING})
    message(STATUS "Building ${Yellow}profiling${ColourReset} cxx files")
    file(GLOB_RECURSE PROFILING_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/blt/profiling/*.cpp")
else ()
    set(PROFILING_FILES "")
endif ()

if (${BUILD_FS})
    message(STATUS "Building ${Yellow}filesystem${ColourReset} cxx files")
    file(GLOB_RECURSE FS_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/blt/fs/*.cpp")
else ()
    set(FS_FILES "")
endif ()

if (${BUILD_PARSE})
    message(STATUS "Building ${Yellow}parser${ColourReset} cxx files")
    file(GLOB_RECURSE PARSE_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/blt/parse/*.cpp")
else ()
    set(PARSE_FILES "")
endif ()

if (${BUILD_FORMAT})
    message(STATUS "Building ${Yellow}format${ColourReset} cxx files")
    file(GLOB_RECURSE FORMAT_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/blt/format/*.cpp")
else ()
    set(FORMAT_FILES "")
endif ()

if (${BUILD_LOGGING})
    message(STATUS "Building ${Yellow}logging${ColourReset} cxx files")
    file(GLOB_RECURSE LOGGING_FILES "${CMAKE_CURRENT_SOURCE_DIR}/src/blt/logging/*.cpp")
else ()
    set(LOGGING_FILES "")
endif ()

if (EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/libraries/parallel-hashmap)
    message("Found Parallel Hashmaps library, using ${Yellow}phmap${ColourReset} over ${Red}std::unordered_map${ColourReset}")
    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/libraries/parallel-hashmap)
else ()
    message("Parallel Hashmaps library not found! using ${Yellow}std::unordered_map${ColourReset}")
endif ()

#include zlib if the user has it.
find_package(ZLIB QUIET)
find_package(CURL QUIET)

if (${ZLIB_FOUND})
    include_directories(${ZLIB_INCLUDE_DIRS})
else ()
    message("ZLIB was not found, this is fine however if you wish you use gzip with NBT it is required.")
endif ()

if (${CURL_FOUND})
    message(STATUS "Linking cURL!")
    include_directories(${CURL_INCLUDE_DIRS})
else ()
    message(STATUS "cURL not found, some library features will be disabled!")
endif ()

include_directories(include/)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/config/)

add_library(${BLT_TARGET} ${STD_FILES} ${PROFILING_FILES} ${FS_FILES} ${PARSE_FILES} ${FORMAT_FILES} ${LOGGING_FILES})

string(REPLACE "+" "\\+" escaped_source ${CMAKE_CURRENT_SOURCE_DIR})
string(APPEND escaped_source "/src/blt/.*/")
list(TRANSFORM STD_FILES REPLACE ${escaped_source} "")
list(TRANSFORM PROFILING_FILES REPLACE ${escaped_source} "")
list(TRANSFORM FS_FILES REPLACE ${escaped_source} "")
list(TRANSFORM PARSE_FILES REPLACE ${escaped_source} "")
message("Standard Files ${Magenta}${STD_FILES}${ColourReset}")
message("Profiler Files ${Magenta}${PROFILING_FILES}${ColourReset}")
message("FS Files ${Magenta}${FS_FILES}${ColourReset}")
message("Parser Files ${Magenta}${PARSE_FILES}${ColourReset}")
message("Source: ${CMAKE_SOURCE_DIR}")
message("Current Source: ${CMAKE_CURRENT_SOURCE_DIR}")
message("Binary: ${CMAKE_BINARY_DIR}")
message("Current Binary: ${CMAKE_CURRENT_BINARY_DIR}")

if (${ZLIB_FOUND})
    target_include_directories(${BLT_TARGET} PUBLIC ${ZLIB_INCLUDE_DIRS})
    target_link_libraries(${BLT_TARGET} PUBLIC ZLIB::ZLIB)
endif ()

if (${CURL_FOUND})
    target_include_directories(${BLT_TARGET} PUBLIC ${CURL_INCLUDE_DIRS})
    target_link_libraries(${BLT_TARGET} PUBLIC ${CURL_LIBRARIES})
endif ()

if ((CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang") AND NOT EMSCRIPTEN)
    message(STATUS "Trying LibBacktrace")
    include(CheckLinkerFlag)
    check_linker_flag(CXX "LINKER:-lbacktrace" LINKER_HAS_BACKTRACE)
    if (LINKER_HAS_BACKTRACE)
        add_compile_definitions(BLT_HAS_BETTER_BACKTRACE)
        target_link_options(${BLT_TARGET} PUBLIC -lbacktrace)
        message(STATUS "Using LibBacktrace for stacktrace support!")
    endif ()
endif ()

if ((NOT LINKER_HAS_BACKTRACE) AND (NOT EMSCRIPTEN))
    find_package(Backtrace)
    if (${Backtrace_FOUND})
        add_compile_definitions(BLT_HAS_BACKTRACE)
        add_compile_definitions(BLT_BACKTRACE_HEADER=${Backtrace_HEADER})
        target_link_libraries(${BLT_TARGET} PUBLIC ${Backtrace_LIBRARIES})
        target_include_directories(${BLT_TARGET} PUBLIC ${Backtrace_INCLUDE_DIRS})
        message(STATUS "Using backtrace(3) for stacktrace support!")
    elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        add_compile_definitions(BLT_HAS_BACKTRACE)
        add_compile_definitions(BLT_BACKTRACE_HEADER=<execinfo.h>)
        message(STATUS "Using backtrace (GNU/Clang) for stacktrace support!")
    endif ()
endif ()

include(cmake/warnings.cmake)

target_include_directories(${BLT_TARGET} PUBLIC include/)
target_include_directories(${BLT_TARGET} PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/config/)
if (EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/libraries/parallel-hashmap)
    message("Including Parallel Hashmap directory")
    target_include_directories(${BLT_TARGET} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/libraries/parallel-hashmap)
endif ()

message("BLT ${Yellow}${BLT_VERSION}${ColourReset} Successfully included!")

message("Installing to ${CMAKE_INSTALL_LIBDIR} with headers at ${CMAKE_INSTALL_INCLUDEDIR}")

file(GLOB_RECURSE BLT_HEADER_FILES "${CMAKE_CURRENT_SOURCE_DIR}/include/*.h")
foreach (S ${BLT_HEADER_FILES})
    string(REPLACE "${CMAKE_CURRENT_SOURCE_DIR}/include/" "" SO ${S})
    string(REGEX REPLACE "\/[A-Z|a-z|0-9|_|-]*\\.h" "/" SA ${SO})
    list(APPEND BLT_F_HEADERS ${SA})
    install(FILES ${S} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${SA})
endforeach ()

install(FILES ${CMAKE_BINARY_DIR}/config/blt/config.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/blt/)

set_target_properties(${BLT_TARGET} PROPERTIES VERSION ${BLT_VERSION})
set_target_properties(${BLT_TARGET} PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR})
if (NOT ${MOLD} STREQUAL MOLD-NOTFOUND)
    target_link_options(${BLT_TARGET} PUBLIC -fuse-ld=mold)
endif ()

install(TARGETS ${BLT_TARGET}
        CONFIGURATIONS RelWithDebInfo
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

macro(blt_add_test name source type)

    message("Adding project ${name} of type ${type}" DEBUG)
    project(${name}-${type})

    add_executable(${name}-${type} ${source})

    if (NOT ${MOLD} STREQUAL MOLD-NOTFOUND)
        add_link_options(-fuse-ld=mold)
    endif ()

    target_link_libraries(${name}-${type} PRIVATE BLT)

    if (NOT MSVC)
        target_compile_options(${name}-${type} PRIVATE -Wall -Wextra -Wpedantic -Wno-comment)
        target_link_options(${name}-${type} PRIVATE -Wall -Wextra -Wpedantic -Wno-comment)
    endif ()
    target_compile_definitions(${name}-${type} PRIVATE BLT_DEBUG_LEVEL=${DEBUG_LEVEL})

    if (${TRACK_ALLOCATIONS})
        target_compile_definitions(${name}-${type} PRIVATE BLT_TRACK_ALLOCATIONS=1)
    endif ()

    if (${ENABLE_ADDRSAN} MATCHES ON)
        target_compile_options(${name}-${type} PRIVATE -fsanitize=address)
        target_link_options(${name}-${type} PRIVATE -fsanitize=address)
    endif ()

    if (${ENABLE_UBSAN} MATCHES ON)
        target_compile_options(${name}-${type} PRIVATE -fsanitize=undefined)
        target_link_options(${name}-${type} PRIVATE -fsanitize=undefined)
    endif ()

    if (${ENABLE_TSAN} MATCHES ON)
        target_compile_options(${name}-${type} PRIVATE -fsanitize=thread)
        target_link_options(${name}-${type} PRIVATE -fsanitize=thread)
    endif ()

    add_test(NAME ${name} COMMAND ${name}-${type})

    set(failRegex "\\[WARN\\]" "FAIL" "ERROR" "FATAL" "exception")
    set_property(TEST ${name} PROPERTY FAIL_REGULAR_EXPRESSION "${failRegex}")

    project(${BLT_TARGET})
endmacro()

if (${BUILD_TESTS})
    message("Building tests for version ${BLT_VERSION}")

    blt_add_test(blt_iterator tests/iterator_tests.cpp test)
    blt_add_test(blt_argparse tests/argparse_tests.cpp test)
    blt_add_test(blt_logging tests/logger_tests.cpp test)
    blt_add_test(blt_variant tests/variant_tests.cpp test)

    message("Built tests")
endif ()

project(BLT)

if (BLT_DEBUG_OTEL)
    target_link_libraries(${BLT_TARGET} PUBLIC ${OPENTELEMETRY_CPP_LIBRARIES})
    target_include_directories(${BLT_TARGET} PUBLIC ${OPENTELEMETRY_CPP_INCLUDE_DIRS})
endif ()