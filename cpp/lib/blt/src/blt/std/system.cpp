/*
 * Created by Brett on 04/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */
#include <blt/std/system.h>
#include <blt/logging/logging.h>

#if !defined(_MSC_VER) && !defined(WIN32)
#include <sys/time.h>            /* for struct timeval */
#include <sys/resource.h>
#else
    #include <windows.h>
    #define RUSAGE_SELF 1
#endif

#include <climits>                /* for CLK_TCK */
#include <cstring>

#ifndef WIN32
    
    #include <unistd.h>
    #include <blt/fs/loader.h>
#include "blt/std/assert.h"

inline long blt_get_page_size()
{
    return sysconf(_SC_PAGESIZE);
}

//struct proc_statm_t
//{
//    // total program size (pages)		(same as VmSize in status)
//    std::uint64_t size;
//    // size of memory portions (pages)	(same as VmRSS in status)
//    std::uint64_t resident;
//    // number of pages that are shared	(i.e. backed by a file, same as RssFile+RssShmem in status)
//    std::uint64_t shared;
//    // number of pages that are 'code'	(not including libs; broken, includes data segment)
//    std::uint64_t text;
//    // number of pages of library (0)
//    std::uint64_t lib;
//    // number of pages of data/stack    (including libs; broken, includes library text)
//    std::uint64_t data;
//    // number of dirty pages (0)
//    std::uint64_t dt;
//};

blt::system::memory_info_t process_proc()
{
    static auto page_size = blt_get_page_size();
    
    auto str = blt::fs::getFile("/proc/self/statm");
    
    auto data = blt::string::split(str, ' ');
    BLT_ASSERT(data.size() == 7 && "Something went wrong when parsing /proc/self/statm! Expected 7 values!");
    
    blt::system::memory_info_t mem{};
    
    mem.size = page_size * std::stoull(data[0]);
    mem.resident = page_size * std::stoull(data[1]);
    mem.shared = page_size * std::stoull(data[2]);
    mem.text = page_size * std::stoull(data[3]);
    mem.lib = page_size * std::stoull(data[4]);
    mem.data = page_size * std::stoull(data[5]);
    mem.dt = page_size * std::stoull(data[6]);
    
    return mem;
}

#endif


namespace blt
{
    std::optional<system::rusage> get_resources(int who)
    {
        system::rusage usage{};
        std::memset(&usage, 0, sizeof(system::rusage));
#ifdef WIN32
        FILETIME starttime;
        FILETIME exittime;
        FILETIME kerneltime;
        FILETIME usertime;
        ULARGE_INTEGER li;
        
        if (who != RUSAGE_SELF)
        {
            /* Only RUSAGE_SELF is supported in this implementation for now */
            BLT_WARN("Only RUSAGE_SELF is supported in this implementation for now");
            return {};
        }
        
        if (GetProcessTimes(GetCurrentProcess(),
                            &starttime, &exittime, &kerneltime, &usertime) == 0)
        {
            BLT_WARN("Unable to get process resource usage, error: {:d}", GetLastError());
            return {};
        }
        
        /* Convert FILETIMEs (0.1 us) to struct timeval */
        memcpy(&li, &kerneltime, sizeof(FILETIME));
        li.QuadPart /= 10L;            /* Convert to microseconds */
        usage.ru_stime.tv_sec = li.QuadPart / 1000000L;
        usage.ru_stime.tv_usec = li.QuadPart % 1000000L;
        
        memcpy(&li, &usertime, sizeof(FILETIME));
        li.QuadPart /= 10L;            /* Convert to microseconds */
        usage.ru_utime.tv_sec = li.QuadPart / 1000000L;
        usage.ru_utime.tv_usec = li.QuadPart % 1000000L;
#else
        if (getrusage(who, (struct rusage*) &usage) != 0)
        {
            BLT_ERROR("Failed to get rusage {:d}", errno);
            return {};
        }
#endif
        return usage;
    }
    
    std::optional<system::rusage> system::get_resources_process()
    {
        return get_resources(RUSAGE_SELF);
    }
    
    std::optional<system::rusage> system::get_resources_thread()
    {
#ifdef WIN32
        return get_resources(RUSAGE_SELF);
#else
        return get_resources(RUSAGE_THREAD);
#endif
    }
    
    system::memory_info_t system::get_memory_process()
    {
#ifdef WIN32
        BLT_WARN("Unsupported OS");
        return {};
#else
        
        return process_proc();
#endif
    }
}


