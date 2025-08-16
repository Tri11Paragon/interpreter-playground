/*
 * Created by Brett on 04/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_SYSTEM_H
#define BLT_SYSTEM_H

#ifndef __EMSCRIPTEN__
    #ifdef _WIN32
        #include <intrin.h>
    #else
        
        #include <x86intrin.h>
    
    #endif
#else
    #include <chrono>
    using suseconds_t = int;
#endif

#include <cstdint>
#include <string>
#include <optional>

namespace blt::system
{
//#ifdef __GNUC__
//    #define GNU_INLINE __attribute__((__gnu_inline__, __always_inline__))
//#else
//    #define GNU_INLINE
//#endif
    inline std::uint64_t rdtsc()
    {
#ifdef __EMSCRIPTEN__
        return std::chrono::high_resolution_clock::now().time_since_epoch().count();
#else
        return __rdtsc();
#endif
    }
    // TODO: system memory and current CPU usage. (Linux Only currently)
    
    struct linux_proc_stat
    {
        // pid %d
        std::int32_t PID;
        // comm %s
        std::string exec_name;
        /*
         * R      Running
         * S      Sleeping in an interruptible wait
         * D      Waiting in uninterruptible disk sleep
         * Z      Zombie
         * T      Stopped (on a signal) or (before Linux 2.6.33) trace stopped
         * t      Tracing stop (Linux 2.6.33 onward)
         * W      Paging (only before Linux 2.6.0)
         * X      Dead (from Linux 2.6.0 onward)
         * x      Dead (Linux 2.6.33 to 3.13 only)
         * K      Wakekill (Linux 2.6.33 to 3.13 only)
         * W      Waking (Linux 2.6.33 to 3.13 only)
         * P      Parked (Linux 3.9 to 3.13 only)
         * I      Idle (Linux 4.14 onward)
         */
        // state %c
        char state;
        // pid of parent
        std::int32_t parent_pid;
        // group id of process
        std::int32_t group_id;
        // session id of process
        std::int32_t session_id;
        // controlling terminal
        std::int32_t tty_nr;
        // The ID of the foreground process group of the controlling terminal of the process.
        std::int32_t tpgid;
        std::uint32_t flags;
        std::uint64_t minflt;
        std::uint64_t cminflt;
        std::uint64_t majflt;
        std::uint64_t cmajflt;
        // amount of time process has been scheduled in user mode measured in clock ticks (divide by sysconf(_SC_CLK_TCK))
        std::uint64_t utime;
        // amount of time that this process has been scheduled in kernel mode, measured in clock ticks (divide by sysconf(_SC_CLK_TCK)).
        std::uint64_t stime;
        // children times
        std::int64_t cutime;
        std::int64_t cstime;
        std::int64_t priority;
        std::int64_t nice;
        std::int64_t num_threads;
        std::int64_t itrealvalue;
        // The time the process started after system boot. Since Linux 2.6, the value is expressed in clock ticks (divide by sysconf(_SC_CLK_TCK)).
        std::uint64_t starttime;
        // Virtual memory size in bytes.
        std::uint64_t vsize;
        // Resident  Set  Size:  number of pages the process has in real memory.
        // This is just the pages which count toward text, data, or stack space.  This does not include pages which have not been demand-loaded
        // in, or which are swapped out.  This value is inaccurate; see /proc/pid/statm below.
        std::int64_t rss;
        // Current soft limit in bytes on the rss of the process; see the description of RLIMIT_RSS in getrlimit(2).
        std::uint64_t rsslim;
        std::uint64_t startcode;
        std::uint64_t endcode;
        std::uint64_t startstack;
        std::uint64_t kstkesp;
        std::uint64_t signal;
        std::uint64_t blocked;
        std::uint64_t sigignore;
        std::uint64_t sigcatch;
        std::uint64_t wchan;
        std::uint64_t nswap;
        std::uint64_t cnswap;
        std::int32_t exit_signal;
        std::int32_t processor;
        std::uint32_t rt_priority;
        std::uint32_t policy;
        std::uint64_t delayacct_blkio_ticks;
        std::uint64_t guest_time;
        std::int64_t cguest_time;
        std::uint64_t start_data;
        std::uint64_t end_data;
        std::uint64_t start_brk;
        std::uint64_t arg_start;
        std::uint64_t arg_end;
        std::uint64_t env_start;
        std::uint64_t env_end;
        std::int32_t exit_code;
    };
    
    struct memory_info_t
    {
        // total program size (bytes)		(same as VmSize in status)
        std::uint64_t size;
        // size of memory portions (bytes)	(same as VmRSS in status)
        std::uint64_t resident;
        // number of bytes that are shared	(i.e. backed by a file, same as RssFile+RssShmem in status)
        std::uint64_t shared;
        // number of bytes that are 'code'	(not including libs; broken, includes data segment)
        std::uint64_t text;
        // number of pages of library (0)
        std::uint64_t lib;
        // number of bytes of data/stack    (including libs; broken, includes library text)
        std::uint64_t data;
        // number of dirty pages (0)
        std::uint64_t dt;
    };

#if defined(_MSC_VER) || defined(WIN32)
    using suseconds_t = std::size_t;
#endif
    
    struct timeval {
        time_t       tv_sec;    /* Seconds */
        suseconds_t  tv_usec;   /* Microseconds */
    };

    struct rusage {
        timeval ru_utime;       /* user CPU time used */
        timeval ru_stime;       /* system CPU time used */
        long   ru_maxrss;       /* maximum resident set size */
        
        long   ru_ixrss;        /* integral shared memory size */
        long   ru_idrss;        /* integral unshared data size */
        long   ru_isrss;        /* integral unshared stack size */
        
        long   ru_minflt;       /* page reclaims (soft page faults) */
        long   ru_majflt;       /* page faults (hard page faults) */
        
        long   ru_nswap;        /* swaps */
        
        long   ru_inblock;      /* block input operations */
        long   ru_oublock;      /* block output operations */
        long   ru_msgsnd;       /* IPC messages sent */
        long   ru_msgrcv;       /* IPC messages received */
        long   ru_nsignals;     /* signals received */
        long   ru_nvcsw;        /* voluntary context switches */
        long   ru_nivcsw;       /* involuntary context switches */
    };
    
    std::optional<blt::system::rusage> get_resources_process();
    std::optional<blt::system::rusage> get_resources_thread();
    
    memory_info_t get_memory_process();
    
}

#endif //BLT_SYSTEM_H
