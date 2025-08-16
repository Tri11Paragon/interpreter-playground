/*
 * Created by Brett on 04/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#include <chrono>
#include <ctime>
#include <sstream>

#ifndef BLT_TIME_H
#define BLT_TIME_H

namespace blt::system
{
#ifdef WIN32
    #define BLT_TIME_FUNC(name) auto t = std::time(nullptr); tm name{}; localtime_s(&name, &t)
#else
    #define BLT_TIME_FUNC(name) auto t = std::time(nullptr); auto ptr_##name = std::localtime(&t); auto& name = *ptr_##name
#endif
    
    static inline std::string ensureHasDigits(int current, int digits)
    {
        std::string asString = std::to_string(current);
        auto length = digits - asString.length();
        if (length <= 0)
            return asString;
        std::string zeros;
        zeros.reserve(length);
        for (unsigned int i = 0; i < length; i++)
        {
            zeros += '0';
        }
        return zeros + asString;
    }
    
    static inline auto getCurrentTimeNanoseconds()
    {
        return std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now().time_since_epoch()).count();
    }
    
    static inline auto nanoTime()
    {
        return getCurrentTimeNanoseconds();
    }
    
    static inline auto getCurrentTimeMilliseconds()
    {
        return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now().time_since_epoch()).count();
    }
    
    static inline int64_t getCPUThreadTime()
    {
#ifdef unix
        timespec time{};
        clock_gettime(CLOCK_THREAD_CPUTIME_ID, &time);
        return time.tv_sec * static_cast<int64_t>(1e9) + time.tv_nsec;
#else
        return std::clock();
#endif
    }
    
    static inline int64_t getCPUTime()
    {
#ifdef unix
        timespec time{};
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time);
        return time.tv_sec * static_cast<int64_t>(1e9) + time.tv_nsec;
#else
        return std::clock();
#endif
    }
    
    static inline auto getCPUTimerResolution()
    {
#ifdef unix
        timespec res{};
        clock_getres(CLOCK_PROCESS_CPUTIME_ID, &res);
        return res.tv_sec * static_cast<int64_t>(1e9) + res.tv_nsec;
#else
        return 1;
#endif
    }
    
    /**
     * Standard time string is formatted as follows:
     * Year-Month-Date Hour:Min:Second
     * If you do not want a space in the string use getTimeStringFS(); (Time String for easy filesystem)
     * @return the BLT standard string of time.now
     */
    static inline std::string getTimeString()
    {
        BLT_TIME_FUNC(now);
        std::stringstream timeString;
        timeString << (1900 + now.tm_year);
        timeString << "-";
        timeString << (1 + now.tm_mon);
        timeString << "-";
        timeString << now.tm_mday;
        timeString << " ";
        timeString << now.tm_hour;
        timeString << ":";
        timeString << now.tm_min;
        timeString << ":";
        timeString << now.tm_sec;
        return timeString.str();
    }
    
    /**
     * Standard logging time string is formatted as follows:
     * Hour:Min:Second
     * @return the BLT standard logging string of time.now
     */
    static inline std::string getTimeStringLog()
    {
        BLT_TIME_FUNC(now);
        std::string timeString = "[";
        timeString += ensureHasDigits(now.tm_hour, 2);
        timeString += ":";
        timeString += ensureHasDigits(now.tm_min, 2);
        timeString += ":";
        timeString += ensureHasDigits(now.tm_sec, 2);
        timeString += "] ";
        return timeString;
    }
    
    /**
     * @return the BLT standard string of time.now (See getTimeString()) that is filesystem friendly (FAT compatible).
     */
    static inline std::string getTimeStringFS()
    {
        BLT_TIME_FUNC(now);
        std::stringstream timeString;
        timeString << (1900 + now.tm_year);
        timeString << "-";
        timeString << (1 + now.tm_mon);
        timeString << "-";
        timeString << now.tm_mday;
        timeString << "_";
        timeString << now.tm_hour;
        timeString << "-";
        timeString << now.tm_min;
        timeString << "-";
        timeString << now.tm_sec;
        return timeString.str();
    }
}

#endif //BLT_TIME_H
