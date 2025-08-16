/*
 * Created by Brett on 23/12/22.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_PROFILER_H
#define BLT_PROFILER_H

#include <string>
#include <blt/std/queue.h>
#include <vector>
#include <unordered_map>
#include <blt/logging/logging.h>
#include <fstream>
#include <cstdint>


/**
 * Defines several disableable macros (#define BLT_DISABLE_PROFILING). If you do not use these macros profiling cannot be disabled!
 */

namespace blt::profiling {
    struct capture_point {
        std::string name;
        long point{};
    };
    
    
    struct capture_interval {
        long start;
        long end;
    };
    
    struct capture_history {
        std::uint64_t count;
        std::uint64_t total;
    };
    
    struct profile {
        std::unordered_map<std::string, capture_interval> intervals;
        std::unordered_map<std::string, capture_history> intervals_total;
        blt::flat_queue<capture_point> points;
    };
    
    void startInterval(const std::string& profileName, const std::string& intervalName);
    
    void endInterval(const std::string& profileName, const std::string& intervalName);
    
    void point(const std::string& profileName, const std::string& pointName);
    
    capture_interval getInterval(const std::string& profileName, const std::string& intervalName);
    
    profile getProfile(const std::string& profileName);
    
    void printProfile(
            const std::string& profileName, logging::log_level_t loggingLevel = logging::log_level_t::NONE,
            bool averageHistory = false
    );
    
    void writeProfile(std::ofstream& out, const std::string& profileName, bool averageHistory = false);
    
    void discardProfiles();
    
    void discardIntervals(const std::string& profileName);
    
    void discardPoints(const std::string& profileName);
    
    class scoped_interval {
        private:
            std::string m_Profile;
            std::string m_Interval;
        public:
            scoped_interval(std::string profile, std::string interval):
                    m_Profile(std::move(profile)), m_Interval(std::move(interval)) {
                blt::profiling::startInterval(m_Profile, m_Interval);
            }
            
            ~scoped_interval() {
                blt::profiling::endInterval(m_Profile, m_Interval);
            }
    };
}

#endif //BLT_PROFILER_H

#ifdef BLT_DISABLE_PROFILING
    #define BLT_START_INTERVAL(profileName, intervalName)
    #define BLT_END_INTERVAL(profileName, intervalName)
    #define BLT_POINT(profileName, pointName)
    #define BLT_PRINT_PROFILE(profileName, ...)
    #define BLT_WRITE_PROFILE(stream, profileName)
#else
/**
 * Starts an interval to be measured, when ended the row will be added to the specified profile.
 */
    #define BLT_START_INTERVAL(profileName, intervalName) blt::profiling::startInterval(profileName, intervalName)
/**
 * Ends an interval, adds the interval to the profile.
 */
    #define BLT_END_INTERVAL(profileName, intervalName) blt::profiling::endInterval(profileName, intervalName)
/**
 * Measures this exact point in time.
 */
    #define BLT_POINT(profileName, pointName) blt::profiling::point(profileName, pointName)
/**
 * Prints the profile order from least time to most time.
 * @param profileName the profile to print
 * @param loggingLevel blt::logging::LOG_LEVEL to log with (default: BLT_NONE)
 * @param averageHistory use the historical collection of interval rows in an average or just the latest? (default: false)
 */
    #define BLT_PRINT_PROFILE(profileName, ...) blt::profiling::printProfile(profileName, ##__VA_ARGS__)
/**
 * writes the profile to an output stream, ordered from least time to most time, in CSV format.
 */
    #define BLT_WRITE_PROFILE(stream, profileName) blt::profiling::writeProfile(stream, profileName, true)
#endif