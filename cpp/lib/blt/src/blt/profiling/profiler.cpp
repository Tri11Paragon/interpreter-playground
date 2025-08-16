/*
 * Created by Brett on 23/12/22.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */
#include <blt/profiling/profiler.h>
#include <mutex>
#include <vector>
#include <blt/std/time.h>
#include <blt/logging/logging.h>
#include <iostream>
#include <algorithm>
#include <blt/format/format.h>

#define TIME_FUNCTION blt::system::getCPUThreadTime()

namespace blt::profiling {
    
    // TODO: a better way
    std::mutex profileLock{};
    std::unordered_map<std::string, profile> profiles;
    
    struct IntervalComparable {
        long difference;
        std::string name;
        std::uint64_t total;
        
        IntervalComparable(long difference, std::string name):
                difference(difference), name(std::move(name)), total(difference) {}
        
        IntervalComparable(long difference, std::string name, std::uint64_t total):
                difference(difference), name(std::move(name)), total(total) {}
    };
    
    inline void println(const std::vector<std::string>&& lines, const logging::log_level_t level) {
        for (const auto& line : lines)
            BLT_LOG(level, "{}", line);
//        auto& logger = logging::getLoggerFromLevel(level);
//        for (const auto& line : lines)
//            logger << line << "\n";
    }
    
    /**
     * Copy-sorts the unordered vector into the ordered vector, min -> max
     */
    inline void orderIntervals(
            const std::unordered_map<std::string, capture_interval>& unordered,
            std::vector<IntervalComparable>& ordered, bool history
    ) {
        // copy
        for (const auto& i : unordered) {
            if (history)
                ordered.emplace_back(i.second.end, i.first, i.second.start);
            else
                ordered.emplace_back(i.second.end - i.second.start, i.first);
        }
        
        // sort
        std::sort(
                ordered.begin(), ordered.end(),
                [&](const IntervalComparable& c1, const IntervalComparable& c2) -> bool {
                    return c1.difference < c2.difference;
                }
        );
    }
    
    inline void averageIntervals(
            const std::unordered_map<std::string, capture_history>& intervals,
            std::unordered_map<std::string, capture_interval>& averagedIntervals
    ) {
        // average all intervals
        for (const auto& i : intervals) {
            const auto& name = i.first;
            const auto& interval_vec = i.second;
            
            // we can exploit how the order func works by supplying the difference into end,
            // which sorts correctly despite not being a true interval.
            averagedIntervals.insert(
                    {name, capture_interval{(long)interval_vec.total, (long) (interval_vec.total / interval_vec.count)}}
            );
        }
    }
    
    void writeProfile(std::ofstream& out, const std::string& profileName, bool averageHistory) {
        auto& profile = profiles[profileName];
        const auto& intervals = profile.intervals;
        const auto& intervals_total = profile.intervals_total;
        
        std::vector<IntervalComparable> order_rows;
        std::unordered_map<std::string, capture_interval> averaged_intervals;
        
        if (averageHistory)
            averageIntervals(intervals_total, averaged_intervals);
        
        orderIntervals(averageHistory ? averaged_intervals : intervals, order_rows, averageHistory);
        
        out << "Order,Count,Interval,Time (ms),Time (ns),Total (ms)\n";
        int index = 1;
        for (const auto& row : order_rows) {
            out << std::to_string(index++) << ","
                << std::to_string(averageHistory ? intervals_total.at(row.name).count : 1) << ","
                << row.name << ","
                << std::to_string((double) row.difference / 1000000.0) << ","
                << std::to_string(row.difference) << ","
                << std::to_string((double) row.total / 1000000.0) << "\n";
        }
        out.flush();
    }
    
    void printProfile(
            const std::string& profileName, const logging::log_level_t loggingLevel, const bool averageHistory
    ) {
        auto& profile = profiles[profileName];
        const auto& intervals = profile.intervals;
        const auto& intervals_total = profile.intervals_total;
        
        std::vector<IntervalComparable> ordered_rows;
        std::unordered_map<std::string, capture_interval> averaged_intervals;
        
        if (averageHistory)
            averageIntervals(intervals_total, averaged_intervals);
        
        orderIntervals(averageHistory ? averaged_intervals : intervals, ordered_rows, averageHistory);
        
        string::TableFormatter formatter{profileName};
        formatter.addColumn("Order");
        formatter.addColumn("Count");
        formatter.addColumn("Interval");
        formatter.addColumn("Time (ms)");
        formatter.addColumn("Time (ns)");
        formatter.addColumn("Total (ms)");
        
        int index = 1;
        for (const auto& row : ordered_rows) {
            formatter.addRow(
                    {std::to_string(index++),
                     std::to_string(averageHistory ? intervals_total.at(row.name).count : 1),
                     row.name,
                     std::to_string((double) row.difference / 1000000.0),
                     std::to_string(row.difference),
                     std::to_string((double) row.total / 1000000.0)}
            );
        }
        
        println(formatter.createTable(true, true), loggingLevel);
    }

// --- small helper functions below ---
    
    void startInterval(const std::string& profileName, const std::string& intervalName) {
        std::scoped_lock lock(profileLock);
        capture_interval interval{};
        interval.start = TIME_FUNCTION;
        profiles[profileName].intervals[intervalName] = interval;
    }
    
    void endInterval(const std::string& profileName, const std::string& intervalName) {
        std::scoped_lock lock(profileLock);
        auto& prof = profiles[profileName];
        auto& ref = prof.intervals[intervalName];
        
        ref.end = TIME_FUNCTION;
        
        auto difference = ref.end - ref.start;
        
        auto& href = prof.intervals_total[intervalName];
        
        href.total += difference;
        href.count++;
    }
    
    void point(const std::string& profileName, const std::string& pointName) {
        std::scoped_lock lock(profileLock);
        capture_point point{};
        point.point = TIME_FUNCTION;
        point.name = pointName;
        profiles[profileName].points.push(point);
    }
    
    capture_interval getInterval(const std::string& profileName, const std::string& intervalName) {
        return profiles[profileName].intervals[intervalName];
    }
    
    profile getProfile(const std::string& profileName) {
        return profiles[profileName];
    }
    
    void discardProfiles() {
        profiles = {};
    }
    
    void discardIntervals(const std::string& profileName) {
        profiles[profileName].intervals = {};
        profiles[profileName].intervals_total = {};
    }
    
    void discardPoints(const std::string& profileName) {
        profiles[profileName].points = {};
    }
    
}