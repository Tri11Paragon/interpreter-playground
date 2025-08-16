/*
 * Created by Brett on 20/09/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */
#include <blt/profiling/profiler_v2.h>
#include <blt/std/time.h>
#include <blt/std/system.h>
#include <blt/format/format.h>
#include <functional>
#include <blt/std/hashmap.h>
#include <blt/compatibility.h>

namespace blt
{

    /**
     * General profiler functions
     * --------------------------
     */
    
#define SORT_INTERVALS_FUNC_MACRO(TYPE_END, TYPE_START, TYPE_TOTAL)                                         \
    [](const interval_t* a, const interval_t* b) -> bool {                                                  \
            return a->TYPE_TOTAL < b->TYPE_TOTAL;                                                           \
    }

#define INTERVAL_DIFFERENCE_MACRO(printHistory, interval)                                                   \
    auto wall = printHistory                                                                                \
                ? (static_cast<double>(interval->wall_total) / static_cast<double>(interval->count))        \
                : static_cast<double>(interval->wall_total);                                                \
                                                                                                            \
    auto thread = printHistory                                                                              \
                  ? (static_cast<double>(interval->thread_total) / static_cast<double>(interval->count))    \
                  : (static_cast<double>(interval->thread_total));                                          \
                                                                                                            \
    auto cycles = printHistory                                                                              \
                  ? ((interval->cycles_total) / (interval->count))                                          \
                  : (interval->cycles_total);
    
    enum class unit
    {
        MS, NS, S
    };
    
    struct unit_container
    {
        unit thread;
        unit wall;
    };
    
    interval_t* createInterval(profile_t& profiler, std::string interval_name)
    {
        auto interval = new interval_t(
                blt::system::getCurrentTimeNanoseconds(), 0, 0,
                blt::system::getCPUThreadTime(), 0, 0,
                blt::system::rdtsc(), 0, 0,
                0, std::move(interval_name));
        profiler.intervals.push_back(interval);
        return interval;
    }
    
    void startInterval(interval_t* interval)
    {
        interval->wall_start = blt::system::getCurrentTimeNanoseconds();
        interval->thread_start = blt::system::getCPUThreadTime();
        interval->cycles_start = blt::system::rdtsc();
    }
    
    void endInterval(interval_t* interval)
    {
        interval->cycles_end = blt::system::rdtsc();
        interval->wall_end = blt::system::getCurrentTimeNanoseconds();
        interval->thread_end = blt::system::getCPUThreadTime();
        
        interval->cycles_total += interval->cycles_end - interval->cycles_start;
        interval->wall_total += interval->wall_end - interval->wall_start;
        interval->thread_total += interval->thread_end - interval->thread_start;
        
        interval->count++;
    }
    
    void clearProfile(profile_t& profiler)
    {
        for (auto* p : profiler.intervals)
            delete p;
        for (auto* p : profiler.cycle_intervals)
            delete p;
        profiler.intervals.clear();
        profiler.cycle_intervals.clear();
    }
    
    void sort_intervals(std::vector<interval_t*>& intervals, sort_by sort, bool)
    {
        std::function<bool(const interval_t* a, const interval_t* b)> sort_func;
        switch (sort)
        {
            case sort_by::CYCLES:
                sort_func = SORT_INTERVALS_FUNC_MACRO(cycles_start, cycles_end, cycles_total);
                break;
            case sort_by::WALL:
                sort_func = SORT_INTERVALS_FUNC_MACRO(wall_start, wall_end, wall_total);
                break;
            case sort_by::THREAD:
                sort_func = SORT_INTERVALS_FUNC_MACRO(thread_start, thread_end, thread_total);
                break;
        }
        std::sort(intervals.begin(), intervals.end(), sort_func);
    }
    
    unit_container determine_max_unit(const std::vector<interval_t*>& intervals, bool use_history)
    {
        // start at lowest unit
        unit_container container{unit::NS, unit::NS};
        // last one will always be the longest, at least according to the sort order
        // which should at least be close.
        auto interval = intervals[intervals.size() - 1];
        INTERVAL_DIFFERENCE_MACRO(use_history, interval);
        (void)(cycles);
        // make sure is in range + that we don't convert Seconds back to MS
        // we want to use the largest unit possible to keep the numbers small
        // we might run into a case where something took 1000ns but other thing took 100s
        // which would be hard to deal with either way. So TODO
        if (wall > 1e6 && wall < 1e9 && container.wall == unit::NS)
            container.wall = unit::MS;
        else if (wall > 1e9)
            container.wall = unit::S;
        
        if (thread > 1e6 && thread < 1e9 && container.thread == unit::NS)
            container.thread = unit::MS;
        else if (thread > 1e9)
            container.thread = unit::S;
        return container;
    }

    void writeProfile(std::ostream& stream, profile_t& profiler, std::uint32_t flags, sort_by sort)
    {
        bool printHistory = flags & AVERAGE_HISTORY;
        bool printCycles = flags & PRINT_CYCLES;
        bool printThread = flags & PRINT_THREAD;
        bool printWall = flags & PRINT_WALL;

        sort_intervals(profiler.intervals, sort, printHistory);

        auto units = determine_max_unit(profiler.intervals, printHistory);
        std::string thread_unit_string = units.thread == unit::MS ? "ms" : units.thread == unit::NS ? "ns" : "s";
        std::string wall_unit_string = units.wall == unit::MS ? "ms" : units.wall == unit::NS ? "ns" : "s";
        auto thread_unit_divide = units.thread == unit::MS ? 1e6 : units.thread == unit::NS ? 1 : 1e9;
        auto wall_unit_divide = units.wall == unit::MS ? 1e6 : units.wall == unit::NS ? 1 : 1e9;

        string::TableFormatter formatter{profiler.name};
        formatter.addColumn("Order");
        if (printHistory)
            formatter.addColumn("Count");
        formatter.addColumn("Interval");
        if (printCycles)
            formatter.addColumn("Cycles");
        if (printThread)
            formatter.addColumn("CPU Time (" + thread_unit_string += ")");
        if (printWall)
            formatter.addColumn("Wall Time (" + wall_unit_string += ")");

        for (size_t i = 0; i < profiler.intervals.size(); i++)
        {
            blt::string::TableRow row;
            auto interval = profiler.intervals[i];

            if (interval->count == 0)
                continue;

            INTERVAL_DIFFERENCE_MACRO(printHistory, interval);

            row.rowValues.push_back(std::to_string(i + 1));
            if (printHistory)
                row.rowValues.push_back(std::to_string(interval->count));
            row.rowValues.push_back(interval->interval_name);
            if (printCycles)
                row.rowValues.push_back(blt::string::withGrouping(cycles));
            if (printThread)
                row.rowValues.push_back(std::to_string(thread / static_cast<double>(thread_unit_divide)));
            if (printWall)
                row.rowValues.push_back(std::to_string(wall / static_cast<double>(wall_unit_divide)));
            formatter.addRow(row);
        }

        auto lines = formatter.createTable(true, true);
        for (const auto& line : lines)
            stream << line << "\n";
    }
    
    void printProfile(profile_t& profiler, const std::uint32_t flags, sort_by sort, blt::logging::log_level_t log_level)
    {
        std::stringstream stream;
        writeProfile(stream, profiler, flags, sort);
        BLT_LOG(log_level, "{}", stream.str());
    }
    
    profile_t::~profile_t()
    {
        blt::clearProfile(*this);
    }
    
    /**
     * profiler V1 partial backwards compat
     * ----------------------------
     */
    
    hashmap_t<std::string, hashmap_t<std::string, interval_t*>> profiles;
    
    void _internal::startInterval(const std::string& profile_name, const std::string& interval_name)
    {
        auto& profile = profiles[profile_name];
        if (profile.find(interval_name) == profile.end())
        {
            auto interval = new interval_t();
            interval->interval_name = interval_name;
            profile[interval_name] = interval;
        }
        blt::startInterval(profile[interval_name]);
    }
    
    void _internal::endInterval(const std::string& profile_name, const std::string& interval_name)
    {
        if (profiles[profile_name].empty() || profiles[profile_name].find(interval_name) == profiles[profile_name].end())
            return;
        blt::endInterval(profiles[profile_name].at(interval_name));
    }
    
    void _internal::writeProfile(std::ostream& stream, const std::string& profile_name, std::uint32_t flags, sort_by sort)
    {
        if (profiles.find(profile_name) == profiles.end())
            return;
        auto& pref = profiles[profile_name];
        profile_t profile{profile_name};
        for (const auto& i : pref)
            profile.intervals.push_back(i.second);
        blt::writeProfile(stream, profile, flags, sort);
        profiles.erase(profile_name);
    }
    
    void _internal::printProfile(const std::string& profile_name, std::uint32_t flags, sort_by sort, blt::logging::log_level_t log_level)
    {
        if (profiles.find(profile_name) == profiles.end())
            return;
        auto& pref = profiles[profile_name];
        profile_t profile{profile_name};
        for (const auto& i : pref)
            profile.intervals.push_back(i.second);
        blt::printProfile(profile, flags, sort, log_level);
        profiles.erase(profile_name);
    }
    
    interval_t::interval_t(pf_time_t wallStart, pf_time_t wallEnd, pf_time_t wallTotal, pf_time_t threadStart, pf_time_t threadEnd,
                           pf_time_t threadTotal, pf_cycle_t cyclesStart, pf_cycle_t cyclesEnd, pf_cycle_t cyclesTotal, uint64_t count,
                           std::string intervalName):
            wall_start(wallStart), wall_end(wallEnd), wall_total(wallTotal), thread_start(threadStart), thread_end(threadEnd),
            thread_total(threadTotal), cycles_start(cyclesStart), cycles_end(cyclesEnd), cycles_total(cyclesTotal), count(count),
            interval_name(std::move(intervalName))
    {}
}