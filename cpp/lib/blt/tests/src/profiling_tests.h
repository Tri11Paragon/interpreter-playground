/*
 * Created by Brett on 29/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_TESTS_PROFILING_TESTS_H
#define BLT_TESTS_PROFILING_TESTS_H

#include "blt/profiling/profiler_v2.h"
#include "blt/std/logging.h"
#include "blt/std/time.h"
#include "blt/format/format.h"

void print(const std::vector<std::string>& vtr) {
    for (const auto& line : vtr)
        BLT_TRACE(line);
}

[[maybe_unused]] static void runProfilingAndTableTests() {
    BLT_START_INTERVAL("Help", "SuperSet");
    
    BLT_END_INTERVAL("Help", "SuperSet");
    
    BLT_START_INTERVAL("Help", "SecondSet");
    for (int i = 0; i < 1000; i++){
        ;;
    }
    BLT_END_INTERVAL("Help", "SecondSet");
    BLT_START_INTERVAL("Help", "UnderSet");
    for (int i = 0; i < 1000; i++){
        ;;
    }
    BLT_END_INTERVAL("Help", "UnderSet");
    
    for (int i = 0; i < 15; i++) {
        BLT_START_INTERVAL("Help", "UnderSet" + std::to_string(i));
        BLT_END_INTERVAL("Help", "UnderSet" + std::to_string(i));
    }
    
    BLT_PRINT_PROFILE("Help");
    BLT_TRACE("");
    
    blt::string::TableFormatter formatter;
    formatter.addColumn(blt::string::TableColumn{"Test1"});
    formatter.addColumn(blt::string::TableColumn{"Test2"});
    formatter.addColumn(blt::string::TableColumn{"Test Column"});
    formatter.addColumn(blt::string::TableColumn{"Test3"});
    
    formatter.addRow({"This", "This LARGE", "222", "5000"});
    formatter.addRow({"OTHER", "LARGE", "6", "1"});
    formatter.addRow({"hheee", "looo", "m8", "opsiedo"});
    
    print(formatter.createTable(true, true));
    BLT_TRACE("");
    print(formatter.createTable(false, true));
    BLT_TRACE("");
    print(formatter.createTable(true, false));
    BLT_TRACE("");
    print(formatter.createTable(false, false));
    BLT_TRACE("");
}

#endif //BLT_TESTS_PROFILING_TESTS_H
