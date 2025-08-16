/*
 * Created by Brett on 29/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_TESTS_LOGGING_TEST_H
#define BLT_TESTS_LOGGING_TEST_H

void run_logging() {
    BLT_TRACE(10);
    BLT_TRACE(10.5);
    
    BLT_TRACE("Hello %d World!\n", 50);
    BLT_DEBUG("Hello %E World!\n", 1205200.0);
    BLT_INFO("Hello World!\n");
    BLT_WARN("Hello World!\n");
    BLT_ERROR("Hello World!\n");
    BLT_FATAL("Hello World!\n");
    
//    blt::logging::trace << "Hello! " << "Double stream insertion! " << 51 << 65 << " ";
//    blt::logging::trace << "Same Line! ";
//    blt::logging::trace << "Write the end!\n";
//    blt::logging::trace << "Seeee\n Super\n";
    
    std::string hello = "superSexyMax";
    std::cout << "String starts with: " << blt::string::contains(main_graph, "superSexyMaxE") << "\n";
}

#endif //BLT_TESTS_LOGGING_H
