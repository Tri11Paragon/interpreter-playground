/*
 *  <Short Description>
 *  Copyright (C) 2023  Brett Terpstra
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <blt/std/utility.h>
#include <blt/format/format.h>
#include <blt/std/logging.h>
#include <blt/profiling/profiler_v2.h>
#include <utility_test.h>
#include <vector>
#include <cstring>
#include <iostream>
#include <limits>
#include "blt/std/assert.h"

void printLines(const std::vector<std::string>& lines)
{
    for (const auto& v : lines)
        std::cout << v << "\n";
    std::cout << std::endl;
}


template<typename T>
void e1(const T& test)
{
    BLT_START_INTERVAL("Enumeration (" + blt::type_string<T>().substr(0, 30) + ":" + std::to_string(test.size()) + ")", "blt::enumerate");
    for (auto pair : blt::enumerate(test))
    {
        blt::black_box(pair);
    }
    BLT_END_INTERVAL("Enumeration (" + blt::type_string<T>().substr(0, 30) + ":" + std::to_string(test.size()) + ")", "blt::enumerate");
}

template<typename T>
void e2(const T& test)
{
    BLT_START_INTERVAL("Enumeration (" + blt::type_string<T>().substr(0, 30) + ":" + std::to_string(test.size()) + ")", "for index");
    for (size_t i = 0; i < test.size(); i++)
    {
        const auto& v = test[i];
        blt::black_box(std::pair<size_t, const typename T::value_type&>{i, v});
    }
    BLT_END_INTERVAL("Enumeration (" + blt::type_string<T>().substr(0, 30) + ":" + std::to_string(test.size()) + ")", "for index");
}

template<typename T>
void e3(const T& test)
{
    BLT_START_INTERVAL("Enumeration (" + blt::type_string<T>().substr(0, 30) + ":" + std::to_string(test.size()) + ")", "for range");
    size_t counter = 0;
    for (const auto& s : test)
    {
        blt::black_box(std::pair<size_t, const typename T::value_type&>{counter, s});
        ++counter;
    }
    BLT_END_INTERVAL("Enumeration (" + blt::type_string<T>().substr(0, 30) + ":" + std::to_string(test.size()) + ")", "for range");
}

template<typename T>
void testEnumerate(const T& test)
{
    e1(test);
    e2(test);
    e3(test);
    BLT_PRINT_PROFILE("Enumeration (" + blt::type_string<T>().substr(0, 30) + ":" + std::to_string(test.size()) + ")");
}

void getfucked()
{
    //BLT_ASSERT(false);
}

void fuckered()
{
    getfucked();
}

void blt::test::utility::run()
{
    blt::string::TableFormatter tableTest("Intrinsic Action Value Table");
    tableTest.addColumn("Thing");
    tableTest.addColumn("Value");
    
    tableTest.addRow({"Cuddles", "1 / minute"});
    tableTest.addRow({"Hand Job", "10"});
    tableTest.addRow({"Head", "100"});
    tableTest.addRow({"Sleeping Together (Non-Sexual)", "1,000"});
    tableTest.addRow({"Actual Sex", "5,000"});
    tableTest.addRow({"Sleeping Together (Sexual)", "10,000"});
    tableTest.addRow({"Relationship (I would do anything for you)", "1,000,000,000,000"});
    
    printLines(tableTest.createTable(true, true));
    
    blt::string::tree_format format;
    format.horizontalPadding = 3;
    format.verticalPadding = 0;
    format.boxFormat.verticalPadding = 3;
    format.collapse = true;
    blt::string::BinaryTreeFormatter treeFormatter("I love Men", format);
    treeFormatter.getRoot()->with(
            (new string::BinaryTreeFormatter::Node("Guys"))->with(nullptr, new string::BinaryTreeFormatter::Node("Child2")),
            (new string::BinaryTreeFormatter::Node("Femboys"))
                    ->with(nullptr, new string::BinaryTreeFormatter::Node("Child1")));
    printLines(treeFormatter.construct());
    
    blt::string::BinaryTreeFormatter assign1("price > 50", format);
    assign1.getRoot()->with(
            // left
            (new string::BinaryTreeFormatter::Node("member"))
                    ->with((new string::BinaryTreeFormatter::Node("total -= total * 0.15"))
                                   ->with((new string::BinaryTreeFormatter::Node("total > 500"))
                                                  ->with(new string::BinaryTreeFormatter::Node("total -= 25"))),
                           (new string::BinaryTreeFormatter::Node("total -= total * 0.05"))),
            // right
            (new string::BinaryTreeFormatter::Node("quality"))
                    ->with((new string::BinaryTreeFormatter::Node("total -= total * 0.02")),
                           (new string::BinaryTreeFormatter::Node("total -= total * 0.05")))
    );
    printLines(assign1.construct());
    
    for (int gensize = 1; gensize < 8; gensize++)
    {
        size_t size = static_cast<size_t>(std::pow(10, gensize));
        
        std::vector<std::string> str;
        std::vector<size_t> in;
        for (size_t i = 0; i < size; i++)
        {
            str.push_back(std::to_string(i));
            in.push_back(i);
        }
        
        testEnumerate(str);
        testEnumerate(in);
    }
    
    blt::string::ascii_padding_format bf;
    blt::string::ascii_titled_box bt1{"Pants are inside my lungs", "I sold your child", bf};
    blt::string::ascii_titled_box bt2{"With your child", "Your my whole world", bf};
    blt::string::ascii_box b1{"Single Word", bf};
    blt::string::ascii_box b2{"Never Was Never Will", bf};
    
    auto btd = blt::string::constructBox(bt1);
    auto btd1 = blt::string::constructBox(bt2);
    
    auto bd = blt::string::constructBox(b1);
    auto bd1 = blt::string::constructBox(b2);
    
//    blt::string::ascii_boxes boxes;
//    boxes.push_back(bt1);
//    boxes.push_back(b1);
//    boxes.push_back(bt2);
//    boxes.push_back(b2);
    
    blt::string::ascii_boxes boxes{bt1, b1, bt2, b2};
    auto bd2 = blt::string::constructBox(boxes);
    
    printLines(btd.toVec());
    printLines(bd.toVec());
    printLines(btd1.toVec());
    printLines(bd1.toVec());
    printLines(bd2.toVec());
    
    for (auto r : blt::range(0, 10))
        BLT_TRACE_STREAM << r << " ";
    BLT_TRACE_STREAM << "\n";
    
    for (auto r : blt::range(10, 0))
        BLT_TRACE_STREAM << r << " ";
    BLT_TRACE_STREAM << "\n";
    
    
    fuckered();
}
