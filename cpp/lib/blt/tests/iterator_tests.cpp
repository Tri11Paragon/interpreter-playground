/*
 *  <Short Description>
 *  Copyright (C) 2024  Brett Terpstra
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
#include <blt/logging/logging.h>
#include <blt/std/types.h>
#include <blt/std/assert.h>
#include <blt/math/vectors.h>
#include <blt/math/log_util.h>
#include <blt/iterator/zip.h>
#include <blt/iterator/enumerate.h>
#include <blt/iterator/iterator.h>
#include <blt/std/ranges.h>
#include <blt/format/boxing.h>
#include <array>
#include <forward_list>

constexpr auto increasing_reverse_pairs =
        [](blt::size_t i, blt::size_t index, blt::size_t size) { return i == 0 ? index : (size - 1) - index; };
constexpr auto increasing_pairs =
        [](blt::size_t, blt::size_t index, blt::size_t) { return index; };
constexpr auto decreasing_pairs =
        [](blt::size_t, blt::size_t index, blt::size_t size) { return size - index; };

template<blt::size_t n, typename Func>
std::array<blt::vec2, n> make_array(Func func)
{
    std::array<blt::vec2, n> array;
    for (auto&& [index, value] : blt::enumerate(array))
        value = blt::vec2(func(0, index, n), func(1, index, n));
    return array;
}

template<blt::size_t n, typename Func>
std::forward_list<blt::vec2> make_list(Func func)
{
    std::forward_list<blt::vec2> array;
    for (auto index : blt::range(0ul, n))
        array.push_front(blt::vec2(func(0, index, n), func(1, index, n)));
    return array;
}

constexpr blt::size_t array_size = 10;
auto array_1 = make_array<array_size>(increasing_reverse_pairs);
auto array_2 = make_array<array_size>(increasing_pairs);
auto array_3 = make_array<array_size>(decreasing_pairs);

auto list_1 = make_list<array_size>(increasing_reverse_pairs);

void test_enumerate()
{
    blt::log_box_t box(std::cout, "Enumerate Tests", 25);
    for (const auto& [index, item] : blt::enumerate(array_1))
        BLT_TRACE("{}, : {}", index, item);
    
    BLT_TRACE("");
    
    for (const auto& [index, item] : blt::enumerate(array_1).rev())
        BLT_TRACE("{}, : {}", index, item);
    
    BLT_TRACE("");
    
    for (const auto& [index, item] : blt::enumerate(array_1).take(3))
    {
        BLT_TRACE("{}, : {}", index, item);
        BLT_ASSERT(index < 3);
    }
    
    BLT_TRACE("");
    
    for (const auto& [index, item] : blt::enumerate(array_1).take(3).rev())
    {
        BLT_TRACE("{}, : {}", index, item);
        BLT_ASSERT(index < 3);
    }
    
    BLT_TRACE("");
    
    for (const auto& [index, item] : blt::enumerate(array_1).skip(3))
    {
        BLT_TRACE("{}, : {}", index, item);
        BLT_ASSERT(index >= 3);
    }
    
    BLT_TRACE("");
    
    for (const auto& [index, item] : blt::enumerate(array_1).skip(3).rev())
    {
        BLT_TRACE("{}, : {}", index, item);
        BLT_ASSERT(index >= 3);
    }
    
    BLT_TRACE("");
    
    for (const auto& [index, item] : blt::enumerate(array_1).skip(3).take(5))
    {
        BLT_TRACE("{}, : {}", index, item);
        BLT_ASSERT(index >= 3 && index < (array_1.size() - 5) + 3);
    }
    
    BLT_TRACE("");
    
    for (const auto& [index, item] : blt::enumerate(array_1).skip(3).rev().take(5))
    {
        BLT_TRACE("{}, : {}", index, item);
        BLT_ASSERT(index >= 5);
    }
}

void test_pairs()
{
    blt::log_box_t box(std::cout, "Pairs Tests", 25);
    for (auto [a1, a2] : blt::in_pairs(array_1, array_2))
    {
        BLT_TRACE("{}, : {}", a1, a2);
    }
}

void test_zip()
{
    blt::log_box_t box(std::cout, "Zip Tests", 25);
    for (auto [a1, a2, a3] : blt::zip(array_1, array_2, list_1))
    {
        BLT_TRACE("{:.4} : {:.4} : {:.4}", a1, a2, a3);
    }
    BLT_TRACE("================================");
    for (auto [a1, a2, a3] : blt::zip(array_1, array_2, list_1).take(3))
    {
        BLT_TRACE("{:.4} : {:.4} : {:.4}", a1, a2, a3);
    }
    BLT_TRACE("================================");
    for (auto [a1, a2, a3] : blt::zip(array_1, array_2, array_3).take(3).rev())
    {
        BLT_TRACE("{:.4} : {:.4} : {:.4}", a1, a2, a3);
    }
    BLT_TRACE("================================");
    for (auto [a1, a2, a3] : blt::zip(array_1, array_2, array_3).take_or(13))
    {
        BLT_TRACE("{:.4} : {:.4} : {:.4}", a1, a2, a3);
    }
    BLT_TRACE("================================");
    for (auto [a1, a2, a3] : blt::zip(array_1, array_2, array_3).rev().take(3))
    {
        BLT_TRACE("{:.4} : {:.4} : {:.4}", a1, a2, a3);
    }
    BLT_TRACE("================================");
    for (auto [a1, a2, a3] : blt::zip(array_1, array_2, array_3).skip(2).rev())
    {
        BLT_TRACE("{:.4} : {:.4} : {:.4}", a1, a2, a3);
    }
}

void test_iterate()
{
    blt::log_box_t box(std::cout, "Iterate Tests", 25);
    for (auto v : blt::iterate(array_1))
    {
        BLT_TRACE("Element: {:.4f}", v);
    }
    BLT_TRACE("================================");
    for (auto v : blt::iterate(array_1).skip(5))
    {
        BLT_TRACE("Element: {:.4f}", v);
    }
    BLT_TRACE("================================");
    for (auto v : blt::iterate(array_1).take(5))
    {
        BLT_TRACE("Element: {:.4f}", v);
    }
    BLT_TRACE("================================");
    for (auto v : blt::iterate(array_1).rev())
    {
        BLT_TRACE("Element: {:.4f}", v);
    }
    BLT_TRACE("================================");
    for (auto [a, b] : blt::iterate(array_1).zip(list_1))
    {
        BLT_TRACE("Zip: {:.4f} {:.4f}", a, b);
    }
    BLT_TRACE("================================");
    for (auto [i, data] : blt::iterate(array_1).map([](const blt::vec2& in) { return in.normalize(); }).zip(list_1).skip(3).take(4).enumerate())
    {
        auto [a, b] = data;
        BLT_TRACE("Map + Zip + Skip + Take + Enumerate (Index: {})> {:.4f} {:.4f}", i, a, b);
    }
    BLT_TRACE("================================");
    for (auto [i, data] : blt::iterate(array_1).map(
            [](const blt::vec2& in) {
                return in.normalize();
            }).zip(list_1).skip(3).take(4).enumerate())
    {
        auto [a, b] = data;
        BLT_TRACE("Map + Zip + Skip + Take + Enumerate (Index: {})> {:.4f} {:.4f}", i, a, b);
    }
    BLT_TRACE("================================");
    for (auto a : blt::iterate(array_1).map([](const blt::vec2& in) { return in.normalize(); })
                                       .filter([](const blt::vec2& f) { return f.x() > 0.5; }))
    {
        if (!a)
            continue;
        auto v = *a;
        BLT_TRACE(" So this one works? {:.4f}", v);
    }
    BLT_TRACE("================================");
    for (auto a : blt::iterate(array_1).map([](const blt::vec2& in) { return in.normalize(); })
                                       .enumerate().filter([](const auto& f) { return f.second.x() > 0.5; }))
    {
        if (!a)
            continue;
        auto [index, v] = *a;
        BLT_TRACE(" So this one works? ({}) {:.4f}", index, v);
    }
    BLT_TRACE("================================");
//    for (auto a : blt::iterate(array_1).filter([](const auto& f) { return f.x() > 3 && f.y() < 6; }).take(2))
//    {
//        if (!a)
//            continue;
//        auto v = *a;
//        BLT_TRACE_STREAM << " How about this one?? " << v.get() << "\n";
//    }
    for (auto a : blt::iterate(array_1).map([](const auto& f) { return f.x() > 3 && f.y() < 6; }))
    {
        BLT_TRACE(" How about this one?? ({}) {:.4f}", a);
    }

//    for (auto [value, a] : blt::iterate(array_1).map(
//            [](const blt::vec2& in) {
//                return in.normalize();
//            }).enumerate().filter([](const auto& v) {
//                return v.index % 2 == 0;
//            }).zip(array_2).skip(3))
//    {
//        if (!value)
//            continue;
//        auto [i, v] = *value;
//        BLT_TRACE_STREAM << "Enumerate Filter (Index: " << i << ")> " << v << "\n";
//    }
}

int main()
{
    test_enumerate();
    std::cout << std::endl;
    test_pairs();
    std::cout << std::endl;
    test_zip();
    std::cout << std::endl;
    test_iterate();
}