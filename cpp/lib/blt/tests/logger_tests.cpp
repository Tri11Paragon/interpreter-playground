/*
 *  <Short Description>
 *  Copyright (C) 2025  Brett Terpstra
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
#include <chrono>
#include <fstream>
#include <iostream>
#include <sstream>
#include <thread>
#include <blt/fs/filesystem.h>
#include <blt/logging/ansi.h>
#include <blt/logging/logging.h>
#include <blt/logging/status.h>
#include <blt/std/assert.h>
#include <blt/std/utility.h>

struct some_silly_type_t
{};

auto expected_str = std::string(R"(This is a println!
This is a println with args '42'
This is a println with multiple args '42' '32.342311859130859375' 'Hello World!'
This is a 'Well so am I except cooler :3' fmt string with positionals 'I am a string!'
This is a println with a sign +4120
This is a println with a sign -4120
This is a println with a space  4120
This is a println with a space -4120
This is a println with a minus 4120
This is a println with a minus -4120
This is a println with a with       4120
This is a println with a with leading zeros 0000004120
This is a println with a precision 42.2323423490
This is a println with hex 109a
This is a println with hex with leading 0x109a
This is a println with binary 0b00110010000110100101011000000000
This is a println with binary with space 0b10110010 00011010 01010110 00000000
This is a println with binary with space 10100010 00000000 00000000 00000000
This is a println with octal 015015
This is a println with hexfloat 0x1.926e978d4fdf4p+8
This is a println with exponent 4.4320902431999996e+07
This is a println with exponent 9.5324342340423400e+15
This is a println with general 953243.49
This is a println with general 9.532433240234033e+17
This is a println with a char B
This is a println with type some_silly_type_t
This is a println with boolean true
This is a println with boolean as int 0
This is a println with boolean as hex 0x1
This is a println with boolean as octal 1
This is a println with alignment left 64         end value
This is a println with alignment right         46 end value
This is a println with alignment left (fill)  46******** end value
This is a println with alignment right (fill) ********46 end value
This is a println with alignment right (fill with reserved character) ^^^^^^^^46 end value
This is a println with fill no alignment %%%%%%%%%%%%%%%%%%46 end value
This is a println with arg reference                46.02
This is a println with arg reference &&&&&&&&&&&&&&&&&&&&
)");

std::pair<bool, std::string> compare_strings(const std::string& s1, const std::string& s2)
{
	const auto size = std::min(s1.size(), s2.size());
	size_t index = 0;
	for (; index < size; ++index)
	{
		if (s1[index] != s2[index])
		{
			std::stringstream ss;
			const auto i1 = std::max(static_cast<blt::i64>(index) - 32, 0l);
			const auto l1 = std::min(static_cast<blt::i64>(size) - i1, 65l);
			ss << "Strings differ at index " << index << "!\n";
			ss << "'" << s1.substr(i1, l1) << "' vs '" << s2.substr(i1, l1) << "'" << std::endl;
			return {false, ss.str()};
		}
	}
	if (s1.size() != s2.size())
		return {false, "Strings size do not match '" + std::to_string(s1.size()) + "' vs '" + std::to_string(s2.size()) + "'"};
	return {true, ""};
}

int main()
{
	std::stringstream ss;
	blt::logging::println(ss, "This is a println!");
	blt::logging::println(ss, "This is a println with args '{}'", 42);
	blt::logging::println(ss, "This is a println with multiple args '{}' '{:.100}' '{}'", 42, 32.34231233f, "Hello World!");
	blt::logging::println(ss, "This is a '{1}' fmt string with positionals '{0}'", "I am a string!", "Well so am I except cooler :3");
	blt::logging::println(ss, "This is a println with a sign {:+}", 4120);
	blt::logging::println(ss, "This is a println with a sign {:+}", -4120);
	blt::logging::println(ss, "This is a println with a space {: }", 4120);
	blt::logging::println(ss, "This is a println with a space {: }", -4120);
	blt::logging::println(ss, "This is a println with a minus {:-}", 4120);
	blt::logging::println(ss, "This is a println with a minus {:-}", -4120);
	blt::logging::println(ss, "This is a println with a with {:10}", 4120);
	blt::logging::println(ss, "This is a println with a with leading zeros {:010}", 4120);
	blt::logging::println(ss, "This is a println with a precision {:.10f}", 42.232342349);
	blt::logging::println(ss, "This is a println with hex {:.10x}", 4250);
	blt::logging::println(ss, "This is a println with hex with leading {:#.10x}", 4250);
	blt::logging::println(ss, "This is a println with binary {:#b}", 6969420);
	blt::logging::println(ss, "This is a println with binary with space {: #b}", 6969421);
	blt::logging::println(ss, "This is a println with binary with space {: b}", 69);
	blt::logging::println(ss, "This is a println with octal {:#o}", 6669);
	blt::logging::println(ss, "This is a println with hexfloat {:a}", 402.4320);
	blt::logging::println(ss, "This is a println with exponent {:e}", 44320902.4320);
	blt::logging::println(ss, "This is a println with exponent {:e}", 9532434234042340.0);
	blt::logging::println(ss, "This is a println with general {:g}", 953243.49);
	blt::logging::println(ss, "This is a println with general {:g}", 953243324023403240.49);
	blt::logging::println(ss, "This is a println with a char {:c}", 66);
	blt::logging::println(ss, "This is a println with type {:t}", some_silly_type_t{});
	blt::logging::println(ss, "This is a println with boolean {}", true);
	blt::logging::println(ss, "This is a println with boolean as int {:d}", false);
	blt::logging::println(ss, "This is a println with boolean as hex {:#x}", true);
	blt::logging::println(ss, "This is a println with boolean as octal {:o}", true);
	blt::logging::println(ss, "This is a println with alignment left {:<10} end value", 64);
	blt::logging::println(ss, "This is a println with alignment right {:>10} end value", 46);
	blt::logging::println(ss, "This is a println with alignment left (fill)  {:*<10} end value", 46);
	blt::logging::println(ss, "This is a println with alignment right (fill) {:*>10} end value", 46);
	blt::logging::println(ss, "This is a println with alignment right (fill with reserved character) {:\\^>10} end value", 46);
	blt::logging::println(ss, "This is a println with fill no alignment {:%20} end value", 46);
	blt::logging::println(ss, "This is a println with arg reference {0:{1}.{2}f}", 46.0232, 20, 2);
	blt::logging::println(ss, "This is a println with arg reference {0:&{1}}", "", 20);
	// blt::logging::print(ss.str());
	auto [passed, error_msg] = compare_strings(expected_str, ss.str());
	BLT_ASSERT_MSG(passed && "Logger logged string doesn't match precomputed expected string!", error_msg.c_str());

	namespace ansi = blt::logging::ansi;
	namespace color = ansi::color;

	// for (blt::u8 r = 0; r < 6; r++)
	// {
	// 	for (blt::u8 g = 0; g < 6; g++)
	// 	{
	// 		for (blt::u8 b = 0; b < 6; b++)
	// 		{
	// 			blt::logging::println("{}This is a println with a color {:#3x} {:#3x} {:#3x}{}",
	// 								build(fg(color::color256{r, g, b}), bg(color::color256{
	// 										static_cast<unsigned char>(5 - r),
	// 										static_cast<unsigned char>(5 - g),
	// 										static_cast<unsigned char>(5 - b)
	// 									})), r, g, b, build(color::color_mode::RESET_ALL));
	// 		}
	// 	}
	// }
	// blt::logging::println("{}This is a color now with background{}",
	// 					build(color::color_mode::BOLD, fg(color::color8::RED), color::color_mode::DIM, bg(color::color_rgb(0, 100, 255))),
	// 					build(color::color_mode::RESET_ALL));


	std::ofstream os("test.txt");
	blt::fs::fstream_writer_t wtr(os);
	blt::fs::writer_string_wrapper_t writer(wtr);

	writer.write("This is a println with a stream\n");
	writer.write("This is a mixed print ");
	writer.write(std::to_string(25));
	writer.write(" with multiple types ");
	writer.write(std::to_string(34.23340));
	writer.write('\n');
	writer.write("What about just a new line character?\n");

	// blt::logging::println("Logged {} characters", charCount);
	//
	// BLT_TRACE("Hello this is am empty trace!");
	// BLT_TRACE("This is a trace with data {} {} {}", "bad at code", 413, "boy");
	//
	// BLT_DEBUG("This is complete? {}", "this is working!");
	// BLT_INFO("Hello there!");
	// BLT_WARN("This is a warning!");
	// BLT_ERROR("This is an error!");
	// BLT_FATAL("This is a fatal error!");
	// BLT_TRACE("This is a pointer {:f}", &charCount);
	//
	// BLT_TRACE("Now time to test the logger status box");

	blt::logging::status_progress_bar_t progress;
	blt::logging::status_bar_t status;
	status.add(progress);
	blt::logging::get_global_config().add_injector(status);

	progress.set_progress(1.0 / 103.0);
	BLT_TRACE("Hello There!");
	std::this_thread::sleep_for(std::chrono::milliseconds(1500));
	progress.set_progress(2.0 / 103.0);
	BLT_TRACE("I am printing stuff!");
	std::this_thread::sleep_for(std::chrono::milliseconds(1500));
	progress.set_progress(3.0 / 103.0);
	BLT_TRACE("How are you!?");

	for (int i = 0; i < 100; i++)
	{
		progress.set_progress((4.0 + i) / 103.0);
		BLT_INFO("I am printing some output {} times!", i + 1);
		std::this_thread::sleep_for(std::chrono::milliseconds(100));
	}

	std::this_thread::sleep_for(std::chrono::milliseconds(5000));

	/*std::cout << "\033[2J";
	constexpr int totalRows = 24;
	// std::cout << "\033[1;" << (totalRows - 1) << "r";
	std::cout << use_mode(ansi::mode::color80x25_text);

	for (int i = 1; i <= 10; ++i)
	{

		std::cout << "\033[1;1H";
		std::cout << "printed line " << i << std::endl;
		std::cout << "\033[" << totalRows << ";1H";
		std::cout << "[----status----]" << std::flush;
		std::this_thread::sleep_for(std::chrono::milliseconds(500));
	}

	std::cout << "\033[r";*/

	// blt::logging::println("This is println {}\twith a std::endl in the middle of it");
}
