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
#include <cstdio>
#include <iostream>
#ifdef unix
#include <termios.h>
#include <unistd.h>
#endif
#include <blt/logging/ansi.h>
#include <blt/logging/fmt_tokenizer.h>
#include <blt/logging/logging.h>
#include <blt/logging/status.h>
#include <blt/math/vectors.h>
#include <blt/math/log_util.h>

namespace blt::logging
{
	vec2i get_cursor_position()
	{
#ifdef unix
		termios save{}, raw{};

		tcgetattr(0, &save);
		cfmakeraw(&raw);
		tcsetattr(0,TCSANOW, &raw);

		char buf[32];
		char cmd[] = "\033[6n";

		int row = 0;
		int col = 0;

		if (isatty(fileno(stdin)))
		{
			ssize_t i = write(1, cmd, sizeof(cmd));
			(void) i;
			i = read(0, buf, sizeof(buf));
			(void) i;

			int sep = 0;
			int end = 0;
			for (int i = 2; i < 8; i++)
			{
				if (buf[i] == ';')
					sep = i;
				if (buf[i] == 'R')
				{
					end = i;
					break;
				}
			}
			row = std::stoi(std::string(buf + 2, buf + sep));
			col = std::stoi(std::string(buf + sep + 1, buf + end));
		}

		tcsetattr(0,TCSANOW, &save);

		return vec2i{row, col};
#else
		return {0,0};
#endif
	}

#define SIZE 100

	vec2i get_screen_size()
	{
#ifdef unix
		char in[SIZE] = "";
		int each = 0;
		int ch = 0;
		int rows = 0;
		int cols = 0;
		termios original, changed;

		// change terminal settings
		tcgetattr( STDIN_FILENO, &original);
		changed = original;
		changed.c_lflag &= ~( ICANON | ECHO);
		changed.c_cc[VMIN] = 1;
		changed.c_cc[VTIME] = 0;
		tcsetattr( STDIN_FILENO, TCSANOW, &changed);

		printf ( "\033[2J"); //clear screen

		printf ( "\033[9999;9999H"); // cursor should move as far as it can

		printf ( "\033[6n"); // ask for cursor position
		while ( ( ch = getchar ()) != 'R') { // R terminates the response
			if ( EOF == ch) {
				break;
			}
			if ( isprint ( ch)) {
				if ( each + 1 < SIZE) {
					in[each] = ch;
					each++;
					in[each] = '\0';
				}
			}
		}

		printf ( "\033[1;1H"); // move to upper left corner
		if ( 2 == sscanf ( in, "[%d;%d", &rows, &cols)) {
			tcsetattr( STDIN_FILENO, TCSANOW, &original);
			return {rows, cols};
		}
		throw std::runtime_error("Could not get screen size");
#else
		return {0,0};
#endif
	}

	i32 get_size_no_ansi(const std::string& str)
	{
		i32 size = 0;
		for (size_t i = 0; i < str.size(); i++)
		{
			if (str[i] == BLT_ANSI_ESCAPE[0])
			{
				while (i < str.size())
				{
					if (std::isalpha(str[i++]))
						break;
				}
			}
			++size;
		}
		return size - 1;
	}

	std::string status_progress_bar_t::print(const vec2i, const i32 max_printed_length) const
	{
		std::string output = "[";
		output.reserve(max_printed_length);
		const auto amount_filled = (max_printed_length - 2) * m_progress;
		auto amount_filled_int = static_cast<i32>(amount_filled);
		const auto frac = amount_filled - static_cast<double>(amount_filled_int);

		for (i64 i = 0; i < amount_filled_int; i++)
			output += '#';
		if (frac >= 0.5)
		{
			output += '|';
			++amount_filled_int;
		}
		for (i64 i = amount_filled_int; i < max_printed_length - 2; i++)
			output += ' ';

		output += ']';
		return output;
	}

	void status_progress_bar_t::set_progress(const double progress)
	{
		if (std::isnan(progress) || progress < 0 || progress > 1 || std::isinf(progress))
			throw std::invalid_argument("Progress must be between 0 and 1 (got: " + std::to_string(progress) + ")");
		m_progress = progress;
		// m_status->redraw();
	}

	status_bar_t::status_bar_t()
	{
		m_screen_size = get_screen_size();
		std::cout << ansi::cursor::home << std::flush;
		std::cout << ansi::erase::entire_screen << std::flush;
		m_begin_position = m_last_log_position = get_cursor_position();
		std::cout << ansi::cursor::hide_cursor << std::flush;
	}

	injector_output_t status_bar_t::inject(const std::string& input)
	{
		std::scoped_lock lock{m_print_mutex};
		injector_output_t output{input, false, false};
		if (output.new_logging_output.back() != '\n')
			output.new_logging_output += '\n';

		if (get_cursor_position() != m_begin_position)
		{
			for (int i = 0; i < m_status_size; i++)
				std::cout << ansi::erase::entire_line << ansi::cursor::move_begin_up(1) << std::flush;
		}
		std::cout << ansi::erase::entire_line << std::flush;
		std::cout << output.new_logging_output << std::flush;
		m_max_printed_length = std::max(get_size_no_ansi(output.new_logging_output), m_max_printed_length);
		m_last_log_position = get_cursor_position();
		redraw();

		return output;
	}

	void status_bar_t::compute_size()
	{
		m_status_size = 0;
		for (const auto* ptr : m_status_items)
			m_status_size += ptr->lines_used();
	}

	void status_bar_t::redraw() const
	{
		std::cout << ansi::cursor::move_to(m_last_log_position.x(), m_last_log_position.y());
		for (const auto* ptr : m_status_items)
			std::cout << ansi::erase::entire_line << ptr->print(m_screen_size, m_max_printed_length) << std::endl;
		std::cout << std::flush;
	}

	status_bar_t::~status_bar_t()
	{
		std::cout << ansi::cursor::show_cursor << std::flush;
	}
}
