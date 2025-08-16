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
#include <ctime>
#include <iostream>
#include <blt/fs/path_helper.h>
#include <blt/fs/stream_wrappers.h>
#include <blt/logging/ansi.h>
#include <blt/logging/logging_config.h>
#include <blt/std/hashmap.h>
#include <blt/std/system.h>
#include <blt/std/time.h>

namespace blt::logging
{
	namespace tags::detail
	{
		hashmap_t<std::string_view, log_tag_token_t> make_map()
		{
			hashmap_t<std::string_view, log_tag_token_t> map{};
			map[YEAR] = log_tag_token_t::YEAR;
			map[MONTH] = log_tag_token_t::MONTH;
			map[DAY] = log_tag_token_t::DAY;
			map[HOUR] = log_tag_token_t::HOUR;
			map[MINUTE] = log_tag_token_t::MINUTE;
			map[SECOND] = log_tag_token_t::SECOND;
			map[MILLISECOND] = log_tag_token_t::MS;
			map[NANOSECOND] = log_tag_token_t::NS;
			map[UNIX_TIME] = log_tag_token_t::UNIX;
			map[UNIX_TIME_NANO] = log_tag_token_t::UNIX_NANO;
			map[ISO_YEAR] = log_tag_token_t::ISO_YEAR;
			map[TIME] = log_tag_token_t::TIME;
			map[FULL_TIME] = log_tag_token_t::FULL_TIME;
			map[LOG_COLOR] = log_tag_token_t::LC;
			map[ERROR_COLOR] = log_tag_token_t::EC;
			map[CONDITIONAL_ERROR_COLOR] = log_tag_token_t::CEC;
			map[RESET] = log_tag_token_t::RESET;
			map[LOG_LEVEL] = log_tag_token_t::LL;
			map[THREAD_NAME] = log_tag_token_t::TN;
			map[FILE] = log_tag_token_t::FILE;
			map[LINE] = log_tag_token_t::LINE;
			map[STR] = log_tag_token_t::STR;
			return map;
		}
	}

	void logging_config_t::compile()
	{
		static hashmap_t<std::string_view, tags::detail::log_tag_token_t> tag_map = tags::detail::make_map();
		m_log_tag_content.clear();
		m_log_tag_tokens.clear();

		size_t i = 0;
		for (; i < m_log_format.size(); ++i)
		{
			size_t start = i;
			while (i < m_log_format.size() && m_log_format[i] != '{')
				++i;
			if (i == m_log_format.size() || (i < m_log_format.size() && (i - start) > 0))
			{
				m_log_tag_content.emplace_back(std::string_view(m_log_format.data() + start, i - start));
				m_log_tag_tokens.emplace_back(tags::detail::log_tag_token_t::CONTENT);
				if (i == m_log_format.size())
					break;
			}
			start = i;
			while (i < m_log_format.size() && m_log_format[i] != '}')
				++i;
			const auto tag = std::string_view(m_log_format.data() + start, i - start + 1);
			auto it = tag_map.find(tag);
			if (it == tag_map.end())
				throw std::runtime_error("Invalid log tag: " + std::string(tag));
			m_log_tag_tokens.emplace_back(it->second);
		}

		if (i < m_log_format.size())
		{
			m_log_tag_content.emplace_back(std::string_view(m_log_format.data() + i, m_log_format.size() - i));
			m_log_tag_tokens.emplace_back(tags::detail::log_tag_token_t::CONTENT);
		}

		m_longest_name_length = 0;
		for (const auto& name : m_log_level_names)
			m_longest_name_length = std::max(m_longest_name_length, name.size());
	}

	std::string add_year(const tm* current_time)
	{
		return std::to_string(current_time->tm_year + 1900);
	}

	std::string add_month(const tm* current_time, const bool ensure_alignment)
	{
		auto str = std::to_string(current_time->tm_mon + 1);
		if (ensure_alignment && str.size() < 2)
			str.insert(str.begin(), '0');
		return str;
	}

	std::string add_day(const tm* current_time, const bool ensure_alignment)
	{
		auto str = std::to_string(current_time->tm_mday);
		if (ensure_alignment && str.size() < 2)
			str.insert(str.begin(), '0');
		return str;
	}

	std::string add_hour(const tm* current_time, const bool ensure_alignment)
	{
		auto str = std::to_string(current_time->tm_hour);
		if (ensure_alignment && str.size() < 2)
			str.insert(str.begin(), '0');
		return str;
	}

	std::string add_minute(const tm* current_time, const bool ensure_alignment)
	{
		auto str = std::to_string(current_time->tm_min);
		if (ensure_alignment && str.size() < 2)
			str.insert(str.begin(), '0');
		return str;
	}

	std::string add_second(const tm* current_time, const bool ensure_alignment)
	{
		auto str = std::to_string(current_time->tm_sec);
		if (ensure_alignment && str.size() < 2)
			str.insert(str.begin(), '0');
		return str;
	}

	std::optional<std::string> logging_config_t::generate(const std::string& user_str, const std::string& thread_name, const log_level_t level,
														const char* file, const i32 line) const
	{
		if (level < m_level)
			return {};

		std::string fmt;

		const std::time_t time = std::time(nullptr);
		const auto current_time = std::localtime(&time);
		const auto millis_time = system::getCurrentTimeMilliseconds();
		const auto nano_time = system::getCurrentTimeNanoseconds();

		size_t content = 0;
		for (const auto& log_tag_token : m_log_tag_tokens)
		{
			switch (log_tag_token)
			{
				case tags::detail::log_tag_token_t::YEAR:
					fmt += add_year(current_time);
					break;
				case tags::detail::log_tag_token_t::MONTH:
					fmt += add_month(current_time, m_ensure_alignment);
					break;
				case tags::detail::log_tag_token_t::DAY:
					fmt += add_day(current_time, m_ensure_alignment);
					break;
				case tags::detail::log_tag_token_t::HOUR:
					fmt += add_hour(current_time, m_ensure_alignment);
					break;
				case tags::detail::log_tag_token_t::MINUTE:
					fmt += add_minute(current_time, m_ensure_alignment);
					break;
				case tags::detail::log_tag_token_t::SECOND:
					fmt += add_second(current_time, m_ensure_alignment);
					break;
				case tags::detail::log_tag_token_t::MS:
				{
					auto str = std::to_string(millis_time % 1000);
					if (m_ensure_alignment)
					{
						for (size_t i = str.size(); i < 4; ++i)
							str.insert(str.begin(), '0');
					}
					fmt += str;
					break;
				}
				case tags::detail::log_tag_token_t::NS:
				{
					auto str = std::to_string(nano_time % 1000000000ul);
					if (m_ensure_alignment)
					{
						for (size_t i = str.size(); i < 9; ++i)
							str.insert(str.begin(), '0');
					}
					fmt += str;
					break;
				}
				case tags::detail::log_tag_token_t::UNIX:
				{
					fmt += std::to_string(millis_time);
					break;
				}
				case tags::detail::log_tag_token_t::UNIX_NANO:
				{
					fmt += std::to_string(nano_time);
					break;
				}
				case tags::detail::log_tag_token_t::ISO_YEAR:
				{
					fmt += add_year(current_time);
					fmt += '-';
					fmt += add_month(current_time, m_ensure_alignment);
					fmt += '-';
					fmt += add_day(current_time, m_ensure_alignment);
					break;
				}
				case tags::detail::log_tag_token_t::TIME:
					fmt += add_hour(current_time, m_ensure_alignment);
					fmt += ':';
					fmt += add_minute(current_time, m_ensure_alignment);
					fmt += ':';
					fmt += add_second(current_time, m_ensure_alignment);
					break;
				case tags::detail::log_tag_token_t::FULL_TIME:
					fmt += add_year(current_time);
					fmt += '-';
					fmt += add_month(current_time, m_ensure_alignment);
					fmt += '-';
					fmt += add_day(current_time, m_ensure_alignment);
					fmt += ' ';
					fmt += add_hour(current_time, m_ensure_alignment);
					fmt += ':';
					fmt += add_minute(current_time, m_ensure_alignment);
					fmt += ':';
					fmt += add_second(current_time, m_ensure_alignment);
					break;
				case tags::detail::log_tag_token_t::LC:
					if (!m_use_color)
						break;
					fmt += m_log_level_colors[static_cast<u8>(level)];
					break;
				case tags::detail::log_tag_token_t::EC:
					if (!m_use_color)
						break;
					fmt += m_error_color;
					break;
				case tags::detail::log_tag_token_t::CEC:
					if (!m_use_color)
						break;
					if (static_cast<u8>(level) >= static_cast<u8>(log_level_t::ERROR))
						fmt += m_error_color;
					break;
				case tags::detail::log_tag_token_t::RESET:
					if (!m_use_color)
						break;
					fmt += build(ansi::color::color_mode::RESET_ALL);
					break;
				case tags::detail::log_tag_token_t::LL:
					fmt += m_log_level_names[static_cast<u8>(level)];
					break;
				case tags::detail::log_tag_token_t::TN:
					fmt += thread_name;
					break;
				case tags::detail::log_tag_token_t::FILE:
					if (m_print_full_name)
						fmt += file;
					else
						fmt += fs::filename_sv(file);
					break;
				case tags::detail::log_tag_token_t::LINE:
					fmt += std::to_string(line);
					break;
				case tags::detail::log_tag_token_t::STR:
					fmt += user_str;
					break;
				case tags::detail::log_tag_token_t::CONTENT:
					fmt += m_log_tag_content[content++];
					break;
			}
		}

		return fmt;
	}

	std::string logging_config_t::get_default_log_format()
	{
		return build(fg(ansi::color::color8_bright::BLUE)) + "[" + tags::FULL_TIME + "]" + tags::RESET + " " + tags::LOG_COLOR + "[" + tags::LOG_LEVEL
		+ "]" + tags::RESET + " " + build(fg(ansi::color::color8::MAGENTA)) + "(" + tags::FILE + ":" + tags::LINE + ")" + tags::RESET + " " +
		tags::CONDITIONAL_ERROR_COLOR + tags::STR + tags::RESET + "\n";
	}

	std::vector<fs::writer_t*> logging_config_t::get_default_log_outputs()
	{
		static fs::fstream_writer_t cout_writer{std::cout};
		std::vector<fs::writer_t*> outputs{};
		outputs.push_back(&cout_writer);
		return outputs;
	}

	std::array<std::string, LOG_LEVEL_COUNT> logging_config_t::get_default_log_level_colors()
	{
		return {
			// TRACE
			build(fg(ansi::color::color8_bright::WHITE)),
			// DEBUG
			build(fg(ansi::color::color8::CYAN)),
			// INFO
			build(fg(ansi::color::color8_bright::GREEN)),
			// WARN
			build(fg(ansi::color::color8_bright::YELLOW)),
			// ERROR
			build(fg(ansi::color::color8_bright::RED)),
			// FATAL
			build(fg(ansi::color::color8_bright::WHITE), bg(ansi::color::color8_bright::RED)),
		};
	}

	std::array<std::string, LOG_LEVEL_COUNT> logging_config_t::get_default_log_level_names()
	{
		return {"TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL",};
	}

	std::string logging_config_t::get_default_error_color()
	{
		return build(fg(ansi::color::color8_bright::RED));
	}
}
