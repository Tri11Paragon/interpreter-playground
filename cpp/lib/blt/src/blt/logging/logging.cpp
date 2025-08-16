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
#include <iomanip>
#include <iostream>
#include <mutex>
#include <sstream>
#include <thread>
#include <blt/logging/injector.h>
#include <blt/logging/logging.h>
#include <blt/std/hashmap.h>
#include <blt/std/types.h>

namespace blt::logging
{
	struct global_context_t
	{
		logging_config_t global_config;
	};

	static global_context_t global_context;
#ifdef BLT_LOGGING_THREAD_SAFE
	static std::mutex global_logging_mutex;
#endif

	struct logging_thread_context_t
	{
		std::stringstream stream;
		std::stringstream logging_stream;
		std::string thread_name;
		logger_t logger{stream};
	};

	logging_thread_context_t& get_thread_context()
	{
		thread_local logging_thread_context_t context;
		return context;
	}

	std::string logger_t::to_string() const
	{
		return dynamic_cast<std::stringstream&>(m_stream).str();
	}

	size_t logger_t::find_ending_brace(size_t begin) const
	{
		size_t braces = 0;
		for (; begin < m_fmt.size(); ++begin)
		{
			if (m_fmt[begin] == '{')
				++braces;
			else if (m_fmt[begin] == '}')
				--braces;
			if (braces == 0)
				return begin;
		}
		return std::string::npos;
	}

	void logger_t::setup_stream(const fmt_spec_t& spec) const
	{
		if (spec.prefix_char)
			m_stream << std::setfill(*spec.prefix_char);
		else
			m_stream << std::setfill(' ');
		switch (spec.alignment)
		{
			case fmt_align_t::LEFT:
				m_stream << std::left;
				break;
			case fmt_align_t::CENTER:
				// TODO?
				break;
			case fmt_align_t::RIGHT:
				m_stream << std::right;
				break;
		}
		if (spec.width > 0)
			m_stream << std::setw(static_cast<i32>(spec.width));
		else
			m_stream << std::setw(0);
		if (spec.precision > 0)
			m_stream << std::setprecision(static_cast<i32>(spec.precision));
		else
			m_stream << std::setprecision(16);
		if (spec.alternate_form)
			m_stream << std::showbase;
		else
			m_stream << std::noshowbase;
		if (spec.uppercase)
			m_stream << std::uppercase;
		else
			m_stream << std::nouppercase;
		if (spec.sign == fmt_sign_t::PLUS)
			m_stream << std::showpos;
		else
			m_stream << std::noshowpos;
	}

	std::string logger_t::process_string(const std::string_view str)
	{
		auto result = std::string(str);
		size_t pos = 0;
		while (pos = result.find('{', pos), pos != std::string::npos)
		{
			if (pos > 0 && result[pos - 1] == '\\')
			{
				auto before = result.substr(0, pos - 1);
				auto after = result.substr(pos);
				result = before + after;
			} else
				++pos;
		}
		return result;
	}

	void logger_t::process_strings()
	{
		auto spec_it = m_fmt_specs.begin();
		auto str_it = m_string_sections.begin();
		for (; spec_it != m_fmt_specs.end(); ++spec_it, ++str_it)
		{
			m_stream << process_string(*str_it);
			auto arg_pos = spec_it->arg_id;
			if (arg_pos == -1)
				arg_pos = static_cast<i64>(m_arg_pos++);

			setup_stream(*spec_it);
			m_arg_print_funcs[arg_pos](m_stream, *spec_it);
		}
		m_stream << process_string(*str_it);
	}

	void logger_t::handle_type(std::ostream& stream, const fmt_spec_t& spec)
	{
		switch (spec.type)
		{
			case fmt_type_t::DECIMAL:
				stream << std::noboolalpha;
				stream << std::dec;
				break;
			case fmt_type_t::OCTAL:
				stream << std::oct;
				break;
			case fmt_type_t::HEX:
				stream << std::hex;
				break;
			case fmt_type_t::HEX_FLOAT:
				stream << std::hexfloat;
				break;
			case fmt_type_t::EXPONENT:
				stream << std::scientific;
				break;
			case fmt_type_t::FIXED_POINT:
				stream << std::fixed;
				break;
			case fmt_type_t::GENERAL:
				stream << std::defaultfloat;
				break;
			case fmt_type_t::UNSPECIFIED:
				stream << std::boolalpha;
				stream << std::dec;
				break;
			default:
				break;
		}
	}

	void logger_t::exponential(std::ostream& stream)
	{
		stream << std::scientific;
	}

	void logger_t::fixed(std::ostream& stream)
	{
		stream << std::fixed;
	}

	void logger_t::compile(std::string fmt)
	{
		m_fmt = std::move(fmt);
		m_last_fmt_pos = 0;
		m_arg_pos = 0;
		auto& ss = dynamic_cast<std::stringstream&>(m_stream);
		ss.str("");
		m_stream.clear();
		m_string_sections.clear();
		m_fmt_specs.clear();
		ptrdiff_t last_pos = 0;
		while (auto pair = consume_to_next_fmt())
		{
			const auto [begin, end] = *pair;
			m_string_sections.emplace_back(m_fmt.data() + last_pos, begin - last_pos);
			if (end - begin > 1)
				m_fmt_specs.push_back(m_parser.parse(std::string_view{m_fmt.data() + static_cast<ptrdiff_t>(begin) + 1, end - begin - 1}));
			else
				m_fmt_specs.emplace_back();
			last_pos = static_cast<ptrdiff_t>(end) + 1;
		}
		m_string_sections.emplace_back(m_fmt.data() + last_pos, m_fmt.size() - last_pos);
		m_last_fmt_pos = 0;
	}

	std::optional<std::pair<size_t, size_t>> logger_t::consume_to_next_fmt()
	{
		auto begin = m_fmt.find('{', m_last_fmt_pos);
		while (begin != std::string::npos && begin > 0 && m_fmt[begin - 1] == '\\')
			begin = m_fmt.find('{', begin + 1);;
		if (begin == std::string::npos)
			return {};
		const auto end = find_ending_brace(begin);
		if (end == std::string::npos)
		{
			std::stringstream ss;
			ss << "Invalid format string, missing closing '}' near " << m_fmt.substr(std::min(static_cast<i64>(begin) - 5, static_cast<i64>(0)));
			throw std::runtime_error(ss.str());
		}
		m_last_fmt_pos = end + 1;
		return std::pair{begin, end};
	}

	logger_t& get_global_logger()
	{
		return get_thread_context().logger;
	}

	void print(std::string str)
	{
#ifdef BLT_LOGGING_THREAD_SAFE
		std::scoped_lock lock{global_logging_mutex};
#endif
		const auto& config = get_global_config();
		bool should_print = true;
		if (!config.get_injectors().empty())
		{
			for (const auto& injector : config.get_injectors())
			{
				auto [new_logging_output, should_continue, should_log] = injector->inject(str);
				if (!should_log)
					should_print = false;
				str = std::move(new_logging_output);
				if (!should_continue)
					break;
			}
		}
		if (should_print)
			std::cout << str;
	}

	void newline()
	{
#ifdef BLT_LOGGING_THREAD_SAFE
		std::scoped_lock lock{global_logging_mutex};
#endif
		std::cout << std::endl;
	}

	logging_config_t& get_global_config()
	{
		return global_context.global_config;
	}

	void set_thread_name(const std::string& name)
	{
		get_thread_context().thread_name = name;
	}

	const std::string& get_thread_name()
	{
		return get_thread_context().thread_name;
	}

	std::ostream& get_local_stream()
	{
		auto& context = get_thread_context();
		context.logging_stream.str("");
		return context.logging_stream;
	}
}
