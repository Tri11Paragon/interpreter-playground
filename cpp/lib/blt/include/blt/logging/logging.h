#pragma once
/*
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

#ifndef BLT_LOGGING_LOGGING_H
#define BLT_LOGGING_LOGGING_H

#include <cstring>
#include <functional>
#include <string>
#include <string_view>
#include <vector>
#include <blt/logging/fmt_tokenizer.h>
#include <blt/logging/logging_config.h>
#include <blt/meta/is_streamable.h>
#include <blt/std/utility.h>

namespace blt::logging
{
	struct logger_t
	{
		explicit logger_t(std::ostream& stream): m_stream(stream)
		{}

		template <typename... Args>
		std::string log(std::string fmt, Args&&... args)
		{
			if (fmt.empty())
				return fmt;
			auto sequence = std::make_integer_sequence<size_t, sizeof...(Args)>{};
			m_arg_print_funcs.clear();
			m_arg_print_funcs.resize(sizeof...(Args));
			create_conv_funcs(sequence, std::forward<Args>(args)...);
			compile(std::move(fmt));
			process_strings();
			return to_string();
		}

		[[nodiscard]] std::string to_string() const;

	private:
		template <typename... Args, size_t... Indexes>
		void create_conv_funcs(std::integer_sequence<size_t, Indexes...>, Args&&... args)
		{
			((handle_func<Indexes>(std::forward<Args>(args))), ...);
		}

		template <size_t index, typename T>
		void handle_func(const T& t)
		{
			m_arg_print_funcs[index] = [&t](std::ostream& stream, const fmt_spec_t& type) {
				switch (type.sign)
				{
					case fmt_sign_t::SPACE:
						if constexpr (std::is_arithmetic_v<T>)
						{
							if (type.type != fmt_type_t::BINARY && static_cast<i64>(t) >= 0l)
								stream << ' ';
						}
						break;
					case fmt_sign_t::PLUS:
					case fmt_sign_t::MINUS:
						break;
				}
				switch (type.type)
				{
					case fmt_type_t::BINARY:
					{
						if constexpr (std::is_trivially_copyable_v<T>)
						{
							// copy bytes of type
							char buffer[sizeof(T)];
							std::memcpy(buffer, &t, sizeof(T));
							// display prefix
							if (type.alternate_form)
								stream << '0' << (type.uppercase ? 'B' : 'b');
							// print bytes
							for (size_t i = 0; i < sizeof(T); ++i)
							{
								// print bits
								for (size_t j = 0; j < 8; ++j)
									stream << ((buffer[i] & (1 << j)) ? '1' : '0');
								// special seperator defined via sign (weird hack, change?)
								if (type.sign == fmt_sign_t::SPACE && i != sizeof(T) - 1)
									stream << ' ';
							}
						} else
						{
							if constexpr (blt::meta::is_streamable_v<T>)
								stream << t;
							else
								stream << "{INVALID TYPE}";
						}
						break;
					}
					case fmt_type_t::CHAR:
						if constexpr (std::is_arithmetic_v<T> || std::is_convertible_v<T, char>)
						{
							stream << static_cast<char>(t);
						} else
						{
							if constexpr (blt::meta::is_streamable_v<T>)
								stream << t;
							else
								stream << "{INVALID TYPE}";
						}
						break;
					case fmt_type_t::TYPE:
						stream << blt::type_string<T>();
						break;
					default:
						handle_type(stream, type);
						if constexpr (blt::meta::is_streamable_v<T>)
						{
							if constexpr (std::is_same_v<T, char> || std::is_same_v<T, signed char>)
								stream << static_cast<int>(t);
							else if constexpr (std::is_same_v<T, unsigned char>)
								stream << static_cast<unsigned int>(t);
							else
								stream << t;
						} else
							stream << "{INVALID TYPE}";
				}
			};
		}

		[[nodiscard]] size_t find_ending_brace(size_t begin) const;
		void setup_stream(const fmt_spec_t& spec) const;
		std::string process_string(std::string_view str);
		void process_strings();
		static void handle_type(std::ostream& stream, const fmt_spec_t& spec);

		static void exponential(std::ostream& stream);
		static void fixed(std::ostream& stream);

		void compile(std::string fmt);

		std::optional<std::pair<size_t, size_t>> consume_to_next_fmt();

		std::string m_fmt;
		std::ostream& m_stream;
		fmt_parser_t m_parser{m_arg_print_funcs};
		// normal sections of string
		std::vector<std::string_view> m_string_sections;
		// processed format specs
		std::vector<fmt_spec_t> m_fmt_specs;
		std::vector<std::function<void(std::ostream&, const fmt_spec_t&)>> m_arg_print_funcs;
		size_t m_last_fmt_pos = 0;
		size_t m_arg_pos = 0;
	};

	void print(std::string str);

	void newline();

	logger_t& get_global_logger();

	logging_config_t& get_global_config();

	std::ostream& get_local_stream();

	void set_thread_name(const std::string& name);

	const std::string& get_thread_name();

	template <typename... Args>
	void print(std::string fmt, Args&&... args)
	{
		auto& logger = get_global_logger();
		print(logger.log(std::move(fmt), std::forward<Args>(args)...));
	}

	template <typename... Args>
	void print(std::ostream& stream, std::string fmt, Args&&... args)
	{
		auto& logger = get_global_logger();
		stream << logger.log(std::move(fmt), std::forward<Args>(args)...);
	}

	template <typename... Args>
	void println(std::string fmt, Args&&... args)
	{
		print(std::move(fmt), std::forward<Args>(args)...);
		newline();
	}

	template <typename... Args>
	void println(std::ostream& stream, std::string fmt, Args&&... args)
	{
		print(stream, std::move(fmt), std::forward<Args>(args)...);
		stream << std::endl;
	}

	template <typename... Args>
	void log(const log_level_t level, const char* file, const i32 line, std::string fmt, Args&&... args)
	{
		auto& logger = get_global_logger();
		const auto& config = get_global_config();
		std::string user_str = logger.log(std::move(fmt), std::forward<Args>(args)...);
		if (!user_str.empty() && user_str.back() == '\n')
			user_str.pop_back();
		if (level == log_level_t::NONE)
		{
			print(user_str);
			newline();
			return;
		}
		auto log_fmt_str = config.generate(user_str, get_thread_name(), level, file, line);
		if (log_fmt_str)
			print(std::move(*log_fmt_str));
	}

	namespace detail
	{

	}
}

#if defined(__clang__) || defined(__llvm__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wgnu-zero-variadic-macro-arguments"
#endif

#ifdef BLT_DISABLE_LOGGING
#define BLT_LOG(level, fmt, ...)

#else
#define BLT_LOG(level, fmt, ...) blt::logging::log(level, __FILE__, __LINE__, fmt, ##__VA_ARGS__)

#ifdef BLT_DISABLE_TRACE
#define BLT_TRACE(format, ...)
#else
#define BLT_TRACE(format, ...) BLT_LOG(blt::logging::log_level_t::TRACE, format, ##__VA_ARGS__)
#endif

#ifdef BLT_DISABLE_DEBUG
#define BLT_DEBUG(format, ...)
#else
#define BLT_DEBUG(format, ...) BLT_LOG(blt::logging::log_level_t::DEBUG, format, ##__VA_ARGS__)
#endif

#ifdef BLT_DISABLE_INFO
#define BLT_INFO(format, ...)
#else
#define BLT_INFO(format, ...) BLT_LOG(blt::logging::log_level_t::INFO, format, ##__VA_ARGS__)
#endif

#ifdef BLT_DISABLE_WARN
#define BLT_WARN(format, ...)
#else
#define BLT_WARN(format, ...) BLT_LOG(blt::logging::log_level_t::WARN, format, ##__VA_ARGS__)
#endif

#ifdef BLT_DISABLE_ERROR
#define BLT_ERROR(format, ...)
#else
#define BLT_ERROR(format, ...) BLT_LOG(blt::logging::log_level_t::ERROR, format, ##__VA_ARGS__)
#endif

#ifdef BLT_DISABLE_FATAL
#define BLT_FATAL(format, ...)
#else
#define BLT_FATAL(format, ...) BLT_LOG(blt::logging::log_level_t::FATAL, format, ##__VA_ARGS__)
#endif

#endif

#if defined(__clang__) || defined(__llvm__)
#pragma clang diagnostic pop
#endif

#endif // BLT_LOGGING_LOGGING_H
