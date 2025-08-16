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
#include <cstring>
#include <ctime>
#include <stdexcept>
#include <utility>
#include <blt/fs/file_writers.h>

namespace blt::fs
{
	i64 fwriter_t::write(const char* buffer, const size_t bytes)
	{
		return static_cast<i64>(std::fwrite(buffer, 1, bytes, m_file));
	}

	void fwriter_t::flush()
	{
		writer_t::flush();
		std::fflush(m_file);
	}

	void fwriter_t::newfile(const std::string& new_name)
	{
		if (m_file != nullptr)
			std::fclose(m_file);
		m_file = std::fopen(new_name.c_str(), m_mode.c_str());
		if (!m_file)
			throw std::runtime_error("Failed to open file for writing");
	}

	buffered_writer::buffered_writer(const std::string& name, const size_t buffer_size): fwriter_t{name, "ab"}
	{
		m_buffer.resize(buffer_size);
	}

	buffered_writer::buffered_writer(const size_t buffer_size)
	{
		m_buffer.resize(buffer_size);
	}

	void buffered_writer::newfile(const std::string& new_name)
	{
		fwriter_t::newfile(new_name);
		setvbuf(this->m_file, nullptr, _IONBF, 0);
	}

	i64 buffered_writer::write(const char* buffer, const size_t bytes)
	{
		if (bytes > m_buffer.size())
			return fwriter_t::write(buffer, bytes);
		if (bytes + m_current_pos > m_buffer.size())
			flush();
		std::memcpy(m_buffer.data() + m_current_pos, buffer, bytes);
		m_current_pos += bytes;
		return static_cast<i64>(bytes);
	}

	void buffered_writer::flush()
	{
		fwriter_t::write(m_buffer.data(), m_current_pos);
		fwriter_t::flush();
		m_current_pos = 0;
	}

	bounded_writer::bounded_writer(fwriter_t& writer, std::optional<std::string> base_name, const size_t max_size, naming_function_t naming_function):
		m_writer(&writer), m_base_name(std::move(base_name)), m_max_size(max_size), m_naming_function(std::move(naming_function))
	{
		bounded_writer::newfile(m_base_name.value_or(""));
	}

	i64 bounded_writer::write(const char* buffer, const size_t bytes)
	{
		m_currently_written += bytes;
		if (m_currently_written > m_max_size)
			this->newfile(m_base_name.value_or(""));
		return m_writer->write(buffer, bytes);
	}

	void bounded_writer::newfile(const std::string& new_name)
	{
		++m_current_invocation;
		m_currently_written = 0;
		m_writer->newfile(m_naming_function(m_current_invocation, new_name));
	}

	void bounded_writer::flush()
	{
		m_writer->flush();
	}

	rotating_writer::rotating_writer(fwriter_t& writer, const time_t period): m_writer{&writer}, m_period{period}
	{
		newfile();
	}

	i64 rotating_writer::write(const char* buffer, const size_t bytes)
	{
		check_for_time();
		return m_writer->write(buffer, bytes);
	}

	void rotating_writer::flush()
	{
		check_for_time();
		m_writer->flush();
	}

	void rotating_writer::newfile(const std::string& new_name)
	{
		m_writer->newfile(new_name);
	}

	void rotating_writer::newfile()
	{
		m_last_time = get_current_time();
		std::string name;
		name += std::to_string(m_last_time.year);
		name += "-" + std::to_string(m_last_time.month);
		name += "-" + std::to_string(m_last_time.day);
		if (m_period.hour >= 0)
			name += "-" + std::to_string(m_last_time.hour);
		name += ".txt";
		newfile(name);
	}

	void rotating_writer::check_for_time()
	{
		const auto current_time = get_current_time();
		if ((m_period.hour > 0 && current_time.hour > m_last_time.hour + m_period.hour) ||
			(m_period.day > 0 && current_time.day > m_last_time.day + m_period.day) ||
			(m_period.month > 0 && current_time.month > m_last_time.month + m_period.month) ||
			(m_period.year > 0 && current_time.year > m_last_time.year + m_period.year))
			newfile();
	}

	time_t rotating_writer::get_current_time()
	{
		const std::time_t time = std::time(nullptr);
		const auto current_time = std::localtime(&time);
		return {current_time->tm_year + 1900, current_time->tm_mon + 1, current_time->tm_mday, current_time->tm_hour};
	}
}
