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

#ifndef BLT_FS_STREAM_WRAPPERS_H
#define BLT_FS_STREAM_WRAPPERS_H

#include <iosfwd>
#include <sstream>
#include <blt/fs/fwddecl.h>

namespace blt::fs
{
	/**
		* reader_t wrapper for fstream
		*/
	class fstream_reader_t final : public reader_t
	{
	public:
		explicit fstream_reader_t(std::istream& stream);

		i64 read(char* buffer, size_t bytes) override;

	private:
		std::istream* m_stream;
	};

	class fstream_writer_t final : public writer_t
	{
	public:
		explicit fstream_writer_t(std::ostream& stream);

		i64 write(const char* buffer, size_t bytes) override;

		void flush() override;

		i64 tell() override;

		void seek(i64 offset, seek_origin origin = seek_origin::seek_set) override;

		virtual ~fstream_writer_t() override // NOLINT
		{
			flush();
		}

	private:
		std::ostream* m_stream;
	};

	class reader_wrapper_t
	{
	public:
		explicit reader_wrapper_t(reader_t& reader): m_reader(&reader)
		{}

		template <typename T>
		void read(T& out)
		{
			if (!m_reader->read(reinterpret_cast<char*>(&out), sizeof(T)))
				throw std::runtime_error("Failed to read from reader");
		}

		template <typename T>
		friend reader_wrapper_t& operator>>(reader_wrapper_t& reader, T& t)
		{
			reader.read(t);
			return reader;
		}

	private:
		reader_t* m_reader;
	};

	class writer_wrapper_t
	{
	public:
		explicit writer_wrapper_t(writer_t& writer): m_writer(&writer)
		{}

		template <typename T>
		void write(const T& t)
		{
			static_assert(std::is_trivially_copyable_v<T>);
			m_writer->write(reinterpret_cast<const char*>(&t), sizeof(T));
		}

		template <typename T>
		friend writer_wrapper_t& operator<<(writer_wrapper_t& writer, const T& t)
		{
			writer.write(t);
			return writer;
		}

	private:
		writer_t* m_writer;
	};

	class writer_string_wrapper_t
	{
	public:
		explicit writer_string_wrapper_t(writer_t& writer): m_writer(&writer)
		{}

		template <typename T>
		void write(const T& t)
		{
			std::stringstream ss;
			ss << t;
			const auto str = ss.str();
			m_writer->write(str.data(), str.size());
		}

		template <typename T>
		friend writer_string_wrapper_t& operator<<(writer_string_wrapper_t& writer, const T& t)
		{
			writer.write(t);
			return writer;
		}

	private:
		writer_t* m_writer;
	};
}

#endif //BLT_FS_STREAM_WRAPPERS_H
