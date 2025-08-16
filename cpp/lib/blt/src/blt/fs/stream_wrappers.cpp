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
#include <istream>
#include <ostream>
#include <blt/fs/stream_wrappers.h>

namespace blt::fs
{
	fstream_reader_t::fstream_reader_t(std::istream& stream): m_stream{&stream}
	{}

	i64 fstream_reader_t::read(char* buffer, const size_t bytes)
	{
		m_stream->read(buffer, static_cast<std::streamsize>(bytes));
		return m_stream->gcount();
	}

	fstream_writer_t::fstream_writer_t(std::ostream& stream): m_stream{&stream}
	{}

	i64 fstream_writer_t::write(const char* buffer, const size_t bytes)
	{
		m_stream->write(buffer, static_cast<std::streamsize>(bytes));
		return static_cast<i64>(bytes);
	}

	void fstream_writer_t::flush()
	{
		m_stream->flush();
	}

	i64 fstream_writer_t::tell()
	{
		return m_stream->tellp();
	}

	void fstream_writer_t::seek(const i64 offset, const seek_origin origin)
	{
		switch (origin)
		{
		case seek_origin::seek_cur:
			m_stream->seekp(offset, std::ios_base::cur);
			break;
		case seek_origin::seek_end:
			m_stream->seekp(offset, std::ios_base::end);
			break;
		case seek_origin::seek_set:
			m_stream->seekp(offset, std::ios_base::beg);
			break;
		}
	}
}
