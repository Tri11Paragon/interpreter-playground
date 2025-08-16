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
#include <blt/fs/threaded_writers.h>

namespace blt::fs
{
	concurrent_file_writer::concurrent_file_writer(writer_t* writer): m_writer{writer}
	{}

	i64 concurrent_file_writer::write(const char* buffer, const size_t bytes)
	{
		std::scoped_lock lock{m_mutex};
		return m_writer->write(buffer, bytes);
	}

	void concurrent_file_writer::flush()
	{
		std::scoped_lock lock{m_mutex};
		m_writer->flush();
	}
}