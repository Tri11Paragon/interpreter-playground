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

#ifndef BLT_FS_THREADED_WRITERS_H
#define BLT_FS_THREADED_WRITERS_H

#include <mutex>
#include <blt/fs/fwddecl.h>

namespace blt::fs
{
	// ReSharper disable once CppClassCanBeFinal
	class concurrent_file_writer : public writer_t
	{
	public:
		explicit concurrent_file_writer(writer_t* writer);

		i64 write(const char* buffer, size_t bytes) override;

		void flush() override;
	private:
		writer_t* m_writer;
		std::mutex m_mutex;
	};
}

#endif //BLT_FS_THREADED_WRITERS_H
