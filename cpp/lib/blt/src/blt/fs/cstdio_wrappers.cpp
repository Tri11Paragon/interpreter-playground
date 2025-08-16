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
#include <blt/fs/cstdio_wrappers.h>

namespace blt::fs
{
    i64 file_reader_t::read(char* buffer, const size_t bytes)
    {
        return fread(buffer, bytes, 1, static_cast<FILE*>(m_file));
    }

    i64 file_writer_t::write(const char* buffer, const size_t bytes)
    {
        return fwrite(buffer, bytes, 1, static_cast<FILE*>(m_file));
    }

    i64 file_writer_t::tell()
    {
        return ftell(static_cast<FILE*>(m_file));
    }

    void file_writer_t::seek(const i64 offset, const seek_origin origin)
    {
        fseek(static_cast<FILE*>(m_file), offset, static_cast<int>(origin));
    }
}
