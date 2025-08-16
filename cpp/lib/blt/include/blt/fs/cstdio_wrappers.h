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

#ifndef BLT_FS_CSTDIO_WRAPPERS_H
#define BLT_FS_CSTDIO_WRAPPERS_H

#include <blt/fs/fwddecl.h>

namespace blt::fs
{
    class file_reader_t final : public reader_t
    {
    public:
        explicit file_reader_t(void* file): m_file(file)
        {
        }

        i64 read(char* buffer, size_t bytes) override;

    private:
        void* m_file;
    };

    class file_writer_t final : public writer_t
    {
    public:
        explicit file_writer_t(void* file): m_file(file)
        {
        }

        i64 write(const char* buffer, size_t bytes) override;

        i64 tell() override;

        void seek(i64 offset, seek_origin origin = seek_origin::seek_set) override;
    private:
        void* m_file;
    };
}

#endif //BLT_FS_CSTDIO_WRAPPERS_H
