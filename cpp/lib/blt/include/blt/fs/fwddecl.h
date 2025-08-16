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

#ifndef BLT_FS_FWDDECL_H
#define BLT_FS_FWDDECL_H

#include <blt/std/types.h>
#include <cstdio>

namespace blt::fs
{
	/**
	* A simple interface which provides a way of reading the next block of data from a resource. This is designed to replace the overly complex
	* std::ostream
	*/
	class reader_t
	{
	public:
		virtual ~reader_t() = default;
		explicit reader_t() = default;

		reader_t(const reader_t&) = delete;
		reader_t& operator=(const reader_t&) = delete;

		/**
		* Reads bytes from the internal filesystem implementation
		* @param buffer buffer to copy the read bytes into
		* @param bytes number of bytes to read
		* @return number of bytes read, or negative value if error. Errors are not required and can just return 0
		*/
		virtual i64 read(char* buffer, size_t bytes) = 0;

		virtual i64 read(void* buffer, const size_t bytes)
		{
			return this->read(static_cast<char*>(buffer), bytes);
		}
	};

	/**
	* A block writer without a definite backend implementation. Exactly the same as a block_reader but for writing to the filesystem.
	* this is designed to replace the overly complex std::istream
	*/
	class writer_t
	{
	public:
		enum class seek_origin
		{
			// Seek from current position
			seek_cur = SEEK_CUR,
			// Seek from end of file. Not valid on binary streams
			seek_end = SEEK_END,
			// Seek from start of file
			seek_set = SEEK_SET
		};

		virtual ~writer_t() = default;
		explicit writer_t() = default;

		writer_t(const writer_t&) = delete;
		writer_t& operator=(const writer_t&) = delete;

		/**
		* Writes the bytes to the filesystem backend implementation
		* @param buffer bytes to write
		* @param bytes number of bytes to write
		* @return number of bytes, or negative value if error. Zero is also a valid return, not indicating error in itself but can be the result of one.
		*/
		virtual i64 write(const char* buffer, size_t bytes) = 0;

		virtual i64 tell()
		{
			return 0;
		}

		virtual void seek(i64, seek_origin)
		{
		}

		i64 write(const void* buffer, const size_t bytes)
		{
			return this->write(static_cast<const char*>(buffer), bytes);
		}

		/**
		* Optional flush command which syncs the underlying objects
		*/
		virtual void flush()
		{};
	};
}

#endif //BLT_FS_FWDDECL_H
