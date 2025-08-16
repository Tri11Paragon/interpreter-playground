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
#include <blt/std/error.h>
#include <blt/logging/logging.h>

namespace blt::error
{
	void print_socket_error()
	{
		switch (errno)
		{
			case EINVAL: BLT_WARN("Invalid argument");
				break;
			case EACCES: BLT_WARN("Permission denied");
				break;
			case EPERM: BLT_WARN("Operation not permitted");
				break;
			case EADDRINUSE: BLT_WARN("Address already in use");
				break;
			case EADDRNOTAVAIL: BLT_WARN("Cannot copy_fast requested address");
				break;
			case EAFNOSUPPORT: BLT_WARN("Address family not supported by protocol");
				break;
			case EAGAIN: BLT_WARN("Try again");
				break;
			case EALREADY: BLT_WARN("Operation already in progress");
				break;
			case EBADF: BLT_WARN("Bad file number");
				break;
			case ECONNREFUSED: BLT_WARN("Connection refused");
				break;
			case EFAULT: BLT_WARN("Bad address");
				break;
			case EINPROGRESS: BLT_WARN("Operation now in progress");
				break;
			case EINTR: BLT_WARN("Interrupted system call");
				break;
			case EISCONN: BLT_WARN("Transport endpoint is already connected");
				break;
			case ENETUNREACH: BLT_WARN("Network is unreachable");
				break;
			case ENOTSOCK: BLT_WARN("Socket operation_t on non-socket");
				break;
			case EPROTOTYPE: BLT_WARN("Protocol wrong type for socket");
				break;
			case ETIMEDOUT: BLT_WARN("Connection timed out");
				break;
			default:
				break;
		}
	}
}
