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

#ifndef BLT_LOGGING_FWDDECL_H
#define BLT_LOGGING_FWDDECL_H

namespace blt::logging
{
	struct logger_t;
	enum class fmt_token_type : u8;
	enum class fmt_align_t : u8;
	enum class fmt_sign_t : u8;
	enum class fmt_type_t : u8;
	struct fmt_spec_t;
	struct fmt_token_t;
	class fmt_tokenizer_t;
	class fmt_parser_t;

	class injector_t;
}

#endif //BLT_LOGGING_FWDDECL_H
