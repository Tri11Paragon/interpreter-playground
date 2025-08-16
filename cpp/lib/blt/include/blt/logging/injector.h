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

#ifndef BLT_LOGGING_INJECTOR_H
#define BLT_LOGGING_INJECTOR_H

namespace blt::logging
{
	struct injector_output_t
	{
		std::string new_logging_output;
		// should we continue processing the injector call chain?
		bool should_continue = true;
		// should we log the resulting string at the end of the injector call chain? If false for any injector, it becomes false for all injectors.
		bool should_log = true;
	};

	class injector_t
	{
	public:
		virtual ~injector_t() = default;
		virtual injector_output_t inject(const std::string& input) = 0;
	};
}

#endif //BLT_LOGGING_INJECTOR_H
