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

#ifndef BLT_FS_PATH_HELPER_H
#define BLT_FS_PATH_HELPER_H

#include <string>
#include <string_view>

namespace blt::fs
{

    std::string base_name(const std::string& str);
    std::string_view base_name_sv(std::string_view str);

	std::string filename(const std::string& str);
	std::string_view filename_sv(std::string_view str);

	std::string extension(const std::string& str);
	std::string_view extension_sv(std::string_view str);

}

#endif //BLT_FS_PATH_HELPER_H
