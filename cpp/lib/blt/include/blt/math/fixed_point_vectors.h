/*
 *  <Short Description>
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

#ifndef BLT_FIXED_POINT_VECTORS_H
#define BLT_FIXED_POINT_VECTORS_H

#include <blt/math/fixed_point.h>
#include <blt/math/vectors.h>

namespace blt
{
    
    using vec2fp = blt::vec<blt::fp64, 2>;
    using vec3fp = blt::vec<blt::fp64, 3>;
    using vec4fp = blt::vec<blt::fp64, 4>;
    
}

#endif //BLT_FIXED_POINT_VECTORS_H
