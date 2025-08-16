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

#ifndef BLT_META_CODEGEN_H
#define BLT_META_CODEGEN_H

namespace blt
{
#define BLT_CONST_LVALUE_GETTER(TYPE, NAME) const TYPE& get_##NAME() const { return NAME; }
#define BLT_LVALUE_GETTER(TYPE, NAME) TYPE& get_##NAME() { return NAME; }
#define BLT_PRVALUE_GETTER(TYPE, NAME) TYPE get_##NAME() const { return NAME; }

#define BLT_GLVALUE_GETTER(TYPE, NAME)                                  \
    BLT_CONST_LVALUE_GETTER(TYPE, NAME)                                 \
    BLT_LVALUE_GETTER(TYPE, NAME)

#define BLT_PRVALUE_SETTER(TYPE, NAME)                                  \
    auto& set_##NAME(TYPE new_##NAME)                                   \
    {                                                                   \
        NAME = new_##NAME;                                              \
        return *this;                                                   \
    }

#define BLT_PRVALUE_MOVE_SETTER(TYPE, NAME)                             \
    auto& set_##NAME(TYPE new_##NAME)                                   \
    {                                                                   \
        NAME = std::move(new_##NAME);                                   \
        return *this;                                                   \
    }

#define BLT_LVALUE_SETTER(TYPE, NAME)                                   \
    auto& set_##NAME(const TYPE& new_##NAME)                            \
    {                                                                   \
        NAME = new_##NAME;                                              \
        return *this;                                                   \
    }
    
}

#endif //BLT_META_CODEGEN_H
