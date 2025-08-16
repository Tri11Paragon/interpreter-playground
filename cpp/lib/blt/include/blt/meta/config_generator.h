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

#ifndef BLT_CONFIG_GENERATOR_H
#define BLT_CONFIG_GENERATOR_H

#include <utility>

namespace blt
{
#define BLT_MAKE_GETTER_LVALUE(TYPE, NAME)                              \
    TYPE& get_##NAME() { return NAME; }

#define BLT_MAKE_GETTER_CLVALUE(TYPE, NAME)                             \
    const TYPE& get_##NAME() const { return NAME; }

#define BLT_MAKE_GETTER_RVALUE(TYPE, NAME)                              \
    TYPE get_##NAME() const { return NAME; }

#define BLT_MAKE_GETTER(TYPE, NAME)                                     \
    BLT_MAKE_GETTER_LVALUE(TYPE, NAME)                                  \
    BLT_MAKE_GETTER_CLVALUE(TYPE, NAME)

#define BLT_MAKE_SETTER_LVALUE(TYPE, NAME)                              \
    auto& set_##NAME(const TYPE& new_##NAME)                            \
    {                                                                   \
        NAME = new_##NAME;                                              \
        return *this;                                                   \
    }

#define BLT_MAKE_SETTER_RVALUE(TYPE, NAME)                              \
    auto& set_##NAME(TYPE&& new_##NAME)                                 \
    {                                                                   \
        NAME = std::move(new_##NAME);                                   \
        return *this;                                                   \
    }

#define BLT_MAKE_VALUE(TYPE, NAME)                                      \
    TYPE NAME;                                                          \
    BLT_MAKE_GETTER_CLVALUE(TYPE, NAME)                                 \
    BLT_MAKE_SETTER_RVALUE(TYPE, NAME)

#define BLT_MAKE_VALUE_DEFAULT(TYPE, NAME, DEFAULT)                     \
    TYPE NAME = DEFAULT;                                                \
    BLT_MAKE_GETTER_CLVALUE(TYPE, NAME)                                 \
    BLT_MAKE_SETTER_RVALUE(TYPE, NAME)

#define BLT_MAKE_SETTER(TYPE, NAME)                                     \
    BLT_MAKE_SETTER_LVALUE(TYPE, NAME)                                  \
    BLT_MAKE_SETTER_RVALUE(TYPE, NAME)

#define BLT_MAKE_GETTER_AND_SETTER(TYPE, NAME)                          \
    BLT_MAKE_GETTER(TYPE, NAME)                                         \
    BLT_MAKE_SETTER(TYPE, NAME)
    
#define BLT_MAKE_FRIEND(FRIEND) friend FRIEND;

#define BLT_MAKE_CONFIG_TYPE(OBJECT, ...)                               \
    class OBJECT {                                                      \
        __VA_ARGS__                                                     \
    };                                                                  \

}

#endif //BLT_CONFIG_GENERATOR_H
