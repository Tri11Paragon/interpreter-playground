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

#ifndef BLT_LOGGING_FMT_TOKENIZER_H
#define BLT_LOGGING_FMT_TOKENIZER_H

#include <functional>
#include <optional>
#include <string_view>
#include <vector>
#include <blt/std/types.h>

namespace blt::logging
{
    enum class fmt_token_type : u8
    {
        STRING,
        NUMBER,
        SPACE,
        COLON,
        DOT,
        MINUS,
        PLUS,
        POUND,
        LEFT_CHEVRON,
        RIGHT_CHEVRON,
        OPEN_BRACKET,
        CLOSE_BRACKET,
        CARET
    };

    enum class fmt_align_t : u8
    {
        LEFT,
        CENTER,
        RIGHT
    };

    enum class fmt_sign_t : u8
    {
        SPACE,
        PLUS,
        MINUS
    };

    enum class fmt_type_t : u8
    {
        BINARY,             // 'b'
        CHAR,               // 'c'
        DECIMAL,            // 'd'
        OCTAL,              // 'o'
        HEX,                // 'x'
        HEX_FLOAT,          // 'a'
        EXPONENT,           // 'e'
        FIXED_POINT,        // 'f'
        GENERAL,            // 'g'
        TYPE,               // 't'
        UNSPECIFIED         // default
    };

    struct fmt_spec_t
    {
        i64 arg_id = -1;
        i64 width = -1;
        i64 precision = -1;
        fmt_type_t type = fmt_type_t::UNSPECIFIED;
        fmt_sign_t sign = fmt_sign_t::MINUS;
        fmt_align_t alignment = fmt_align_t::RIGHT;
        std::optional<char> prefix_char;
        bool uppercase = false;
        bool alternate_form = false;
    };

    struct fmt_token_t
    {
        fmt_token_type type;
        std::string_view value;
    };

    class fmt_tokenizer_t
    {
    public:
        explicit fmt_tokenizer_t() = default;

        static fmt_token_type get_type(char c);

        std::optional<fmt_token_t> next();

        std::vector<fmt_token_t> tokenize(std::string_view fmt);

    private:
        size_t m_pos = 0;
        std::string_view m_fmt{};
    };


    class fmt_parser_t
    {
    public:
        explicit fmt_parser_t(std::vector<std::function<void(std::ostream&, const fmt_spec_t&)>>& handlers): m_handlers(handlers)
        {
        }

        fmt_token_t& peek(const size_t offset)
        {
            return m_tokens[m_pos + offset];
        }

        fmt_token_t& peek()
        {
            return m_tokens[m_pos];
        }

        [[nodiscard]] bool has_next() const
        {
            return m_pos < m_tokens.size();
        }

        [[nodiscard]] bool has_next(const size_t offset) const
        {
            return (m_pos + offset) < m_tokens.size();
        }

        [[nodiscard]] fmt_token_t& next()
        {
            return m_tokens[m_pos++];
        }

        void consume()
        {
            ++m_pos;
        }

        void consume(const size_t amount)
        {
            m_pos += amount;
        }

        const fmt_spec_t& parse(std::string_view fmt);

    private:
        static bool is_align_t(fmt_token_type type);

        void parse_fmt_field();
        void parse_arg_id();
        std::string parse_arg_or_number();

        void parse_fmt_spec();
        void parse_fmt_spec_fill();
        void parse_fmt_spec_align();
        void parse_fmt_spec_sign();
        void parse_fmt_spec_form();
        void parse_fmt_spec_width();
        void parse_fmt_spec_precision();

        void parse_fill();
        void parse_align();
        void parse_sign();
        void parse_form();
        void parse_width();
        void parse_precision();
        void parse_type();

        size_t m_pos = 0;
        std::vector<fmt_token_t> m_tokens;
        fmt_tokenizer_t m_tokenizer;
        fmt_spec_t m_spec;

        std::vector<std::function<void(std::ostream&, const fmt_spec_t&)>>& m_handlers;
    };
}

#endif //BLT_LOGGING_FMT_TOKENIZER_H
