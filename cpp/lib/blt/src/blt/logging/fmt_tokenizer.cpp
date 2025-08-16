/*
 *  <Short Description>
 *  Copyright (C) 2025  Brett Terpstra
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
#include <iomanip>
#include <iostream>
#include <sstream>
#include <blt/logging/fmt_tokenizer.h>

namespace blt::logging
{
    fmt_token_type fmt_tokenizer_t::get_type(const char c)
    {
        switch (c)
        {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            return fmt_token_type::NUMBER;
        case '+':
            return fmt_token_type::PLUS;
        case '-':
            return fmt_token_type::MINUS;
        case '.':
            return fmt_token_type::DOT;
        case ':':
            return fmt_token_type::COLON;
        case ' ':
            return fmt_token_type::SPACE;
        case '#':
            return fmt_token_type::POUND;
        case '<':
            return fmt_token_type::LEFT_CHEVRON;
        case '>':
            return fmt_token_type::RIGHT_CHEVRON;
        case '^':
            return fmt_token_type::CARET;
        case '{':
            return fmt_token_type::OPEN_BRACKET;
        case '}':
            return fmt_token_type::CLOSE_BRACKET;
        default:
            return fmt_token_type::STRING;
        }
    }

    std::optional<fmt_token_t> fmt_tokenizer_t::next()
    {
        if (m_pos >= m_fmt.size())
            return {};
        bool is_escaped = false;
        if (m_fmt[m_pos] == '\\')
        {
            is_escaped = true;
            ++m_pos;
        }
        switch (const auto base_type = get_type(m_fmt[m_pos]))
        {
        case fmt_token_type::SPACE:
        case fmt_token_type::PLUS:
        case fmt_token_type::MINUS:
        case fmt_token_type::DOT:
        case fmt_token_type::COLON:
        case fmt_token_type::POUND:
        case fmt_token_type::LEFT_CHEVRON:
        case fmt_token_type::RIGHT_CHEVRON:
        case fmt_token_type::CARET:
        case fmt_token_type::OPEN_BRACKET:
        case fmt_token_type::CLOSE_BRACKET:
            if (is_escaped)
                return fmt_token_t{fmt_token_type::STRING, std::string_view{m_fmt.data() + m_pos++, 1}};
            return fmt_token_t{base_type, std::string_view{m_fmt.data() + m_pos++, 1}};
        default:
            {
                const auto begin = m_pos;
                for (; m_pos < m_fmt.size() && get_type(m_fmt[m_pos]) == base_type; ++m_pos)
                {
                }
                return fmt_token_t{base_type, std::string_view{m_fmt.data() + begin, m_pos - begin}};
            }
        }
    }

    std::vector<fmt_token_t> fmt_tokenizer_t::tokenize(const std::string_view fmt)
    {
        m_fmt = fmt;
        m_pos = 0;
        std::vector<fmt_token_t> tokens;
        while (auto token = next())
            tokens.push_back(*token);
        return tokens;
    }

    const fmt_spec_t& fmt_parser_t::parse(const std::string_view fmt)
    {
        m_spec = {};
        m_pos = 0;
        m_tokens = m_tokenizer.tokenize(fmt);

        parse_fmt_field();

        return m_spec;
    }

    bool fmt_parser_t::is_align_t(const fmt_token_type type)
    {
        switch (type)
        {
        case fmt_token_type::LEFT_CHEVRON:
        case fmt_token_type::RIGHT_CHEVRON:
        case fmt_token_type::CARET:
            return true;
        default:
            return false;
        }
    }

    void fmt_parser_t::parse_fmt_field()
    {
        if (!has_next())
            throw std::runtime_error("Expected token when parsing format field");
        const auto [type, value] = peek();
        switch (type)
        {
        case fmt_token_type::NUMBER:
            parse_arg_id();
            if (has_next())
            {
                if (peek().type == fmt_token_type::COLON)
                    parse_fmt_spec();
                else
                    throw std::runtime_error("Expected ':' when parsing format field after arg id!");
            }
            break;
        case fmt_token_type::COLON:
            parse_fmt_spec();
            break;
        default:
            {
                std::stringstream ss;
                ss << "Expected unknown token '" << static_cast<u8>(type) << "' value '" << value << "' when parsing format field";
                throw std::runtime_error(ss.str());
            }
        }
        if (has_next())
            parse_type();
    }

    void fmt_parser_t::parse_arg_id()
    {
        const auto [type, value] = next();
        if (type != fmt_token_type::NUMBER)
        {
            std::stringstream ss;
            ss << "Expected number when parsing arg id, unexpected value '" << value << '\'';
            throw std::runtime_error(ss.str());
        }
        m_spec.arg_id = std::stoll(std::string(value));
    }

    std::string fmt_parser_t::parse_arg_or_number()
    {
        auto [type, value] = next();
        if (type == fmt_token_type::NUMBER)
            return std::string(value);
        if (type == fmt_token_type::OPEN_BRACKET)
        {
            auto [next_type, next_value] = next();
            if (next_type != fmt_token_type::NUMBER)
                throw std::runtime_error("Expected number when parsing arg or number, unexpected value '" + std::string(next_value) + '\'');
            if (next().type != fmt_token_type::CLOSE_BRACKET)
                throw std::runtime_error("Expected closing bracket when parsing arg or number, unexpected value '" + std::string(next_value) + '\'');
            // TODO: this feels like an evil hack.
            const auto arg_id = std::stoul(std::string(next_value));
            if (arg_id >= m_handlers.size())
                throw std::runtime_error("Invalid arg id " + std::to_string(arg_id) + ", max arg supported: " + std::to_string(m_handlers.size()));
            std::stringstream ss;
            m_handlers[arg_id](ss, fmt_spec_t{});
            return ss.str();
        }
        throw std::runtime_error("Expected number when parsing arg or number, unexpected value '" + std::string(value) + '\'');
    }

    void fmt_parser_t::parse_fmt_spec()
    {
        // consume :
        consume();
        auto [type, value] = peek();
        switch (type)
        {
        case fmt_token_type::STRING:
            // if there is a token beyond this string, it is not a type string
            if (has_next(1))
                parse_fmt_spec_fill();
            return;
        case fmt_token_type::RIGHT_CHEVRON:
        case fmt_token_type::LEFT_CHEVRON:
        case fmt_token_type::CARET:
            parse_fmt_spec_align();
            break;
        case fmt_token_type::COLON:
        case fmt_token_type::CLOSE_BRACKET:
            {
                std::stringstream ss;
                ss << "(Stage (Begin)) Invalid token type " << static_cast<u8>(type) << " value " << value;
                throw std::runtime_error(ss.str());
            }
        case fmt_token_type::NUMBER:
        case fmt_token_type::OPEN_BRACKET:
            parse_fmt_spec_width();
            break;
        case fmt_token_type::DOT:
            parse_fmt_spec_precision();
            break;
        case fmt_token_type::SPACE:
        case fmt_token_type::MINUS:
        case fmt_token_type::PLUS:
            parse_fmt_spec_sign();
            break;
        case fmt_token_type::POUND:
            parse_fmt_spec_form();
            break;
        }
    }

    void fmt_parser_t::parse_fmt_spec_fill()
    {
        parse_fill();
        if (!has_next())
            return;
        auto [type, value] = peek();
        switch (type)
        {
        case fmt_token_type::RIGHT_CHEVRON:
        case fmt_token_type::LEFT_CHEVRON:
        case fmt_token_type::CARET:
            parse_fmt_spec_align();
            break;
        case fmt_token_type::NUMBER:
        case fmt_token_type::OPEN_BRACKET:
            parse_fmt_spec_width();
            break;
        case fmt_token_type::DOT:
            parse_fmt_spec_precision();
            break;
        case fmt_token_type::SPACE:
        case fmt_token_type::MINUS:
        case fmt_token_type::PLUS:
            parse_fmt_spec_sign();
            break;
        case fmt_token_type::POUND:
            parse_fmt_spec_form();
            break;
        case fmt_token_type::STRING:
            return;
        case fmt_token_type::COLON:
        case fmt_token_type::CLOSE_BRACKET:
            {
                std::stringstream ss;
                ss << "(Stage (Fill)) Invalid token type " << static_cast<u8>(type) << " value " << value;
                throw std::runtime_error(ss.str());
            }
        }
    }

    void fmt_parser_t::parse_fmt_spec_align()
    {
        parse_align();
        if (!has_next())
            return;
        auto [type, value] = peek();
        switch (type)
        {
        case fmt_token_type::STRING:
            return;
        case fmt_token_type::NUMBER:
        case fmt_token_type::OPEN_BRACKET:
            parse_fmt_spec_width();
            break;
        case fmt_token_type::DOT:
            parse_fmt_spec_precision();
            break;
        case fmt_token_type::SPACE:
        case fmt_token_type::MINUS:
        case fmt_token_type::PLUS:
            parse_fmt_spec_sign();
            break;
        case fmt_token_type::POUND:
            parse_fmt_spec_form();
            break;
        case fmt_token_type::CARET:
        case fmt_token_type::COLON:
        case fmt_token_type::LEFT_CHEVRON:
        case fmt_token_type::RIGHT_CHEVRON:
        case fmt_token_type::CLOSE_BRACKET:
            {
                std::stringstream ss;
                ss << "(Stage (Align)) Invalid token type " << static_cast<u8>(type) << " value " << value;
                throw std::runtime_error(ss.str());
            }
        }
    }

    // handle start of fmt, with sign
    void fmt_parser_t::parse_fmt_spec_sign()
    {
        parse_sign();
        if (!has_next())
            return;
        auto [type, value] = peek();
        switch (type)
        {
        case fmt_token_type::STRING:
            return;
        case fmt_token_type::SPACE:
        case fmt_token_type::MINUS:
        case fmt_token_type::PLUS:
        case fmt_token_type::COLON:
        case fmt_token_type::CARET:
        case fmt_token_type::LEFT_CHEVRON:
        case fmt_token_type::RIGHT_CHEVRON:
        case fmt_token_type::CLOSE_BRACKET:
            {
                std::stringstream ss;
                ss << "(Stage (Sign)) Invalid token type " << static_cast<u8>(type) << " value " << value;
                throw std::runtime_error(ss.str());
            }
        case fmt_token_type::NUMBER:
        case fmt_token_type::OPEN_BRACKET:
            parse_fmt_spec_width();
            break;
        case fmt_token_type::DOT:
            parse_fmt_spec_precision();
            break;
        case fmt_token_type::POUND:
            parse_fmt_spec_form();
            break;
        }
    }

    void fmt_parser_t::parse_fmt_spec_form()
    {
        parse_form();
        if (!has_next())
            return;
        auto [type, value] = peek();
        switch (type)
        {
        case fmt_token_type::STRING:
            return;
        case fmt_token_type::SPACE:
        case fmt_token_type::COLON:
        case fmt_token_type::MINUS:
        case fmt_token_type::PLUS:
        case fmt_token_type::POUND:
        case fmt_token_type::CARET:
        case fmt_token_type::LEFT_CHEVRON:
        case fmt_token_type::RIGHT_CHEVRON:
        case fmt_token_type::CLOSE_BRACKET:
            {
                std::stringstream ss;
                ss << "(Stage (Form)) Invalid token type " << static_cast<u8>(type) << " value " << value;
                throw std::runtime_error(ss.str());
            }
        case fmt_token_type::NUMBER:
        case fmt_token_type::OPEN_BRACKET:
            parse_fmt_spec_width();
            break;
        case fmt_token_type::DOT:
            parse_fmt_spec_precision();
            break;
        }
    }

    // handle width parsing
    void fmt_parser_t::parse_fmt_spec_width()
    {
        parse_width();
        if (!has_next())
            return;
        auto [type, value] = peek();
        switch (type)
        {
        case fmt_token_type::STRING:
            return;
        case fmt_token_type::COLON:
        case fmt_token_type::MINUS:
        case fmt_token_type::PLUS:
        case fmt_token_type::SPACE:
        case fmt_token_type::POUND:
        case fmt_token_type::NUMBER:
        case fmt_token_type::CARET:
        case fmt_token_type::LEFT_CHEVRON:
        case fmt_token_type::RIGHT_CHEVRON:
        case fmt_token_type::OPEN_BRACKET:
        case fmt_token_type::CLOSE_BRACKET:
            {
                std::stringstream ss;
                ss << "(Stage (Width)) Invalid token type " << static_cast<u8>(type) << " value " << value;
                throw std::runtime_error(ss.str());
            }
        case fmt_token_type::DOT:
            parse_fmt_spec_precision();
            break;
        }
    }

    void fmt_parser_t::parse_fmt_spec_precision()
    {
        // consume .
        consume();
        parse_precision();
    }

    void fmt_parser_t::parse_fill()
    {
        auto [type, value] = next();
        if (type != fmt_token_type::STRING)
        {
            std::stringstream ss;
            ss << "Expected string when parsing fill, got " << static_cast<u8>(type) << " value " << value;
            throw std::runtime_error(ss.str());
        }
        m_spec.prefix_char = value.front();
    }

    void fmt_parser_t::parse_align()
    {
        auto [type, value] = next();
        switch (type)
        {
        case fmt_token_type::LEFT_CHEVRON:
            m_spec.alignment = fmt_align_t::LEFT;
            break;
        case fmt_token_type::RIGHT_CHEVRON:
            m_spec.alignment = fmt_align_t::RIGHT;
            break;
        case fmt_token_type::CARET:
            m_spec.alignment = fmt_align_t::CENTER;
            break;
        default:
            {
                std::stringstream ss;
                ss << "Invalid align type " << static_cast<u8>(type) << " value " << value;
                throw std::runtime_error(ss.str());
            }
        }
    }

    void fmt_parser_t::parse_sign()
    {
        auto [_, value] = next();
        if (value.size() > 1)
        {
            std::stringstream ss;
            ss << "Sign contains more than one character, we are not sure how to interpret this. Value '" << value << "'";
            throw std::runtime_error(ss.str());
        }
        switch (value[0])
        {
        case '+':
            m_spec.sign = fmt_sign_t::PLUS;
            break;
        case '-':
            m_spec.sign = fmt_sign_t::MINUS;
            break;
        case ' ':
            m_spec.sign = fmt_sign_t::SPACE;
            break;
        default:
            {
                std::stringstream ss;
                ss << "Invalid sign " << value[0];
                throw std::runtime_error(ss.str());
            }
        }
    }

    void fmt_parser_t::parse_form()
    {
        consume();
        m_spec.alternate_form = true;
    }

    void fmt_parser_t::parse_width()
    {
        const auto value = parse_arg_or_number();
        if (value.front() == '0' && !m_spec.prefix_char.has_value())
            m_spec.prefix_char = '0';
        m_spec.width = std::stoll(value);
    }

    void fmt_parser_t::parse_precision()
    {
        if (!has_next())
            throw std::runtime_error("Missing token when parsing precision");
        auto value = parse_arg_or_number();
        m_spec.precision = std::stoll(std::string(value));
    }

    void fmt_parser_t::parse_type()
    {
        auto [_, value] = next();
        if (value.size() != 1)
        {
            std::stringstream ss;
            ss << "Type contains more than one character, we are not sure how to interpret this value '" << value << "'";
            throw std::runtime_error(ss.str());
        }
        m_spec.uppercase = std::isupper(value.front());
        switch (value.front())
        {
        case 'b':
        case 'B':
            m_spec.type = fmt_type_t::BINARY;
            break;
        case 'c':
            m_spec.type = fmt_type_t::CHAR;
            break;
        case 'd':
            m_spec.type = fmt_type_t::DECIMAL;
            break;
        case 'o':
            m_spec.type = fmt_type_t::OCTAL;
            break;
        case 'x':
        case 'X':
            m_spec.type = fmt_type_t::HEX;
            break;
        case 'a':
        case 'A':
            m_spec.type = fmt_type_t::HEX_FLOAT;
            break;
        case 'e':
        case 'E':
            m_spec.type = fmt_type_t::EXPONENT;
            break;
        case 'f':
        case 'F':
            m_spec.type = fmt_type_t::FIXED_POINT;
            break;
        case 'g':
        case 'G':
            m_spec.type = fmt_type_t::GENERAL;
            break;
        case 't':
        case 'T':
            m_spec.type = fmt_type_t::TYPE;
            break;
        default:
            std::stringstream ss;
            ss << "Invalid type " << value;
            ss << std::endl << std::endl;
            ss << "Expected one of: " << std::endl;
            ss << std::setw(4) << ' ' << std::left << std::setw(5) << "b | B" << std::setw(6) << ' ' << "Print as binary output" << std::endl;
            ss << std::setw(4) << ' ' << std::left << std::setw(5) << "c" << std::setw(6) << ' ' << "Print as character output" << std::endl;
            ss << std::setw(4) << ' ' << std::left << std::setw(5) << "d" << std::setw(6) << ' ' << "Print as decimal (base 10) output" << std::endl;
            ss << std::setw(4) << ' ' << std::left << std::setw(5) << "o" << std::setw(6) << ' ' << "Print as octal (base 8) output" << std::endl;
            ss << std::setw(4) << ' ' << std::left << std::setw(5) << "x | X" << std::setw(6) << ' ' << "Print as hexadecimal (base 16) output" <<
                std::endl;
            ss << std::setw(4) << ' ' << std::left << std::setw(5) << "a | A" << std::setw(6) << ' ' << "Print floats as hexadecimal (base 16) output"
                << std::endl;
            ss << std::setw(4) << ' ' << std::left << std::setw(5) << "e | E" << std::setw(6) << ' ' << "Print floats in scientific notation" <<
                std::endl;
            ss << std::setw(4) << ' ' << std::left << std::setw(5) << "f | F" << std::setw(6) << ' ' <<
                "Print floats in fixed point output, useful for setting precision of output" << std::endl;
            ss << std::setw(4) << ' ' << std::left << std::setw(5) << "g | G" << std::setw(6) << ' ' <<
                "Print floats in general output, switching between fixed point and scientific notation based on magnitude" << std::endl;
            ss << std::setw(4) << ' ' << std::left << std::setw(5) << "t | T" << std::setw(6) << ' ' << "Print the type as a string" << std::endl;
            throw std::runtime_error(ss.str());
        }
    }
}
