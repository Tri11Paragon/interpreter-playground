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
#include <blt/parse/templating.h>
#include <blt/std/string.h>
#include <cctype>
#include "blt/logging/logging.h"

namespace blt
{
    
    bool isNonStringNext(char c)
    {
        switch (c)
        {
            case '$':
            case '{':
            case '}':
            case '(':
            case ')':
            case '"':
            case '^':
            case '!':
            case '&':
            case ';':
            case ',':
            case '.':
            case '|':
            case '+':
                return true;
            default:
                return false;
        }
    }
    
    blt::expected<std::vector<template_token_data_t>, template_tokenizer_failure_t> template_engine_t::process_string(std::string_view str)
    {
        std::vector<template_token_data_t> tokens;
        
        template_char_consumer_t consumer(str);
        
        i64 start = -1;
        size_t paren_level = 0;
        size_t level = 0;
        bool open = false;
        while (consumer.hasNext())
        {
            i64 current_start = static_cast<i64>(consumer.getCurrentIndex());
            char c = consumer.consume();
            switch (c)
            {
                case '$':
                    tokens.emplace_back(template_token_t::IDENT, level, consumer.from(current_start, current_start + 1));
                    if (consumer.next() == '{')
                    {
                        paren_level = 0;
                        open = true;
                    }
                    continue;
                case '{':
                    tokens.emplace_back(template_token_t::CURLY_OPEN, level, consumer.from(current_start, current_start + 1));
                    if (open)
                        level++;
                    continue;
                case '}':
                    tokens.emplace_back(template_token_t::CURLY_CLOSE, level, consumer.from(current_start, current_start + 1));
                    if (open)
                        level--;
                    if (level == 0)
                    {
                        open = false;
                        if (paren_level != 0)
                            return blt::unexpected(template_tokenizer_failure_t::MISMATCHED_PAREN);
                    }
                    continue;
                case '(':
                    tokens.emplace_back(template_token_t::PAR_OPEN, level, consumer.from(current_start, current_start + 1), paren_level);
                    paren_level++;
                    break;
                case ')':
                    tokens.emplace_back(template_token_t::PAR_CLOSE, level, consumer.from(current_start, current_start + 1), paren_level);
                    paren_level--;
                    break;
                case '"':
                    tokens.emplace_back(template_token_t::QUOTE, level, consumer.from(current_start, current_start + 1));
                    // if we just encountered a quote, we need to consume characters until we find its matching quote
                    // only if we are currently inside a template though...
                    if (open)
                    {
                        current_start = static_cast<i64>(consumer.getCurrentIndex());
                        while (consumer.hasNext() && consumer.next() != '"')
                            consumer.advance();
                        if (!consumer.hasNext())
                            return blt::unexpected(template_tokenizer_failure_t::MISMATCHED_QUOTE);
                        tokens.emplace_back(template_token_t::STRING, level, consumer.from(current_start, consumer.getCurrentIndex()));
                        consumer.advance();
                        current_start = static_cast<i64>(consumer.getCurrentIndex());
                        tokens.emplace_back(template_token_t::QUOTE, level, consumer.from(current_start, current_start + 1));
                    }
                    break;
                case '^':
                    tokens.emplace_back(template_token_t::XOR, level, consumer.from(current_start, current_start + 1));
                    break;
                case '!':
                    tokens.emplace_back(template_token_t::NOT, level, consumer.from(current_start, current_start + 1));
                    break;
                case ';':
                    tokens.emplace_back(template_token_t::SEMI, level, consumer.from(current_start, current_start + 1));
                    break;
                case ',':
                    tokens.emplace_back(template_token_t::COMMA, level, consumer.from(current_start, current_start + 1));
                    break;
                case '+':
                    tokens.emplace_back(template_token_t::ADD, level, consumer.from(current_start, current_start + 1));
                    break;
                case '.':
                    tokens.emplace_back(template_token_t::PERIOD, level, consumer.from(current_start, current_start + 1));
                    break;
                case '~':
                    tokens.emplace_back(template_token_t::FUNCTION, level, consumer.from(current_start, current_start + 1));
                    break;
                case '|':
                    if (consumer.hasNext() && consumer.next() == '|')
                    {
                        consumer.advance();
                        tokens.emplace_back(template_token_t::OR, level, consumer.from(current_start, current_start + 2));
                        continue;
                    }
                    start = current_start;
                    break;
                case '&':
                    if (consumer.hasNext() && consumer.next() == '&')
                    {
                        consumer.advance();
                        tokens.emplace_back(template_token_t::AND, level, consumer.from(current_start, current_start + 2));
                        continue;
                    }
                    start = current_start;
                    break;
                default:
                    // do not add whitespace to anything
                    if (std::isspace(c))
                    {
                        break;
                    }
                    if (start == -1)
                        start = current_start;
                    if (consumer.hasNext() && (isNonStringNext(consumer.next()) || std::isspace(consumer.next())))
                    {
                        tokens.emplace_back(template_token_t::STRING, level, consumer.from(start, consumer.getCurrentIndex()));
                        start = -1;
                    }
                    break;
            }
        }
        
        if (start != -1)
            tokens.emplace_back(template_token_t::STRING, level, consumer.from(start, consumer.getCurrentIndex()));
        
        for (auto& token : tokens)
        {
            if (token.type == template_token_t::STRING && detail::identifiers.contains(token.token))
                token.type = detail::identifiers.at(token.token);
        }
        
        if (level != 0)
            return unexpected(template_tokenizer_failure_t::MISMATCHED_CURLY);
        
        return tokens;
    }
    
    blt::expected<std::string, template_parser_failure_t> template_engine_t::internal_evaluate(std::string_view str, bool discard)
    {
        auto tokens = process_string(str);
        
        if (!tokens)
        {
            switch (tokens.error())
            {
                case template_tokenizer_failure_t::MISMATCHED_CURLY:
                    BLT_ERROR("Mismatched curly braces");
                    break;
                case template_tokenizer_failure_t::MISMATCHED_PAREN:
                    BLT_ERROR("Mismatched parentheses");
                    break;
                case template_tokenizer_failure_t::MISMATCHED_QUOTE:
                    BLT_ERROR("Mismatched quotes");
                    break;
            }
            return blt::unexpected(template_parser_failure_t::TOKENIZER_FAILURE);
        }
        
        std::string return_str;
        //return_str.reserve(str.size());
        
        template_token_consumer_t consumer{tokens.value(), str};
        
        template_parser_t parser(*this, consumer);
        
        while (consumer.hasNext())
        {
            while (consumer.hasNext(2))
            {
                if (consumer.next().type == template_token_t::IDENT && consumer.next(1).type == template_token_t::CURLY_OPEN)
                {
                    return_str += consumer.from_last();
                    break;
                }
                consumer.advance();
            }
            if (!consumer.hasNext(2))
                break;
            
            if (auto result = parser.parse())
                return_str += result.value();
            else
            {
                if (result.error() == template_parser_failure_t::FUNCTION_DISCARD)
                {
                    if (discard)
                        return blt::unexpected(template_parser_failure_t::FUNCTION_DISCARD);
                } else
                    return result;
            }
            consumer.set_marker();
        }
        while (consumer.hasNext())
            consumer.advance();
        return_str += consumer.from_last();
        
        return return_str;
    }
    
    template_parser_t::ebool template_parser_t::bool_expression()
    {
        // this whole thing is just bad. please redo. TODO
        std::vector<int> values;
        while (consumer.next().type != template_token_t::CURLY_OPEN)
        {
            auto next = consumer.next();
            auto bv = bool_value();
            if (!bv)
                return bv;
            values.push_back(bv.value());
            
            if (values.size() == 2)
            {
                auto b1 = values[0];
                auto b2 = values[1];
                values.pop_back();
                values.pop_back();
                switch (next.type)
                {
                    case template_token_t::AND:
                        values.push_back(b1 && b2);
                        break;
                    case template_token_t::OR:
                        values.push_back(b1 || b2);
                        break;
                    case template_token_t::XOR:
                        values.push_back(b1 ^ b2);
                        break;
                    default:
                        BLT_WARN("Unexpected token '{}'", std::string(next.token).c_str());
                        return blt::unexpected(template_parser_failure_t::BOOL_TYPE_NOT_FOUND);
                }
            }
            next = consumer.next();
            if (next.type == template_token_t::CURLY_OPEN)
                break;
            consumer.advance();
        }
        if (values.empty())
            BLT_WARN("This is not possible!");
        return values[0];
    }
    
    template_parser_t::ebool template_parser_t::bool_value()
    {
        bool b1;
        auto next = consumer.next();
        bool invert = false;
        // prefixes
        if (next.type == template_token_t::NOT)
        {
            invert = true;
            consumer.advance();
            next = consumer.next();
        }
        if (next.type == template_token_t::PAR_OPEN)
        {
            auto b = bool_statement();
            if (!b)
                return b;
            b1 = b.value();
        } else
        {
            if (consumer.next().type == template_token_t::PAR_OPEN)
            {
                auto b = bool_statement();
                if (!b)
                    return b;
                b1 = b.value();
            } else
            {
                auto b = statement();
                if (!b)
                    return blt::unexpected(b.error());
                b1 = !b.value().empty();
            }
        }
        if (invert)
            b1 = !b1;
        return b1;
    }
    
    template_parser_t::ebool template_parser_t::bool_statement()
    {
        auto next = consumer.next();
        if (next.type == template_token_t::PAR_OPEN)
        {
            consumer.advance();
            auto b = bool_statement();
            if (consumer.consume().type != template_token_t::PAR_CLOSE)
                return blt::unexpected(template_parser_failure_t::BOOL_EXPECTED_PAREN);
            consumer.advance();
            return b;
        }
        return bool_expression();
    }
    
    template_parser_t::estring template_parser_t::string()
    {
        auto next = consumer.consume();
        if (next.type == template_token_t::STRING)
        {
//
//                        return blt::unexpected(template_parser_failure_t::SUBSTITUTION_NOT_FOUND);
            if (consumer.next().type == template_token_t::SEMI || consumer.next().type == template_token_t::ELSE ||
                consumer.next().type == template_token_t::CURLY_CLOSE || consumer.next().type == template_token_t::PAR_CLOSE)
            {
                if (consumer.next().type == template_token_t::SEMI)
                    consumer.advance();
                if (!engine.contains(next.token))
                    return "";
                return engine.get(next.token);
            }
            
            if (consumer.next().type != template_token_t::ADD)
                return blt::unexpected(template_parser_failure_t::STRING_EXPECTED_CONCAT);
            consumer.advance();
            auto str = string();
            if (!str)
                return str;
            auto sub = engine.get(next.token);
            if (!sub)
                return sub;
            return sub.value() + str.value();
        } else
        {
            if (consumer.next().type == template_token_t::SEMI)
            {
                consumer.advance();
                return std::string(next.token);
            }
            auto str = string();
            if (str)
                return std::string(next.token) + str.value();
            else
                return str;
        }
    }
    
    template_parser_t::estring template_parser_t::if_func()
    {
        // IF(
        if (consumer.consume().type != template_token_t::PAR_OPEN)
            return blt::unexpected(template_parser_failure_t::IF_EXPECTED_PAREN);
        // (statement)
        auto bool_eval = bool_statement();
        if (!bool_eval)
            return blt::unexpected(bool_eval.error());
        
        if (consumer.consume().type != template_token_t::CURLY_OPEN)
            return blt::unexpected(template_parser_failure_t::IF_EXPECTED_CURLY);
        auto true_statement = statement();
        if (consumer.consume().type != template_token_t::CURLY_CLOSE)
            return blt::unexpected(template_parser_failure_t::IF_EXPECTED_CURLY);
        
        estring false_statement = blt::unexpected(template_parser_failure_t::UNKNOWN_ERROR);
        bool has_false = false;
        if (consumer.next().type == template_token_t::ELSE)
        {
            consumer.advance();
            if (consumer.consume().type != template_token_t::CURLY_OPEN)
                return blt::unexpected(template_parser_failure_t::IF_EXPECTED_CURLY);
            false_statement = statement();
            if (consumer.consume().type != template_token_t::CURLY_CLOSE)
                return blt::unexpected(template_parser_failure_t::IF_EXPECTED_CURLY);
            
            has_false = true;
        }
        if (bool_eval.value())
            return true_statement;
        else
        {
            if (has_false)
                return false_statement;
            return "";
        }
    }
    
    template_parser_t::estring template_parser_t::function()
    {
        auto str = consumer.consume();
        if (consumer.next().type == template_token_t::SEMI)
            consumer.advance();
        if (str.type != template_token_t::STRING)
            return blt::unexpected(template_parser_failure_t::FUNCTION_EXPECTED_STRING);
        if (str.token == "DISCARD")
            return blt::unexpected(template_parser_failure_t::FUNCTION_DISCARD);
        return blt::unexpected(template_parser_failure_t::FUNCTION_NOT_FOUND);
    }
    
    template_parser_t::estring template_parser_t::statement()
    {
        auto next = consumer.consume();
        if (next.type == template_token_t::STRING || next.type == template_token_t::QUOTE)
        {
            consumer.back();
            auto str = string();
            return str;
        } else if (next.type == template_token_t::FUNCTION)
        {
            return function();
        } else if (next.type == template_token_t::IDENT && consumer.hasNext() && consumer.next().type == template_token_t::CURLY_OPEN)
        {
            consumer.advance();
            auto stmt = statement();
            // should never occur
            if (consumer.hasNext() && consumer.next().type != template_token_t::CURLY_CLOSE)
                return blt::unexpected(template_parser_failure_t::NO_MATCHING_CURLY);
            consumer.advance();
            return stmt;
        } else if (next.type == template_token_t::IF)
        {
            return if_func();
        }
        return blt::unexpected(template_parser_failure_t::UNKNOWN_STATEMENT_ERROR);
    }
    
    
}