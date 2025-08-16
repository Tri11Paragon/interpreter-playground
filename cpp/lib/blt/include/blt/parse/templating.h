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

#ifndef BLT_TEMPLATING_H
#define BLT_TEMPLATING_H

#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <utility>
#include <blt/std/hashmap.h>
#include <blt/std/types.h>
#include <blt/std/expected.h>
#include <blt/logging/logging.h>
#include <variant>

namespace blt
{
    
    class template_engine_t;
    
    template<typename Storage, typename Consumable>
    class template_consumer_base_t
    {
        public:
            explicit template_consumer_base_t(Storage storage): storage(std::move(storage))
            {}
            
            [[nodiscard]] Consumable next(size_t offset = 0) const
            {
                return storage[current_index + offset];
            }
            
            void advance(size_t offset = 1)
            {
                current_index += offset;
            }
            
            [[nodiscard]] bool hasNext(size_t offset = 1) const
            {
                return (current_index + (offset - 1)) < storage.size();
            }
            
            [[nodiscard]] Consumable consume()
            {
                Consumable c = next();
                advance();
                return c;
            }
            
            [[nodiscard]] size_t getCurrentIndex() const
            {
                return current_index;
            }
            
            [[nodiscard]] size_t getPreviousIndex() const
            {
                return current_index - 1;
            }
        
        protected:
            size_t current_index = 0;
            Storage storage;
    };
    
    enum class template_token_t
    {
        //STRING,         // A string of characters not $ { or }
        IDENT,          // $
        ADD,            // +
        CURLY_OPEN,     // {
        CURLY_CLOSE,    // }
        IF,             // IF
        ELSE,           // ELSE
        PAR_OPEN,       // (
        PAR_CLOSE,      // )
        OR,             // ||
        AND,            // &&
        XOR,            // ^
        NOT,            // !
        QUOTE,          // "
        SEMI,           // ;
        COMMA,          // ,
        PERIOD,         // .
        FUNCTION,       // ~
        STRING          // variable name
    };
    
    namespace detail
    {
        inline const blt::hashmap_t<std::string_view, template_token_t> identifiers = {
                {"IF",   template_token_t::IF},
                {"ELSE", template_token_t::ELSE}
        };
    }
    
    inline std::string template_token_to_string(template_token_t token)
    {
        switch (token)
        {
            case template_token_t::IDENT:
                return "[Template Identifier]";
            case template_token_t::CURLY_OPEN:
                return "[Curly Open]";
            case template_token_t::CURLY_CLOSE:
                return "[Curly Close]";
            case template_token_t::IF:
                return "[IF]";
            case template_token_t::ELSE:
                return "[ELSE]";
            case template_token_t::PAR_OPEN:
                return "[Par Open]";
            case template_token_t::PAR_CLOSE:
                return "[Par Close]";
            case template_token_t::OR:
                return "[OR]";
            case template_token_t::AND:
                return "[AND]";
            case template_token_t::XOR:
                return "[XOR]";
            case template_token_t::NOT:
                return "[NOT]";
            case template_token_t::QUOTE:
                return "[QUOTE]";
            case template_token_t::FUNCTION:
                return "[FUNC]";
            case template_token_t::STRING:
                return "[STR]";
            case template_token_t::SEMI:
                return "[SEMI]";
            case template_token_t::COMMA:
                return "[COMMA]";
            case template_token_t::PERIOD:
                return "[PERIOD]";
            case template_token_t::ADD:
                return "[ADD]";
        }
    }
    
    enum class template_tokenizer_failure_t
    {
        MISMATCHED_CURLY,
        MISMATCHED_PAREN,
        MISMATCHED_QUOTE,
    };
    
    enum class template_parser_failure_t
    {
        SUBSTITUTION_NOT_FOUND,
        TOKENIZER_FAILURE,
        NO_MATCHING_CURLY,
        MISSING_IDENT_BRACES,
        FUNCTION_EXPECTED_STRING,
        FUNCTION_NOT_FOUND,
        FUNCTION_DISCARD,
        STRING_EXPECTED_CONCAT,
        IF_EXPECTED_PAREN,
        IF_EXPECTED_CURLY,
        BOOL_EXPECTED_PAREN,
        BOOL_TYPE_NOT_FOUND,
        UNKNOWN_STATEMENT_ERROR,
        UNKNOWN_ERROR
    };
    
    struct template_token_data_t
    {
        template_token_t type;
        size_t level;
        std::string_view token;
        size_t paren_level = 0;
        
        template_token_data_t(template_token_t type, size_t level, const std::string_view& token): type(type), level(level), token(token)
        {}
        
        template_token_data_t(template_token_t type, size_t level, const std::string_view& token, size_t parenLevel):
                type(type), level(level), token(token), paren_level(parenLevel)
        {}
    };
    
    class template_char_consumer_t : public template_consumer_base_t<std::string_view, char>
    {
        public:
            explicit template_char_consumer_t(std::string_view statement): template_consumer_base_t(statement)
            {}
            
            [[nodiscard]] std::string_view from(size_t begin, size_t end)
            {
                return std::string_view{&storage[begin], end - begin};
            }
    };
    
    class template_token_consumer_t : public template_consumer_base_t<std::vector<template_token_data_t>, template_token_data_t>
    {
        public:
            explicit template_token_consumer_t(const std::vector<template_token_data_t>& statement, std::string_view raw_string):
                    template_consumer_base_t(statement), raw_string(raw_string)
            {}
            
            void set_marker()
            {
                // when setting the marker, we need to go from the last closing brace
                auto index = storage.begin() + getCurrentIndex();
                while (index->type != template_token_t::CURLY_CLOSE)
                    index--;
                last_read_index = ((&index->token.front() + index->token.size()) - &raw_string[last_read_index]);
            }
            
            std::string_view from_last()
            {
                if (!hasNext()) {
                    auto size = raw_string.size() - last_read_index;
                    if (size > 0)
                        return std::string_view(&raw_string[last_read_index], size);
                    return "";
                }
                auto token = storage[getCurrentIndex()];
                auto len = ((&token.token.back()) - &raw_string[last_read_index]);
                auto str = std::string_view(&raw_string[last_read_index], len);
                return str;
            }
            
            void back()
            {
                current_index--;
            }
            
            auto prev()
            {
                if (current_index == 0)
                    throw std::runtime_error("Current Index cannot be zero!");
                return storage[current_index - 1];
            }
        
        private:
            std::string_view raw_string;
            size_t last_read_index = 0;
    };
    
    class template_engine_t
    {
        public:
            inline std::string& operator[](const std::string& key)
            {
                return substitutions[key];
            }
            
            inline std::string& operator[](std::string_view key)
            {
                return substitutions[key];
            }
            
            inline template_engine_t& set(std::string_view key, std::string_view replacement)
            {
                substitutions[key] = replacement;
                return *this;
            }
            
            inline bool contains(std::string_view token)
            {
                return substitutions.contains(token);
            }
            
            inline auto get(std::string_view token)
            {
                return internal_evaluate(substitutions[token], true);
            }
            
            static blt::expected<std::vector<template_token_data_t>, template_tokenizer_failure_t> process_string(std::string_view str);
            
            blt::expected<std::string, template_parser_failure_t> evaluate(std::string_view str)
            {
                auto eval = internal_evaluate(str, false);
                if (eval.has_value())
                    return eval;
                else
                    if (eval.error() == template_parser_failure_t::FUNCTION_DISCARD)
                        return "";
                    else
                        return eval;
            }
        
        private:
            blt::expected<std::string, template_parser_failure_t> internal_evaluate(std::string_view str, bool discard);
            
            blt::hashmap_t<std::string, std::string> substitutions;
    };
    
    class template_parser_t
    {
        public:
            using estring = blt::expected<std::string, template_parser_failure_t>;
            using ebool = blt::expected<bool, template_parser_failure_t>;
            
            template_parser_t(template_engine_t& engine, template_token_consumer_t& consumer):
                    engine(engine), consumer(consumer)
            {}
            
            estring parse()
            {
                auto next = consumer.consume();
                if (next.type == template_token_t::IDENT && consumer.next().type == template_token_t::CURLY_OPEN)
                {
                    consumer.advance();
                    auto str = statement();
                    consumer.advance();
                    return str;
                }
                return blt::unexpected(template_parser_failure_t::MISSING_IDENT_BRACES);
            }
        
        private:
            estring statement();
            
            estring function();
            
            estring if_func();
            
            estring string();
            
            ebool bool_statement();
            
            ebool bool_value();
            
            ebool bool_expression();
            
            template_engine_t& engine;
            template_token_consumer_t& consumer;
    };
    
}

#endif //BLT_TEMPLATING_H
