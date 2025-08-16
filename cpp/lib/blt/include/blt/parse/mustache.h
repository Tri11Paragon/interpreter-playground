//
// Created by brett on 21/08/23.
//

#ifndef BLT_MUSTACHE_H
#define BLT_MUSTACHE_H

#include <string>
#include <exception>
#include <blt/std/hashmap.h>

namespace blt
{
    
    class mustache_syntax_error : public std::runtime_error
    {
        public:
            explicit mustache_syntax_error(): std::runtime_error("mustache syntax is invalid!")
            {}
            
            explicit mustache_syntax_error(const std::string& str): std::runtime_error(str)
            {}
    };
    
    class mustache_lexer
    {
        private:
            std::string str;
            size_t index = 0;
        public:
            explicit mustache_lexer(std::string str): str(std::move(str))
            {}
            
            inline bool hasNext()
            {
                if (index >= str.size())
                    return false;
                return true;
            }
            
            inline char peek()
            {
                return str[index];
            }
            
            inline bool hasTemplatePrefix()
            {
                if (index + 1 >= str.size())
                    return false;
                return str[index] == '{' && str[index + 1] == '{';
            }
            
            inline bool hasTemplateSuffix()
            {
                if (index + 1 >= str.size())
                    return false;
                return str[index] == '}' && str[index + 1] == '}';
            }
            
            inline void consumeTemplatePrefix()
            {
                index += 2;
            }
            
            inline void consumeTemplateSuffix()
            {
                index += 2;
            }
            
            inline std::string consumeToken()
            {
                std::string token;
                while (!hasTemplateSuffix())
                {
                    if (!hasNext())
                        throw mustache_syntax_error("Error processing token. Mustache template incomplete.");
                    token += consume();
                }
                return token;
            }
            
            inline char consume()
            {
                return str[index++];
            }
    };
    
    class mustache
    {
        private:
            mustache_lexer lexer;
            
            std::string assemble(){
                std::string buffer;
                
                while (lexer.hasNext()){
                    if (lexer.hasTemplatePrefix()){
                        lexer.consumeTemplatePrefix();
                        if (!lexer.hasNext())
                            throw mustache_syntax_error("template incomplete, found '{{' missing '}}'");
                        auto c = lexer.peek();
                        switch (c) {
                            case '%':
                                break;
                            case '$':
                                break;
                            case '#':
                                break;
                            case '@':
                                break;
                            case '/':
                                break;
                            default:
                                break;
                        }
                        
                    } else
                        buffer += lexer.consume();
                }
                
                return buffer;
            }
            
        public:
            explicit mustache(std::string str): lexer(std::move(str))
            {}
            
            static std::string compile(std::string input) {
                mustache compiler(std::move(input));
                return compiler.assemble();
            }
    };
    
}

#endif //BLT_MUSTACHE_H
