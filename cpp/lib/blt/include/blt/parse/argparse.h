/*
 * Created by Brett on 06/08/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_TESTS_ARGPARSE_H
#define BLT_TESTS_ARGPARSE_H

#include <utility>
#include <vector>
#include <string>
#include <initializer_list>
#include <optional>
#include <blt/std/hashmap.h>
#include <blt/std/string.h>
#include <variant>
#include <algorithm>
#include <type_traits>
#include "blt/std/utility.h"

namespace blt
{
    typedef std::variant<std::string, bool, int32_t> arg_data_internal_t;
    typedef std::vector<arg_data_internal_t> arg_data_vec_t;
    typedef std::variant<arg_data_internal_t, arg_data_vec_t> arg_data_t;
    
    enum class arg_action_t
    {
        STORE,
        STORE_CONST,
        STORE_TRUE,
        STORE_FALSE,
        APPEND,
        APPEND_CONST,
        COUNT,
        HELP,
        VERSION,
        EXTEND,
        SUBCOMMAND
    };
    
    class arg_vector_t
    {
            friend class arg_parse;
        
        private:
            std::vector<std::string> flags;
            std::string name;
            
            void validateFlags();
        
        public:
            explicit arg_vector_t(std::vector<std::string> flags): flags(std::move(flags))
            {
                validateFlags();
            }
            
            arg_vector_t(std::initializer_list<std::string> f): flags(f)
            {
                if (flags.size() == 1)
                {
                    if (!blt::string::starts_with(flags[0], '-'))
                    {
                        name = flags[0];
                        flags.clear();
                        return;
                    }
                }
                validateFlags();
            }
            
            explicit arg_vector_t(const char* str);
            
            explicit arg_vector_t(const std::string& str);
            
            [[nodiscard]] inline bool isFlag() const
            {
                return !flags.empty();
            }
            
            [[nodiscard]] inline bool contains(const std::string& str)
            {
                return std::any_of(
                        flags.begin(), flags.end(), [&str](const std::string& flag) -> bool {
                            return flag == str;
                        }
                ) || str == name;
            }
            
            // returns the first flag that starts with '--' otherwise return the first '-' flag
            [[nodiscard]] std::string getFirstFullFlag() const;
            
            [[nodiscard]] std::string getArgName() const;
    };
    
    class arg_nargs_t
    {
            friend class arg_parse;
        
        private:
            static constexpr int UNKNOWN = 0x1;
            static constexpr int ALL = 0x2;
            static constexpr int ALL_REQUIRED = 0x4;
            // 0 means ignore
            int args = 1;
            // 0 indicates args is used
            int flags = 0;
            
            void decode(char c);
        
        public:
            arg_nargs_t() = default;
            
            arg_nargs_t(int args): args(args)
            {}
            
            arg_nargs_t(char c);
            
            arg_nargs_t(std::string s);
            
            arg_nargs_t(const char* s);
            
            [[nodiscard]] bool takesArgs() const
            {
                return args > 0 || flags > 0;
            }
    };
    
    struct arg_properties_t
    {
        public:
            arg_vector_t a_flags;
            arg_action_t a_action = arg_action_t::STORE;
            arg_nargs_t a_nargs = 1;
            std::string a_const{};
            arg_data_internal_t a_default{};
            std::string a_dest{};
            std::string a_help{};
            std::string a_version{};
            std::string a_metavar{};
            bool a_required = true;
            
            arg_properties_t() = delete;
            
            explicit arg_properties_t(arg_vector_t flags): a_flags(std::move(flags))
            {}
            
            explicit arg_properties_t(const std::string& pos_arg): a_flags(pos_arg)
            {}
    };
    
    class arg_builder
    {
        private:
            arg_properties_t properties;
        public:
            explicit arg_builder(const arg_vector_t& flags): properties(flags)
            {}
            
            explicit arg_builder(const std::string& pos_arg): properties(pos_arg)
            {}
            
            arg_builder(const std::initializer_list<std::string>& flags): properties(flags)
            {}
            
            template<typename... string_args>
            explicit arg_builder(string_args... flags): properties(arg_vector_t{flags...})
            {}
            
            inline arg_properties_t build()
            {
                return properties;
            }
            
            inline arg_builder& setAction(arg_action_t action)
            {
                properties.a_action = action;
                return *this;
            }
            
            inline arg_builder& setNArgs(const arg_nargs_t& nargs)
            {
                properties.a_nargs = nargs;
                return *this;
            }
            
            inline arg_builder& setConst(const std::string& a_const)
            {
                properties.a_const = a_const;
                return *this;
            }
            
            inline arg_builder& setDefault(const arg_data_internal_t& def)
            {
                properties.a_default = def;
                return *this;
            }
            
            inline arg_builder& setDest(const std::string& dest)
            {
                properties.a_dest = dest;
                return *this;
            }
            
            inline arg_builder& setHelp(const std::string& help)
            {
                properties.a_help = help;
                return *this;
            }
            
            inline arg_builder& setVersion(const std::string& version)
            {
                properties.a_version = version;
                return *this;
            }
            
            inline arg_builder& setMetavar(const std::string& metavar)
            {
                properties.a_metavar = metavar;
                return *this;
            }
            
            inline arg_builder& setRequired()
            {
                properties.a_required = true;
                return *this;
            }
        
    };
    
    class arg_tokenizer
    {
        private:
            std::vector<std::string> args;
            size_t currentIndex = 0;
        public:
            explicit arg_tokenizer(std::vector<std::string> args): args(std::move(args))
            {}
            
            // returns the current arg
            inline const std::string& get()
            {
                return args[currentIndex];
            }
            
            // returns if we have next arg to process
            inline bool hasNext()
            {
                return currentIndex + 1 < args.size();
            }
            
            inline bool hasCurrent()
            {
                return currentIndex < args.size();
            }
            
            // returns true if the current arg is a flag
            inline bool isFlag()
            {
                return blt::string::starts_with(args[currentIndex], '-');
            }
            
            // returns true if we have next and the next arg is a flag
            inline bool isNextFlag()
            {
                return hasNext() && blt::string::starts_with(args[currentIndex + 1], '-');
            }
            
            // advances to the next flag
            inline size_t advance()
            {
                return currentIndex++;
            }
    };
    
    class arg_parse
    {
        public:
            
            template<typename T>
            static inline bool holds_alternative(const arg_data_t& v)
            {
                if constexpr (std::is_same_v<T, arg_data_vec_t>)
                    return std::holds_alternative<T>(v);
                else
                    return std::holds_alternative<arg_data_internal_t>(v) && std::holds_alternative<T>(std::get<arg_data_internal_t>(v));
            }
            
            
            template<typename T>
            static inline T& get(arg_data_t& v)
            {
                if constexpr (std::is_same_v<T, arg_data_vec_t>)
                    return std::get<arg_data_vec_t>(v);
                else
                    return std::get<T>(std::get<arg_data_internal_t>(v));
            }
            
            /**
             * Attempt to cast the variant stored in the arg results to the requested type
             * if user is requesting an int, but holds a string, we are going to make the assumption the data can be converted
             * it is up to the user to deal with the variant if they do not want this behaviour!
             * @tparam T type to convert to
             * @param v
             * @return
             */
            template<typename T>
            static inline T get_cast(arg_data_t& v)
            {
                if (std::holds_alternative<arg_data_vec_t>(v))
                {
                    if constexpr (std::is_same_v<T, arg_data_vec_t>)
                        return std::get<arg_data_vec_t>(v);
                    else
                        throw std::runtime_error("Cannot request singular data from stored vector type");
                }
                auto t = std::get<arg_data_internal_t>(v);
                // user is requesting an int, but holds a string, we are going to make the assumption the data can be converted
                // it is up to the user to deal with the variant if they do not want this behaviour!
                if constexpr (!std::is_arithmetic_v<T>)
                    return std::get<T>(t);
                // ugly!
                if (std::holds_alternative<int32_t>(t))
                    return static_cast<T>(std::get<int32_t>(t));
                if (std::holds_alternative<bool>(t))
                    return static_cast<T>(std::get<bool>(t));
                
                auto s = std::get<std::string>(t);
                
                if (s.empty())
                    throw std::runtime_error("Key does not have value!");
                
                if constexpr (std::is_floating_point_v<T>)
                    return static_cast<T>(std::stod(s));
                else if constexpr (std::is_signed_v<T>)
                    return static_cast<T>(std::stoll(s));
                else if constexpr (std::is_unsigned_v<T>)
                    return static_cast<T>(std::stoull(s));
                else
                    return static_cast<T>(s);
            }
            
            struct arg_results
            {
                    friend arg_parse;
                private:
                    // stores dest value not the flag/name!
                    hashset_t<std::string> found_args;
                    std::vector<std::string> unrecognized_args;
                public:
                    std::string program_name;
                    hashmap_t<std::string, arg_data_t> data;
                    
                    inline arg_data_t& operator[](const std::string& key)
                    {
                        return data[key];
                    }
                    
                    template<typename T>
                    inline T get(const std::string& key)
                    {
                        hashmap_t<std::string, arg_data_t>::iterator val;
                        if (blt::string::starts_with(key, "--"))
                            val = data.find(key.substr(2));
                        else if (blt::string::starts_with(key, '-'))
                            val = data.find(key.substr(1));
                        else
                            val = data.find(key);
                        if constexpr (std::is_same_v<T, std::string>)
                            return blt::arg_parse::get<T>(val->second);
                        else
                            return blt::arg_parse::get_cast<T>(val->second);
                    }
                    
                    inline auto begin()
                    {
                        return data.begin();
                    }
                    
                    inline auto end()
                    {
                        return data.end();
                    }
                    
                    inline bool contains(const std::string& key)
                    {
                        if (blt::string::starts_with(key, "--"))
                            return data.find(key.substr(2)) != data.end();
                        if (blt::string::starts_with(key, '-'))
                            return data.find(key.substr(1)) != data.end();
                        return data.find(key) != data.end();
                    }
            };
        
        private:
            struct
            {
                    friend arg_parse;
                private:
                    std::vector<arg_properties_t*> arg_properties_storage;
                    size_t max_line_length = 80;
                    // TODO: grouping like git's help
                    // pre/postfix applied to the help message
                    std::string prefix;
                    std::string postfix;
                public:
                    std::vector<arg_properties_t*> name_associations;
                    hashmap_t<std::string, arg_properties_t*> flag_associations;
            } user_args;
            
            arg_results loaded_args;
            
            bool subcommand_found = false;
            bool use_full_name = false;
            std::string subcommand_name;
        private:
            static std::string getMetavar(const arg_properties_t* const& arg);
            
            static std::string getFlagHelp(const arg_properties_t* const& arg);
            
            static bool takesArgs(const arg_properties_t* const& arg);
            
            /**
             * prints out a new line if current line length is greater than max line length, using spacing to generate the next line's
             * beginning spaces.
             */
            void checkAndPrintFormattingLine(size_t& current_line_length, size_t spacing) const;
            
            // expects that the current flag has already been consumed (advanced past), leaves tokenizer in a state where the next element is 'current'
            bool consumeArguments(
                    arg_tokenizer& tokenizer, const std::string& flag, const arg_properties_t& properties, std::vector<arg_data_internal_t>& v_out
                                 ) const;
            
            void handlePositionalArgument(arg_tokenizer& tokenizer, size_t& last_pos);
            
            void handleFlagArgument(arg_tokenizer& tokenizer);
            
            void processFlag(arg_tokenizer& tokenizer, const std::string& flag);
            
            void handleFlag(arg_tokenizer& tokenizer, const std::string& flag, const arg_properties_t* properties);
            
            [[nodiscard]] std::string getProgramName() const
            {
                return use_full_name ? loaded_args.program_name : filename(loaded_args.program_name);
            }
        
        public:
            arg_parse(const std::string& helpMessage = "show this help menu and exit")
            {
                addArgument(arg_builder({"--help", "-h"}).setAction(arg_action_t::HELP).setHelp(helpMessage).build());
            };
            
            void addArgument(const arg_properties_t& args);
            
            arg_results parse_args(int argc, const char** argv);
            
            arg_results parse_args(const std::vector<std::string>& args);
            
            void printUsage() const;
            
            void printHelp() const;
            
            /**
             * Sets the parser to use the full file path when printing usage messages
             */
            inline void useFullPath()
            {
                use_full_name = true;
            }
            
            inline void setHelpPrefix(const std::string& str)
            {
                user_args.prefix = str;
            }
            
            inline void setHelpPostfix(const std::string& str)
            {
                user_args.postfix = str;
            }
            
            inline void setMaxLineLength(size_t size)
            {
                user_args.max_line_length = size;
            }
            
            inline void setHelpExtras(std::string str)
            {
                subcommand_name = std::move(str);
            }
            
            static std::string filename(const std::string& path);
            
            ~arg_parse()
            {
                for (auto* p : user_args.arg_properties_storage)
                    delete p;
            }
    };
    
    std::string to_string(const blt::arg_data_t& v);
    
    std::string to_string(const blt::arg_data_internal_t& v);
    
    std::string to_string(const blt::arg_data_vec_t& v);
    
}

#endif //BLT_TESTS_ARGPARSE_H
