/*
 * Created by Brett on 06/08/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */
#include <blt/parse/argparse.h>
#include <iostream>
#include <blt/std/string.h>
#include <blt/iterator/iterator.h>
#include <algorithm>
#include "blt/std/utility.h"
#include <blt/std/variant.h>

namespace blt
{
    
    void arg_nargs_t::decode(char c)
    {
        if (c == '?')
            flags = UNKNOWN;
        else if (c == '+')
            flags = ALL_REQUIRED;
        else if (c == '*')
            flags = ALL;
        else
            flags = 0;
    }
    
    arg_nargs_t::arg_nargs_t(char c)
    {
        decode(c);
    }
    
    arg_nargs_t::arg_nargs_t(std::string s)
    {
        decode(s[0]);
    }
    
    arg_nargs_t::arg_nargs_t(const char* s)
    {
        decode(*s);
    }
    
    class invalid_argument_exception : public std::runtime_error
    {
        public:
            invalid_argument_exception(const std::string& str): std::runtime_error(str)
            {}
    };
    
    void arg_vector_t::validateFlags()
    {
        for (const auto& flag : flags)
            if (!blt::string::starts_with(flag, '-'))
                throw invalid_argument_exception("Flag '" + flag + "' must start with - or --");
    }
    
    arg_vector_t::arg_vector_t(const char* str)
    {
        std::string as_string(str);
        if (blt::string::starts_with(str, '-'))
            flags.emplace_back(as_string);
        else
            name = as_string;
    }
    
    arg_vector_t::arg_vector_t(const std::string& str)
    {
        if (blt::string::starts_with(str, '-'))
            flags.emplace_back(str);
        else
            name = str;
    }
    
    std::string arg_vector_t::getFirstFullFlag() const
    {
        // assign flag so it always exists, will be first '-' flag if we fail to find a '--' flag
        std::string flag = flags[0];
        // search for first '--' flag
        for (const auto& f : flags)
            if (blt::string::starts_with(f, "--"))
            {
                flag = f;
                break;
            }
        return flag;
    }
    
    std::string arg_vector_t::getArgName() const
    {
        if (name.empty())
            return getFirstFullFlag();
        return name;
    }
    
    std::string to_string(const arg_data_t& v)
    {
        return std::visit(blt::lambda_visitor{
                [](const arg_data_internal_t& v) {
                    return to_string(v);
                },
                [](const arg_data_vec_t& v) {
                    return to_string(v);
                }
        }, v);
    }
    
    std::string to_string(const arg_data_internal_t& v)
    {
        return std::visit(blt::lambda_visitor{
                [](const std::string& str) {
                    return str;
                },
                [](bool b) {
                    return std::string(b ? "True" : "False");
                },
                [](int32_t i) {
                    return std::to_string(i);
                },
        }, v);
    }
    
    std::string to_string(const blt::arg_data_vec_t& vec)
    {
        std::string result = "[";
        
        for (const auto& [index, value] : blt::enumerate(vec))
        {
            result += to_string(value);
            if (index != vec.size() - 1)
                result += ", ";
        }
        result += "]";
        
        return result;
    }
    
    std::string arg_parse::filename(const std::string& path)
    {
        auto paths = blt::string::split(path, "/");
        auto final = paths[paths.size() - 1];
        if (final == "/")
            return paths[paths.size() - 2];
        return final;
    }
    
    void arg_parse::addArgument(const arg_properties_t& args)
    {
        auto properties = new arg_properties_t(args);
        
        // determine where to store the arg when parsing
        if (properties->a_dest.empty())
        {
            if (properties->a_flags.isFlag())
                properties->a_dest = properties->a_flags.getFirstFullFlag();
            else
                properties->a_dest = properties->a_flags.name;
        }
        
        if (blt::string::starts_with(properties->a_dest, "--"))
            properties->a_dest = properties->a_dest.substr(2);
        else if (blt::string::starts_with(properties->a_dest, '-'))
            properties->a_dest = properties->a_dest.substr(1);
        
        // associate flags with their properties
        for (const auto& flag : properties->a_flags.flags)
            user_args.flag_associations[flag] = properties;
        
        // positional args uses index (vector) to store the properties
        if (!properties->a_flags.isFlag())
            user_args.name_associations.push_back(properties);
        
        user_args.arg_properties_storage.push_back(properties);
    }
    
    bool arg_parse::consumeArguments(
            arg_tokenizer& tokenizer, const std::string& flag, const arg_properties_t& properties, std::vector<arg_data_internal_t>& v_out
                                    ) const
    {
        switch (properties.a_nargs.flags)
        {
            case 0:
                for (int i = 0; i < properties.a_nargs.args; i++)
                {
                    // if we don't have another arg to consume we have a problem!
                    if (!tokenizer.hasCurrent())
                    {
                        // TODO: S
                        printUsage();
                        std::cout << getProgramName() << ": error: flag '" << flag << "' expected " << properties.a_nargs.args
                                  << " argument(s) got " << i << " argument(s) instead!\n";
                        return false;
                    }
                    // if we do have one, but it is a flag then we also have a problem!
                    if (tokenizer.isFlag())
                    {
                        printUsage();
                        std::cout << getProgramName() << ": error: flag '" << flag << "' expected " << properties.a_nargs.args
                                  << " argument(s) but found '" << tokenizer.get() << "' instead!\n";
                        //BLT_WARN("Expected {:d} arguments, found flag instead!", properties.a_nargs.args);
                        return false;
                    }
                    // get the value and advance
                    v_out.emplace_back(tokenizer.get());
                    tokenizer.advance();
                }
                return true;
            case arg_nargs_t::UNKNOWN:
                // no arg next
                if (!tokenizer.hasCurrent() || tokenizer.isFlag())
                {
                    // python's default is to store const if around otherwise store default
                    if (!properties.a_const.empty())
                        v_out.emplace_back(properties.a_const);
                    else // this should no longer be required with the changes to how defaults are handled
                        v_out.emplace_back(properties.a_default);
                    return true;
                }
                v_out.emplace_back(tokenizer.get());
                tokenizer.advance();
                return true;
            case arg_nargs_t::ALL:
                while (tokenizer.hasCurrent() && !tokenizer.isFlag())
                {
                    v_out.emplace_back(tokenizer.get());
                    tokenizer.advance();
                }
                return true;
            case arg_nargs_t::ALL_REQUIRED:
                if (tokenizer.hasCurrent() && tokenizer.isFlag())
                {
                    printUsage();
                    std::cout << getProgramName() << ": at least one argument is required for '" << flag << "'\n";
                    return false;
                }
                while (tokenizer.hasCurrent() && !tokenizer.isFlag())
                {
                    v_out.emplace_back(tokenizer.get());
                    tokenizer.advance();
                }
                return true;
        }
        return false;
    }
    
    void arg_parse::handlePositionalArgument(arg_tokenizer& tokenizer, size_t& last_pos)
    {
        auto index = last_pos++;
        if (index >= user_args.name_associations.size())
            loaded_args.unrecognized_args.push_back(tokenizer.get());
        else
        {
            bool found = false;
            for (const auto& pos_properties : user_args.name_associations)
            {
                const auto& flag = tokenizer.get();
                if (pos_properties->a_flags.contains(flag))
                {
                    tokenizer.advance();
                    handleFlag(tokenizer, flag, pos_properties);
                    found = true;
                    break;
                }
            }
            if (!found)
            {
                const auto& arg_property = user_args.name_associations[index];
                loaded_args.data[arg_property->a_dest] = tokenizer.get();
                loaded_args.found_args.insert(arg_property->a_dest);
            }
        }
        tokenizer.advance();
    }
    
    void arg_parse::handleFlagArgument(arg_tokenizer& tokenizer)
    {
        auto flag = tokenizer.get();
        tokenizer.advance();
        
        // token is a flag, figure out how to handle it
        if (blt::string::starts_with(flag, "--"))
            processFlag(tokenizer, flag);
        else
        {
            // handle special args like -vvv
            if (!blt::string::starts_with(flag, '-'))
                std::cerr << "Flag processed but does not start with '-' (this is a serious error!)\n";
            // make sure the flag only contains the same character
            auto type = flag[1];
            for (char c : flag.substr(1))
            {
                if (c != type)
                {
                    printUsage();
                    std::cout << "found different characters in flag '" << flag.c_str() << "' expected '" << type << "' but found '" << c << "'\n";
                    return;
                }
            }
            // size without -
            auto len = flag.size() - 1;
            // get flag type
            std::string str = "- ";
            str[1] = flag[1];
            for (size_t i = 0; i < len; i++)
                processFlag(tokenizer, str);
        }
    }
    
    void arg_parse::processFlag(arg_tokenizer& tokenizer, const std::string& flag)
    {
        auto flag_itr = user_args.flag_associations.find(flag);
        if (flag_itr == user_args.flag_associations.end())
        {
            loaded_args.unrecognized_args.push_back(flag);
            return;
        }
        
        const auto* const properties = user_args.flag_associations.at(flag);
        
        handleFlag(tokenizer, flag, properties);
    }
    
    void arg_parse::handleFlag(arg_tokenizer& tokenizer, const std::string& flag, const arg_properties_t* properties)
    {
        if (properties->a_dest.empty())
        {
            loaded_args.unrecognized_args.push_back(flag);
            return;
        }
        auto dest = properties->a_dest;
        
        loaded_args.found_args.insert(dest);
        
        switch (properties->a_action)
        {
            case arg_action_t::HELP:
                printUsage();
                printHelp();
                break;
            case arg_action_t::STORE:
            {
                arg_data_t& data = loaded_args.data[dest];
                arg_data_vec_t v;
                if (!consumeArguments(tokenizer, flag, *properties, v))
                {
                    printHelp();
                    return;
                }
                if (v.empty())
                    data = "";
                else if (v.size() == 1)
                    data = v[0];
                else
                    data = v;
                break;
            }
            case arg_action_t::STORE_CONST:
                loaded_args.data[dest] = properties->a_const;
                break;
            case arg_action_t::STORE_FALSE:
                loaded_args.data[dest] = false;
                break;
            case arg_action_t::STORE_TRUE:
                loaded_args.data[dest] = true;
                break;
            case arg_action_t::COUNT:
            {
                auto& data = loaded_args.data[dest];
                if (!holds_alternative<int32_t>(data))
                    data = 0;
                data = get<int32_t>(data) + 1;
                break;
            }
            case arg_action_t::SUBCOMMAND:
            {
                loaded_args[flag] = true;
                subcommand_found = true;
                break;
            }
            case arg_action_t::EXTEND:
            {
                
                break;
            }
            case arg_action_t::VERSION:
            {
                auto file = getProgramName();
                std::cout << file.c_str() << " " << properties->a_version << "\n";
                break;
            }
            case arg_action_t::APPEND_CONST:
            {
                auto& data = loaded_args.data[dest];
                if (!std::holds_alternative<arg_data_vec_t>(data))
                {
                    data = arg_data_vec_t();
                }
                auto& l = get<arg_data_vec_t>(data);
                l.emplace_back(properties->a_const);
                break;
            }
            case arg_action_t::APPEND:
            {
                auto& data = loaded_args.data[dest];
                if (!holds_alternative<arg_data_vec_t>(data))
                    data = arg_data_vec_t();
                auto& l = get<arg_data_vec_t>(data);
                consumeArguments(tokenizer, flag, *properties, l);
                break;
            }
        }
    }
    
    arg_parse::arg_results arg_parse::parse_args(int argc, const char** argv)
    {
        std::vector<std::string> args;
        args.reserve(argc);
        for (int i = 0; i < argc; i++)
            args.emplace_back(argv[i]);
        return parse_args(args);
    }
    
    arg_parse::arg_results arg_parse::parse_args(const std::vector<std::string>& args)
    {
        arg_tokenizer tokenizer(args);
        loaded_args.program_name = tokenizer.get();
        tokenizer.advance();
        
        if (!subcommand_name.empty())
        {
            // advance the tokenizer to post grouped args allowing for flags
            while (tokenizer.hasCurrent() && tokenizer.get() != subcommand_name)
                tokenizer.advance();
            tokenizer.advance();
        }
        
        size_t last_positional = 0;
        while (tokenizer.hasCurrent())
        {
            // if we find an arg which disables help (basically a grouping flag) then we should stop processing args
            if (subcommand_found)
                break;
            
            if (tokenizer.isFlag())
                handleFlagArgument(tokenizer);
            else
                handlePositionalArgument(tokenizer, last_positional);
        }
        
        // load defaults for args which were not found
        for (const auto* arg : user_args.arg_properties_storage)
        {
            if (!to_string(arg->a_default).empty() && !loaded_args.contains(arg->a_dest))
                loaded_args.data[arg->a_dest] = arg->a_default;
        }
        
        if (!loaded_args.unrecognized_args.empty())
        {
            // otherwise construct a string detailing the unrecognized args
            std::string unrec;
            for (const auto& r : loaded_args.unrecognized_args)
            {
                unrec += '\'';
                unrec += r;
                unrec += '\'';
                unrec += ' ';
            }
            // remove the last space caused by the for loop
            unrec = unrec.substr(0, unrec.size() - 1);
            printUsage();
            std::cout << getProgramName() << ": error: unrecognized args: " << unrec << "\n";
            std::exit(0);
        }
        
        for (const auto& named_arg : user_args.name_associations)
        {
            if (takesArgs(named_arg) && !loaded_args.contains(named_arg->a_dest))
            {
                printUsage();
                std::cout << "positional argument '" << named_arg->a_flags.name << "' expected " << named_arg->a_nargs.args << " argument"
                          << (named_arg->a_nargs.args > 1 ? "s!" : "!") << std::endl;
                std::exit(0);
            }
        }
        
        // if there was no problems processing then return the loaded args
        return loaded_args;
    }
    
    bool arg_parse::takesArgs(const arg_properties_t* const& arg)
    {
        switch (arg->a_action)
        {
            case arg_action_t::STORE_CONST:
            case arg_action_t::STORE_TRUE:
            case arg_action_t::STORE_FALSE:
            case arg_action_t::APPEND_CONST:
            case arg_action_t::COUNT:
            case arg_action_t::HELP:
            case arg_action_t::VERSION:
            case arg_action_t::SUBCOMMAND:
                return false;
            case arg_action_t::STORE:
            case arg_action_t::APPEND:
            case arg_action_t::EXTEND:
                return arg->a_nargs.takesArgs();
        }
        return false;
    }
    
    void arg_parse::printHelp() const
    {
        if (subcommand_found)
            return;
        if (!user_args.prefix.empty())
        {
            std::cout << "\n";
            std::cout << user_args.prefix;
        }
        std::cout << "\npositional arguments:\n";
        // spaces per tab
        const size_t tab_size = 8;
        size_t max_length = 0;
        // search for longest pos arg length
        for (const auto& arg : user_args.arg_properties_storage)
        {
            if (!arg->a_flags.isFlag())
                max_length = std::max(arg->a_flags.name.size(), max_length);
            else
            {
                auto tmp = getFlagHelp(arg);
                max_length = std::max(tmp.size(), max_length);
            }
        }
        for (const auto& arg : user_args.arg_properties_storage)
        {
            if (!arg->a_flags.isFlag())
            {
                const auto& name = arg->a_flags.name;
                std::cout << name;
                auto size = std::max(static_cast<int64_t>(max_length) - static_cast<int64_t>(name.size()), static_cast<int64_t>(0));
                size += tab_size;
                for (int64_t i = 0; i < size; i++)
                    std::cout << " ";
                std::cout << arg->a_help << "\n";
            }
        }
        std::cout << "\noptions:\n";
        for (const auto& arg : user_args.arg_properties_storage)
        {
            if (arg->a_flags.isFlag())
            {
                const auto& name = getFlagHelp(arg);
                std::cout << name;
                auto size = std::max(static_cast<int64_t>(max_length) - static_cast<int64_t>(name.size()), static_cast<int64_t>(0));
                size += tab_size;
                for (int64_t i = 0; i < size; i++)
                    std::cout << " ";
                std::cout << arg->a_help << "\n";
            }
        }
        if (!user_args.postfix.empty())
        {
            std::cout << user_args.postfix;
            std::cout << "\n";
        }
        
        std::exit(0);
    }
    
    void arg_parse::printUsage() const
    {
        if (subcommand_found)
            return;
        std::string usage = "Usage: " + getProgramName() + " " + subcommand_name + " ";
        std::cout << usage;
        size_t current_line_length = 0;
        
        for (const auto& arg : user_args.arg_properties_storage)
        {
            auto meta = getMetavar(arg);
            
            std::string str = "[";
            if (arg->a_flags.isFlag())
            {
                str += arg->a_flags.getFirstFullFlag();
                if (takesArgs(arg))
                {
                    str += " ";
                    str += meta;
                    str += "";
                }
                str += "]";
                str += ' ';
            } else
            {
                str += "<";
                str += arg->a_flags.name;
                str += ">] ";
            }
            
            current_line_length += str.size();
            checkAndPrintFormattingLine(current_line_length, usage.size());
            
            std::cout << str;
        }
        std::cout << "\n";
    }
    
    void arg_parse::checkAndPrintFormattingLine(size_t& current_line_length, size_t spacing) const
    {
        if (current_line_length > user_args.max_line_length)
        {
            std::cout << "\n";
            for (size_t i = 0; i < spacing; i++)
                std::cout << " ";
            current_line_length = 0;
        }
    }
    
    std::string arg_parse::getMetavar(const arg_properties_t* const& arg)
    {
        auto meta = arg->a_metavar;
        
        if (meta.empty())
            meta = blt::string::toUpperCase(arg->a_dest);
        
        return meta;
    }
    
    std::string arg_parse::getFlagHelp(const arg_properties_t* const& arg)
    {
        auto meta = getMetavar(arg);
        // bad that we have to create this twice!
        std::string tmp;
        for (const auto& flag : arg->a_flags.flags)
        {
            tmp += flag;
            if (takesArgs(arg))
            {
                tmp += ' ';
                tmp += meta;
            }
            tmp += ", ";
        }
        tmp = tmp.substr(0, tmp.size() - 2);
        return tmp;
    }
}