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

#ifndef BLT_PARSE_ARGPARSE_V2_H
#define BLT_PARSE_ARGPARSE_V2_H

#include <complex>
#include <blt/std/types.h>
#include <blt/std/hashmap.h>
#include <blt/meta/meta.h>
#include <string>
#include <string_view>
#include <vector>
#include <variant>
#include <optional>
#include <memory>
#include <functional>
#include <type_traits>
#include <blt/iterator/iterator.h>
#include <blt/meta/type_traits.h>
#include <blt/std/assert.h>
#include <blt/std/expected.h>
#include <blt/std/ranges.h>
#include <blt/std/utility.h>
#include <blt/std/variant.h>

namespace blt::argparse
{
    class argument_string_t;
    class argument_consumer_t;
    class argument_parser_t;
    class argument_subparser_t;
    class argument_builder_t;
    class argument_storage_t;
    class argument_positional_storage_t;

    enum class action_t
    {
        STORE,
        STORE_CONST,
        STORE_TRUE,
        STORE_FALSE,
        APPEND,
        APPEND_CONST,
        EXTEND,
        COUNT,
        HELP,
        VERSION
    };

    enum class nargs_t
    {
        IF_POSSIBLE,
        ALL,
        ALL_AT_LEAST_ONE
    };

    using nargs_v = std::variant<nargs_t, i32>;

    namespace detail
    {
        class bad_flag final : public std::runtime_error
        {
        public:
            explicit bad_flag(const std::string& message): std::runtime_error(message)
            {
            }
        };

        class bad_positional final : public std::runtime_error
        {
        public:
            explicit bad_positional(const std::string& message): std::runtime_error(message)
            {
            }
        };

        class missing_argument_error final : public std::runtime_error
        {
        public:
            explicit missing_argument_error(const std::string& message): std::runtime_error(message)
            {
            }
        };

        class missing_value_error final : public std::runtime_error
        {
        public:
            explicit missing_value_error(const std::string& message): std::runtime_error(message)
            {
            }
        };

        class type_error final : public std::runtime_error
        {
        public:
            explicit type_error(const std::string& message): std::runtime_error(message)
            {
            }
        };

        class unexpected_argument_error final : public std::runtime_error
        {
        public:
            explicit unexpected_argument_error(const std::string& message): std::runtime_error(message)
            {
            }
        };

        class bad_choice_error final : public std::runtime_error
        {
        public:
            explicit bad_choice_error(const std::string& message): std::runtime_error(message)
            {
            }
        };

        class subparse_error final : public std::exception
        {
        public:
            explicit subparse_error(const std::string_view found_string, std::vector<std::vector<std::string_view>> allowed_strings):
                m_found_string(found_string),
                m_allowed_strings(std::move(allowed_strings))
            {
            }

            [[nodiscard]] const std::vector<std::vector<std::string_view>>& get_allowed_strings() const
            {
                return m_allowed_strings;
            }

            [[nodiscard]] std::string_view get_found_string() const
            {
                return m_found_string;
            }

            [[nodiscard]] std::string error_string() const;

            [[nodiscard]] const char* what() const noexcept override
            {
                m_error_string = error_string();
                return m_error_string.c_str();
            }

        private:
            mutable std::string m_error_string;
            std::string_view m_found_string;
            std::vector<std::vector<std::string_view>> m_allowed_strings;
        };

        template <typename... Args>
        struct arg_data_helper_t
        {
            using variant_t = std::variant<Args..., std::vector<Args>...>;
            using arg_t = meta::arg_helper<Args...>;
            using arg_vec_t = meta::arg_helper<std::vector<Args>...>;

            template <typename T>
            constexpr static bool is_type_stored_v = std::disjunction_v<std::is_same<T, Args>...>;

            template <typename DefaultPrimitiveAction, typename DefaultListAction>
            static auto make_visitor(const DefaultPrimitiveAction& primitive_action, const DefaultListAction& list_action)
            {
                return lambda_visitor{
                    ([&primitive_action](Args& arg)
                    {
                        return primitive_action(arg);
                    })...,
                    ([&list_action](std::vector<Args>& arg_vec)
                    {
                        return list_action(arg_vec);
                    })...
                };
            }
        };

        using arg_meta_type_helper_t = arg_data_helper_t<bool, i8, i16, i32, i64, u8, u16, u32, u64, float, double, std::string>;
        using arg_data_t = arg_meta_type_helper_t::variant_t;

        template <typename T>
        struct arg_string_converter_t
        {
            static T convert(const std::string_view value)
            {
                static_assert(std::is_arithmetic_v<T> || std::is_same_v<T, std::string_view> || std::is_same_v<T, std::string>,
                              "Type must be arithmetic, string_view or string!");
                const std::string temp{value};

                if constexpr (std::is_same_v<T, float>)
                {
                    return std::stof(temp);
                }
                else if constexpr (std::is_same_v<T, double>)
                {
                    return std::stod(temp);
                }
                else if constexpr (std::is_unsigned_v<T>)
                {
                    return static_cast<T>(std::stoull(temp));
                }
                else if constexpr (std::is_signed_v<T>)
                {
                    return static_cast<T>(std::stoll(temp));
                }
                else if constexpr (std::is_same_v<T, std::string_view>)
                {
                    return value;
                }
                else if constexpr (std::is_same_v<T, std::string>)
                {
                    return std::string(value);
                }
                BLT_UNREACHABLE;
            }
        };

        template <typename T>
        auto ensure_is_string(T&& t)
        {
            using NO_REF = meta::remove_cvref_t<T>;
            if constexpr (std::is_same_v<NO_REF, bool>)
                return t ? "true" : "false";
            else if constexpr (std::is_arithmetic_v<NO_REF> && !(std::is_same_v<NO_REF, char>
                || std::is_same_v<NO_REF, unsigned char> || std::is_same_v<NO_REF, signed char>))
                return std::to_string(std::forward<T>(t));
            else
                return std::forward<T>(t);
        }

        void test();
    }

    class argument_string_t
    {
    public:
        explicit argument_string_t(const std::string_view input, const hashset_t<char>& allowed_flag_prefix): m_argument(input),
            m_allowed_flag_prefix(&allowed_flag_prefix)
        {
            process_argument();
        }


        [[nodiscard]] std::string_view get_flag() const
        {
            return m_flag_section;
        }

        [[nodiscard]] std::string_view get_name() const
        {
            return m_name_section;
        }

        [[nodiscard]] std::string_view value() const
        {
            return get_name();
        }

        [[nodiscard]] bool is_flag() const
        {
            return !m_flag_section.empty();
        }

        [[nodiscard]] std::string_view get_argument() const
        {
            return m_argument;
        }

    private:
        /**
         * This function takes the command line argument represented by this class,
         * stored in m_argument and converts into flag side and name side arguments.
         *
         * What this means is in the case of a flag provided, for example passing --foo or --bar, this function will split the argument into
         *
         * m_flag_section = "--"
         * m_name_section = "foo" || "bar"
         *
         * If the argument is purely positional, meaning there is no flag prefix,
         * this function will do nothing and m_flag_section will be true for .empty()
         *
         * For example, if you provide res/some/folder/or/file as a command line positional argument,
         * this function will create the following internal state:
         *
         * m_flag_section = ""
         * m_flag_section.empty() == true
         * m_name_section = "res/some/folder/or/file"
         *
         * m_argument is not modified by this function
         */
        void process_argument()
        {
            // user provides a list of allowed prefix characters to argument_parser_t, which is then provided to this class at construction time
            // it is not the job of this class to validate flag prefixes beyond this. // TODO: requiring this pointer is a code smell.
            size_t start = 0;
            for (; start < m_argument.size() && m_allowed_flag_prefix->contains(m_argument[start]); start++)
            {
            }

            m_flag_section = {m_argument.data(), start};
            m_name_section = {m_argument.data() + start, m_argument.size() - start};
        }

        std::string_view m_argument;
        std::string_view m_flag_section;
        std::string_view m_name_section;
        const hashset_t<char>* m_allowed_flag_prefix;
    };

    class argument_consumer_t
    {
    public:
        explicit argument_consumer_t(const span<argument_string_t>& args): m_absolute_begin(args.data()), m_begin(args.data() + 1),
                                                                           m_end(args.data() + args.size())
        {
            BLT_ASSERT(!args.empty() &&
                "Argument consumer must have at least one argument allocated to it. First argument is always assumed to be program");
        }

        [[nodiscard]] const argument_string_t& absolute_first() const
        {
            return *m_absolute_begin;
        }

        [[nodiscard]] argument_string_t peek(const i32 offset = 0) const
        {
            return *(m_begin + offset);
        }

        [[nodiscard]] argument_string_t r_peek(const i32 offset = 0) const
        {
            return *(m_end - 1 - offset);
        }

        argument_string_t consume()
        {
            return *(m_begin++);
        }

        argument_string_t r_consume()
        {
            return *(--m_end);
        }

        [[nodiscard]] i32 remaining() const
        {
            return static_cast<i32>(size());
        }

        [[nodiscard]] bool can_consume(const i32 amount = 0) const
        {
            return amount < remaining();
        }

    private:
        [[nodiscard]] ptrdiff_t size() const
        {
            return m_end - m_begin;
        }

        argument_string_t* m_absolute_begin;
        argument_string_t* m_begin;
        argument_string_t* m_end;
    };

    class argument_storage_t
    {
        friend argument_parser_t;
        friend argument_subparser_t;
        friend argument_builder_t;

    public:
        template <typename T>
        [[nodiscard]] const T& get(const std::string_view key) const
        {
            return std::get<T>(m_data.at(key));
        }

        [[nodiscard]] const std::string& get(const std::string_view key) const
        {
            return std::get<std::string>(m_data.at(key));
        }

        [[nodiscard]] bool contains(const std::string_view key) const
        {
            return m_data.find(key) != m_data.end();
        }

        [[nodiscard]] size_t size() const
        {
            return m_data.size();
        }

    private:
        void add(const argument_storage_t& values)
        {
            for (const auto& value : values.m_data)
                m_data.insert(value);
        }

        hashmap_t<std::string, detail::arg_data_t> m_data;
    };

    class argument_builder_t
    {
        friend argument_parser_t;

    public:
        argument_builder_t()
        {
            m_dest_func = [](const std::string_view dest, argument_storage_t& storage, std::string_view value)
            {
                storage.m_data.emplace(std::string{dest}, std::string{value});
            };
            m_dest_vec_func = [](const std::string_view dest, argument_storage_t& storage, const std::vector<std::string>& values)
            {
                storage.m_data.emplace(std::string{dest}, values);
            };
        }

        template <typename T>
        argument_builder_t& as_type()
        {
            static_assert(detail::arg_data_helper_t<T>::template is_type_stored_v<T>, "Type is not valid to be stored/converted as an argument");
            m_dest_func = [](const std::string_view dest, argument_storage_t& storage, std::string_view value)
            {
                storage.m_data.emplace(std::string{dest}, detail::arg_string_converter_t<T>::convert(value));
            };
            m_dest_vec_func = [](const std::string_view dest, argument_storage_t& storage, const std::vector<std::string>& values)
            {
                if (storage.m_data.contains(dest))
                {
                    auto& data = storage.m_data[dest];
                    if (!std::holds_alternative<std::vector<T>>(data))
                        throw detail::type_error("Invalid type conversion. Trying to add type " + blt::type_string<T>() +
                            " but this does not match existing type index '" + std::to_string(data.index()) + "'!");
                    auto& converted_values = std::get<std::vector<T>>(data);
                    for (const auto& value : values)
                        converted_values.push_back(detail::arg_string_converter_t<T>::convert(value));
                }
                else
                {
                    std::vector<T> converted_values;
                    for (const auto& value : values)
                        converted_values.push_back(detail::arg_string_converter_t<T>::convert(value));
                    storage.m_data.emplace(std::string{dest}, std::move(converted_values));
                }
            };
            return *this;
        }

        argument_builder_t& make_flag()
        {
            return set_action(action_t::STORE_TRUE);
        }

        argument_builder_t& set_action(action_t action);

        argument_builder_t& set_required(const bool required)
        {
            m_required = required;
            return *this;
        }

        argument_builder_t& set_nargs(const nargs_v nargs)
        {
            m_nargs = nargs;
            return *this;
        }

        argument_builder_t& set_metavar(const std::string& metavar)
        {
            m_metavar = metavar;
            return *this;
        }

        argument_builder_t& set_help(const std::string& help)
        {
            m_help = help;
            return *this;
        }

        argument_builder_t& set_choices(const std::vector<std::string>& choices)
        {
            m_choices = hashset_t<std::string>{};
            for (const auto& choice : choices)
                m_choices->emplace(choice);
            return *this;
        }

        argument_builder_t& set_choices(const hashset_t<std::string>& choices)
        {
            m_choices = choices;
            return *this;
        }

        template <typename... Args>
        argument_builder_t& set_choices(Args&&... args)
        {
            m_choices = hashset_t<std::string>{};
            ((m_choices->emplace(detail::ensure_is_string(std::forward<Args>(args)))), ...);
            return *this;
        }

        argument_builder_t& set_default(const detail::arg_data_t& default_value)
        {
            m_default_value = default_value;
            return *this;
        }

        argument_builder_t& set_const(const detail::arg_data_t& const_value)
        {
            m_const_value = const_value;
            return *this;
        }

        argument_builder_t& set_dest(const std::string_view& dest)
        {
            m_dest = dest;
            return *this;
        }

    private:
        action_t m_action = action_t::STORE;
        bool m_required = false; // do we require this argument to be provided as an argument?
        nargs_v m_nargs = 1; // number of arguments to consume
        std::optional<std::string> m_metavar; // variable name to be used in the help string
        std::optional<std::string> m_help; // help string to be used in the help string
        std::optional<hashset_t<std::string>> m_choices; // optional allowed choices for this argument
        std::optional<detail::arg_data_t> m_default_value;
        std::optional<detail::arg_data_t> m_const_value;
        std::optional<std::string> m_dest;
        // dest, storage, value input
        std::function<void(std::string_view, argument_storage_t&, std::string_view)> m_dest_func;
        // dest, storage, value input
        std::function<void(std::string_view, argument_storage_t&, const std::vector<std::string>& values)> m_dest_vec_func;
    };

    class argument_positional_storage_t
    {
    public:
        explicit argument_positional_storage_t(std::vector<std::pair<std::string, argument_builder_t>> storage): positional_arguments(
            std::move(storage))
        {
        }

        argument_builder_t& peek()
        {
            return positional_arguments[current_positional].second;
        }

        argument_builder_t& next()
        {
            return positional_arguments[current_positional++].second;
        }

        [[nodiscard]] bool has_positional() const
        {
            return current_positional < positional_arguments.size();
        }

        [[nodiscard]] auto remaining() const
        {
            return iterate(positional_arguments).skip(current_positional);
        }

    private:
        std::vector<std::pair<std::string, argument_builder_t>> positional_arguments;
        size_t current_positional = 0;
    };

    class argument_parser_t
    {
        friend argument_subparser_t;
        explicit argument_parser_t(const argument_subparser_t* parent): m_parent(parent)
        {
        }
    public:
        explicit argument_parser_t(const std::optional<std::string_view> description = {}, const std::optional<std::string_view> epilogue = {},
                                   const std::optional<std::string_view> version = {},
                                   const std::optional<std::string_view> usage = {}, const std::optional<std::string_view> name = {}):
            m_name(name), m_usage(usage), m_description(description), m_epilogue(epilogue), m_version(version)
        {
        }

        template <typename... Aliases>
        argument_builder_t& add_flag(const std::string_view arg, Aliases... aliases)
        {
            static_assert(
                std::conjunction_v<std::disjunction<std::is_convertible<Aliases, std::string>, std::is_constructible<
                                                        std::string, Aliases>>...>,
                "Arguments must be of type string_view, convertible to string_view or be string_view constructable");
            m_argument_builders.emplace_back(std::make_unique<argument_builder_t>());
            m_argument_builders.back()->set_dest(arg);
            m_flag_arguments.emplace(arg, m_argument_builders.back().get());
            (m_flag_arguments.emplace(aliases, m_argument_builders.back().get()), ...);
            return *m_argument_builders.back().get();
        }

        argument_builder_t& add_positional(const std::string_view arg)
        {
            auto& b = m_positional_arguments.emplace_back(arg, argument_builder_t{}).second;
            b.set_dest(std::string{arg});
            b.set_required(true);
            b.set_nargs(1);
            return b;
        }

        argument_subparser_t* add_subparser(std::string_view dest);

        argument_parser_t& with_help()
        {
            add_flag("--help", "-h").set_action(action_t::HELP).set_help("Show this help menu and exit");
            return *this;
        }

        [[nodiscard]] bool has_help() const;

        argument_parser_t& with_version()
        {
            add_flag("--version").set_action(action_t::VERSION);
            return *this;
        }

        argument_storage_t parse(argument_consumer_t& consumer); // NOLINT

        argument_storage_t parse(const std::vector<std::string_view>& args)
        {
            std::vector<argument_string_t> arg_strings;
            arg_strings.reserve(args.size());
            for (const auto& arg : args)
                arg_strings.emplace_back(arg, allowed_flag_prefixes);
            argument_consumer_t consumer{arg_strings};
            return parse(consumer);
        }

        argument_storage_t parse(const std::vector<std::string>& args)
        {
            std::vector<argument_string_t> arg_strings;
            arg_strings.reserve(args.size());
            for (const auto& arg : args)
                arg_strings.emplace_back(arg, allowed_flag_prefixes);
            argument_consumer_t consumer{arg_strings};
            return parse(consumer);
        }

        argument_storage_t parse(const int argc, const char** argv)
        {
            std::vector<argument_string_t> arg_strings;
            arg_strings.reserve(argc);
            for (int i = 0; i < argc; ++i)
                arg_strings.emplace_back(argv[i], allowed_flag_prefixes);
            argument_consumer_t consumer{arg_strings};
            return parse(consumer);
        }

        void print_help();

        void print_usage();

        void print_version() const;

        argument_parser_t& set_name(const std::optional<std::string>& name)
        {
            m_name = name;
            return *this;
        }

        argument_parser_t& set_usage(const std::optional<std::string>& usage)
        {
            m_usage = usage;
            return *this;
        }

        [[nodiscard]] const std::optional<std::string>& get_usage() const
        {
            return m_usage;
        }

        argument_parser_t& set_description(const std::optional<std::string>& description)
        {
            m_description = description;
            return *this;
        }

        [[nodiscard]] const std::optional<std::string>& get_description() const
        {
            return m_description;
        }

        argument_parser_t& set_epilogue(const std::optional<std::string>& epilogue)
        {
            m_epilogue = epilogue;
            return *this;
        }

        [[nodiscard]] const std::optional<std::string>& get_epilogue() const
        {
            return m_epilogue;
        }

        argument_parser_t& set_version(const std::optional<std::string>& version)
        {
            m_epilogue = version;
            return *this;
        }

        [[nodiscard]] const std::optional<std::string>& get_version() const
        {
            return m_version;
        }

        [[nodiscard]] const hashset_t<char>& get_allowed_flag_prefixes() const
        {
            return allowed_flag_prefixes;
        }

    private:
        void handle_compound_flags(hashset_t<std::string>& found_flags, argument_storage_t& parsed_args, argument_consumer_t& consumer,
                                   const argument_string_t& arg);
        void parse_flag(argument_storage_t& parsed_args, argument_consumer_t& consumer, std::string_view arg);
        void parse_positional(argument_storage_t& parsed_args, argument_consumer_t& consumer, argument_positional_storage_t& storage,
                              std::string_view arg);
        static void handle_missing_and_default_args(hashmap_t<std::string_view, argument_builder_t*>& arguments,
                                                    const hashset_t<std::string>& found, argument_storage_t& parsed_args, std::string_view type);
        static expected<std::vector<std::string>, std::string> consume_until_flag_or_end(argument_consumer_t& consumer,
                                                                                         hashset_t<std::string>* allowed_choices);
        static std::vector<std::string> consume_argc(i32 argc, argument_consumer_t& consumer, hashset_t<std::string>* allowed_choices,
                                                     std::string_view arg);

        std::optional<std::string> m_name;
        std::optional<std::string> m_usage;
        std::optional<std::string> m_description;
        std::optional<std::string> m_epilogue;
        std::optional<std::string> m_version;
        const argument_subparser_t* m_parent = nullptr;
        std::vector<std::pair<std::string_view, std::unique_ptr<argument_subparser_t>>> m_subparsers;
        std::vector<std::unique_ptr<argument_builder_t>> m_argument_builders;
        hashmap_t<std::string_view, argument_builder_t*> m_flag_arguments;
        std::vector<std::pair<std::string, argument_builder_t>> m_positional_arguments;
        hashset_t<char> allowed_flag_prefixes = {'-', '+', '/'};
    };

    class argument_subparser_t
    {
        friend argument_parser_t;
    public:
        explicit argument_subparser_t(const argument_parser_t& parent): m_parent(&parent)
        {
        }

        template <typename... Aliases>
        argument_parser_t* add_parser(const std::string_view name, Aliases... aliases)
        {
            static_assert(
                std::conjunction_v<std::disjunction<std::is_convertible<Aliases, std::string_view>, std::is_constructible<
                                                        std::string_view, Aliases>>...>,
                "Arguments must be of type string_view, convertible to string_view or be string_view constructable");
            m_parsers.emplace_back(new argument_parser_t{this});
            if (m_parent->has_help())
                m_parsers.back()->with_help();
            m_aliases[name] = m_parsers.back().get();
            ((m_aliases[std::string_view{aliases}] = m_parsers.back().get()), ...);
            return m_parsers.back().get();
        }

        argument_subparser_t* set_help(const std::optional<std::string>& help)
        {
            m_help = help;
            return this;
        }


        /**
         * Parses the next argument using the provided argument consumer.
         *
         * This function uses an argument consumer to extract and process the next argument.
         * If the argument is a flag or if it cannot be matched against the available parsers,
         * an exception is thrown.
         *
         * @param consumer Reference to an argument_consumer_t object, which handles argument parsing.
         *                  The consumer provides the next argument to be parsed.
         *
         * @throws detail::subparse_error If the argument is a flag or does not match any known parser.
         */
        std::pair<argument_string_t, argument_storage_t> parse(argument_consumer_t& consumer); // NOLINT

    private:
        [[nodiscard]] hashmap_t<argument_parser_t*, std::vector<std::string_view>> get_allowed_strings() const;

        // annoying compatability because im lazy
        static std::vector<std::vector<std::string_view>> to_vec(const hashmap_t<argument_parser_t*, std::vector<std::string_view>>& map);

        const argument_parser_t* m_parent = nullptr;
        std::optional<std::string> m_last_parsed_parser; // bad hack
        std::optional<std::string> m_help;
        std::vector<std::unique_ptr<argument_parser_t>> m_parsers;
        hashmap_t<std::string_view, argument_parser_t*> m_aliases;
    };
}

#endif //BLT_PARSE_ARGPARSE_V2_H
