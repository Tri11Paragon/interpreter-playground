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
#include <iostream>
#include <utility>
#include <blt/parse/argparse_v2.h>
#include <blt/std/assert.h>
#include <blt/meta/type_traits.h>
#include <blt/logging/logging.h>
#include <blt/iterator/enumerate.h>
#include <blt/fs/path_helper.h>
#include <blt/std/string.h>
#include <blt/iterator/flatten.h>

namespace blt::argparse
{
	constexpr static auto printer_primitive = [](const auto& v) {
		std::cout << v;
	};

	constexpr static auto printer_vector = [](const auto& v) {
		std::cout << "[";
		for (const auto& [i, a] : enumerate(v))
		{
			std::cout << a;
			if (i != v.size() - 1)
				std::cout << ", ";
		}
		std::cout << "]";
	};

	auto print_visitor = detail::arg_meta_type_helper_t::make_visitor(printer_primitive, printer_vector);

	template <typename T>
	size_t get_const_char_size(const T& t)
	{
		if constexpr (std::is_convertible_v<T, const char*>)
		{
			return std::char_traits<char>::length(t);
		} else if constexpr (std::is_same_v<T, char> || std::is_same_v<T, unsigned char> || std::is_same_v<T, signed char>)
		{
			return 1;
		} else if constexpr (std::is_same_v<T, std::string_view> || std::is_same_v<T, std::string>)
		{
			return t.size();
		} else
		{
			return 0;
		}
	}

	template <typename T>
	std::string to_string(const T& t)
	{
		if constexpr (std::is_same_v<T, const char*> || std::is_same_v<T, std::string_view>)
		{
			return std::string(t);
		} else if constexpr (std::is_same_v<T, char> || std::is_same_v<T, unsigned char> || std::is_same_v<T, signed char>)
		{
			std::string str;
			str += t;
			return str;
		} else
		{
			return t;
		}
	}

	template <typename... Strings>
	std::string make_string(Strings&&... strings)
	{
		std::string out;
		out.reserve((get_const_char_size(strings) + ...));
		((out += detail::ensure_is_string(std::forward<Strings>(strings))), ...);
		return out;
	}

	template <typename... Strings>
	std::vector<std::string_view> make_arguments(Strings... strings)
	{
		return std::vector<std::string_view>{"./program", strings...};
	}

	class aligned_internal_string_t
	{
	public:
		explicit aligned_internal_string_t(std::string& str, const size_t max_line_length, const size_t line_start_size): string(str),
																														max_line_size(
																															max_line_length),
																														line_start_size(
																															line_start_size)
		{}

		void add(const std::string_view str) const
		{
			const auto lines = string::split(string, '\n');
			if (lines.empty())
			{
				string += str;
				return;
			}
			if (lines.back().size() + str.size() > max_line_size)
			{
				string += '\n';
				for (size_t i = 0; i < line_start_size; i++)
					string += ' ';
				bool blank = true;
				// we don't want to write blank only strings
				for (const char c : str)
				{
					if (!std::isblank(c))
					{
						blank = false;
						break;
					}
				}
				if (blank)
					return;
			}

			string += str;
		}

		template <typename T>
		aligned_internal_string_t& operator+=(T&& value)
		{
			const auto str = to_string(detail::ensure_is_string(std::forward<T>(value)));
			for (size_t i = 0; i < str.size(); i++)
			{
				size_t j = i;
				for (; j < str.size() && !std::isblank(str[j]); ++j)
				{}
				add(std::string_view(str.data() + i, j - i));
				if (j < str.size())
					add(std::string_view(str.data() + j, 1));
				i = j;
			}
			return *this;
		}

		[[nodiscard]] std::string& str() const
		{
			return string;
		}

	private:
		std::string& string;
		size_t max_line_size;
		size_t line_start_size;
	};

	class aligner_t
	{
	public:
		aligner_t(std::vector<std::string>& buffer, const size_t start_index, const size_t max_line_size): buffer(buffer), start_index(start_index),
																											max_line_size(max_line_size)
		{}

		void align(const size_t spaces_between) const
		{
			const size_t take = compute_take();
			size_t aligned_size = 0;
			for (const auto& v : iterate(buffer).skip(start_index).take(take))
			{
				auto size = static_cast<i64>(v.size());
				for (; size > 0 && std::isblank(v[size - 1]); size--)
				{}
				aligned_size = std::max(aligned_size, static_cast<size_t>(size));
			}
			const auto offset_size = aligned_size + spaces_between;

			for (auto& v : iterate(buffer).skip(start_index).take(take))
			{
				for (size_t i = v.size(); i < offset_size; i++)
					v += ' ';
			}
		}

		[[nodiscard]] auto iter()
		{
			return iterate(buffer).skip(start_index).take(compute_take()).map([this](std::string& x) {
				return aligned_internal_string_t{x, max_line_size, x.size()};
			});
		}

		[[nodiscard]] auto iter() const
		{
			return iterate(buffer).skip(start_index).take(compute_take()).map([this](std::string& x) {
				return aligned_internal_string_t{x, max_line_size, x.size()};
			});
		}

		void take(const size_t amount)
		{
			this->amount = amount;
		}

	private:
		[[nodiscard]] size_t compute_take() const
		{
			return amount == -1ul ? (buffer.size() - start_index - 1) : amount;;
		}

		std::vector<std::string>& buffer;
		size_t start_index;
		size_t max_line_size;
		size_t amount = -1;
	};

	class aligned_printer_t
	{
	public:
		explicit aligned_printer_t(std::string line_begin = "\t", const size_t max_line_size = 120, const size_t spaces_per_tab = 4):
			line_begin(std::move(line_begin)), max_line_size(max_line_size)
		{
			buffer.emplace_back();
			for (size_t i = 0; i < spaces_per_tab; i++)
				spaces_from_tab += ' ';
		}

		[[nodiscard]] std::string str() const
		{
			std::string combined;
			for (const auto& str : buffer)
			{
				combined += str;
				combined += '\n';
			}
			return combined;
		}

		auto mark()
		{
			return aligner_t{buffer, buffer.size() - 1, max_line_size};
		}

		template <typename T>
		aligned_printer_t& add(T&& value)
		{
			const auto str = to_string(detail::ensure_is_string(std::forward<T>(value)));
			if (buffer.back().size() + str.size() > max_line_size)
				newline();
			buffer.back() += replace_tabs(str);
			return *this;
		}

		void newline()
		{
			buffer.emplace_back(replace_tabs(line_begin));
		}

		[[nodiscard]] std::string replace_tabs(std::string str) const
		{
			string::replaceAll(str, "\t", spaces_from_tab);
			return str;
		}

		template <typename T>
		aligned_printer_t& operator+=(T&& value)
		{
			return add(std::forward<T>(value));
		}

		[[nodiscard]] auto iter()
		{
			return iterate(buffer);
		}

		[[nodiscard]] auto iter() const
		{
			return iterate(buffer);
		}

	private:
		std::vector<std::string> buffer;
		std::string line_begin;
		std::string spaces_from_tab;
		size_t max_line_size;
	};

	argument_builder_t& argument_builder_t::set_action(const action_t action)
	{
		m_action = action;
		switch (m_action)
		{
			case action_t::STORE_TRUE:
				set_nargs(0);
				as_type<bool>();
				set_default(false);
				break;
			case action_t::STORE_FALSE:
				set_nargs(0);
				as_type<bool>();
				set_default(true);
				break;
			case action_t::STORE_CONST:
			case action_t::APPEND_CONST:
				set_nargs(0);
				break;
			case action_t::COUNT:
				set_nargs(0);
				as_type<u64>();
				break;
			case action_t::EXTEND:
				set_nargs(nargs_t::ALL);
				break;
			case action_t::HELP:
			case action_t::VERSION:
				set_nargs(0);
				break;
			default:
				break;
		}
		return *this;
	}

	argument_subparser_t* argument_parser_t::add_subparser(const std::string_view dest)
	{
		m_subparsers.emplace_back(dest, std::make_unique<argument_subparser_t>(*this));
		return m_subparsers.back().second.get();
	}

	bool argument_parser_t::has_help() const
	{
		for (const auto& action : m_argument_builders)
		{
			if (action->m_action == action_t::HELP)
				return true;
		}
		return false;
	}

	argument_storage_t argument_parser_t::parse(argument_consumer_t& consumer)
	{
		if (!m_name)
			m_name = fs::base_name_sv(consumer.absolute_first().get_argument());
		argument_positional_storage_t positional_storage{m_positional_arguments};
		hashset_t<std::string> found_flags;
		argument_storage_t parsed_args;
		// First, we consume flags which may be part of this parser.
		// If the user is providing a flag for a parent parser, this is considered an error. Flags for parents must come before subparser selection
		while (consumer.can_consume() && consumer.peek().is_flag())
			handle_compound_flags(found_flags, parsed_args, consumer, consumer.consume());

		for (auto& [key, subparser] : m_subparsers)
		{
			auto [parsed_subparser, storage] = subparser->parse(consumer);
			storage.m_data.emplace(std::string{key}, detail::arg_data_t{std::string{parsed_subparser.get_argument()}});
			parsed_args.add(storage);
		}

		while (consumer.can_consume())
		{
			if (consumer.peek().is_flag())
				handle_compound_flags(found_flags, parsed_args, consumer, consumer.consume());
			else
			{
				// We have a parent, meaning we do not worry about being unable to parse any arguments. (They will handle it)
				// This enables multi-subparser chaining, creating a tree like structure of parsable objects.
				// Useful when you have multiple selections that each require their own configuration arguments (thus making a simple positional + switch a poor solution)
				if (!positional_storage.has_positional() && m_parent != nullptr)
					break;
				parse_positional(parsed_args, consumer, positional_storage, consumer.peek().get_argument());
			}
		}
		handle_missing_and_default_args(m_flag_arguments, found_flags, parsed_args, "flag");

		for (auto& [name, value] : positional_storage.remaining())
		{
			std::visit(lambda_visitor{
							[](const nargs_t) {},
							[](const int argc) {
								if (argc == 0)
									throw detail::bad_positional("Positional Argument takes no values, this is invalid!");
							}
						}, value.m_nargs);

			if (value.m_required)
				throw detail::missing_argument_error(make_string("Error: argument '", name, "' was not found but is required by the program"));
			if (value.m_default_value && !parsed_args.contains(value.m_dest.value_or(name)))
				parsed_args.m_data.emplace(value.m_dest.value_or(name), *value.m_default_value);
		}

		return parsed_args;
	}

	void argument_parser_t::print_help()
	{
		print_usage();
		aligned_printer_t help{""};

		if (!m_subparsers.empty())
		{
			help += "Subcommands:";
			help.newline();
			for (const auto& [key, value] : m_subparsers)
			{
				auto map = value->get_allowed_strings();
				help += '\t';
				help += key;
				help += ": {";
				for (const auto& [i, parser, strings] : enumerate(map).flatten())
				{
					if (strings.size() > 1)
						help += '[';
					for (const auto& [i, str] : enumerate(strings))
					{
						help += str;
						if (i != strings.size() - 1)
							help += ", ";
					}
					if (strings.size() > 1)
						help += ']';
					if (i != map.size() - 1)
						help += ", ";
				}
				help += "}";
				help.newline();
			}
			help.newline();
		}

		if (!m_positional_arguments.empty())
		{
			help += "Positional Arguments:";
			help.newline();
			auto mark = help.mark();
			for (auto& [name, builder] : m_positional_arguments)
			{
				help += '\t';
				if (!builder.m_required)
					help += '[';
				help += name;
				if (!builder.m_required)
					help += ']';
				help.newline();
			}
			mark.align(4);
			for (auto zipped_positionals : mark.iter().zip(m_positional_arguments))
			{
				auto& line = std::get<0>(zipped_positionals);
				auto& [name, builder] = std::get<1>(zipped_positionals);
				line += builder.m_help.value_or("");
				if (builder.m_default_value && !(builder.m_action == action_t::STORE_TRUE || builder.m_action == action_t::STORE_FALSE))
                {
					if (!std::isblank(line.str().back()))
						line += " ";
                    line += "(Default: ";
                    std::visit(detail::arg_meta_type_helper_t::make_visitor(
                                   [&](auto& value)
                                   {
                                       line += value;
                                   },
                                   [&](auto& vec)
                                   {
                                       if constexpr (!std::is_same_v<std::decay_t<meta::remove_cvref_t<decltype(vec)>>, std::vector<bool>>)
                                       {
                                           line += '[';
                                           for (const auto& [i, v] : enumerate(vec))
                                           {
                                               line += v;
                                               if (i != vec.size() - 1)
                                                   line += ", ";
                                           }
                                           line += ']';
                                       }
                                   }), *builder.m_default_value);
                    line += ")";
                }
                if (builder.m_choices)
                {
                    if (!std::isblank(line.str().back()))
                        line += " ";
                    line += "(Choices: ";
                    for (const auto& [i, v] : enumerate(*builder.m_choices))
                    {
                        line += '\'';
                        line += v;
                        line += '\'';
                        if (i != builder.m_choices->size() - 1)
                            line += ", ";
                    }
                    line += ')';
                }
			}
		}

		if (!m_flag_arguments.empty())
		{
			help += "Options:";
			help.newline();
			hashmap_t<argument_builder_t*, std::vector<std::string>> same_flags;
			for (const auto& [key, value] : m_flag_arguments)
				same_flags[value].emplace_back(key);
			auto mark = help.mark();
			for (const auto& [builder, flag_list] : same_flags)
			{
				// find max size and align?
				help += '\t';
				for (const auto& [i, flag] : enumerate(flag_list))
				{
					help += flag;
					if (i != flag_list.size() - 1)
						help += ", ";
				}
				const argument_string_t arg{flag_list.front(), allowed_flag_prefixes};
				auto metavar = builder->m_metavar.value_or(string::toUpperCase(arg.get_name()));
				auto lambda = [&]() {
					help += ' ';
					help += metavar;
				};
				std::visit(lambda_visitor{
								[&](const nargs_t type) {
									lambda();
									switch (type)
									{
										case nargs_t::IF_POSSIBLE:
											break;
										case nargs_t::ALL:
										case nargs_t::ALL_AT_LEAST_ONE:
											help += "...";
											break;
									}
								},
								[&](const int argc) {
									if (argc == 0)
										return;
									lambda();
									if (argc > 1)
									{
										help += "... x";
										help += std::to_string(argc);
									}
								}
							}, builder->m_nargs);
				help.newline();
			}
			mark.align(4);
			for (auto zipped_flags : mark.iter().zip(same_flags))
			{
				auto& str = std::get<0>(zipped_flags);
				auto& [builder, flag_list] = std::get<1>(zipped_flags);
				str += builder->m_help.value_or("");
				if (builder->m_default_value && !(builder->m_action == action_t::STORE_TRUE || builder->m_action == action_t::STORE_FALSE))
				{
					if (!std::isblank(str.str().back()))
						str += " ";
					str += "(Default: '";
					std::visit(detail::arg_meta_type_helper_t::make_visitor([&](auto& value) {
						str += value;
					}, [&](auto& vec) {
						if constexpr (!std::is_same_v<std::decay_t<meta::remove_cvref_t<decltype(vec)>>, std::vector<bool>>)
						{
							str += '[';
							for (const auto& [i, v] : enumerate(vec))
							{
								str += v;
								if (i != vec.size() - 1)
									str += ", ";
							}
							str += ']';
						}
					}), *builder->m_default_value);
					str += "')";
				}
				if (builder->m_choices)
				{
					if (!std::isblank(str.str().back()))
						str += " ";
					str += "(Choices: ";
					for (const auto& [i, v] : enumerate(*builder->m_choices))
					{
						str += '\'';
						str += v;
						str += '\'';
						if (i != builder->m_choices->size() - 1)
							str += ", ";
					}
					str += ')';
				}
				if (builder->m_required)
				{
					if (!std::isblank(str.str().back()))
						str += " ";
					str += "(Required)";
				}
			}
		}

		std::cout << help.str() << std::endl;
	}

	void argument_parser_t::print_usage()
	{
		if (!m_usage)
		{
			aligned_printer_t aligner;
			aligner += m_name.value_or("");
			aligner += ' ';

			auto parent = m_parent;
			std::vector<const argument_subparser_t*> parsers;
			while (parent != nullptr)
			{
				parsers.push_back(parent);
				parent = parent->m_parent->m_parent;
			}

			// TODO: this doesn't handle parsers with multiple subparsers.
			// eg: "blt-rice-classification-example manual best one_point_crossover --help single_point_mutation"
			// will print "blt-rice-classification-example manual one_point_crossover [-h] [--min_tree_size MIN_TREE_SIZE]..."
			for (const auto parser : iterate(parsers).rev())
			{
				if (!parser->m_last_parsed_parser)
					throw detail::missing_value_error(
						"Error: Help called on subparser but unable to find parser chain. This condition should be impossible.");
				aligner += parser->m_last_parsed_parser.value();
				aligner += ' ';
			}

			for (const auto& [key, _] : m_subparsers)
			{
				aligner += '{';
				aligner += key;
				aligner += '}';
				aligner += ' ';
			}

			hashmap_t<std::string, std::vector<std::string>> singleFlags;
			std::vector<std::pair<argument_string_t, argument_builder_t*>> compoundFlags;

			for (const auto& [key, value] : m_flag_arguments)
			{
				const argument_string_t arg{key, allowed_flag_prefixes};
				if (arg.get_flag().size() == 1)
				{
					if (std::holds_alternative<i32>(value->m_nargs) && std::get<i32>(value->m_nargs) == 0)
						singleFlags[arg.get_flag()].emplace_back(arg.get_name());
					else
						compoundFlags.emplace_back(arg, value);
				} else
					compoundFlags.emplace_back(arg, value);
			}

			for (const auto& [i, kv] : enumerate(singleFlags))
			{
				const auto& [key, value] = kv;
				aligner += '[';
				aligner += key;
				for (const auto& name : value)
					aligner += name;
				aligner += ']';
				aligner += ' ';
			}

			for (const auto& [i, kv] : enumerate(compoundFlags))
			{
				const auto& name = kv.first;
				const auto& builder = kv.second;
				aligner += builder->m_required ? '<' : '[';
				aligner += name.get_argument();
				auto lambda = [&]() {
					aligner += ' ';
					aligner += builder->m_metavar.value_or(string::toUpperCase(name.get_name()));
				};
				std::visit(lambda_visitor{
								[&](const nargs_t type) {
									lambda();
									switch (type)
									{
										case nargs_t::IF_POSSIBLE:
											break;
										case nargs_t::ALL:
										case nargs_t::ALL_AT_LEAST_ONE:
											aligner += "...";
											break;
									}
								},
								[&](const int argc) {
									for (int j = 0; j < argc; j++)
										lambda();
								}
							}, builder->m_nargs);
				aligner += builder->m_required ? '>' : ']';
				aligner += ' ';
			}

			for (const auto& [i, pair] : enumerate(m_positional_arguments))
			{
				const auto& [name, _] = pair;
				aligner += '<';
				aligner += name;
				aligner += '>';
				if (i != m_positional_arguments.size() - 1)
					aligner += ' ';
			}

			m_usage = aligner.str();
		}
		std::cout << "Usage: " << *m_usage << std::endl;
	}

	void argument_parser_t::print_version() const
	{
		std::cout << m_name.value_or("NO NAME") << " " << m_version.value_or("NO VERSION") << std::endl;
	}

	void argument_parser_t::handle_compound_flags(hashset_t<std::string>& found_flags, argument_storage_t& parsed_args, argument_consumer_t& consumer,
												const argument_string_t& arg)
	{
		// i kinda hate this, TODO?
		std::vector<std::string> compound_flags;
		if (arg.get_flag().size() == 1)
		{
			for (const auto c : arg.get_name())
				compound_flags.emplace_back(std::string{arg.get_flag()} + c);
		} else
		{
			if (arg.get_flag().size() > 2)
				throw detail::bad_flag(make_string("Error: Flag '", arg.get_argument(), "' is too long!"));
			compound_flags.emplace_back(arg.get_argument());
		}

		for (const auto& flag_key : compound_flags)
		{
			const auto flag = m_flag_arguments.find(flag_key);
			if (flag == m_flag_arguments.end())
				throw detail::bad_flag(make_string("Error: Unknown flag: ", flag_key));
			found_flags.insert(flag_key);
			parse_flag(parsed_args, consumer, flag_key);
		}
	}

	void argument_parser_t::parse_flag(argument_storage_t& parsed_args, argument_consumer_t& consumer, const std::string_view arg)
	{
		auto flag = m_flag_arguments.find(arg)->second;
		const auto dest = flag->m_dest.value_or(std::string{arg});
		std::visit(lambda_visitor{
						[&parsed_args, &consumer, &dest, &flag, arg](const nargs_t arg_enum) {
							switch (arg_enum)
							{
								case nargs_t::IF_POSSIBLE:
									if (consumer.can_consume() && !consumer.peek().is_flag())
										flag->m_dest_func(dest, parsed_args, consumer.consume().get_argument());
									else
									{
										if (flag->m_const_value)
											parsed_args.m_data.insert({dest, *flag->m_const_value});
									}
									break;
								case nargs_t::ALL_AT_LEAST_ONE:
									if (!consumer.can_consume())
										throw detail::missing_argument_error(
											make_string("Error expected at least one argument to be consumed by '", arg, '\''));
									[[fallthrough]];
								case nargs_t::ALL:
									auto result = consume_until_flag_or_end(consumer, flag->m_choices ? &*flag->m_choices : nullptr);
									if (!result)
										throw detail::bad_choice_error(make_string('\'', consumer.peek().get_argument(),
																					"' is not a valid choice for argument '", arg,
																					"'! Expected one of ", result.error()));
									flag->m_dest_vec_func(dest, parsed_args, result.value());
									break;
							}
						},
						[&parsed_args, &consumer, &dest, &flag, arg, this](const i32 argc) {
							const auto args = consume_argc(argc, consumer, flag->m_choices ? &*flag->m_choices : nullptr, arg);

							switch (flag->m_action)
							{
								case action_t::STORE:
									if (argc == 0)
										throw detail::missing_argument_error(
											make_string("Argument '", arg, "'s action is store but takes in no arguments?"));
									if (argc == 1)
										flag->m_dest_func(dest, parsed_args, args.front());
									else
										throw detail::unexpected_argument_error(make_string("Argument '", arg,
																							"'s action is store but takes in more than one argument. "
																							"Did you mean to use action_t::APPEND or action_t::EXTEND?"));
									break;
								case action_t::APPEND:
								case action_t::EXTEND:
									if (argc == 0)
										throw detail::missing_argument_error(
											make_string("Argument '", arg, "'s action is append or extend but takes in no arguments."));
									flag->m_dest_vec_func(dest, parsed_args, args);
									break;
								case action_t::APPEND_CONST:
									if (argc != 0)
										throw detail::unexpected_argument_error(
											make_string("Argument '", arg, "'s action is append const but takes in arguments."));
									if (!flag->m_const_value)
									{
										throw detail::missing_value_error(make_string(
											"Append const chosen as an action but const value not provided for argument '", arg, '\''));
									}
									if (parsed_args.contains(dest))
									{
										auto& data = parsed_args.m_data[dest];
										std::visit(detail::arg_meta_type_helper_t::make_visitor([arg](auto& primitive) {
											throw detail::type_error(make_string("Invalid type for argument '", arg, "' expected list type, found '",
																				blt::type_string<decltype(primitive)>(), "' with value ", primitive));
										}, [&flag, arg](auto& vec) {
											using type = typename meta::remove_cvref_t<decltype(vec)>::value_type;
											if (!std::holds_alternative<type>(*flag->m_const_value))
											{
												throw detail::type_error(make_string("Constant value for argument '", arg,
																					"' type doesn't match values already present! Expected to be of type '",
																					blt::type_string<type>(), "'!"));
											}
											vec.push_back(std::get<type>(*flag->m_const_value));
										}), data);
									} else
									{
										std::visit(detail::arg_meta_type_helper_t::make_visitor([&parsed_args, &dest](auto& primitive) {
											std::vector<meta::remove_cvref_t<decltype(primitive)>> vec;
											vec.emplace_back(primitive);
											parsed_args.m_data.emplace(dest, std::move(vec));
										}, [](auto&) {
											throw detail::type_error("Append const should not be a list type!");
										}), *flag->m_const_value);
									}
									break;
								case action_t::STORE_CONST:
									if (argc != 0)
									{
										print_usage();
										throw detail::unexpected_argument_error(
											make_string("Argument '", arg, "' is store const but called with an argument."));
									}
									if (!flag->m_const_value)
										throw detail::missing_value_error(
											make_string("Argument '", arg, "' is store const, but const storage has no value."));
									parsed_args.m_data.emplace(dest, *flag->m_const_value);
									break;
								case action_t::STORE_TRUE:
									if (argc != 0)
									{
										print_usage();
										throw detail::unexpected_argument_error("Store true flag called with an argument.");
									}
									parsed_args.m_data.emplace(dest, true);
									break;
								case action_t::STORE_FALSE:
									if (argc != 0)
									{
										print_usage();
										throw detail::unexpected_argument_error("Store false flag called with an argument.");
									}
									parsed_args.m_data.insert({dest, false});
									break;
								case action_t::COUNT:
									if (parsed_args.m_data.contains(dest))
									{
										auto visitor = detail::arg_meta_type_helper_t::make_visitor([](auto& primitive) -> detail::arg_data_t {
											using type = meta::remove_cvref_t<decltype(primitive)>;
											if constexpr (std::is_convertible_v<decltype(1), type>)
											{
												return primitive + static_cast<type>(1);
											} else
												throw detail::type_error("Error: count called but stored type is " + blt::type_string<type>());
										}, [](auto&) -> detail::arg_data_t {
											throw detail::type_error(
												"List present on count. This condition doesn't make any sense! "
												"(How did we get here, please report this!)");
										});
										parsed_args.m_data[dest] = std::visit(visitor, parsed_args.m_data[dest]);
									} else // I also hate this!
										flag->m_dest_func(dest, parsed_args, "1");
									break;
								case action_t::HELP:
									print_help();
									std::exit(0);
								case action_t::VERSION:
									print_version();
									std::exit(0);
							}
						}
					}, flag->m_nargs);
	}

	void argument_parser_t::parse_positional(argument_storage_t& parsed_args, argument_consumer_t& consumer, argument_positional_storage_t& storage,
											const std::string_view arg)
	{
		if (!storage.has_positional())
			throw detail::missing_argument_error(make_string("Error: '", arg, "' excess positional argument(s) detected! This parser does not consume any more arguments!"));
		auto& positional = storage.next();
		const auto dest = positional.m_dest.value_or(std::string{arg});
		std::visit(lambda_visitor{
						[&consumer, &positional, &dest, &parsed_args, arg](const nargs_t arg_enum) {
							switch (arg_enum)
							{
								case nargs_t::IF_POSSIBLE:
									throw detail::bad_positional(
										"Positional argument asked to consume if possible. We do not consider this to be a valid ask.");
								case nargs_t::ALL_AT_LEAST_ONE:
									if (!consumer.can_consume())
										throw detail::missing_argument_error(
											make_string("Error expected at least one argument to be consumed by '", arg, '\''));
									[[fallthrough]];
								case nargs_t::ALL:
									auto result = consume_until_flag_or_end(consumer, positional.m_choices ? &*positional.m_choices : nullptr);
									if (!result)
										throw detail::bad_choice_error(make_string('\'', consumer.peek().get_argument(),
																					"' is not a valid choice for argument '", arg,
																					"'! Expected one of ", result.error()));
									positional.m_dest_vec_func(dest, parsed_args, result.value());
									break;
							}
						},
						[this, &consumer, &positional, &dest, &parsed_args, arg](const i32 argc) {
							const auto args = consume_argc(argc, consumer, positional.m_choices ? &*positional.m_choices : nullptr, arg);

							switch (positional.m_action)
							{
								case action_t::STORE:
									if (argc == 0)
										throw detail::missing_argument_error(
											make_string("Argument '", arg, "'s action is store but takes in no arguments?"));
									if (argc == 1)
										positional.m_dest_func(dest, parsed_args, args.front());
									else
										throw detail::unexpected_argument_error(make_string("Argument '", arg,
																							"'s action is store but takes in more than one argument. "
																							"Did you mean to use action_t::APPEND or action_t::EXTEND?"));
									break;
								case action_t::APPEND:
								case action_t::EXTEND:
									if (argc == 0)
										throw detail::missing_argument_error(
											make_string("Argument '", arg, "'s action is append or extend but takes in no arguments."));
									positional.m_dest_vec_func(dest, parsed_args, args);
									break;
								case action_t::APPEND_CONST:
									throw detail::bad_positional("action_t::APPEND_CONST does not make sense for positional arguments");
								case action_t::STORE_CONST:
									throw detail::bad_positional("action_t::STORE_CONST does not make sense for positional arguments");
								case action_t::STORE_TRUE:
									throw detail::bad_positional("action_t::STORE_TRUE does not make sense for positional arguments");
								case action_t::STORE_FALSE:
									throw detail::bad_positional("action_t::STORE_FALSE does not make sense for positional arguments");
								case action_t::COUNT:
									throw detail::bad_positional("action_t::COUNT does not make sense for positional arguments");
								case action_t::HELP:
									print_help();
									std::exit(0);
								case action_t::VERSION:
									print_version();
									std::exit(0);
							}
						}
					}, positional.m_nargs);
	}

	void argument_parser_t::handle_missing_and_default_args(hashmap_t<std::string_view, argument_builder_t*>& arguments,
															const hashset_t<std::string>& found, argument_storage_t& parsed_args,
															const std::string_view type)
	{
		for (const auto& [key, value] : arguments)
		{
			if (!found.contains(key))
			{
				if (value->m_required)
					throw detail::missing_argument_error(make_string("Error: ", type, " argument '", key,
																	"' was not found but is required by the program"));
				auto dest = value->m_dest.value_or(std::string{key});
				if (value->m_default_value && !parsed_args.contains(dest))
					parsed_args.m_data.emplace(dest, *value->m_default_value);
			}
		}
	}

	expected<std::vector<std::string>, std::string> argument_parser_t::consume_until_flag_or_end(argument_consumer_t& consumer,
																								hashset_t<std::string>* allowed_choices)
	{
		std::vector<std::string> args;
		while (consumer.can_consume() && !consumer.peek().is_flag())
		{
			if (allowed_choices != nullptr && !allowed_choices->contains(consumer.peek().get_argument()))
			{
				std::string valid_choices = "{";
				for (const auto& [i, choice] : enumerate(*allowed_choices))
				{
					valid_choices += choice;
					if (i != allowed_choices->size() - 1)
						valid_choices += ", ";
				}
				valid_choices += "}";
				return unexpected(valid_choices);
			}
			args.emplace_back(consumer.consume().get_argument());
		}
		return args;
	}

	std::vector<std::string> argument_parser_t::consume_argc(const int argc, argument_consumer_t& consumer, hashset_t<std::string>* allowed_choices,
															const std::string_view arg)
	{
		std::vector<std::string> args;
		for (i32 i = 0; i < argc; ++i)
		{
			if (!consumer.can_consume())
			{
				throw detail::missing_argument_error(make_string("Expected ", argc, " arguments to be consumed by '", arg, "' but found ", i));
			}
			if (consumer.peek().is_flag())
			{
				std::cout << "Warning: arg '" << arg << "' expects " << argc << " arguments to be consumed but we found a flag '" << consumer.peek().
																																			get_argument()
				<< "'. We will comply as this may be desired if this argument is a file." << std::endl;
			}
			if (allowed_choices != nullptr && !allowed_choices->contains(consumer.peek().get_argument()))
			{
				std::string valid_choices = "{";
				for (const auto& [i, choice] : enumerate(*allowed_choices))
				{
					valid_choices += choice;
					if (i != allowed_choices->size() - 1)
						valid_choices += ", ";
				}
				valid_choices += "}";
				throw detail::bad_choice_error(make_string('\'', consumer.peek().get_argument(), "' is not a valid choice for argument '", arg,
															"'! Expected one of ", valid_choices));
			}
			args.emplace_back(consumer.consume().get_argument());
		}
		if (args.size() != static_cast<size_t>(argc))
		{
			throw std::runtime_error(
				"This error condition should not be possible. " "Args consumed didn't equal the arguments requested and previous checks didn't fail. "
				"Please report as an issue on the GitHub");
		}
		return args;
	}

	std::pair<argument_string_t, argument_storage_t> argument_subparser_t::parse(argument_consumer_t& consumer)
	{
		if (!consumer.can_consume())
			throw detail::missing_argument_error("Subparser requires an argument.");
		const auto key = consumer.consume();
		if (key.is_flag())
			throw detail::subparse_error(key.get_argument(), to_vec(get_allowed_strings()));
		const auto it = m_aliases.find(key.get_name());
		if (it == m_aliases.end())
			throw detail::subparse_error(key.get_argument(), to_vec(get_allowed_strings()));
		it->second->m_name = m_parent->m_name;
		m_last_parsed_parser = key.get_name();
		return {key, it->second->parse(consumer)};
	}

	hashmap_t<argument_parser_t*, std::vector<std::string_view>> argument_subparser_t::get_allowed_strings() const
	{
		hashmap_t<argument_parser_t*, std::vector<std::string_view>> map;
		for (const auto& [key, value] : m_aliases)
			map[value].emplace_back(key);
		return map;
	}

	std::vector<std::vector<std::string_view>> argument_subparser_t::to_vec(const hashmap_t<argument_parser_t*, std::vector<std::string_view>>& map)
	{
		std::vector<std::vector<std::string_view>> vec;
		for (const auto& [key, value] : map)
			vec.push_back(value);
		return vec;
	}

	namespace detail
	{
		// Unit Tests for class argument_string_t
		// Test Case 1: Ensure the constructor handles flags correctly
		void test_argument_string_t_flag_basic(const hashset_t<char>& prefixes)
		{
			const argument_string_t arg("-f", prefixes);
			BLT_ASSERT(arg.is_flag() && "Expected argument to be identified as a flag.");
			BLT_ASSERT(arg.value() == "f" && "Flag value should match the input string.");
		}

		// Test Case 2: Ensure the constructor handles long flags correctly
		void test_argument_string_t_long_flag(const hashset_t<char>& prefixes)
		{
			const argument_string_t arg("--file", prefixes);
			BLT_ASSERT(arg.is_flag() && "Expected argument to be identified as a flag.");
			BLT_ASSERT(arg.value() == "file" && "Long flag value should match the input string.");
		}

		// Test Case 3: Ensure positional arguments are correctly identified
		void test_argument_string_t_positional_argument(const hashset_t<char>& prefixes)
		{
			const argument_string_t arg("filename.txt", prefixes);
			BLT_ASSERT(!arg.is_flag() && "Expected argument to be identified as positional.");
			BLT_ASSERT(arg.value() == "filename.txt" && "Positional argument value should match the input string.");
		}

		// Test Case 5: Handle an empty string
		void test_argument_string_t_empty_input(const hashset_t<char>& prefixes)
		{
			const argument_string_t arg("", prefixes);
			BLT_ASSERT(!arg.is_flag() && "Expected an empty input to be treated as positional, not a flag.");
			BLT_ASSERT(arg.value().empty() && "Empty input should have an empty value.");
		}

		// Test Case 6: Handle edge case of a single hyphen (`-`) which might be ambiguous
		void test_argument_string_t_single_hyphen(const hashset_t<char>& prefixes)
		{
			const argument_string_t arg("-", prefixes);
			BLT_ASSERT(arg.is_flag() && "Expected single hyphen (`-`) to be treated as a flag.");
			BLT_ASSERT(arg.value().empty() && "Single hyphen flag should have empty value.");
			BLT_ASSERT(arg.get_flag() == "-" && "Single hyphen flag should match the input string.");
		}

		// Test Case 8: Handle arguments with prefix only (like "--")
		void test_argument_string_t_double_hyphen(const hashset_t<char>& prefixes)
		{
			const argument_string_t arg("--", prefixes);
			BLT_ASSERT(arg.is_flag() && "Double hyphen ('--') should be treated as a flag.");
			BLT_ASSERT(arg.value().empty() && "Double hyphen flag should have empty value.");
			BLT_ASSERT(arg.get_flag() == "--" && "Double hyphen value should match the input string.");
		}

		// Test Case 9: Validate edge case of an argument with spaces
		void test_argument_string_t_with_spaces(const hashset_t<char>& prefixes)
		{
			const argument_string_t arg("  ", prefixes);
			BLT_ASSERT(!arg.is_flag() && "Arguments with spaces should not be treated as flags.");
			BLT_ASSERT(arg.value() == "  " && "Arguments with spaces should match the input string.");
		}

		// Test Case 10: Validate arguments with numeric characters
		void test_argument_string_t_numeric_flag(const hashset_t<char>& prefixes)
		{
			const argument_string_t arg("-123", prefixes);
			BLT_ASSERT(arg.is_flag() && "Numeric flags should still be treated as flags.");
			BLT_ASSERT(arg.value() == "123" && "Numeric flag value should match the input string.");
		}

		// Test Case 11: Ensure the constructor handles '+' flag correctly
		void test_argument_string_t_plus_flag_basic(const hashset_t<char>& prefixes)
		{
			const argument_string_t arg("+f", prefixes);
			BLT_ASSERT(arg.is_flag() && "Expected argument to be identified as a flag.");
			BLT_ASSERT(arg.value() == "f" && "Plus flag value should match the input string.");
		}

		// Test Case 13: Handle edge case of a single plus (`+`) which might be ambiguous
		void test_argument_string_t_single_plus(const hashset_t<char>& prefixes)
		{
			const argument_string_t arg("+", prefixes);
			BLT_ASSERT(arg.is_flag() && "Expected single plus (`+`) to be treated as a flag.");
			BLT_ASSERT(arg.value().empty() && "Single plus flag should have empty value.");
			BLT_ASSERT(arg.get_flag() == "+" && "Single plus flag should match the input string.");
		}

		// Test Case 14: Handle arguments with prefix only (like '++')
		void test_argument_string_t_double_plus(const hashset_t<char>& prefixes)
		{
			const argument_string_t arg("++", prefixes);
			BLT_ASSERT(arg.is_flag() && "Double plus ('++') should be treated as a flag.");
			BLT_ASSERT(arg.value().empty() && "Double plus flag should have empty value.");
			BLT_ASSERT(arg.get_flag() == "++" && "Double plus value should match the input string.");
		}

		void run_all_tests_argument_string_t()
		{
			const hashset_t<char> prefixes = {'-', '+'};
			test_argument_string_t_flag_basic(prefixes);
			test_argument_string_t_long_flag(prefixes);
			test_argument_string_t_positional_argument(prefixes);
			test_argument_string_t_empty_input(prefixes);
			test_argument_string_t_single_hyphen(prefixes);
			test_argument_string_t_double_hyphen(prefixes);
			test_argument_string_t_with_spaces(prefixes);
			test_argument_string_t_numeric_flag(prefixes);
			test_argument_string_t_plus_flag_basic(prefixes);
			test_argument_string_t_single_plus(prefixes);
			test_argument_string_t_double_plus(prefixes);
		}

		void test_argparse_empty()
		{
			const std::vector<std::string> argv{"./program"};
			argument_parser_t parser;
			const auto args = parser.parse(argv);
			BLT_ASSERT(args.size() == 0 && "Empty argparse should have no args on output");
		}

		void test_single_flag_prefixes()
		{
			argument_parser_t parser;
			parser.add_flag("-a").set_action(action_t::STORE_TRUE);
			parser.add_flag("+b").set_action(action_t::STORE_FALSE);
			parser.add_flag("/c").as_type<u32>().set_action(action_t::STORE);

			const std::vector<std::string> args = {"./program", "-a", "+b", "/c", "42"};
			const auto parsed_args = parser.parse(args);

			BLT_ASSERT(parsed_args.get<bool>("-a") == true && "Flag '-a' should store `true`");
			BLT_ASSERT(parsed_args.get<bool>("+b") == false && "Flag '+b' should store `false`");
			BLT_ASSERT(parsed_args.get<u32>("/c") == 42 && "Flag '/c' should store the value 42");
		}

		// Test: Invalid flag prefixes
		void test_invalid_flag_prefixes()
		{
			argument_parser_t parser;
			parser.add_flag("-a");
			parser.add_flag("+b");
			parser.add_flag("/c");

			const std::vector<std::string> args = {"./program", "!d", "-a"};
			try
			{
				parser.parse(args);
				BLT_ASSERT(false && "Parsing should fail with invalid flag prefix '!'");
			} catch (...)
			{
				BLT_ASSERT(true && "Correctly threw on bad flag prefix");
			}
		}

		void test_compound_flags()
		{
			argument_parser_t parser;
			parser.add_flag("-v").set_action(action_t::COUNT);

			const std::vector<std::string> args = {"./program", "-vvv"};
			const auto parsed_args = parser.parse(args);

			BLT_ASSERT(parsed_args.get<u64>("-v") == 3 && "Flag '-v' should count occurrences in compound form");
		}

		void test_combination_of_valid_and_invalid_flags()
		{
			using namespace argparse;

			argument_parser_t parser;
			parser.add_flag("-x").as_type<int>();
			parser.add_flag("/y").as_type<std::string>();

			const std::vector<std::string> args = {"./program", "-x", "10", "!z", "/y", "value"};
			try
			{
				parser.parse(args);
				BLT_ASSERT(false && "Parsing should fail due to invalid flag '!z'");
			} catch (...)
			{
				BLT_ASSERT(true && "Correctly threw an exception for invalid flag");
			}
		}

		void test_flags_with_different_actions()
		{
			using namespace argparse;

			argument_parser_t parser;
			parser.add_flag("-k").as_type<int>().set_action(action_t::STORE); // STORE action
			parser.add_flag("-t").as_type<int>().set_action(action_t::STORE_CONST).set_const(999); // STORE_CONST action
			parser.add_flag("-f").set_action(action_t::STORE_FALSE); // STORE_FALSE action
			parser.add_flag("-c").set_action(action_t::STORE_TRUE); // STORE_TRUE action

			const std::vector<std::string> args = {"./program", "-k", "100", "-t", "-f", "-c"};
			const auto parsed_args = parser.parse(args);

			BLT_ASSERT(parsed_args.get<int>("-k") == 100 && "Flag '-k' should store 100");
			BLT_ASSERT(parsed_args.get<int>("-t") == 999 && "Flag '-t' should store a const value of 999");
			BLT_ASSERT(parsed_args.get<bool>("-f") == false && "Flag '-f' should store `false`");
			BLT_ASSERT(parsed_args.get<bool>("-c") == true && "Flag '-c' should store `true`");
		}

		// Helper function to simulate argument parsing for nargs tests
		bool parse_arguments(const std::vector<std::string_view>& args, const nargs_v expected_nargs)
		{
			argument_parser_t parser;

			std::vector<argument_string_t> arg_strings;
			arg_strings.reserve(args.size());
			for (const auto& arg : args)
				arg_strings.emplace_back(arg, parser.get_allowed_flag_prefixes());
			argument_consumer_t consumer{arg_strings};

			parser.add_positional("positional").set_nargs(expected_nargs);
			try
			{
				auto parsed_args = parser.parse(consumer);
				return consumer.remaining() == 0;
			} catch (const std::exception&)
			{
				return false;
			}
		}

		// Test case for nargs = 0
		void test_nargs_0()
		{
			std::cout << "[Running Test: test_nargs_0]\n";

			// Valid case: No arguments
			const std::vector<std::string_view> valid_args = {"./program"};
			BLT_ASSERT(!parse_arguments(valid_args, 0) && "nargs=0: Should fail");

			// Invalid case: 1 argument
			const std::vector<std::string_view> invalid_args = {"./program", "arg1"};
			BLT_ASSERT(!parse_arguments(invalid_args, 0) && "nargs=0: Should not accept any arguments");

			std::cout << "Success: test_nargs_0\n";
		}

		// Test case for nargs = 1
		void test_nargs_1()
		{
			std::cout << "[Running Test: test_nargs_1]\n";

			// Valid case: 1 argument
			const std::vector<std::string_view> valid_args = {"./program", "arg1"};
			BLT_ASSERT(parse_arguments(valid_args, 1) && "nargs=1: Should accept exactly 1 argument");

			// Invalid case: 0 arguments
			const std::vector<std::string_view> invalid_args_0 = {"./program"};
			BLT_ASSERT(!parse_arguments(invalid_args_0, 1) && "nargs=1: Should not accept 0 arguments");

			// Invalid case: 2 arguments
			const std::vector<std::string_view> invalid_args_2 = {"./program", "arg1", "arg2"};
			BLT_ASSERT(!parse_arguments(invalid_args_2, 1) && "nargs=1: Should not accept more than 1 argument");

			std::cout << "Success: test_nargs_1\n";
		}

		// Test case for nargs = 2
		void test_nargs_2()
		{
			std::cout << "[Running Test: test_nargs_2]\n";

			// Valid case: 2 arguments
			const std::vector<std::string_view> valid_args = {"./program", "arg1", "arg2"};
			BLT_ASSERT(!parse_arguments(valid_args, 2) && "nargs=2: Should fail as action is store");

			// Invalid case: 0 arguments
			const std::vector<std::string_view> invalid_args_0 = {"./program"};
			BLT_ASSERT(!parse_arguments(invalid_args_0, 2) && "nargs=2: Should not accept 0 arguments");

			// Invalid case: 1 argument
			const std::vector<std::string_view> invalid_args_1 = {"./program", "arg1"};
			BLT_ASSERT(!parse_arguments(invalid_args_1, 2) && "nargs=2: Should not accept less than 2 arguments");

			// Invalid case: 3 arguments
			const std::vector<std::string_view> invalid_args_3 = {"./program", "arg1", "arg2", "arg3"};
			BLT_ASSERT(!parse_arguments(invalid_args_3, 2) && "nargs=2: Should not accept more than 2 arguments");

			std::cout << "Success: test_nargs_2\n";
		}

		void test_nargs_all()
		{
			std::cout << "[Running Test: test_nargs_all]\n";

			// Valid case: No arguments
			const std::vector<std::string_view> valid_args_0 = {"./program"};
			BLT_ASSERT(!parse_arguments(valid_args_0, argparse::nargs_t::ALL) && "nargs=ALL: No arguments present. Should fail.)");

			// Valid case: Multiple arguments
			const std::vector<std::string_view> valid_args_2 = {"./program", "arg1", "arg2"};
			BLT_ASSERT(parse_arguments(valid_args_2, argparse::nargs_t::ALL) && "nargs=ALL: Should accept all remaining arguments");

			// Valid case: Many arguments
			const std::vector<std::string_view> valid_args_many = {"./program", "arg1", "arg2", "arg3", "arg4"};
			BLT_ASSERT(parse_arguments(valid_args_many, argparse::nargs_t::ALL) && "nargs=ALL: Should accept all remaining arguments");

			std::cout << "Success: test_nargs_all\n";
		}

		// Test case for nargs_t::ALL_AT_LEAST_ONE
		void test_nargs_all_at_least_one()
		{
			std::cout << "[Running Test: test_nargs_all_at_least_one]\n";

			// Valid case: 1 argument
			const std::vector<std::string_view> valid_args_1 = {"./program", "arg1"};
			BLT_ASSERT(parse_arguments(valid_args_1, argparse::nargs_t::ALL_AT_LEAST_ONE) &&
				"nargs=ALL_AT_LEAST_ONE: Should accept at least one argument and consume it");

			// Valid case: Multiple arguments
			const std::vector<std::string_view> valid_args_3 = {"./program", "arg1", "arg2", "arg3"};
			BLT_ASSERT(parse_arguments(valid_args_3, argparse::nargs_t::ALL_AT_LEAST_ONE) &&
				"nargs=ALL_AT_LEAST_ONE: Should accept at least one argument and consume all remaining arguments");

			// Invalid case: No arguments
			const std::vector<std::string_view> invalid_args_0 = {"./program"};
			BLT_ASSERT(!parse_arguments(invalid_args_0, argparse::nargs_t::ALL_AT_LEAST_ONE) &&
				"nargs=ALL_AT_LEAST_ONE: Should reject if no arguments are provided");

			std::cout << "Success: test_nargs_all_at_least_one\n";
		}

		void run_combined_flag_test()
		{
			std::cout << "[Running Test: run_combined_flag_test]\n";
			argument_parser_t parser;

			parser.add_flag("-a").set_action(action_t::STORE_TRUE);
			parser.add_flag("--deep").set_action(action_t::STORE_FALSE);
			parser.add_flag("-b", "--combined").set_action(action_t::STORE_CONST).set_const(50);
			parser.add_flag("--append").set_action(action_t::APPEND).as_type<int>();
			parser.add_flag("--required").set_required(true);
			parser.add_flag("--default").set_default("I am a default value");
			parser.add_flag("-t").set_action(action_t::APPEND_CONST).set_dest("test").set_const(5);
			parser.add_flag("-g").set_action(action_t::APPEND_CONST).set_dest("test").set_const(10);
			parser.add_flag("-e").set_action(action_t::APPEND_CONST).set_dest("test").set_const(15);
			parser.add_flag("-f").set_action(action_t::APPEND_CONST).set_dest("test").set_const(20);
			parser.add_flag("-d").set_action(action_t::APPEND_CONST).set_dest("test").set_const(25);
			parser.add_flag("--end").set_action(action_t::EXTEND).set_dest("wow").as_type<float>();

			const auto a1 = make_arguments("-a", "--required", "hello");
			const auto r1 = parser.parse(a1);
			BLT_ASSERT(r1.get<bool>("-a") == true && "Flag '-a' should store true");
			BLT_ASSERT(r1.get<std::string>("--default") == "I am a default value" && "Flag '--default' should store default value");
			BLT_ASSERT(r1.get("--required") == "hello" && "Flag '--required' should store 'hello'");

			const auto a2 = make_arguments("-a", "--deep", "--required", "soft");
			const auto r2 = parser.parse(a2);
			BLT_ASSERT(r2.get<bool>("-a") == true && "Flag '-a' should store true");
			BLT_ASSERT(r2.get<bool>("--deep") == false && "Flag '--deep' should store false");
			BLT_ASSERT(r2.get("--required") == "soft" && "Flag '--required' should store 'soft'");

			const auto a3 = make_arguments("--required", "silly", "--combined", "-t", "-f", "-e");
			const auto r3 = parser.parse(a3);
			BLT_ASSERT((r3.get<std::vector<int>>("test") == std::vector{5, 20, 15}) && "Flags should add to vector of {5, 20, 15}");
			BLT_ASSERT(r3.get<int>("-b") == 50 && "Combined flag should store const of 50");

			const auto a4 = make_arguments("--required", "crazy", "--end", "10", "12.05", "68.11", "100.00", "200532", "-d", "-t", "-g", "-e", "-f");
			const auto r4 = parser.parse(a4);
			BLT_ASSERT(
				(r4.get<std::vector<int>>("test") == std::vector{25, 5, 10, 15, 20}) &&
				"Expected test vector to be filled with all arguments in order of flags");
			BLT_ASSERT(
				(r4.get<std::vector<float>>("wow") == std::vector<float>{10, 12.05, 68.11, 100.00, 200532}) &&
				"Extend vector expected to contain all elements");

			std::cout << "Success: run_combined_flag_test\n";
		}

		void run_choice_test()
		{
			std::cout << "[Running Test: run_choice_test]\n";
			argument_parser_t parser;

			parser.add_flag("--hello").set_choices("silly", "crazy", "soft");
			parser.add_positional("iam").set_choices("different", "choices", "for", "me");

			const auto a1 = make_arguments("--hello", "crazy", "different");
			const auto r1 = parser.parse(a1);
			BLT_ASSERT(r1.get("--hello") == "crazy" && "Flag '--hello' should store 'crazy'");
			BLT_ASSERT(r1.get("iam") == "different" && "Positional 'iam' should store 'different'");

			const auto a2 = make_arguments("--hello", "not_an_option", "different");
			try
			{
				parser.parse(a2);
				BLT_ASSERT(false && "Parsing should fail due to invalid flag '--hello'");
			} catch (...)
			{}

			const auto a3 = make_arguments("--hello", "crazy", "not_a_choice");
			try
			{
				parser.parse(a3);
				BLT_ASSERT(false && "Parsing should fail due to invalid positional 'iam'");
			} catch (...)
			{}

			std::cout << "Success: run_choice_test\n";
		}

		void run_subparser_test()
		{
			std::cout << "[Running Test: run_subparser_test]\n";
			argument_parser_t parser;

			parser.add_flag("--open").make_flag();

			const auto subparser = parser.add_subparser("mode");

			const auto n1 = subparser->add_parser("n1");
			n1->add_flag("--silly").make_flag();
			n1->add_positional("path");

			const auto n2 = subparser->add_parser("n2");
			n2->add_flag("--crazy").make_flag();
			n2->add_positional("path");
			n2->add_positional("output");

			const auto n3 = subparser->add_parser("n3");
			n3->add_flag("--deep").make_flag();

			const auto a1 = make_arguments("n1", "--silly");
			try
			{
				parser.parse(a1);
				BLT_ASSERT(false && "Subparser should throw an error when positional not supplied");
			} catch (...)
			{}

			const auto a2 = make_arguments("--open");
			try
			{
				parser.parse(a2);
				BLT_ASSERT(false && "Subparser should throw an error when no subparser is supplied");
			} catch (...)
			{}

			const auto a3 = make_arguments("n1", "--silly", "path_n1");
			const auto r3 = parser.parse(a3);
			BLT_ASSERT(r3.get<bool>("--open") == false && "Flag '--open' should default to false");
			BLT_ASSERT(r3.get("mode") == "n1" && "Subparser should store 'n1'");
			BLT_ASSERT(r3.get("path") == "path_n1" && "Subparser path should be 'path'");

			const auto a4 = make_arguments("n2", "--crazy", "path");

			try
			{
				parser.parse(a4);
				BLT_ASSERT(false && "Subparser should throw an error when second positional is not supplied");
			} catch (...)
			{}

			const auto a5 = make_arguments("--open", "n2", "path_n2", "output_n2");
			const auto r5 = parser.parse(a5);
			BLT_ASSERT(r5.get<bool>("--open") == true && "Flag '--open' should store true");
			BLT_ASSERT(r5.get("mode") == "n2" && "Subparser should store 'n2'");
			BLT_ASSERT(r5.get("path") == "path_n2" && "Subparser path should be 'path'");
			BLT_ASSERT(r5.get("output") == "output_n2" && "Subparser output should be 'output'");

			const auto a6 = make_arguments("not_an_option", "silly");

			try
			{
				parser.parse(a6);
				BLT_ASSERT(false && "Subparser should throw an error when first positional is not a valid subparser");
			} catch (const std::exception&)
			{}

			const auto a7 = make_arguments("n3");
			const auto r7 = parser.parse(a7);
			BLT_ASSERT(r7.get("mode") == "n3" && "Subparser should store 'n3'");

			std::cout << "Success: run_subparser_test\n";
		}

		void run_argparse_flag_tests()
		{
			test_single_flag_prefixes();
			test_invalid_flag_prefixes();
			test_compound_flags();
			test_combination_of_valid_and_invalid_flags();
			test_flags_with_different_actions();
			run_combined_flag_test();
			run_choice_test();
			run_subparser_test();
		}

		void run_all_nargs_tests()
		{
			test_nargs_0();
			test_nargs_1();
			test_nargs_2();
			test_nargs_all();
			test_nargs_all_at_least_one();
			std::cout << "All nargs tests passed successfully.\n";
		}

		void test()
		{
			run_all_tests_argument_string_t();
			test_argparse_empty();
			run_argparse_flag_tests();
			run_all_nargs_tests();
		}

		[[nodiscard]] std::string subparse_error::error_string() const
		{
			std::string message = "Subparser Error: ";
			message += m_found_string;
			message += " is not a valid command. Allowed commands are: {";
			for (const auto [i, allowed_string] : enumerate(m_allowed_strings))
			{
				if (allowed_string.size() > 1)
					message += '[';
				for (const auto [j, alias] : enumerate(allowed_string))
				{
					message += alias;
					if (static_cast<i64>(j) < static_cast<i64>(allowed_string.size()) - 2)
						message += ", ";
					else if (static_cast<i64>(j) < static_cast<i64>(allowed_string.size()) - 1)
						message += ", or ";
				}
				if (allowed_string.size() > 1)
					message += ']';
				if (i != m_allowed_strings.size() - 1)
					message += ", ";
			}
			message += "}";
			return message;
		}
	}
}
