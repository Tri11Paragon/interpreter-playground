/*
 * Created by Brett on 23/08/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */
#include <cstring>
#include <exception>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <blt/logging/logging.h>
#include <blt/std/assert.h>
#include <blt/std/string.h>
#include <blt/std/utility.h>

// TODO
#undef BLT_STACK_TRACE

struct abort_exception final : std::exception
{
public:
	explicit abort_exception(const char* what)
	{
		auto len = std::strlen(what) + 1;
		error = static_cast<char*>(std::malloc(len));
		std::memcpy(static_cast<char*>(error), what, len);
	}

	abort_exception(const abort_exception& copy) = delete;
	abort_exception& operator=(const abort_exception& copy) = delete;

	[[nodiscard]] const char* what() const noexcept override
	{
		if (error == nullptr)
			return "Abort called";
		return error;
	}

	~abort_exception() override
	{
		std::free(static_cast<void*>(error));
	}

private:
	char* error{nullptr};
};

#ifdef BLT_HAS_BACKTRACE
	#include BLT_BACKTRACE_HEADER
#endif

#ifdef BLT_HAS_BETTER_BACKTRACE
#include <backtrace.h>
#endif

namespace blt
{
	#ifdef BLT_HAS_BACKTRACE
	static std::string _macro_filename(const std::string& path)
	{
		auto paths = string::split(path, "/");
		auto final = paths[paths.size() - 1];
		if (final == "/")
			return paths[paths.size() - 2];
		return final;
	}
	#endif

	void b_throw(const char* what, const char* path, int line)
	{
		BLT_ERROR("An exception '{}' has occurred in file '{}:{:d}'", what, path, line);
		BLT_ERROR("Stack Trace:");
		detail::print_stack_trace(path, line);
	}

	void b_assert_failed(const char* expression, const char* msg, const char* path, int line)
	{
		BLT_ERROR("The assertion '{}' has failed in file '{}:{:d}'", expression, path, line);
		if (msg != nullptr)
			BLT_ERROR(msg);
		BLT_ERROR("Stack Trace:");
		detail::print_stack_trace(path, line);
		if (msg != nullptr)
			throw abort_exception(msg);
		throw abort_exception(expression);
	}

	void b_abort(const char* what, const char* path, int line)
	{
		BLT_FATAL("----\\{BLT ABORT}----");
		BLT_FATAL("\tWhat: {}", what);
		BLT_FATAL("\tCalled from {}:{:d}", path, line);
		detail::print_stack_trace(path, line);
		throw abort_exception(what);
	}

	namespace detail
	{
		#ifdef BLT_HAS_BETTER_BACKTRACE
		static backtrace_state* state = nullptr;

		void error_callback(void*, const char* msg, int errnum)
		{
			BLT_ERROR("LibBacktrace Error ({}): {}", errnum, msg);
		}

		int full_callback(void* data, uintptr_t, const char* filename, const int lineno, const char* function)
		{
			if (function == nullptr || filename == nullptr)
				return 1;
			auto& tabs = *static_cast<size_t*>(data);
			std::string msg;
			for (size_t i = 0; i < tabs; i++)
				msg += '\t';
			if (tabs != 0)
				msg += "⮡";
			msg += filename;
			msg += ':';
			msg += std::to_string(lineno);
			msg += " in ";
			msg += demangle(function);

			BLT_ERROR(msg);
			++tabs;
			return 0;
		}
		#endif

		void print_stack_trace(const char* path, int line)
		{
			#if defined(BLT_HAS_BETTER_BACKTRACE)
			if (!state)
				state = backtrace_create_state(nullptr, 1, error_callback, nullptr);
			size_t tabs = 0;
			backtrace_full(state, 1, full_callback, error_callback, &tabs);
			(void) (path);
			(void) (line);
			#elif defined(BLT_HAS_BACKTRACE)
			constexpr auto number = 50;
			void* ptrs[number];
			const int size = backtrace(ptrs, number);
			char** messages = backtrace_symbols(ptrs, size);
			for (int i = 1; i < size; i++)
			{
				int tabs = i - 1;
				std::string buffer;
				for (int j = 0; j < tabs; j++)
					buffer += '\t';
				if (i != 1)
					buffer += "⮡";

				std::string message(messages[i]);

				auto f = message.find('(');

				auto mes = message.substr(f + 1, message.size());
				std::string loc;

				buffer += message.substr(0, f);
				if (i == 1)
				{
					loc = '\'';
					loc += _macro_filename(path);
					loc += ':';
					loc += std::to_string(line);
					loc += '\'';
				} else
					loc = demangle(mes.substr(0, mes.find('+')));

				if (!loc.empty())
					buffer += " in ";
				buffer += loc;

				BLT_ERROR(buffer);
			}
			free(messages);
			#else
            (void) path;
            (void) line;
			BLT_ERROR("No backtrace support available");
			#endif
		}
	}
}
