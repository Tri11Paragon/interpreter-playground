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

#ifndef BLT_LOGGING_COLORS_H
#define BLT_LOGGING_COLORS_H

#include <array>
#include <stdexcept>
#include <string>
#include <variant>
#include <blt/std/types.h>

#define BLT_ANSI_ESCAPE "\x1B"
#define BLT_ANSI_CSI BLT_ANSI_ESCAPE "["
#define BLT_ANSI_DSC BLT_ANSI_ESCAPE "P"
#define BLT_ANSI_OSC BLT_ANSI_ESCAPE "]"

// https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
namespace blt::logging::ansi
{
	namespace color
	{
		inline std::array<u8, 10> reset_sequences = {0, 22, 22, 23, 24, 25, 26, 27, 28, 29};

		enum class color_mode : u8
		{
			RESET_ALL      = 0,
			BOLD           = 1,
			DIM            = 2,
			ITALIC         = 3,
			UNDERLINE      = 4,
			BLINK          = 5,
			REVERSE        = 7,
			HIDDEN         = 8,
			STRIKE_THROUGH = 9, };

		enum class color8 : u8
		{
			BLACK   = 0,
			RED     = 1,
			GREEN   = 2,
			YELLOW  = 3,
			BLUE    = 4,
			MAGENTA = 5,
			CYAN    = 6,
			WHITE   = 7,
			DEFAULT = 9
		};

		enum class color8_bright : u8
		{
			BLACK   = 0,
			RED     = 1,
			GREEN   = 2,
			YELLOW  = 3,
			BLUE    = 4,
			MAGENTA = 5,
			CYAN    = 6,
			WHITE   = 7
		};

		struct rgb_t
		{
			u8 r, g, b;
		};

		struct color256
		{
			explicit color256(u8 index) : color(index)
			{}

			color256(const u8 r, const u8 g, const u8 b) : color(rgb_t{r, g, b})
			{
				if (r > 5)
					throw std::invalid_argument("r must be between 0 and 5");
				if (g > 5)
					throw std::invalid_argument("g must be between 0 and 5");
				if (b > 5)
					throw std::invalid_argument("b must be between 0 and 5");
			}

			[[nodiscard]] u8 index() const
			{
				if (std::holds_alternative<u8>(color))
					return std::get<u8>(color);
				const auto [r, g, b] = std::get<rgb_t>(color);
				return (r * 36) + (g * 6) + b + 16;
			}

		private:
			std::variant<u8, rgb_t> color;
		};

		struct color_rgb
		{
			color_rgb(const u8 r, const u8 g, const u8 b) : color(rgb_t{r, g, b})
			{}

			rgb_t color;
		};

		namespace detail
		{
			template <typename T>
			struct color_holder
			{
				using value_type = T;

				T color;
				bool alt = false;
			};

			template <typename T>
			struct color_converter
			{};

			template <>
			struct color_converter<color8>
			{
				static std::string to_string(const color_holder<color8> color)
				{
					return (color.alt ? "4" : "3") + std::to_string(static_cast<u8>(color.color));
				}
			};

			template <>
			struct color_converter<color8_bright>
			{
				static std::string to_string(const color_holder<color8_bright> color)
				{
					return (color.alt ? "10" : "9") + std::to_string(static_cast<u8>(color.color));
				}
			};

			template <>
			struct color_converter<color_mode>
			{
				static std::string to_string(const color_holder<color_mode> color)
				{
					return color.alt ? std::to_string(reset_sequences[static_cast<u8>(color.color)]) : std::to_string(static_cast<u8>(color.color));
				}
			};

			template <>
			struct color_converter<color256>
			{
				static std::string to_string(const color_holder<color256> color)
				{
					return (color.alt ? "48;5;" : "38;5;") + std::to_string(color.color.index());
				}
			};

			template <>
			struct color_converter<color_rgb>
			{
				static std::string to_string(const color_holder<color_rgb> color)
				{
					return (color.alt ? "48;2;" : "38;2;") + std::to_string(color.color.color.r) + ";" + std::to_string(color.color.color.g) + ";" +
					std::to_string(color.color.color.b);
				}
			};

			template <typename T>
			struct ensure_holder
			{
				static color_holder<T> make(const T& color)
				{
					return color_holder<T>{color, false};
				}
			};

			template <typename T>
			struct ensure_holder<color_holder<T>>
			{
				static color_holder<T> make(const color_holder<T>& color)
				{
					return color;
				}
			};

			template<typename T>
			struct decay
			{
				using type = std::decay_t<T>;
			};

			template<typename T>
			struct decay<color_holder<T>>
			{
				using type = std::decay_t<T>;
			};

			template<typename T>
			using decay_t = typename decay<T>::type;
		}

		template <typename T>
		auto fg(const T& color)
		{
			return detail::color_holder<T>{color, false};
		}

		template <typename T>
		auto bg(const T& color)
		{
			return detail::color_holder<T>{color, true};
		}

		template <typename... Args>
		std::string build(const Args&... args)
		{
			std::string result = BLT_ANSI_CSI;
			((result += detail::color_converter<detail::decay_t<Args>>::to_string(detail::ensure_holder<Args>::make(args)), result += ';'), ...);
			return result.substr(0, result.size() - 1) + "m";
		}
	}

	namespace general
	{
		inline const std::string bell = "\x07";
		inline const std::string bs = "\x08";
		inline const std::string horizontal_tab = "\x09";
		inline const std::string linefeed = "\x0A";
		inline const std::string vertical_tab = "\x0B";
		inline const std::string form_feed = "\x0C";
		inline const std::string carriage_return = "\x0D";
		inline const std::string escape = BLT_ANSI_ESCAPE;
		inline const std::string del = "\x7F";
		inline const std::string csi = BLT_ANSI_CSI;
		inline const std::string dsc = BLT_ANSI_DSC;
		inline const std::string osc = BLT_ANSI_OSC;
	}

	namespace cursor
	{
		inline const std::string home = BLT_ANSI_CSI "H";
		inline const std::string lower_left_corner = BLT_ANSI_ESCAPE " F";
		inline const std::string hide_cursor = BLT_ANSI_CSI "?2 5l";
		inline const std::string show_cursor = BLT_ANSI_CSI "?2 5h";
		inline const std::string report_position = BLT_ANSI_CSI "6n";

		template <bool UseH = true>
		std::string move_to(const i64 line, const i64 column)
		{
			if constexpr (UseH)
				return std::string(BLT_ANSI_CSI) + std::to_string(line) + ";" + std::to_string(column) + "H";
			else
				return std::string(BLT_ANSI_CSI) + std::to_string(line) + ";" + std::to_string(column) + "f";
		}

		inline std::string move_up(const i64 lines)
		{
			return std::string(BLT_ANSI_CSI) + std::to_string(lines) + "A";
		}

		inline std::string move_down(const i64 lines)
		{
			return std::string(BLT_ANSI_CSI) + std::to_string(lines) + "B";
		}

		inline std::string move_right(const i64 columns)
		{
			return std::string(BLT_ANSI_CSI) + std::to_string(columns) + "C";
		}

		inline std::string move_left(const i64 columns)
		{
			return std::string(BLT_ANSI_CSI) + std::to_string(columns) + "D";
		}

		inline std::string move_begin_down(const i64 lines)
		{
			return std::string(BLT_ANSI_CSI) + std::to_string(lines) + "E";
		}

		inline std::string move_begin_up(const i64 lines)
		{
			return std::string(BLT_ANSI_CSI) + std::to_string(lines) + "F";
		}

		inline std::string move_to(const i64 column)
		{
			return std::string(BLT_ANSI_CSI) + std::to_string(column) + "G";
		}

		inline const std::string request_cursor_position = BLT_ANSI_CSI "6n";
		inline const std::string move_up_one_line = BLT_ANSI_ESCAPE " M";
		inline const std::string save_cursor_position_dec = BLT_ANSI_ESCAPE " 7";
		inline const std::string restore_cursor_position_dec = BLT_ANSI_ESCAPE " 8";
		inline const std::string save_cursor_position_sco = BLT_ANSI_CSI "s";
		inline const std::string restore_cursor_position_sco = BLT_ANSI_CSI "u";
	}

	namespace scroll
	{
		inline std::string scroll_up(const int lines)
		{
			return std::string(BLT_ANSI_CSI) + std::to_string(lines) + "S";
		};
	}

	namespace erase
	{
		inline const std::string to_end_of_screen = BLT_ANSI_CSI "0J";
		inline const std::string from_begin_of_screen = BLT_ANSI_CSI "1J";
		inline const std::string entire_screen = BLT_ANSI_CSI "2J";
		inline const std::string saved_lines = BLT_ANSI_CSI "3J";
		inline const std::string to_end_of_line = BLT_ANSI_CSI "0K";
		inline const std::string from_begin_of_line = BLT_ANSI_CSI "1K";
		inline const std::string entire_line = BLT_ANSI_CSI "2K";
	}

	enum class mode : u8
	{
		mono40x25_text = 0,
		color40x25_text = 1,
		mono80x25_text = 2,
		color80x25_text = 3,
		color320x200_4color_graphics = 4,
		mono320x200_graphics = 5,
		mono640x200_graphics = 6,
		line_wrapping = 7,
		color320x200_graphics = 13,
		color640x200_16color_graphics = 14,
		mono640x350_2color_graphics = 15,
		color640x350_16color_graphics = 16,
		mono640x480_2color_graphics = 17,
		color640x480_16color_graphics = 18,
		color320_200_256color_graphics = 19
	};

	inline std::string use_mode(const mode mode)
	{
		return std::string(BLT_ANSI_CSI) + "=" + std::to_string(static_cast<u8>(mode)) + "h";
	}
}

#endif //BLT_LOGGING_COLORS_H
