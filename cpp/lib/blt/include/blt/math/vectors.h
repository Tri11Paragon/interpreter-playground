/*
 * Created by Brett on 28/02/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_TESTS_VECTORS_H
#define BLT_TESTS_VECTORS_H

#include <algorithm>
#include <initializer_list>
#include <cmath>
#include <vector>
#include <array>
#include <type_traits>
#include <blt/math/math.h>
#include <blt/std/types.h>

namespace blt
{
	#define MSVC_COMPILER (!defined(__GNUC__) && !defined(__clang__))

	constexpr float EPSILON = std::numeric_limits<float>::epsilon();

	template <typename T>
	T srgb_to_linear(const T c) noexcept { return (c <= 0.04045) ? c / 12.92 : std::pow((c + 0.055) / 1.055, 2.4); }

	template <typename T>
	T linear_to_srgb(const T c) noexcept
	{
		return (c <= 0.0031308) ? 12.92 * c : 1.055 * std::pow(c, 1.0 / 2.4) - 0.055;
	}

	constexpr bool f_equal(const float v1, const float v2, const float range = 1)
	{
		return v1 >= v2 - (EPSILON * range) && v1 <= v2 + (EPSILON * range);
	}


	template <typename T, blt::u32 size>
	struct vec
	{
		static_assert(std::is_arithmetic_v<T> && "blt::vec must be created using an arithmetic type!");

	private:
		std::array<T, size> elements;

	public:
		constexpr static blt::u32 data_size = size;

		constexpr vec()
		{
			for (auto& v : elements)
				v = static_cast<T>(0);
		}

		/**
		 * Create a vector with initializer list, if the initializer list doesn't contain enough values to fill this vec, it will use t
		 * @param t default value to fill with
		 * @param args list of args
		 */
		template <typename U, std::enable_if_t<std::is_same_v<T, U> || std::is_convertible_v<U, T>, bool>  = true>
		constexpr vec(U t, std::initializer_list<U> args): elements()
		{
			auto b = args.begin();
			for (auto& v : elements)
			{
				if (b == args.end())
				{
					v = t;
					continue;
				}
				v = *b;
				++b;
			}
		}

		/**
		 * Create a vector from an initializer list, if the list doesn't have enough elements it will be filled with the default value (0)
		 * @param args
		 */
		template <typename U, std::enable_if_t<std::is_same_v<T, U> || std::is_convertible_v<U, T>, bool>  = true>
		constexpr vec(std::initializer_list<U> args): vec(U(), args) {}

		template <typename... Args, std::enable_if_t<sizeof...(Args) == size, bool>  = true>
		constexpr explicit vec(Args... args): vec(std::array<T, size>{static_cast<T>(args)...}) {}

		constexpr explicit vec(T t)
		{
			for (auto& v : elements)
				v = t;
		}

		constexpr explicit vec(const T elem[size])
		{
			for (size_t i   = 0; i < size; i++)
				elements[i] = elem[i];
		}

		constexpr explicit vec(std::array<T, size> elem): elements(elem) {}

		template <typename G, size_t base_size, std::enable_if_t<std::is_convertible_v<G, T>, bool>  = true>
		constexpr explicit vec(std::array<G, base_size> el): elements()
		{
			auto b = el.begin();
			auto m = elements.begin();
			while (b != el.end() && m != elements.end())
			{
				*m = *b;
				++m;
				++b;
			}
		}

		[[nodiscard]] const std::array<T, size>& to_array() const { return elements; }

		[[nodiscard]] constexpr T x() const { return elements[0]; }

		[[nodiscard]] constexpr T y() const
		{
			static_assert(size > 1);
			return elements[1];
		}

		[[nodiscard]] constexpr T z() const
		{
			static_assert(size > 2);
			return elements[2];
		}

		[[nodiscard]] constexpr T w() const
		{
			static_assert(size > 3);
			return elements[3];
		}

		[[nodiscard]] constexpr T r() const { return elements[0]; }

		[[nodiscard]] constexpr T g() const
		{
			static_assert(size > 1);
			return elements[1];
		}

		[[nodiscard]] constexpr T b() const
		{
			static_assert(size > 2);
			return elements[2];
		}

		[[nodiscard]] constexpr T a() const
		{
			static_assert(size > 3);
			return elements[3];
		}

		[[nodiscard]] constexpr inline vec<T, size> abs() const
		{
			auto copy = *this;
			for (auto& v : copy.elements)
				v = std::abs(v);
			return copy;
		}

		[[nodiscard]] constexpr vec sqrt() const
		{
			auto copy = *this;
			for (auto& v : copy.elements)
				v = std::sqrt(v);
			return copy;
		}

		[[nodiscard]] constexpr inline vec<T, size> bipolar() const
		{
			auto copy = *this;
			for (auto& v : copy.elements)
				v = v >= 0 ? 1 : -1;
			return copy;
		}

		[[nodiscard]] constexpr inline T magnitude() const
		{
			T total = 0;
			for (blt::u32 i = 0; i < size; i++)
				total += elements[i] * elements[i];
			return std::sqrt(total);
		}

		[[nodiscard]] constexpr inline vec<T, size> normalize() const
		{
			T mag = this->magnitude();
			if (mag == 0)
				return vec<T, size>(*this);
			return *this / mag;
		}

		[[nodiscard]] constexpr vec linear_rgb_to_hsv() const
		{
			const T r = (*this)[0], g = (*this)[1], b = (*this)[2];

			const T max   = std::max({r, g, b});
			const T min   = std::min({r, g, b});
			const T delta = max - min;

			T h = T(0);
			T s = max == T(0) ? T(0) : delta / max;
			T v = max;

			if (delta != T(0))
			{
				if (max == r) { h = T(60) * std::fmod(((g - b) / delta), T(6)); } else if (max == g)
				{
					h = T(60) * (((b - r) / delta) + T(2));
				} else // max == b
				{
					h = T(60) * (((r - g) / delta) + T(4));
				}
				if (h < T(0))
					h += T(360);
			}
			return {h, s, v};
		}

		[[nodiscard]] constexpr vec hsv_to_linear_rgb() const
		{
			T       h = (*this)[0];
			const T s = (*this)[1];
			const T v = (*this)[2];

			if (s == T(0))
				return {v, v, v}; // achromatic

			h = std::fmod(h, T(360));
			if (h < T(0))
				h += T(360);
			h /= T(60);            // into sector 0-5
			const int i = static_cast<int>(std::floor(h));
			const T   f = h - T(i);

			const T p = v * (T(1) - s);
			const T q = v * (T(1) - s * f);
			const T t = v * (T(1) - s * (T(1) - f));

			switch (i)
			{
				case 0:
					return {v, t, p};
				case 1:
					return {q, v, p};
				case 2:
					return {p, v, t};
				case 3:
					return {p, q, v};
				case 4:
					return {t, p, v};
				default:
					return {v, p, q}; // case 5
			}
		}

		[[nodiscard]] constexpr vec srgb_to_linear_rgb() const
		{
			static_assert(size >= 3);
			vec copy = *this;
			copy[0]  = srgb_to_linear(copy[0]);
			copy[1]  = srgb_to_linear(copy[1]);
			copy[2]  = srgb_to_linear(copy[2]);
			return copy;
		}

		[[nodiscard]] constexpr vec linear_to_srgb() const
		{
			static_assert(size >= 3);
			vec copy = *this;
			copy[0]  = blt::linear_to_srgb(copy[0]);
			copy[1]  = blt::linear_to_srgb(copy[1]);
			copy[2]  = blt::linear_to_srgb(copy[2]);
			return copy;
		}

		/**
		 * Vector must be in linear RGB.
		 */
		[[nodiscard]] constexpr vec linear_rgb_to_oklab() const
		{
			static_assert(size >= 3);

			// 2. RGB → LMS
			const double l = 0.4122214708 * r() + 0.5363325363 * g() + 0.0514459929 * b();
			const double m = 0.2119034982 * r() + 0.6806995451 * g() + 0.1073969566 * b();
			const double s = 0.0883024619 * r() + 0.2817188376 * g() + 0.6299787005 * b();

			// 3. cube root
			const double l_ = cbrt(l);
			const double m_ = cbrt(m);
			const double s_ = cbrt(s);

			// 4. LMS′ → OKLab
			vec out;
			out[0] = 0.2104542553 * l_ + 0.7936177850 * m_ - 0.0040720468 * s_;
			out[1] = 1.9779984951 * l_ - 2.4285922050 * m_ + 0.4505937099 * s_;
			out[2] = 0.0259040371 * l_ + 0.7827717662 * m_ - 0.8086757660 * s_;

			return out;
		}

		[[nodiscard]] constexpr vec oklab_to_oklch() const
		{
			vec out;
			out[0] = r();
			out[1] = std::sqrt(g() * g() + b() * b());
			out[2] = blt::toDegrees(std::atan2(b(), g()));
			return out;
		}

		[[nodiscard]] constexpr vec oklch_to_oklab() const
		{
			vec out;
			out[0] = r();
			out[1] = g() * std::cos(blt::toRadians(b()));
			out[2] = g() * std::sin(blt::toRadians(b()));
			return out;
		}

		[[nodiscard]] vec oklab_to_linear_rgb() const
		{
			static_assert(size >= 3);
			auto copy = *this;
			/* 1. OKLab -> non-linear LMS′ */
			const double l_ = copy.r() + 0.3963377774 * copy.g() + 0.2158037573 * copy.b();
			const double m_ = copy.r() - 0.1055613458 * copy.g() - 0.0638541728 * copy.b();
			const double s_ = copy.r() - 0.0894841775 * copy.g() - 1.2914855480 * copy.b();

			/* 2. Undo cube root */
			const double l = l_ * l_ * l_;
			const double m = m_ * m_ * m_;
			const double s = s_ * s_ * s_;

			/* 3. LMS -> linear RGB */
			double R = 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s;
			double G = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s;
			double B = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s;

			/* 4. Optional: clamp to [0,1] before gamma */
			R = std::clamp(R, 0.0, 1.0);
			G = std::clamp(G, 0.0, 1.0);
			B = std::clamp(B, 0.0, 1.0);

			/* 5. Linear RGB -> sRGB */
			copy[0] = R;
			copy[1] = G;
			copy[2] = B;
			return copy;
		}

		constexpr inline T& operator[](blt::size_t index) { return elements[index]; }

		constexpr inline T operator[](blt::size_t index) const { return elements[index]; }

		constexpr inline vec<T, size>& operator=(T v)
		{
			for (blt::u32 i = 0; i < size; i++)
				elements[i] = v;
			return *this;
		}

		constexpr inline vec<T, size> operator-()
		{
			vec<T, size> initializer{};
			for (blt::u32 i    = 0; i < size; i++)
				initializer[i] = -elements[i];
			return vec<T, size>{initializer};
		}

		constexpr inline vec<T, size>& operator+=(const vec<T, size>& other)
		{
			for (blt::u32 i = 0; i < size; i++)
				elements[i] += other[i];
			return *this;
		}

		constexpr inline vec<T, size>& operator*=(const vec<T, size>& other)
		{
			for (blt::u32 i = 0; i < size; i++)
				elements[i] *= other[i];
			return *this;
		}

		constexpr inline vec<T, size>& operator+=(T f)
		{
			for (blt::u32 i = 0; i < size; i++)
				elements[i] += f;
			return *this;
		}

		constexpr inline vec<T, size>& operator*=(T f)
		{
			for (blt::u32 i = 0; i < size; i++)
				elements[i] *= f;
			return *this;
		}

		constexpr inline vec<T, size>& operator-=(const vec<T, size>& other)
		{
			for (blt::u32 i = 0; i < size; i++)
				elements[i] -= other[i];
			return *this;
		}

		constexpr inline vec<T, size>& operator-=(T f)
		{
			for (blt::u32 i = 0; i < size; i++)
				elements[i] -= f;
			return *this;
		}

		/**
		 * performs the dot product of left * right
		 */
		constexpr static inline T dot(const vec<T, size>& left, const vec<T, size>& right)
		{
			T dot = 0;
			for (blt::u32 i = 0; i < size; i++)
				dot += left[i] * right[i];
			return dot;
		}

		constexpr static inline vec<T, size> cross(
			const vec<T, size>& left,
			const vec<T, size>& right
			)
		{
			// cross is only defined on vectors of size 3. 2D could be implemented, which is a TODO
			static_assert(size == 3);
			return {
				left.y() * right.z() - left.z() * right.y(),
				left.z() * right.x() - left.x() * right.z(),
				left.x() * right.y() - left.y() * right.x()
			};
		}

		constexpr static inline vec<T, size> project(
			const vec<T, size>& u,
			const vec<T, size>& v
			)
		{
			T du = dot(u);
			T dv = dot(v);
			return (du / dv) * v;
		}

		constexpr inline auto* data() { return elements.data(); }

		[[nodiscard]] constexpr inline const auto* data() const { return elements.data(); }

		[[nodiscard]] constexpr auto begin() const { return elements.begin(); }

		[[nodiscard]] constexpr auto end() const { return elements.end(); }

		[[nodiscard]] constexpr auto rbegin() const { return elements.rbegin(); }

		[[nodiscard]] constexpr auto rend() const { return elements.rend(); }

		[[nodiscard]] constexpr auto cbegin() const { return elements.cbegin(); }

		[[nodiscard]] constexpr auto cend() const { return elements.cend(); }

		friend std::size_t hash_value(const vec& obj)
		{
			std::size_t seed = 0x5410391E;
			for (const auto& v : obj)
				seed ^= (seed << 6) + (seed >> 2) + std::hash<T>{}(v);
			return seed;
		}
	};


	template <typename T, typename G, blt::u32 size, typename R = decltype(std::declval<T>() + std::declval<G>())>
	inline constexpr vec<R, size> operator+(const vec<T, size>& left, const vec<G, size>& right)
	{
		vec<R, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<R>(left[i]) + static_cast<R>(right[i]);
		return initializer;
	}

	template <typename T, typename G, blt::u32 size, typename R = decltype(std::declval<T>() - std::declval<G>())>
	inline constexpr vec<R, size> operator-(const vec<T, size>& left, const vec<G, size>& right)
	{
		vec<R, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<R>(left[i]) - static_cast<R>(right[i]);
		return initializer;
	}

	template <typename T, typename G, blt::u32 size, typename R = decltype(std::declval<T>() + std::declval<G>())>
	inline constexpr vec<R, size> operator+(const vec<T, size>& left, G right)
	{
		vec<R, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<R>(left[i]) + static_cast<R>(right);
		return initializer;
	}

	template <typename T, typename G, blt::u32 size, typename R = decltype(std::declval<T>() - std::declval<G>())>
	inline constexpr vec<R, size> operator-(const vec<T, size>& left, G right)
	{
		vec<R, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<R>(left[i]) - static_cast<R>(right);
		return initializer;
	}

	template <typename T, typename G, blt::u32 size, typename R = decltype(std::declval<T>() + std::declval<G>())>
	inline constexpr vec<R, size> operator+(G left, const vec<T, size>& right)
	{
		vec<R, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<R>(left) + static_cast<R>(right[i]);
		return initializer;
	}

	template <typename T, typename G, blt::u32 size, typename R = decltype(std::declval<T>() - std::declval<G>())>
	inline constexpr vec<R, size> operator-(G left, const vec<T, size>& right)
	{
		vec<R, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<R>(left) - static_cast<R>(right[i]);
		return initializer;
	}

	template <typename T, typename G, blt::u32 size, typename R = decltype(std::declval<T>() * std::declval<G>())>
	inline constexpr vec<R, size> operator*(const vec<T, size>& left, const vec<G, size>& right)
	{
		vec<R, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<R>(left[i]) * static_cast<R>(right[i]);
		return initializer;
	}

	template <typename T, typename G, blt::u32 size, typename R = decltype(std::declval<T>() * std::declval<G>())>
	inline constexpr vec<R, size> operator*(const vec<T, size>& left, G right)
	{
		vec<R, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<R>(left[i]) * static_cast<R>(right);
		return initializer;
	}

	template <typename T, typename G, blt::u32 size, typename R = decltype(std::declval<T>() * std::declval<G>())>
	inline constexpr vec<R, size> operator*(G left, const vec<T, size>& right)
	{
		vec<R, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<R>(left) * static_cast<R>(right[i]);
		return initializer;
	}

	template <typename T, typename G, blt::u32 size, typename R = decltype(std::declval<T>() / std::declval<G>())>
	inline constexpr vec<R, size> operator/(const vec<T, size>& left, G right)
	{
		vec<R, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<R>(left[i]) / static_cast<R>(right);
		return initializer;
	}

	template <typename T, typename G, blt::u32 size, typename R = decltype(std::declval<T>() / std::declval<G>())>
	inline constexpr vec<R, size> operator/(G left, const vec<T, size>& right)
	{
		vec<R, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<R>(left) / static_cast<R>(right[i]);
		return initializer;
	}

	template <typename T, typename G, blt::u32 size>
	inline constexpr bool operator==(const vec<T, size>& left, const vec<G, size>& right)
	{
		constexpr double E = std::numeric_limits<T>::epsilon();
		for (blt::u32 i = 0; i < size; i++)
		{
			auto diff = left[i] - right[i];
			if (diff > E || diff < -E)
				return false;
		}
		return true;
	}

	template <typename T, typename G, u32 size>
	constexpr bool operator>=(const vec<T, size>& left, const vec<G, size>& right)
	{
		for (u32 i = 0; i < size; i++)
		{
			if (left[i] < right[i])
				return false;
		}
		return true;
	}

	template <typename T, typename G, u32 size>
	constexpr bool operator>(const vec<T, size>& left, const vec<G, size>& right)
	{
		for (u32 i = 0; i < size; i++)
		{
			if (left[i] <= right[i])
				return false;
		}
		return true;
	}

	template <typename T, typename G, u32 size>
	constexpr bool operator<(const vec<T, size>& left, const vec<G, size>& right)
	{
		for (u32 i = 0; i < size; i++)
		{
			if (left[i] >= right[i])
				return false;
		}
		return true;
	}

	template <typename T, typename G, u32 size>
	constexpr bool operator<=(const vec<T, size>& left, const vec<G, size>& right)
	{
		for (u32 i = 0; i < size; i++)
		{
			if (left[i] > right[i])
				return false;
		}
		return true;
	}

	template <typename T, typename G, blt::u32 size>
	inline constexpr bool operator!=(const vec<T, size>& left, const vec<G, size>& right) { return !(left == right); }

	template <typename Ret, typename T, blt::u32 size>
	inline constexpr vec<Ret, size> vec_cast(const vec<T, size>& conv)
	{
		vec<Ret, size> initializer{};
		for (blt::u32 i    = 0; i < size; i++)
			initializer[i] = static_cast<Ret>(conv[i]);
		return initializer;
	}

	using vec2f = vec<float, 2>;
	using vec3f = vec<float, 3>;
	using vec4f = vec<float, 4>;

	using vec2d = vec<double, 2>;
	using vec3d = vec<double, 3>;
	using vec4d = vec<double, 4>;

	using vec2i = vec<blt::i32, 2>;
	using vec3i = vec<blt::i32, 3>;
	using vec4i = vec<blt::i32, 4>;

	using vec2l = vec<blt::i64, 2>;
	using vec3l = vec<blt::i64, 3>;
	using vec4l = vec<blt::i64, 4>;

	using vec2ui = vec<blt::u32, 2>;
	using vec3ui = vec<blt::u32, 3>;
	using vec4ui = vec<blt::u32, 4>;

	using vec2ul = vec<blt::u64, 2>;
	using vec3ul = vec<blt::u64, 3>;
	using vec4ul = vec<blt::u64, 4>;

	using vec2 = vec2f;
	using vec3 = vec3f;
	using vec4 = vec4f;

	using color4 = vec4;
	using color3 = vec3;

	inline constexpr color4 make_color(float r, float g, float b) { return color4{r, g, b, 1.0f}; }

	template <typename ValueType, u32 size>
	inline constexpr blt::vec<ValueType, 2> make_vec2(const blt::vec<ValueType, size>& t, size_t fill = 0)
	{
		if constexpr (size >= 2) { return blt::vec<ValueType, 2>(t.x(), t.y()); } else
		{
			return blt::vec<ValueType, 2>(t.x(), fill);
		}
	}

	template <typename ValueType, u32 size>
	inline constexpr blt::vec<ValueType, 3> make_vec3(const blt::vec<ValueType, size>& t, size_t fill = 0)
	{
		if constexpr (size >= 3) { return blt::vec<ValueType, 3>(t.x(), t.y(), t.z()); } else
		{
			blt::vec<ValueType, 3> ret;
			for (size_t i = 0; i < size; i++)
				ret[i]    = t[i];
			for (size_t i = size; i < 3; i++)
				ret[i]    = fill;
			return ret;
		}
	}

	template <typename ValueType, u32 size>
	inline constexpr blt::vec<ValueType, 4> make_vec4(const blt::vec<ValueType, size>& t, size_t fill = 0)
	{
		if constexpr (size >= 4) { return blt::vec<ValueType, 4>(t.x(), t.y(), t.z(), t.w()); } else
		{
			blt::vec<ValueType, 4> ret;
			for (size_t i = 0; i < size; i++)
				ret[i]    = t[i];
			for (size_t i = size; i < 4; i++)
				ret[i]    = fill;
			return ret;
		}
	}

	namespace vec_algorithm
	{
		static inline void findOrthogonalBasis(const vec3& v, vec3& v1, vec3& v2, vec3& v3)
		{
			v1 = v.normalize();

			vec3 arbitraryVector{1, 0, 0};
			if (std::abs(vec3::dot(v, arbitraryVector)) > 0.9) { arbitraryVector = vec3{0, 1, 0}; }

			v2 = vec3::cross(v, arbitraryVector).normalize();
			v3 = vec3::cross(v1, v2);
		}

		// Gram-Schmidt orthonormalization algorithm
		static inline void gramSchmidt(std::vector<vec3>& vectors)
		{
			int               n = (int) vectors.size();
			std::vector<vec3> basis;

			// normalize first vector
			basis.push_back(vectors[0]);
			basis[0] = basis[0].normalize();

			// iterate over the rest of the vectors
			for (int i = 1; i < n; ++i)
			{
				// subtract the projections of the vector onto the previous basis vectors
				vec3 new_vector = vectors[i];
				for (int j = 0; j < i; ++j)
				{
					float projection = vec3::dot(vectors[i], basis[j]);
					new_vector[0] -= projection * basis[j].x();
					new_vector[1] -= projection * basis[j].y();
					new_vector[2] -= projection * basis[j].z();
				}
				// normalize the new basis vector
				new_vector = new_vector.normalize();
				basis.push_back(new_vector);
			}

			vectors = basis;
		}
	}
}

#endif //BLT_TESTS_VECTORS_H
