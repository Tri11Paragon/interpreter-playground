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

#ifndef BLT_MATH_AABB_H
#define BLT_MATH_AABB_H

#include <array>
#include <blt/math/vectors.h>
#include <blt/std/types.h>

namespace blt
{
	// yes I could use the vector (see tower defense game commit log for this)
	// this feels nicer
	template <typename T = float>
	class axis_t
	{
	public:
		axis_t(const T min, const T max): m_min(min), m_max(max)
		{}

		[[nodiscard]] bool intersects(const T p) const
		{
			return p >= m_min && p <= m_max;
		}

		template <typename G>
		[[nodiscard]] bool intersects(const axis_t<G>& other) const
		{
			return static_cast<T>(other.m_min) <= m_max && static_cast<T>(other.m_max) >= m_min;
		}

		[[nodiscard]] T min() const
		{
			return m_min;
		}

		[[nodiscard]] T max() const
		{
			return m_max;
		}

		[[nodiscard]] T length() const
		{
			return m_max - m_min;
		}

	private:
		T m_min, m_max;
	};

	namespace detail
	{
		template <u32 Axis, typename T>
		class axis_aligned_bounding_box_base_t
		{
		public:
			[[nodiscard]] vec<T, Axis> get_center() const
			{
				vec<T, Axis> min;
				for (u32 i = 0; i < Axis; i++)
					min[i] = m_axes[i].min();
				const auto center = get_size() / 2.0f;
				return min + center;
			}

			[[nodiscard]] vec<T, Axis> get_size() const
			{
				vec<T, Axis> size;
				for (u32 i = 0; i < Axis; i++)
					size[i] = m_axes[i].length();
				return size;
			}

			template <typename G>
			[[nodiscard]] bool intersects(const axis_aligned_bounding_box_base_t<Axis, G>& other) const
			{
				for (u32 i = 0; i < Axis; i++)
					if (!m_axes[i].intersects(other.m_axes[i]))
						return false;
				return true;
			}

			template <typename G>
			[[nodiscard]] bool intersects(const vec<G, Axis>& point) const
			{
				for (u32 i = 0; i < Axis; i++)
					if (!m_axes[i].intersects(point[i]))
						return false;
				return true;
			}

			axis_t<T>& operator[](u32 i)
			{
				return m_axes[i];
			}

			axis_t<T>& axis(u32 i)
			{
				if (i >= Axis)
					throw std::out_of_range("Axis index out of range");
				return m_axes[i];
			}

		protected:
			std::array<axis_t<T>, Axis> m_axes;
		};
	}

	template <u32 Axis, typename T>
	class axis_aligned_bounding_box_t : public detail::axis_aligned_bounding_box_base_t<Axis, T>
	{
	public:
		using detail::axis_aligned_bounding_box_base_t<Axis, T>::axis_aligned_bounding_box_base_t;
	};

	template <typename T>
	class axis_aligned_bounding_box_t<2, T> : public detail::axis_aligned_bounding_box_base_t<2, T>
	{
	public:
		using detail::axis_aligned_bounding_box_base_t<2, T>::axis_aligned_bounding_box_base_t;

		[[nodiscard]] vec2 min() const
		{
			return {this->m_axes[0].min(), this->m_axes[1].min()};
		}

		[[nodiscard]] vec2 max() const
		{
			return {this->m_axes[0].max(), this->m_axes[1].max()};
		}
	};

	template <typename T>
	class axis_aligned_bounding_box_t<3, T> : public detail::axis_aligned_bounding_box_base_t<3, T>
	{
	public:
		using detail::axis_aligned_bounding_box_base_t<2, T>::axis_aligned_bounding_box_base_t;

		[[nodiscard]] vec3 min() const
		{
			return {this->m_axes[0].min(), this->m_axes[1].min(), this->m_axes[2].min()};
		}

		[[nodiscard]] vec3s max() const
		{
			return {this->m_axes[0].max(), this->m_axes[1].max(), this->m_axes[2].max()};
		}
	};

	using aabb_2d_t = axis_aligned_bounding_box_t<2, float>;
	using aabb_3d_t = axis_aligned_bounding_box_t<3, float>;
}

#endif //BLT_MATH_AABB_H
