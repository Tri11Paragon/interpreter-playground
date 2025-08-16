/*
 *  <Short Description>
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

#ifndef BLT_FIXED_POINT_H
#define BLT_FIXED_POINT_H

#include <blt/std/types.h>
//#include <blt/std/utility.h>

//#define BLT_DEBUG_NO_INLINE BLT_ATTRIB_NO_INLINE
#define BLT_DEBUG_NO_INLINE

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#endif
namespace blt
{
    
    struct fp64
    {
        private:
            i64 v = 0;
            
            constexpr fp64() = default;
        public:
            constexpr explicit fp64(u64 ui): v(from_u64(ui))
            {}
            
            constexpr explicit fp64(i64 si): v(from_i64(si))
            {}
            
            constexpr explicit fp64(u32 ui): v(from_u32(ui))
            {}
            
            constexpr explicit fp64(i32 si): v(from_i32(si))
            {}
            
            constexpr explicit fp64(f32 f): v(from_f32(f))
            {}
            
            constexpr explicit fp64(f64 d): v(from_f64(d))
            {}
            
            constexpr static inline fp64 from_raw(i64 i)
            {
                fp64 r;
                r.v = i;
                return r;
            }
            
            constexpr static inline fp64 from_raw_u64(u64 u)
            {
                fp64 r;
                r.v = static_cast<i64>(u);
                return r;
            }
            
            constexpr static inline fp64 from_u64(u64 ui)
            {
                return from_i64(static_cast<i64>(ui));
            }
            
            constexpr static inline fp64 from_i64(i64 si)
            {
                return from_raw(si << 32);
            }
            
            constexpr static inline fp64 from_u32(u32 ui)
            {
                return from_i64(static_cast<i64>(ui));
            }
            
            constexpr static inline fp64 from_i32(i32 si)
            {
                return from_i64(static_cast<i64>(si));
            }
            
            constexpr static inline fp64 from_f64(f64 d)
            {
                fp64 fp;
                fp.v = static_cast<i64>(d * (1ul << 32ul));
                return fp;
            }
            
            constexpr static inline fp64 from_f32(f32 f)
            {
                return from_f64(static_cast<double>(f));
            }
            
            BLT_DEBUG_NO_INLINE constexpr friend inline fp64 operator+(fp64 left, fp64 right)
            {
                return from_raw(left.v + right.v);
            }
            
            BLT_DEBUG_NO_INLINE constexpr friend inline fp64 operator-(fp64 left, fp64 right)
            {
                return from_raw(left.v - right.v);
            }
            
            BLT_DEBUG_NO_INLINE constexpr friend inline fp64 operator*(fp64 left, fp64 right)
            {
                auto lhs = static_cast<__int128>(left.v);
                auto rhs = static_cast<__int128>(right.v);
                return from_raw(static_cast<i64>((lhs * rhs) >> 32));
            }
            
            BLT_DEBUG_NO_INLINE constexpr friend inline fp64 operator/(fp64 left, fp64 right)
            {
                auto lhs = static_cast<__int128>(left.v) << 32;
                return from_raw(static_cast<i64>(lhs / right.v));
            }
            
            BLT_DEBUG_NO_INLINE constexpr inline fp64& operator+=(fp64 add)
            {
                v += add.v;
                return *this;
            }
            
            BLT_DEBUG_NO_INLINE constexpr inline fp64& operator-=(fp64 sub)
            {
                v -= sub.v;
                return *this;
            }
            
            BLT_DEBUG_NO_INLINE constexpr inline fp64& operator*=(fp64 mul)
            {
                auto lhs = static_cast<__int128>(v);
                auto rhs = static_cast<__int128>(mul.v);
                v = static_cast<i64>((lhs * rhs) >> 32);
                return *this;
            }
            
            BLT_DEBUG_NO_INLINE constexpr inline fp64& operator/=(fp64 div)
            {
                auto lhs = static_cast<__int128>(v) << 32;
                v = static_cast<i64>(lhs / div.v);
                return *this;
            }
            
            [[nodiscard]] constexpr inline u64 as_u64() const
            {
                return static_cast<u64>(v) >> 32;
            }
            
            [[nodiscard]] constexpr inline i64 as_i64() const
            {
                return v >> 32;
            }
            
            [[nodiscard]] constexpr inline u32 as_u32() const
            {
                return static_cast<u32>(v >> 32);
            }
            
            [[nodiscard]] constexpr inline i32 as_i32() const
            {
                return static_cast<i32>(v >> 32);
            }
            
            [[nodiscard]] constexpr inline f64 as_f64() const
            {
                return static_cast<f64>(v) / static_cast<f64>(1ul << 32ul);
            }
            
            [[nodiscard]] constexpr inline f32 as_f32() const
            {
                return static_cast<f32>(v) / static_cast<f32>(1ul << 32ul);
            }
            
            constexpr inline explicit operator u64() const
            {
                return as_u64();
            }
            
            constexpr inline explicit operator i64() const
            {
                return as_i64();
            }
            
            constexpr inline explicit operator u32() const
            {
                return as_u32();
            }
            
            constexpr inline explicit operator i32() const
            {
                return as_i32();
            }
            
            constexpr inline explicit operator f32() const
            {
                return as_f32();
            };
            
            constexpr inline explicit operator f64() const
            {
                return as_f64();
            }
            
            [[nodiscard]] constexpr u64 raw() const
            {
                return v;
            }
    };
    
    // max unsigned integer value
    static constexpr const inline fp64 FP64_UMAX = fp64::from_raw_u64(0xFFFFFFFF00000000);
    // min unsigned integer value
    static constexpr const inline fp64 FP64_UMIN = fp64::from_raw_u64(0x0000000000000000);
    // max signed integer value
    static constexpr const inline fp64 FP64_IMAX = fp64::from_raw_u64(0x7FFFFFFF00000000);
    // min signed integer value
    static constexpr const inline fp64 FP64_IMIN = fp64::from_raw_u64(0x8000000000000000);
    // max value storable including floating point
    static constexpr const inline fp64 FP64_FMAX = fp64::from_raw_u64(0x7FFFFFFFFFFFFFFF);
    // min float point number
    static constexpr const inline fp64 FP64_FMIN = fp64::from_raw_u64(0x8000000000000000);
    // smallest decimal number
    static constexpr const inline fp64 FP64_EPSILON = fp64::from_raw_u64(0x0000000000000001);
    // pi
    static constexpr const inline fp64 FP64_PI = fp64::from_f64(3.14159265358979323846);
    // pi / 2
    static constexpr const inline fp64 FP64_PI_2 = fp64::from_f64(1.57079632679489661923);
    // pi / 4
    static constexpr const inline fp64 FP64_PI_4 = fp64::from_f64(0.78539816339744830962);
    // 1 / pi
    static constexpr const inline fp64 FP64_1_PI = fp64::from_f64(0.31830988618379067154);
    // 2 / pi
    static constexpr const inline fp64 FP64_2_PI = fp64::from_f64(0.63661977236758134308);
    // sqrt(2)
    static constexpr const inline fp64 FP64_SQRT2 = fp64::from_f64(1.41421356237309504880);
    // 1 / sqrt(2)
    static constexpr const inline fp64 FP64_1_SQRT2 = fp64::from_f64(0.70710678118654752440);
    // e
    static constexpr const inline fp64 FP64_E = fp64::from_f64(2.7182818284590452354f);
    // log2(e)
    static constexpr const inline fp64 FP64_LOG2E = fp64::from_f64(1.4426950408889634074f);
}

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

#endif //BLT_FIXED_POINT_H
