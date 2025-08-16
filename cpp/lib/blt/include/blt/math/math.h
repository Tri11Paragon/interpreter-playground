/*
 * Created by Brett on 09/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_MATH_H
#define BLT_MATH_H

#include <blt/std/types.h>

namespace blt
{
    
    static inline constexpr double PI = 3.141592653589793238462643383279502884197;
    
    template<typename T>
    static constexpr T toRadians(T deg)
    {
        constexpr double CONV = PI / 180.0;
        return deg * CONV;
    }

    template<typename T>
    static constexpr T toDegrees(T rad)
    {
        constexpr double CONV = 180.0 / PI;
        return rad * CONV;
    }

    
    /**
     * fast number integer
     */
    static inline unsigned int f_randi(unsigned int seed)
    {
        seed = (seed << 13) ^ seed;
        return ((seed * (seed * seed * 15731 + 789221) + 1376312589) & 0x7fffffff);
    }

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#endif
    
    /**
     * fast inverse sqrt
     */
    static inline float fsqrt(float n)
    {
        int i;
        float x, y;
        x = n * 0.5f;
        y = n;
        i = *reinterpret_cast<int*>(&y);
        i = 0x5f3759df - (i >> 1);
        y = *reinterpret_cast<float*>(&i);
        y = y * (1.5f - (x * y * y));
        y = y * (1.5f - (x * y * y));
        return y;
    }

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
    
    
    template<typename B, typename P, typename R = decltype(B() * P())>
    static inline constexpr R pow(B b, P p)
    {
        R collection = 1;
        for (int i = 0; i < p; i++)
            collection *= b;
        return collection;
    }
    
    template<blt::i64 decimal_places>
    struct round_up_t
    {
        constexpr inline double operator()(double value)
        {
            if constexpr (decimal_places < 0)
                return value;
            else
            {
                constexpr double multiplier = pow(10.0, decimal_places);
                auto i_value = static_cast<blt::i64>(value * multiplier);
                auto f_value = (value * multiplier) - static_cast<double>(i_value);
                if (f_value > 0)
                    return ((static_cast<double>(i_value) + 1) / multiplier);
                else
                    return static_cast<double>(i_value);
            }
        }
    };
    
    template<blt::i64 decimal_places>
    struct round_down_t
    {
        constexpr inline double operator()(double value)
        {
            if constexpr (decimal_places < 0)
                return value;
            else
            {
                constexpr double multiplier = pow(10.0, decimal_places);
                return (static_cast<blt::i64>(value * multiplier)) / multiplier;
            }
        }
    };
    
    /**
     * This is a fast rounding function and is not guaranteed to be 100% correct
     * @tparam decimal_places
     * @param value
     * @return
     */
    template<blt::i64 decimal_places>
    constexpr static inline double round_up(double value)
    {
        
        round_up_t<decimal_places> round_func;
        return round_func(value);
    }
    
    template<blt::i64 decimal_places>
    constexpr static inline double round_down(double value)
    {
        
        round_down_t<decimal_places> round_func;
        return round_func(value);
    }
    
    /*inline std::ostream& operator<<(std::ostream& out, const mat4x4& v) {
        return out << "\rMatrix4x4{" << v.m00() << ", " << v.m01() << ", " << v.m02() << ", " << v.m03() << "} \n"\
 << "         {" << v.m10() << ", " << v.m11() << ", " << v.m12() << ", " << v.m13() << "} \n"\
 << "         {" << v.m20() << ", " << v.m21() << ", " << v.m22() << ", " << v.m23() << "} \n"\
 << "         {" << v.m30() << ", " << v.m31() << ", " << v.m32() << ", " << v.m33() << "} \n";
    }*/
    
}

#endif //BLT_MATH_H
