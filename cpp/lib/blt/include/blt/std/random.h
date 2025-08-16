/*
 * Created by Brett on 04/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_RANDOM_H
#define BLT_RANDOM_H

#include <blt/std/types.h>
#include <random>

namespace blt::random
{
    // https://github.com/avaneev/komihash/tree/main
    
    constexpr static inline blt::u32 pcg_hash32(blt::u32 input)
    {
        blt::u32 state = input * 747796405u + 2891336453u;
        blt::u32 word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
        return (word >> 22u) ^ word;
    }
    
    //https://lemire.me/blog/2018/08/15/fast-strongly-universal-64-bit-hashing-everywhere/
    constexpr static inline blt::u64 murmur64(blt::u64 h)
    {
        h ^= h >> 33;
        h *= 0xff51afd7ed558ccdL;
        h ^= h >> 33;
        h *= 0xc4ceb9fe1a85ec53L;
        h ^= h >> 33;
        return h;
    }
    
    constexpr static inline double pcg_double32(blt::u32& seed)
    {
        seed = pcg_hash32(seed);
        return static_cast<double>(seed) / static_cast<double>(std::numeric_limits<blt::u32>::max());
    }
    
    constexpr static inline float pcg_float32(blt::u32& seed)
    {
        return static_cast<float>(pcg_double32(seed));
    }
    
    /**
     * @return random float without changing seed
     */
    constexpr static inline float pcg_float32c(blt::u32 seed)
    {
        return pcg_float32(seed);
    }
    
    constexpr static inline double pcg_double32c(blt::u32 seed)
    {
        return pcg_double32(seed);
    }
    
    /**
     * @param seed seed for random
     * @param min inclusive min
     * @param max exclusive max
     * @return random int between min (inclusive) and max (exclusive)
     */
    template<typename T = blt::i32>
    constexpr static inline T pcg_random32(blt::u32& seed, T min = 0, T max = 2)
    {
        return static_cast<T>((pcg_double32(seed) * static_cast<double>(max - min)) + static_cast<double>(min));
    }
    
    template<typename T = blt::i32>
    constexpr static inline T pcg_random32c(blt::u32 seed, T min = 0, T max = 2)
    {
        return pcg_random32(seed, min, max);
    }
    
    
    constexpr static inline double murmur_double64(blt::u64& seed)
    {
        seed = murmur64(seed);
        return static_cast<double>(seed) / static_cast<double>(std::numeric_limits<blt::u64>::max());
    }
    
    constexpr static inline float murmur_float64(blt::u64& seed)
    {
        return static_cast<float>(murmur_double64(seed));
    }
    
    constexpr static inline float murmur_float64c(blt::u64 seed)
    {
        return murmur_float64(seed);
    }
    
    constexpr static inline double murmur_double64c(blt::u64 seed)
    {
        return murmur_double64(seed);
    }
    
    template<typename T = blt::i32>
    constexpr static inline T murmur_random64(blt::u64& seed, T min = 0, T max = 2)
    {
        return static_cast<T>((murmur_double64(seed) * static_cast<double>(max - min)) + static_cast<double>(min));
    }
    
    template<typename T = blt::i32>
    constexpr static inline T murmur_random64c(blt::u64 seed, T min = 0, T max = 2)
    {
        return murmur_random64(seed, min, max);
    }

#define BLT_RANDOM_FUNCTION blt::random::murmur_random64
#define BLT_RANDOM_FLOAT blt::random::murmur_float64
#define BLT_RANDOM_DOUBLE blt::random::murmur_double64
    
    class random_t
    {
        public:
            using result_type = blt::u64;
            constexpr static result_type MIN = std::numeric_limits<result_type>::min();
            constexpr static result_type MAX = std::numeric_limits<result_type>::max() - 1;
            
            explicit constexpr random_t(blt::u64 seed): seed(seed)
            {}
            
            constexpr void set_seed(blt::u64 s)
            {
                seed = s;
            }
            
            constexpr float get_float()
            {
                return BLT_RANDOM_FLOAT(seed);
            }
            
            constexpr double get_double()
            {
                return BLT_RANDOM_DOUBLE(seed);
            }
            
            // [min, max)
            constexpr double get_double(double min, double max)
            {
                return BLT_RANDOM_FUNCTION(seed, min, max);
            }
            
            // [min, max)
            constexpr float get_float(float min, float max)
            {
                return BLT_RANDOM_FUNCTION(seed, min, max);
            }
            
            constexpr i32 get_i32(i32 min, i32 max)
            {
                return BLT_RANDOM_FUNCTION(seed, min, max);
            }
            
            constexpr u32 get_u32(u32 min, u32 max)
            {
                return BLT_RANDOM_FUNCTION(seed, min, max);
            }
            
            constexpr i64 get_i64(i64 min, i64 max)
            {
                return BLT_RANDOM_FUNCTION(seed, min, max);
            }
            
            constexpr u64 get_u64(u64 min, u64 max)
            {
                return BLT_RANDOM_FUNCTION(seed, min, max);
            }
            
            constexpr blt::size_t get_size_t(blt::size_t min, blt::size_t max)
            {
                return BLT_RANDOM_FUNCTION(seed, min, max);
            }
            
            template<typename T>
            constexpr T get(T min, T max)
            {
                return BLT_RANDOM_FUNCTION(seed, min, max);
            }
            
            constexpr bool choice()
            {
                return BLT_RANDOM_DOUBLE(seed) < 0.5;
            }
            
            constexpr bool choice(double cutoff)
            {
                return BLT_RANDOM_DOUBLE(seed) <= cutoff;
            }
            
            template<typename Container>
            constexpr decltype(auto) select(Container& container)
            {
                return container[get_u64(0, container.size())];
            }
            
            constexpr static result_type min()
            {
                return MIN;
            }
            
            constexpr static result_type max()
            {
                return MAX;
            }
            
            constexpr result_type operator()()
            {
                return get_u64(min(), max() + 1);
            }
        
        private:
            blt::u64 seed;
    };
    
}

#endif //BLT_RANDOM_H
