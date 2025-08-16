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

#ifndef BLT_INTERPOLATION_H
#define BLT_INTERPOLATION_H

#include "vectors.h"

namespace blt
{
    inline constexpr color4 linear_interpolate(const color4& in, const color4& desired, float factor)
    {
        auto diff = desired - in;
        return in + (diff * factor);
    }
    
    class easing_function
    {
        public:
            easing_function() = default;
            
            virtual color4 apply(const color4& start, const color4& end) = 0;
            
            void progress(float progress)
            {
                total_progress += progress;
            }
            
            void reset()
            {
                total_progress = 0;
            }
        
        protected:
            float x()
            {
                return total_progress;
            }
        
        private:
            float total_progress = 0;
    };
    
    class quad_easing : public easing_function
    {
        public:
            color4 apply(const color4& start, const color4& end) final
            {
                if (x() >= 1)
                    return end;
                auto diff = end - start;
                return start + (diff * (x() * x()));
            }
    };
    
    class cubic_easing : public easing_function
    {
        public:
            color4 apply(const color4& start, const color4& end) final
            {
                if (x() >= 1)
                    return end;
                auto diff = end - start;
                return start + (diff * (x() * x() * x()));
            }
    };
    
    class quart_easing : public easing_function
    {
        public:
            color4 apply(const color4& start, const color4& end) final
            {
                if (x() >= 1)
                    return end;
                auto diff = end - start;
                return start + (diff * (x() * x() * x() * x()));
            }
    };
    
    class quint_easing : public easing_function
    {
        public:
            color4 apply(const color4& start, const color4& end) final
            {
                if (x() >= 1)
                    return end;
                auto diff = end - start;
                return start + (diff * (x() * x() * x() * x() * x()));
            }
    };
    
}

#endif //BLT_INTERPOLATION_H
