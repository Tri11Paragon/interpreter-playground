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
#include <blt/math/fixed_point.h>
#include <blt/math/fixed_point_vectors.h>
#include <blt/math/vectors.h>
#include <blt/math/log_util.h>
#include <blt/std/logging.h>
#include <iostream>
#include <ios>
#include <blt_tests.h>
#include <cmath>
#include <iomanip>
#include <limits>

namespace blt::test
{
    void print(fp64 v, const std::string& name = "")
    {
        std::cout << name << " [" << v.raw() << ':' << std::hex << v.raw() << std::dec << "]\tu64(" << v.as_u64() << ")\ti64(" << v.as_i64()
                  << ")\tu32(" << v.as_u32() << ")\ti32(" << v.as_i32() << ")\tf32("
                  << std::setprecision(std::numeric_limits<long double>::digits10 + 1)
                  << std::setw(16) << std::fixed << v.as_f32() << ")\tf64(" << v.as_f64() << ")\n";
    }
    
    void fixed_point()
    {
        blt::vec3f vf(34, 34, 2);
        blt::vec3ul vul(34, 34, 2);
        
        std::cout << vf << std::endl;
        BLT_TRACE(vf);
        BLT_TRACE(vul);
        BLT_DEBUG_STREAM << vul << '\n';
        
        blt::mat4x4 mat;
        std::cout << mat << std::endl;
        BLT_TRACE(mat);
        BLT_DEBUG_STREAM << mat << '\n';
        
        
        //vec3fp hello = {fp64::from_f64(32.023), fp64::from_f64(422.34023), fp64::from_f64(321.023)};
        
        print(FP64_UMAX, "umax");
        print(FP64_UMIN, "umin");
        print(FP64_IMAX, "imax");
        print(FP64_IMIN, "imin");
        print(FP64_FMAX, "fmax");
        print(FP64_FMIN, "fmin");
        print(FP64_EPSILON, "epis");
        print(FP64_PI, "pi  ");
        print(FP64_PI_2, "pi2 ");
        print(FP64_PI_4, "pi4 ");
        print(FP64_1_PI, "1/pi");
        print(FP64_2_PI, "1/p2");
        print(FP64_SQRT2, "sqr2");
        print(FP64_1_SQRT2, "isq2");
        print(FP64_E, "e   ");
        print(FP64_LOG2E, "logE");
        
        fp64 uv = fp64::from_u64(32);
        fp64 iv = fp64::from_i64(16);
        
        fp64 fv = fp64::from_f32(53.4234234);
        fp64 pi = fp64::from_f64(M_PI);
        
        print(uv * iv, "32 * 16");
        print(uv / iv, "32 / 16");
        print(fv / pi, "53.4234234 / pi");
        
        print(uv + iv, "32 + 16");
        print(uv - iv, "32 - 16");
        
        print(fp64::from_f32(32.43242), "32.43242");
        print(fp64::from_f64(634.2349932493423), "634.2349932493423");
        print(fp64::from_u32(3194967295), "3194967295");
        print(fp64::from_i32(-1194967295), "-1194967295");
        print(fp64::from_i64(-13194967295), "-13194967295");
        print(fp64::from_u64(66294967295), "66294967295");
        
        uv *= fp64::from_i32(-32);
        
        print(uv);
        
        uv /= fp64::from_i32(-16);
        
        print(uv);
    }
}