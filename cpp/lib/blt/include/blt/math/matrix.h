/*
 * Created by Brett on 28/02/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_TESTS_MATRIX_H
#define BLT_TESTS_MATRIX_H

#include <cstring>
#include <type_traits>
#include <array>
#include <initializer_list>
#include "blt/iterator/iterator.h"
#include <blt/math/vectors.h>

#ifndef M_PI
// MSVC does not have M_PI
# define M_PI		3.14159265358979323846
#endif

namespace blt
{
    
    template<typename T, blt::u32 rows, blt::u32 columns>
    class generalized_matrix
    {
        public:
            static constexpr auto data_rows = rows;
            static constexpr auto data_columns = columns;
        private:
            using matrix_t = generalized_matrix<T, rows, columns>;
            enum class init_type
            {
                EMPTY,
                IDENTITY
            };
            
            constexpr explicit generalized_matrix(init_type type)
            {
                switch (type)
                {
                    case init_type::EMPTY:
                        break;
                    case init_type::IDENTITY:
                        set_identity();
                        break;
                }
            }
        
        public:
            constexpr generalized_matrix() = default;
            
            constexpr generalized_matrix(const matrix_t& copy)
            {
                std::memcpy(data, copy.data, sizeof(matrix_t));
            }
            
            constexpr generalized_matrix(matrix_t&& move) noexcept
            {
                std::memcpy(data, move.data, sizeof(matrix_t));
            }
            
            constexpr matrix_t& operator=(const matrix_t& copy)
            {
                if (&copy == this)
                    return *this;
                std::memcpy(data, copy.data, sizeof(matrix_t));
                return *this;
            }
            
            constexpr generalized_matrix(std::initializer_list<T> list)
            {
                blt::size_t index = 0;
                for (const auto& v : list)
                {
                    data[(index / rows)][(index % rows)] = v;
                    index++;
                }
            }
            
            constexpr generalized_matrix(std::initializer_list<blt::vec<float, rows>> list)
            {
                blt::size_t index = 0;
                for (const auto& v : list)
                {
                    data[index] = v;
                    index++;
                }
            }
            
            constexpr explicit generalized_matrix(const std::array<T, rows * columns>& dat)
            {
                for (u32 i = 0; i < columns; i++)
                    for (u32 j = 0; j < rows; j++)
                        data[i][j] = dat[j + i * columns];
            }
            
            constexpr explicit generalized_matrix(const T dat[rows * columns])
            {
                for (u32 i = 0; i < columns; i++)
                    for (u32 j = 0; j < rows; j++)
                        data[i][j] = dat[j + i * columns];
            }
            
            constexpr explicit generalized_matrix(const blt::vec<T, rows> dat[columns])
            {
                for (u32 i = 0; i < columns; i++)
                    data[i] = dat[i];
            }
            
            constexpr static matrix_t make_empty()
            {
                return matrix_t{init_type::EMPTY};
            }
            
            constexpr static matrix_t make_identity()
            {
                static_assert(rows == columns && "Identity matrix must be square!");
                return matrix_t{init_type::IDENTITY};
            }
            
            constexpr auto& set_identity()
            {
                for (blt::u32 i = 0; i < rows; i++)
                    data[i][i] = 1;
                return *this;
            }
            
            [[nodiscard]] constexpr generalized_matrix<T, columns, rows> transpose() const
            {
                generalized_matrix<T, columns, rows> mat;
                
                for (blt::u32 i = 0; i < columns; i++)
                {
                    for (blt::u32 j = 0; j < rows; j++)
                        mat[j][i] = data[i][j];
                }
                
                return mat;
            }
            
            [[nodiscard]] constexpr T magnitude() const
            {
                T ret{};
                for (blt::u32 i = 0; i < columns; i++)
                {
                    for (blt::u32 j = 0; j < rows; j++)
                        ret += (data[i][j] * data[i][j]);
                }
                return std::sqrt(ret);
            }
            
            [[nodiscard]] constexpr matrix_t normalize() const
            {
                auto mag = magnitude();
                matrix_t mat = *this;
                if (mag == 0)
                    return mat;
                return mat / mag;
            }
            
            [[nodiscard]] constexpr matrix_t abs() const
            {
                matrix_t copy = *this;
                for (auto& v : copy.data)
                    v = v.abs();
                return copy;
            }
            
            [[nodiscard]] constexpr matrix_t bipolar() const
            {
                matrix_t copy = *this;
                for (auto& v : copy.data)
                    v = v.bipolar();
                return copy;
            }
            
            constexpr inline const blt::vec<T, rows>& operator[](u32 column) const
            {
                return data[column];
            }
            
            constexpr inline blt::vec<T, rows>& operator[](u32 column)
            {
                return data[column];
            }
            
            [[nodiscard]] constexpr inline T m(u32 row, u32 column) const
            {
                return data[column][row];
            };
            
            constexpr inline T m(u32 row, u32 column, T value)
            {
                return data[column][row] = value;
            };
            
            /**
             * Takes a value stored across a row, taking one from each column in the specified row
             * @param row the row to extract from. defaults to the first row
             */
            [[nodiscard]] constexpr inline vec<T, columns> vec_from_column_row(blt::u32 row = 0) const
            {
                vec<T, columns> ret;
                for (blt::u32 j = 0; j < columns; j++)
                    ret[j] = data[j][row];
                return ret;
            }
            
            constexpr inline matrix_t& operator+=(const matrix_t& other)
            {
                for (blt::u32 i = 0; i < columns; i++)
                    data[i] += other[i];
                return *this;
            }
            
            constexpr inline matrix_t& operator-=(const matrix_t& other)
            {
                for (blt::u32 i = 0; i < columns; i++)
                    data[i] -= other[i];
                return *this;
            }
            
            constexpr inline matrix_t& operator*=(const matrix_t& other)
            {
                for (blt::u32 i = 0; i < columns; i++)
                    data[i] *= other[i];
                return *this;
            }
            
            constexpr inline matrix_t& operator/=(const matrix_t& other)
            {
                for (blt::u32 i = 0; i < columns; i++)
                    data[i] /= other[i];
                return *this;
            }
            
            // adds the two mat4x4 left and right
            constexpr inline friend matrix_t operator+(const matrix_t& left, const matrix_t& right)
            {
                matrix_t ret = left;
                for (u32 i = 0; i < columns; i++)
                    ret[i] += right.data[i];
                return ret;
            }
            
            // subtracts the right mat4x4 from the left.
            constexpr inline friend matrix_t operator-(const matrix_t& left, const matrix_t& right)
            {
                matrix_t ret = left;
                for (u32 i = 0; i < columns; i++)
                    ret[i] -= right.data[i];
                return ret;
            }
            
            // multiples the left with the right
            template<blt::u32 p, typename Ret = generalized_matrix<T, rows, p>, std::enable_if_t<!(rows == 1 && p == 1), bool> = true>
            constexpr inline friend Ret operator*(const matrix_t& left, const generalized_matrix<T, columns, p>& right)
            {
                Ret mat = Ret::make_empty();
                
                for (u32 i = 0; i < rows; i++)
                {
                    for (u32 j = 0; j < p; j++)
                    {
                        for (u32 k = 0; k < columns; k++)
                            mat.m(i, j, mat.m(i, j) + left.m(i, k) * right.m(k, j));
                    }
                }
                
                return mat;
            }
            
            template<blt::u32 p, std::enable_if_t<rows == 1 && p == 1, bool> = true>
            constexpr inline friend T operator*(const matrix_t& left, const generalized_matrix<T, columns, p>& right)
            {
                T ret{};
                
                for (u32 i = 0; i < rows; i++)
                {
                    for (u32 j = 0; j < p; j++)
                    {
                        for (u32 k = 0; k < columns; k++)
                            ret += left.m(i, k) * right.m(k, j);
                    }
                }
                
                return ret;
            }
            
            constexpr inline friend vec<T, rows> operator*(const matrix_t& left, const vec<T, columns>& right)
            {
                vec<T, rows> ret;
                
                for (u32 r = 0; r < rows; r++)
                {
                    for (u32 c = 0; c < columns; c++)
                        ret[r] = ret[r] + left.m(r, c) * right[c];
                }
                
                return ret;
            }
            
            // multiplies the const c with each element in the mat4x4 v
            constexpr inline friend matrix_t operator*(float c, const matrix_t& v)
            {
                matrix_t mat = make_empty();
                
                for (u32 i = 0; i < columns; i++)
                {
                    mat.data[i] = c * v.data[i];
                }
                
                return mat;
            }
            
            // same as above but for right sided constants
            constexpr inline friend matrix_t operator*(const matrix_t& v, float c)
            {
                matrix_t mat = make_empty();
                
                for (u32 i = 0; i < columns; i++)
                {
                    mat.data[i] = v.data[i] * c;
                }
                
                return mat;
            }
            
            // divides the mat4x4 by the constant c
            constexpr inline friend matrix_t operator/(const matrix_t& v, float c)
            {
                matrix_t mat = make_empty();
                
                for (u32 i = 0; i < columns; i++)
                {
                    mat.data[i] = v.data[i] / c;
                }
                
                return mat;
            }
            
            // divides each element in the mat4x4 by over the constant
            constexpr inline friend matrix_t operator/(float c, const matrix_t& v)
            {
                matrix_t mat = make_empty();
                
                for (u32 i = 0; i < columns; i++)
                {
                    for (u32 j = 0; j < rows; j++)
                        mat.data[i][j] = c / v.data[i][j];
                }
                
                return mat;
            }
            
            constexpr inline friend bool operator==(const matrix_t& left, const matrix_t& right)
            {
                for (blt::u32 i = 0; i < columns; i++)
                {
                    if (left.data[i] != right.data[i])
                        return false;
                }
                return true;
            }
            
            constexpr inline friend bool operator!=(const matrix_t& left, const matrix_t& right)
            {
                return !(left == right);
            }
            
            auto begin() const
            {
                return data.begin();
            }
            
            auto end() const
            {
                return data.end();
            }
        
        private:
            blt::vec<T, rows> data[columns];
    };
    
    class mat4x4
    {
            static_assert(std::is_trivially_copyable_v<blt::vec4> && "Vector must be trivially copyable!");
        protected:
            // 4x4 = 16
//            union dataType
//            {
//                float single[16];
//                float dim[4][4];
//                blt::vec4 v[4];
//            };
            blt::vec4 data[4];
            
            friend mat4x4 operator+(const mat4x4& left, const mat4x4& right);
            
            friend mat4x4 operator-(const mat4x4& left, const mat4x4& right);
            
            friend mat4x4 operator*(const mat4x4& left, const mat4x4& right);
            
            friend mat4x4 operator*(float c, const mat4x4& v);
            
            friend mat4x4 operator*(const mat4x4& v, float c);
            
            friend mat4x4 operator/(const mat4x4& v, float c);
            
            friend mat4x4 operator/(float c, const mat4x4& v);
        
        public:
            static mat4x4 make_empty()
            {
                mat4x4 ret;
                ret.m00(0);
                ret.m11(0);
                ret.m22(0);
                ret.m33(0);
                return ret;
            }
            
            mat4x4()
            {
//                for (float& i : data.single)
//                    i = 0;
                // set identity matrix default
                m00(1);
                m11(1);
                m22(1);
                m33(1);
            }
            
            mat4x4(const blt::vec4& c1, const blt::vec4& c2, const blt::vec4& c3, const blt::vec4& c4)
            {
                // dangerous?
//                std::memcpy(data.dim[0], c1.data(), 4 * sizeof(float));
//                std::memcpy(data.dim[1], c2.data(), 4 * sizeof(float));
//                std::memcpy(data.dim[2], c3.data(), 4 * sizeof(float));
//                std::memcpy(data.dim[3], c4.data(), 4 * sizeof(float));
                data[0] = c1;
                data[1] = c2;
                data[2] = c3;
                data[3] = c4;
            }
            
            mat4x4(const mat4x4& mat)
            {
                for (int i = 0; i < 4; i++)
                {
                    data[i] = mat.data[i];
                }
            }
            
            mat4x4& operator=(const mat4x4& copy)
            {
                if (&copy == this)
                    return *this;
                for (int i = 0; i < 4; i++)
                {
                    data[i] = copy.data[i];
                }
                return *this;
            }
            
            explicit mat4x4(const float dat[16])
            {
                for (int i = 0; i < 4; i++)
                    for (int j = 0; j < 4; j++)
                        data[i][j] = dat[j + i * 4];
            }
            
            explicit mat4x4(const blt::vec4 dat[4])
            {
                for (int i = 0; i < 4; i++)
                    data[i] = dat[i];
            }
            
            inline mat4x4& translate(float x, float y, float z)
            {
                mat4x4 translation_mat{};
                /**
                 * 9.005 Are OpenGL matrices column-major or row-major?
                 * For programming purposes, OpenGL matrices are 16-value arrays with base vectors laid out contiguously in memory.
                 * The translation components occupy the 13th, 14th, and 15th elements of the 16-element matrix,
                 * where indices are numbered from 1 to 16 as described in section 2.11.2 of the OpenGL 2.1 Specification.
                 */
                translation_mat.m03(x);
                translation_mat.m13(y);
                translation_mat.m23(z);
                
                *this = *this * translation_mat;
                
                return *this;
            }
            
            inline mat4x4& translate(const vec4& vec)
            { return translate(vec[0], vec[1], vec[2]); }
            
            inline mat4x4& translate(const vec3& vec)
            { return translate(vec[0], vec[1], vec[2]); }
            
            inline mat4x4& translate(const vec2& vec)
            { return translate(vec[0], vec[1], 0); }
            
            inline mat4x4& scale(float x, float y, float z)
            {
                mat4x4 scale_mat{};
                
                m00(m00() * x);
                m11(m11() * y);
                m22(m22() * z);
                
                *this = *this * scale_mat;
                
                return *this;
            }
            
            inline mat4x4& scale(const vec4& vec)
            { return scale(vec[0], vec[1], vec[2]); }
            
            inline mat4x4& scale(const vec3& vec)
            { return scale(vec[0], vec[1], vec[2]); }
            
            inline mat4x4& scale(const vec2& vec)
            { return scale(vec[0], vec[1], 1.0f); }
            
            // angle in radians
            inline mat4x4& rotateX(float angle)
            {
                mat4x4 rotationMatrix{};
                rotationMatrix.m(1, 1, std::cos(angle));
                rotationMatrix.m(1, 2, -std::sin(angle));
                rotationMatrix.m(2, 1, std::sin(angle));
                rotationMatrix.m(2, 2, std::cos(angle));
                
                *this = *this * rotationMatrix;
                
                return *this;
            }
            
            inline mat4x4& rotateY(float angle)
            {
                mat4x4 rotationMatrix{};
                rotationMatrix.m(0, 0, std::cos(angle));
                rotationMatrix.m(0, 2, std::sin(angle));
                rotationMatrix.m(2, 0, -std::sin(angle));
                rotationMatrix.m(2, 2, std::cos(angle));
                
                *this = *this * rotationMatrix;
                
                return *this;
            }
            
            inline mat4x4& rotateZ(float angle)
            {
                mat4x4 rotationMatrix{};
                rotationMatrix.m(0, 0, std::cos(angle));
                rotationMatrix.m(0, 1, -std::sin(angle));
                rotationMatrix.m(1, 0, std::sin(angle));
                rotationMatrix.m(1, 1, std::cos(angle));
                
                *this = *this * rotationMatrix;
                
                return *this;
            }
            
            [[nodiscard]] mat4x4 transpose() const
            {
                mat4x4 copy{*this};
                
                for (int i = 0; i < 4; i++)
                {
                    for (int j = 0; j < 4; j++)
                    {
                        copy.m(j, i, m(i, j));
                    }
                }
                
                return copy;
            }
            
            [[nodiscard]] float determinant() const
            {
                return m00() * (m11() * m22() * m33() + m12() * m23() * m31() + m13() * m21() * m32()
                                - m31() * m22() * m13() - m32() * m23() * m11() - m33() * m21() * m12())
                       - m10() * (m01() * m22() * m33() + m02() * m23() * m31() + m03() * m21() * m32()
                                  - m31() * m32() * m03() - m32() * m23() * m01() - m33() * m21() * m02())
                       + m20() * (m01() * m12() * m33() + m02() * m13() * m31() + m03() * m11() * m32()
                                  - m31() * m12() * m03() - m32() * m13() * m01() - m33() * m11() * m02())
                       - m30() * (m01() * m12() * m23() + m02() * m13() * m21() + m03() * m11() * m22()
                                  - m21() * m12() * m03() - m22() * m13() * m01() - m23() * m11() * m02());
            }
            
            [[nodiscard]] mat4x4 adjugate() const
            {
                auto& m = *this;
                auto Coef00 = m[2][2] * m[3][3] - m[3][2] * m[2][3];
                auto Coef02 = m[1][2] * m[3][3] - m[3][2] * m[1][3];
                auto Coef03 = m[1][2] * m[2][3] - m[2][2] * m[1][3];
                
                auto Coef04 = m[2][1] * m[3][3] - m[3][1] * m[2][3];
                auto Coef06 = m[1][1] * m[3][3] - m[3][1] * m[1][3];
                auto Coef07 = m[1][1] * m[2][3] - m[2][1] * m[1][3];
                
                auto Coef08 = m[2][1] * m[3][2] - m[3][1] * m[2][2];
                auto Coef10 = m[1][1] * m[3][2] - m[3][1] * m[1][2];
                auto Coef11 = m[1][1] * m[2][2] - m[2][1] * m[1][2];
                
                auto Coef12 = m[2][0] * m[3][3] - m[3][0] * m[2][3];
                auto Coef14 = m[1][0] * m[3][3] - m[3][0] * m[1][3];
                auto Coef15 = m[1][0] * m[2][3] - m[2][0] * m[1][3];
                
                auto Coef16 = m[2][0] * m[3][2] - m[3][0] * m[2][2];
                auto Coef18 = m[1][0] * m[3][2] - m[3][0] * m[1][2];
                auto Coef19 = m[1][0] * m[2][2] - m[2][0] * m[1][2];
                
                auto Coef20 = m[2][0] * m[3][1] - m[3][0] * m[2][1];
                auto Coef22 = m[1][0] * m[3][1] - m[3][0] * m[1][1];
                auto Coef23 = m[1][0] * m[2][1] - m[2][0] * m[1][1];
                
                blt::vec4 Fac0(Coef00, Coef00, Coef02, Coef03);
                blt::vec4 Fac1(Coef04, Coef04, Coef06, Coef07);
                blt::vec4 Fac2(Coef08, Coef08, Coef10, Coef11);
                blt::vec4 Fac3(Coef12, Coef12, Coef14, Coef15);
                blt::vec4 Fac4(Coef16, Coef16, Coef18, Coef19);
                blt::vec4 Fac5(Coef20, Coef20, Coef22, Coef23);
                
                blt::vec4 Vec0(m[1][0], m[0][0], m[0][0], m[0][0]);
                blt::vec4 Vec1(m[1][1], m[0][1], m[0][1], m[0][1]);
                blt::vec4 Vec2(m[1][2], m[0][2], m[0][2], m[0][2]);
                blt::vec4 Vec3(m[1][3], m[0][3], m[0][3], m[0][3]);
                
                blt::vec4 Inv0(Vec1 * Fac0 - Vec2 * Fac1 + Vec3 * Fac2);
                blt::vec4 Inv1(Vec0 * Fac0 - Vec2 * Fac3 + Vec3 * Fac4);
                blt::vec4 Inv2(Vec0 * Fac1 - Vec1 * Fac3 + Vec3 * Fac5);
                blt::vec4 Inv3(Vec0 * Fac2 - Vec1 * Fac4 + Vec2 * Fac5);
                
                blt::vec4 SignA(+1, -1, +1, -1);
                blt::vec4 SignB(-1, +1, -1, +1);
                return mat4x4(Inv0 * SignA, Inv1 * SignB, Inv2 * SignA, Inv3 * SignB);
            }
            
            [[nodiscard]] mat4x4 inverse() const
            {
                auto& m = *this;
                auto Inverse = adjugate();
                
                blt::vec4 Row0(Inverse[0][0], Inverse[1][0], Inverse[2][0], Inverse[3][0]);
                
                blt::vec4 Dot0(m[0] * Row0);
                auto Dot1 = (Dot0.x() + Dot0.y()) + (Dot0.z() + Dot0.w());
                
                auto OneOverDeterminant = 1.0f / Dot1;
                
                return Inverse * OneOverDeterminant;
            }
            
            inline const blt::vec4& operator[](int column) const
            {
                return data[column];
            }
            
            inline blt::vec4& operator[](int column)
            {
                return data[column];
            }
            
            [[nodiscard]] inline float m(int row, int column) const
            { return data[column][row]; };
            
            [[nodiscard]] inline float m00() const
            { return m(0, 0); }
            
            [[nodiscard]] inline float m10() const
            { return m(1, 0); }
            
            [[nodiscard]] inline float m20() const
            { return m(2, 0); }
            
            [[nodiscard]] inline float m30() const
            { return m(3, 0); }
            
            [[nodiscard]] inline float m01() const
            { return m(0, 1); }
            
            [[nodiscard]] inline float m11() const
            { return m(1, 1); }
            
            [[nodiscard]] inline float m21() const
            { return m(2, 1); }
            
            [[nodiscard]] inline float m31() const
            { return m(3, 1); }
            
            [[nodiscard]] inline float m02() const
            { return m(0, 2); }
            
            [[nodiscard]] inline float m12() const
            { return m(1, 2); }
            
            [[nodiscard]] inline float m22() const
            { return m(2, 2); }
            
            [[nodiscard]] inline float m32() const
            { return m(3, 2); }
            
            [[nodiscard]] inline float m03() const
            { return m(0, 3); }
            
            [[nodiscard]] inline float m13() const
            { return m(1, 3); }
            
            [[nodiscard]] inline float m23() const
            { return m(2, 3); }
            
            [[nodiscard]] inline float m33() const
            { return m(3, 3); }
            
            inline float m(int row, int column, float value)
            { return data[column][row] = value; };
            
            inline float m00(float d)
            { return m(0, 0, d); }
            
            inline float m10(float d)
            { return m(1, 0, d); }
            
            inline float m20(float d)
            { return m(2, 0, d); }
            
            inline float m30(float d)
            { return m(3, 0, d); }
            
            inline float m01(float d)
            { return m(0, 1, d); }
            
            inline float m11(float d)
            { return m(1, 1, d); }
            
            inline float m21(float d)
            { return m(2, 1, d); }
            
            inline float m31(float d)
            { return m(3, 1, d); }
            
            inline float m02(float d)
            { return m(0, 2, d); }
            
            inline float m12(float d)
            { return m(1, 2, d); }
            
            inline float m22(float d)
            { return m(2, 2, d); }
            
            inline float m32(float d)
            { return m(3, 2, d); }
            
            inline float m03(float d)
            { return m(0, 3, d); }
            
            inline float m13(float d)
            { return m(1, 3, d); }
            
            inline float m23(float d)
            { return m(2, 3, d); }
            
            inline float m33(float d)
            { return m(3, 3, d); }
            
            [[nodiscard]] inline float w(int row, int column) const
            { return data[column - 1][row - 1]; };
            
            [[nodiscard]] inline float w11() const
            { return m(0, 0); }
            
            [[nodiscard]] inline float w21() const
            { return m(1, 0); }
            
            [[nodiscard]] inline float w31() const
            { return m(2, 0); }
            
            [[nodiscard]] inline float w41() const
            { return m(3, 0); }
            
            [[nodiscard]] inline float w12() const
            { return m(0, 1); }
            
            [[nodiscard]] inline float w22() const
            { return m(1, 1); }
            
            [[nodiscard]] inline float w32() const
            { return m(2, 1); }
            
            [[nodiscard]] inline float w42() const
            { return m(3, 1); }
            
            [[nodiscard]] inline float w13() const
            { return m(0, 2); }
            
            [[nodiscard]] inline float w23() const
            { return m(1, 2); }
            
            [[nodiscard]] inline float w33() const
            { return m(2, 2); }
            
            [[nodiscard]] inline float w43() const
            { return m(3, 2); }
            
            [[nodiscard]] inline float w14() const
            { return m(0, 3); }
            
            [[nodiscard]] inline float w24() const
            { return m(1, 3); }
            
            [[nodiscard]] inline float w34() const
            { return m(2, 3); }
            
            [[nodiscard]] inline float w44() const
            { return m(3, 3); }
            
            inline float w(int row, int column, float value)
            { return data[column - 1][row - 1] = value; };
            
            inline float w11(float d)
            { return m(0, 0, d); }
            
            inline float w21(float d)
            { return m(1, 0, d); }
            
            inline float w31(float d)
            { return m(2, 0, d); }
            
            inline float w41(float d)
            { return m(3, 0, d); }
            
            inline float w12(float d)
            { return m(0, 1, d); }
            
            inline float w22(float d)
            { return m(1, 1, d); }
            
            inline float w32(float d)
            { return m(2, 1, d); }
            
            inline float w42(float d)
            { return m(3, 1, d); }
            
            inline float w13(float d)
            { return m(0, 2, d); }
            
            inline float w23(float d)
            { return m(1, 2, d); }
            
            inline float w33(float d)
            { return m(2, 2, d); }
            
            inline float w43(float d)
            { return m(3, 2, d); }
            
            inline float w14(float d)
            { return m(0, 3, d); }
            
            inline float w24(float d)
            { return m(1, 3, d); }
            
            inline float w34(float d)
            { return m(2, 3, d); }
            
            inline float w44(float d)
            { return m(3, 3, d); }
            
            inline float* ptr()
            { return data[0].data(); }
    };
    
    // adds the two mat4x4 left and right
    inline mat4x4 operator+(const mat4x4& left, const mat4x4& right)
    {
        mat4x4 ret = left;
        for (int i = 0; i < 4; i++)
            ret[i] += right.data[i];
        return ret;
    }
    
    // subtracts the right mat4x4 from the left.
    inline mat4x4 operator-(const mat4x4& left, const mat4x4& right)
    {
        mat4x4 ret = left;
        for (int i = 0; i < 4; i++)
            ret[i] -= right.data[i];
        return ret;
    }
    
    // since matrices are made identity by default, we need to create the result collector matrix without identity
    // otherwise the diagonal will be 1 off and cause weird results (see black screen issue)
    constexpr float emptyMatrix[16] = {0, 0, 0, 0,
                                       0, 0, 0, 0,
                                       0, 0, 0, 0,
                                       0, 0, 0, 0};
    
    // multiples the left with the right
    inline mat4x4 operator*(const mat4x4& left, const mat4x4& right)
    {
        mat4x4 mat = mat4x4::make_empty();
        
        // TODO: check avx with this??
        for (int i = 0; i < 4; i++)
        {
            for (int j = 0; j < 4; j++)
            {
                for (int k = 0; k < 4; k++)
                {
                    mat.m(i, j, mat.m(i, j) + left.m(i, k) * right.m(k, j));
                }
            }
        }
        
        return mat;
    }
    
    inline vec4 operator*(const mat4x4& left, const vec4& right)
    {
        vec4 ret{0, 0, 0, 0};
        
        for (int m = 0; m < 4; m++)
        {
            for (int n = 0; n < 4; n++)
            {
                ret[m] = ret[m] + left.m(m, n) * right[n];
            }
        }
        
        return ret;
    }
    
    template<typename T, unsigned long size>
    inline vec4 operator*(const mat4x4& left, const vec<T, size>& right)
    {
        vec4 ret{0, 0, 0, 0};
        
        for (int i = 0; i < size; i++)
            ret[i] = (float) right[i];
        
        return left * ret;
    }
    
    // multiplies the const c with each element in the mat4x4 v
    inline mat4x4 operator*(float c, const mat4x4& v)
    {
        mat4x4 mat{};
        
        for (int i = 0; i < 4; i++)
        {
            mat.data[i] = c * v.data[i];
        }
        
        return mat;
    }
    
    // same as above but for right sided constants
    inline mat4x4 operator*(const mat4x4& v, float c)
    {
        mat4x4 mat{};
        
        for (int i = 0; i < 4; i++)
        {
            mat.data[i] = v.data[i] * c;
        }
        
        return mat;
    }
    
    // divides the mat4x4 by the constant c
    inline mat4x4 operator/(const mat4x4& v, float c)
    {
        mat4x4 mat{};
        
        for (int i = 0; i < 4; i++)
        {
            mat.data[i] = v.data[i] / c;
        }
        
        return mat;
    }
    
    // divides each element in the mat4x4 by over the constant
    inline mat4x4 operator/(float c, const mat4x4& v)
    {
        mat4x4 mat{};
        
        for (int i = 0; i < 4; i++)
        {
            for (int j = 0; j < 4; j++)
                mat.data[i][j] = c / v.data[i][j];
        }
        
        return mat;
    }
    
    // https://www.scratchapixel.com/lessons/3d-basic-rendering/perspective-and-orthographic-projection-matrix/building-basic-perspective-projection-matrix.html
    // https://ogldev.org/www/tutorial12/tutorial12.html
    // http://www.songho.ca/opengl/gl_projectionmatrix.html
    static inline mat4x4 perspective(float fov, float aspect_ratio, float near, float far)
    {
        mat4x4 perspectiveMat4x4 = mat4x4::make_empty();
        
        float halfTan = tanf(fov * 0.5f * (float) M_PI / 180.0f);
        perspectiveMat4x4.m00(float(1.0 / (aspect_ratio * halfTan)));
        perspectiveMat4x4.m11(float(1.0 / halfTan));
        perspectiveMat4x4.m22(float(-((far + near) / (far - near))));
        perspectiveMat4x4.m32(-1);
        perspectiveMat4x4.m23(float(-((2 * near * far) / (far - near))));
        
        return perspectiveMat4x4;
    }
    
    static inline mat4x4 ortho(float left, float right, float top, float bottom, float near, float far)
    {
        mat4x4 perspectiveMat4x4 = mat4x4::make_empty();
        
        perspectiveMat4x4.m00(2 / (right - left));
        perspectiveMat4x4.m11(2 / (top - bottom));
        perspectiveMat4x4.m22(2 / (far - near));
        perspectiveMat4x4.m33(1);
        perspectiveMat4x4.m03(-(right + left) / (right - left));
        perspectiveMat4x4.m13(-(top + bottom) / (top - bottom));
        perspectiveMat4x4.m23(-(far + near) / (far - near));
        
        return perspectiveMat4x4;
    }
    
}

#endif //BLT_TESTS_MATRIX_H
