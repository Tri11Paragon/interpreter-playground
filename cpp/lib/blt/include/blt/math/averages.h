/*
 * Created by Brett on 28/02/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_TESTS_AVERAGES_H
#define BLT_TESTS_AVERAGES_H

namespace blt
{
    
    template<typename T, int Size>
    class averagizer_o_matic
    {
        private:
            T* data;
            int index = 0;
            int m_default = 0;
        public:
            averagizer_o_matic(): averagizer_o_matic(0)
            {}
            
            explicit averagizer_o_matic(T default_value)
            {
                data = new T[Size];
                for (int i = 0; i < Size; i++)
                {
                    data[i] = default_value;
                }
                m_default = default_value;
            }
            
            void insert(T t)
            {
                data[index++] = t;
                if (index >= Size)
                    index = 0;
            }
            
            T average()
            {
                T total = 0;
                for (int i = 0; i < Size; i++)
                {
                    total += data[i];
                }
                return total / Size;
            }
            
            ~averagizer_o_matic()
            {
                delete[] data;
            }
    };
    
    template<typename A, typename B>
    double average(A a, B b)
    {
        if (b == 0)
            return 0;
        return static_cast<double>(a) / static_cast<double>(b);
    }
    
}

#endif //BLT_TESTS_AVERAGES_H
