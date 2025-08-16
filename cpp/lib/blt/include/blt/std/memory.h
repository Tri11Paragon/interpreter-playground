/*
 * Created by Brett on 08/02/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_TESTS_MEMORY_H
#define BLT_TESTS_MEMORY_H

#include <blt/std/memory_util.h>
#include <initializer_list>
#include <iterator>
#include <cstring>
#include <blt/std/assert.h>
#include <type_traits>
#include <utility>

namespace blt
{
    
    template<typename T, bool = std::is_copy_constructible_v<T> || std::is_copy_assignable_v<T>>
    class scoped_buffer;

/**
     * Creates an encapsulation of a T array which will be automatically deleted when this object goes out of scope.
     * This is a simple buffer meant to be used only inside of a function and not copied around.
     * The internal buffer is allocated on the heap.
     * The operator * has been overloaded to return the internal buffer.
     * @tparam T type that is stored in buffer eg char
     */
    template<typename T>
    class scoped_buffer<T, true>
    {
        public:
            using element_type = T;
            using value_type = std::remove_cv_t<T>;
            using pointer = T*;
            using const_pointer = const T*;
            using reference = T&;
            using const_reference = const T&;

            using iterator = ptr_iterator<T>;
            using const_iterator = ptr_iterator<const T>;
            using reverse_iterator = std::reverse_iterator<iterator>;
            using const_reverse_iterator = std::reverse_iterator<const_iterator>;
        private:
            T* buffer_ = nullptr;
            size_t size_;
        public:
            constexpr scoped_buffer(): buffer_(nullptr), size_(0)
            {}
            
            constexpr explicit scoped_buffer(size_t size): size_(size)
            {
                if (size > 0)
                    buffer_ = new T[size];
                else
                    buffer_ = nullptr;
            }
            
            constexpr scoped_buffer(const scoped_buffer& copy)
            {
                if (copy.size() == 0)
                {
                    buffer_ = nullptr;
                    size_ = 0;
                    return;
                }
                buffer_ = new T[copy.size()];
                size_ = copy.size_;
                
                if constexpr (std::is_trivially_copyable_v<T>)
                {
                    std::memcpy(buffer_, copy.buffer_, copy.size() * sizeof(T));
                } else
                {
                    if constexpr (std::is_copy_constructible_v<T> && !std::is_copy_assignable_v<T>)
                    {
                        for (size_t i = 0; i < this->size_; i++)
                            buffer_[i] = T(copy[i]);
                    } else
                        for (size_t i = 0; i < this->size_; i++)
                            buffer_[i] = copy[i];
                }
            }
            
            constexpr scoped_buffer& operator=(const scoped_buffer& copy)
            {
                if (&copy == this)
                    return *this;
                
                if (copy.size() == 0)
                {
                    buffer_ = nullptr;
                    size_ = 0;
                    return *this;
                }
                
                delete[] this->buffer_;
                buffer_ = new T[copy.size()];
                size_ = copy.size_;
                
                if constexpr (std::is_trivially_copyable_v<T>)
                {
                    std::memcpy(buffer_, copy.buffer_, copy.size() * sizeof(T));
                } else
                {
                    if constexpr (std::is_copy_constructible_v<T> && !std::is_copy_assignable_v<T>)
                    {
                        for (size_t i = 0; i < this->size_; i++)
                            buffer_[i] = T(copy[i]);
                    } else
                        for (size_t i = 0; i < this->size_; i++)
                            buffer_[i] = copy[i];
                }
                return *this;
            }
            
            constexpr scoped_buffer(scoped_buffer&& move) noexcept
            {
                delete[] buffer_;
                buffer_ = move.buffer_;
                size_ = move.size();
                move.buffer_ = nullptr;
            }
            
            constexpr scoped_buffer& operator=(scoped_buffer&& moveAssignment) noexcept
            {
                delete[] buffer_;
                buffer_ = moveAssignment.buffer_;
                size_ = moveAssignment.size();
                moveAssignment.buffer_ = nullptr;
                
                return *this;
            }
            
            /**
             * Resize the internal buffer. Nothing will occur if the sizes are equal.
             * This function WILL NOT COPY ANY DATA. It is meant for use when creating a scoped buffer without size.
             */
            constexpr void resize(size_t size)
            {
                if (size == 0)
                    return;
                if (size == size_)
                    return;
                delete[] buffer_;
                buffer_ = new T[size];
                size_ = size;
            }
            
            constexpr inline T& operator[](size_t index)
            {
                return buffer_[index];
            }
            
            constexpr inline const T& operator[](size_t index) const
            {
                return buffer_[index];
            }
            
            constexpr inline T* operator*()
            {
                return buffer_;
            }
            
            [[nodiscard]] constexpr inline size_t size() const
            {
                return size_;
            }
            
            constexpr inline T*& ptr()
            {
                return buffer_;
            }
            
            constexpr inline const T* const& ptr() const
            {
                return buffer_;
            }
            
            constexpr inline const T* const& data() const
            {
                return buffer_;
            }
            
            constexpr inline T*& data()
            {
                return buffer_;
            }
            
            constexpr iterator begin() noexcept
            {
                return iterator{data()};
            }
            
            constexpr iterator end() noexcept
            {
                return iterator{data() + size()};
            }
            
            constexpr const_iterator cbegin() const noexcept
            {
                return const_iterator{data()};
            }
            
            constexpr const_iterator cend() const noexcept
            {
                return const_iterator{data() + size()};
            }
            
            constexpr inline reverse_iterator rbegin() noexcept
            {
                return reverse_iterator{end()};
            }
            
            constexpr inline reverse_iterator rend() noexcept
            {
                return reverse_iterator{begin()};
            }
            
            constexpr inline const_iterator crbegin() const noexcept
            {
                return const_reverse_iterator{cend()};
            }
            
            constexpr inline reverse_iterator crend() const noexcept
            {
                return reverse_iterator{cbegin()};
            }
            
            ~scoped_buffer()
            {
                delete[] buffer_;
            }
    };
    
    template<typename T>
    class scoped_buffer<T, false> : scoped_buffer<T, true>
    {
            using scoped_buffer<T, true>::scoped_buffer;
        public:
            scoped_buffer(const scoped_buffer& copy) = delete;
            
            scoped_buffer operator=(scoped_buffer& copyAssignment) = delete;
    };
    
    
    // TODO: might already have a version of this somewhere!
    template<typename T, bool = std::is_copy_constructible_v<T> || std::is_copy_assignable_v<T>>
    class expanding_buffer;
    
    template<typename T>
    class expanding_buffer<T, true>
    {
        public:
            using element_type = T;
            using value_type = std::remove_cv_t<T>;
            using pointer = T*;
            using const_pointer = const T*;
            using reference = T&;
            using const_reference = const T&;
            using iterator = ptr_iterator<T>;
            using const_iterator = ptr_iterator<const T>;
            using reverse_iterator = std::reverse_iterator<iterator>;
            using const_reverse_iterator = std::reverse_iterator<const_iterator>;
        private:
            T* buffer_ = nullptr;
            size_t size_ = 0;
        public:
            constexpr expanding_buffer(): buffer_(nullptr), size_(0)
            {}
            
            constexpr explicit expanding_buffer(size_t size): size_(size)
            {
                if (size > 0)
                    buffer_ = new T[size];
                else
                    buffer_ = nullptr;
            }
            
            constexpr expanding_buffer(const expanding_buffer& copy)
            {
                if (copy.size() == 0)
                {
                    buffer_ = nullptr;
                    size_ = 0;
                    return;
                }
                buffer_ = new T[copy.size()];
                size_ = copy.size_;
                
                if constexpr (std::is_trivially_copyable_v<T>)
                {
                    std::memcpy(buffer_, copy.buffer_, copy.size() * sizeof(T));
                } else
                {
                    if constexpr (std::is_copy_constructible_v<T> && !std::is_copy_assignable_v<T>)
                    {
                        for (size_t i = 0; i < this->size_; i++)
                            buffer_[i] = T(copy[i]);
                    } else
                        for (size_t i = 0; i < this->size_; i++)
                            buffer_[i] = copy[i];
                }
            }
            
            constexpr expanding_buffer& operator=(const expanding_buffer& copy)
            {
                if (&copy == this)
                    return *this;
                
                if (copy.size() == 0)
                {
                    buffer_ = nullptr;
                    size_ = 0;
                    return *this;
                }
                
                delete_this(buffer_, size());
                buffer_ = new T[copy.size()];
                size_ = copy.size_;
                
                if constexpr (std::is_trivially_copyable_v<T>)
                {
                    std::memcpy(buffer_, copy.buffer_, copy.size() * sizeof(T));
                } else
                {
                    if constexpr (std::is_copy_constructible_v<T> && !std::is_copy_assignable_v<T>)
                    {
                        for (size_t i = 0; i < this->size_; i++)
                            buffer_[i] = T(copy[i]);
                    } else
                        for (size_t i = 0; i < this->size_; i++)
                            buffer_[i] = copy[i];
                }
                return *this;
            }
            
            constexpr expanding_buffer(expanding_buffer&& move) noexcept
            {
                delete_this(buffer_, size());
                buffer_ = move.buffer_;
                size_ = move.size();
                move.buffer_ = nullptr;
            }
            
            constexpr expanding_buffer& operator=(expanding_buffer&& moveAssignment) noexcept
            {
                delete_this(buffer_, size());
                buffer_ = moveAssignment.buffer_;
                size_ = moveAssignment.size();
                moveAssignment.buffer_ = nullptr;
                
                return *this;
            }
            
            /**
             * Resize the internal buffer. Nothing will occur if the sizes are equal.
             * This function WILL NOT COPY ANY DATA. It is meant for use when creating a scoped buffer without size.
             */
            constexpr void resize(size_t size)
            {
                if (size == 0)
                    return;
                if (size == size_)
                    return;
                delete_this(buffer_, this->size());
                buffer_ = new T[size];
                size_ = size;
            }
            
            constexpr inline T& operator[](size_t index)
            {
                if (index >= size())
                    allocate_for(index);
                return buffer_[index];
            }
            
            constexpr inline const T& operator[](size_t index) const
            {
                if (index >= size())
                    BLT_ABORT("Index out of bounds");
                return buffer_[index];
            }
            
            constexpr inline T* operator*()
            {
                return buffer_;
            }
            
            [[nodiscard]] constexpr inline size_t size() const
            {
                return size_;
            }
            
            constexpr inline T*& ptr()
            {
                return buffer_;
            }
            
            constexpr inline const T* const& ptr() const
            {
                return buffer_;
            }
            
            constexpr inline const T* const& data() const
            {
                return buffer_;
            }
            
            constexpr inline T*& data()
            {
                return buffer_;
            }
            
            constexpr iterator begin() noexcept
            {
                return iterator{data()};
            }
            
            constexpr iterator end() noexcept
            {
                return iterator{data() + size()};
            }
            
            constexpr const_iterator cbegin() const noexcept
            {
                return const_iterator{data()};
            }
            
            constexpr const_iterator cend() const noexcept
            {
                return const_iterator{data() + size()};
            }
            
            constexpr inline reverse_iterator rbegin() noexcept
            {
                return reverse_iterator{end()};
            }
            
            constexpr inline reverse_iterator rend() noexcept
            {
                return reverse_iterator{begin()};
            }
            
            constexpr inline const_iterator crbegin() const noexcept
            {
                return const_reverse_iterator{cend()};
            }
            
            constexpr inline reverse_iterator crend() const noexcept
            {
                return reverse_iterator{cbegin()};
            }
            
            ~expanding_buffer()
            {
                delete_this(buffer_, size());
            }
            
            void expand(blt::size_t new_size)
            {
                T* new_buffer = new T[new_size];
                if (buffer_ != nullptr)
                {
                    if constexpr (std::is_trivially_copyable_v<T>)
                    {
                        std::memcpy(new_buffer, buffer_, size_ * sizeof(T));
                    } else
                    {
                        if constexpr (std::is_copy_constructible_v<T> && !std::is_move_constructible_v<T>)
                        {
                            for (size_t i = 0; i < size_; i++)
                                new_buffer[i] = T(buffer_[i]);
                        } else
                            for (size_t i = 0; i < size_; i++)
                                new_buffer[i] = std::move(buffer_[i]);
                    }
                    delete[] buffer_;
                }
                buffer_ = new_buffer;
                size_ = new_size;
            }
        
        private:
            void allocate_for(blt::size_t accessing_index)
            {
                accessing_index = std::max(size_, accessing_index);
                accessing_index = blt::mem::next_byte_allocation(accessing_index);
                expand(accessing_index);
            }
            
            inline void delete_this(T* buffer, blt::size_t)
            {
//                if constexpr (std::is_trivially_destructible_v<T>)
//                    return;
//                if (buffer == nullptr)
//                    return;
//                for (blt::size_t i = 0; i < size; i++)
//                    buffer[i]->~T();
//                free(buffer);
                delete[] buffer;
            }
    };
    
    template<typename T>
    class expanding_buffer<T, false> : expanding_buffer<T, true>
    {
            using expanding_buffer<T, true>::expanding_buffer;
        public:
            expanding_buffer(const expanding_buffer& copy) = delete;
            
            expanding_buffer operator=(expanding_buffer& copyAssignment) = delete;
    };
    
    
    template<typename T>
    struct nullptr_initializer
    {
        private:
            T* m_ptr = nullptr;
        public:
            nullptr_initializer() = default;
            
            explicit nullptr_initializer(T* ptr): m_ptr(ptr)
            {}
            
            nullptr_initializer(const nullptr_initializer<T>& ptr): m_ptr(ptr.m_ptr)
            {}
            
            nullptr_initializer(nullptr_initializer<T>&& ptr) noexcept: m_ptr(ptr.m_ptr)
            {}
            
            nullptr_initializer<T>& operator=(const nullptr_initializer<T>& ptr)
            {
                if (&ptr == this)
                    return *this;
                this->m_ptr = ptr.m_ptr;
                return *this;
            }
            
            nullptr_initializer<T>& operator=(nullptr_initializer<T>&& ptr) noexcept
            {
                if (&ptr == this)
                    return *this;
                this->m_ptr = ptr.m_ptr;
                return *this;
            }
            
            inline T* operator->()
            {
                return m_ptr;
            }
            
            ~nullptr_initializer() = default;
    };
    
    /**
     * Creates a hash-map like association between an enum key and any arbitrary value.
     * The storage is backed by a contiguous array for faster access.
     * @tparam K enum value
     * @tparam V associated value
     */
    template<typename K, typename V>
    class enum_storage
    {
        private:
            V* m_values;
            size_t m_size = 0;
        public:
            enum_storage(std::initializer_list<std::pair<K, V>> init)
            {
                for (auto& i : init)
                    m_size = std::max((size_t) i.first, m_size);
                m_values = new V[m_size];
                for (auto& v : init)
                    m_values[(size_t) v.first] = v.second;
            }
            
            inline V& operator[](size_t index)
            {
                return m_values[index];
            }
            
            inline const V& operator[](size_t index) const
            {
                return m_values[index];
            }
            
            [[nodiscard]] inline size_t size() const
            {
                return m_size;
            }
            
            ptr_iterator<V> begin()
            {
                return ptr_iterator{m_values};
            }
            
            ptr_iterator<V> end()
            {
                return ptr_iterator{&m_values[m_size]};
            }
            
            ~enum_storage()
            {
                delete[] m_values;
            }
    };
    
}

#endif //BLT_TESTS_MEMORY_H
