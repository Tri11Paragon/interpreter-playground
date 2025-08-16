#pragma once
/*
 * Created by Brett on 06/02/24.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_EXPECTED_H
#define BLT_EXPECTED_H

#include <blt/compatibility.h>
#include <variant>
#include <type_traits>
#include <utility>

namespace blt
{
    struct unexpect_t
    {
        explicit unexpect_t() = default;
    };
    
    inline constexpr unexpect_t unexpect{};
    
    template<typename T>
    using remove_cvref_t = std::remove_reference_t<std::remove_cv_t<T>>;
    
    template<class E>
    class unexpected
    {
        private:
            E e;
        public:
            constexpr unexpected(const unexpected&) = default;
            
            constexpr unexpected(unexpected&&) = default;
            
            template<class Err = E, std::enable_if_t<
                    !std::is_same_v<remove_cvref_t<Err>, unexpected> && !std::is_same_v<remove_cvref_t<Err>, std::in_place_t> &&
                    std::is_constructible_v<E, Err>, bool> = true>
            constexpr explicit unexpected(Err&& e): e(std::forward<Err>(e))
            {}
            
            template<class... Args, std::enable_if_t<std::is_constructible_v<E, Args...>, bool> = true>
            constexpr explicit unexpected(std::in_place_t, Args&& ... args): e(std::forward<Args>(args)...)
            {}
            
            template<class U, class... Args, std::enable_if_t<std::is_constructible_v<E, std::initializer_list<U>&, Args...>, bool> = true>
            constexpr explicit unexpected(std::in_place_t, std::initializer_list<U> il, Args&& ... args): e(il, std::forward<Args>(args)...)
            {}
            
            constexpr const E& error() const& noexcept
            {
                return e;
            }
            
            constexpr E& error()& noexcept
            {
                return e;
            }
            
            constexpr const E&& error() const&& noexcept
            {
                return e;
            }
            
            constexpr E&& error()&& noexcept
            {
                return e;
            }
            
            constexpr void swap(unexpected& other) noexcept(std::is_nothrow_swappable_v<E>)
            {
                std::swap(error(), other.error());
            }
            
            template<typename E2>
            inline friend constexpr bool operator==(const unexpected& x, const unexpected<E2>& y)
            {
                return x.error() == y.error();
            }
            
            friend constexpr void swap(unexpected& x, unexpected& y) noexcept(noexcept(x.swap(y)))
            {}
    };
    
    template<class E>
    unexpected(E) -> unexpected<E>;
    
    template<class E>
    class bad_expected_access : public std::exception
    {
        private:
            E e;
        public:
            explicit bad_expected_access(E e): e(std::move(e))
            {}
            
            const E& error() const& noexcept
            { return e; }
            
            E& error()& noexcept
            { return e; }
            
            const E&& error() const&& noexcept
            { return e; }
            
            E&& error()&& noexcept
            { return e; }
            
            [[nodiscard]] const char* what() const noexcept override
            { return "blt::expected does not contain a value!"; }
        
    };
    
    template<typename T, typename E, bool = std::is_copy_constructible_v<T>>
    class expected
    {
        protected:
            std::variant<T, E> v;
            
            template<typename U, typename G>
            inline static constexpr bool eight_insanity_v =
                    std::is_constructible_v<T, expected<U, G>&> || std::is_constructible_v<T, expected<U, G>> ||
                    std::is_constructible_v<T, const expected<U, G>&> || std::is_constructible_v<T, const expected<U, G>> ||
                    std::is_convertible_v<expected<U, G>&, T> || std::is_convertible_v<expected<U, G>, T> ||
                    std::is_convertible_v<const expected<U, G>&, T> || std::is_convertible_v<const expected<U, G>, T>;
            
            template<typename U, typename G>
            inline static constexpr bool four_insanity_v =
                    std::is_constructible_v<unexpected<E>, expected<U, G>&> || std::is_constructible_v<unexpected<E>, expected<U, G>> ||
                    std::is_constructible_v<unexpected<E>, const expected<U, G>&> || std::is_constructible_v<unexpected<E>, const expected<U, G>>;
        public:
            template<typename G, std::enable_if_t<std::is_default_constructible_v<G> && std::is_convertible_v<G, T>, bool> = true>
            constexpr expected(): v(G{})
            {}
            
//            template<typename H, std::enable_if_t<!std::is_default_constructible_v<T> && std::is_default_constructible_v<E> && std::is_convertible_v<H, E>, bool> = true>
//            constexpr expected(): v(H{})
//            {}

//            constexpr expected(const expected& copy) = delete;
            constexpr expected(const expected<T, E, true>& copy): expected<T, E, true>::v(copy.v) // NOLINT
            {}
            
            expected& operator=(const expected& copy)
            {
                v = copy.v;
                return *this;
            }
            
            constexpr expected(expected&& move) noexcept: v(std::move(move.v))
            {}
            
            expected& operator=(expected&& move)
            {
                std::swap(v, move.v);
                return *this;
            }
            
            /*
             * (4)...(5)
             */
            template<class U, class G, class UF = std::add_lvalue_reference_t<const U>, class GF = const G&, std::enable_if_t<
                    (!std::is_convertible_v<UF, T> || !std::is_convertible_v<GF, E>) && (std::is_constructible_v<T, UF> || std::is_void_v<U>) &&
                    std::is_constructible_v<E, GF> && !eight_insanity_v<U, G> && !four_insanity_v<U, G>, bool> = true>
            
            constexpr explicit expected(const expected<U, G>& other):
                    v(other.has_value() ? std::forward<UF>(*other) : std::forward<GF>(other.error()))
            {}
            
            template<class U, class G, class UF = U, class GF = G, std::enable_if_t<
                    (!std::is_convertible_v<UF, T> || !std::is_convertible_v<GF, E>) && (std::is_constructible_v<T, UF> || std::is_void_v<U>) &&
                    std::is_constructible_v<E, GF> && !eight_insanity_v<U, G> && !four_insanity_v<U, G>, bool> = true>
            
            constexpr explicit expected(expected<U, G>&& other):
                    v(other.has_value() ? std::forward<UF>(*other) : std::forward<GF>(other.error()))
            {}
            
            template<class U, class G, class UF = std::add_lvalue_reference_t<const U>, class GF = const G&, std::enable_if_t<
                    (std::is_convertible_v<UF, T> && std::is_convertible_v<GF, E>) && (std::is_constructible_v<T, UF> || std::is_void_v<U>) &&
                    std::is_constructible_v<E, GF> && !eight_insanity_v<U, G> && !four_insanity_v<U, G>, bool> = true>
            
            constexpr expected(const expected<U, G>& other): // NOLINT
                    v(other.has_value() ? std::forward<UF>(*other) : std::forward<GF>(other.error()))
            {}
            
            template<class U, class G, class UF = U, class GF = G, std::enable_if_t<
                    (std::is_convertible_v<UF, T> && std::is_convertible_v<GF, E>) && (std::is_constructible_v<T, UF> || std::is_void_v<U>) &&
                    std::is_constructible_v<E, GF> && !eight_insanity_v<U, G> && !four_insanity_v<U, G>, bool> = true>
            
            constexpr expected(expected<U, G>&& other): // NOLINT
                    v(other.has_value() ? std::forward<UF>(*other) : std::forward<GF>(other.error()))
            {}
            
            
            /*
             * (6)
             */
            
            template<class U = T, std::enable_if_t<!std::is_convertible_v<U, T> &&
                                                   !std::is_same_v<remove_cvref_t<T>, void> &&
                                                   !std::is_same_v<remove_cvref_t<U>, std::in_place_t> &&
                                                   !std::is_same_v<expected, remove_cvref_t<U>> &&
                                                   std::is_constructible_v<T, U> &&
                                                   !std::is_same_v<remove_cvref_t<U>, unexpected<U>> &&
                                                   !std::is_same_v<remove_cvref_t<U>, expected<T, E>>, bool> = true>
            constexpr explicit expected(U&& v): v(T(std::forward<U>(v)))
            {}
            
            template<class U = T, std::enable_if_t<std::is_convertible_v<U, T> &&
                                                   !std::is_same_v<remove_cvref_t<T>, void> &&
                                                   !std::is_same_v<remove_cvref_t<U>, std::in_place_t> &&
                                                   !std::is_same_v<expected, remove_cvref_t<U>> &&
                                                   std::is_constructible_v<T, U> &&
                                                   !std::is_same_v<remove_cvref_t<U>, unexpected<U>> &&
                                                   !std::is_same_v<remove_cvref_t<U>, expected<T, E>>, bool> = true>
            constexpr expected(U&& v): v(T(std::forward<U>(v))) // NOLINT
            {}
            
            /*
             * (7)
             */
            
            template<class G, class GF = std::add_const_t<std::add_lvalue_reference_t<G>>, std::enable_if_t<
                    !std::is_convertible_v<const G&, E> && std::is_constructible_v<E, GF>, bool> = true>
            constexpr explicit expected(const unexpected<G>& e): v(std::forward<GF>(e.error()))
            {}
            
            template<class G, class GF = std::add_const_t<std::add_lvalue_reference_t<G>>, std::enable_if_t<
                    std::is_convertible_v<const G&, E> && std::is_constructible_v<E, GF>, bool> = true>
            constexpr expected(const unexpected<G>& e): v(std::forward<GF>(e.error())) // NOLINT
            {}
            
            /*
             * (8)
             */
            
            template<class G, class GF = std::add_const_t<std::add_lvalue_reference_t<G>>, std::enable_if_t<
                    !std::is_convertible_v<G, E> && std::is_constructible_v<E, GF>, bool> = true>
            constexpr explicit expected(unexpected<G>&& e): v(std::forward<GF>(e.error()))
            {}
            
            template<class G, class GF = std::add_const_t<std::add_lvalue_reference_t<G>>, std::enable_if_t<
                    std::is_convertible_v<G, E> && std::is_constructible_v<E, GF>, bool> = true>
            constexpr expected(unexpected<G>&& e): v(std::forward<GF>(e.error())) // NOLINT
            {}
            
            /*
             * (9)...(13)
             */
            template<class... Args, std::enable_if_t<std::is_constructible_v<T, Args...>, bool> = true>
            constexpr explicit expected(std::in_place_t, Args&& ... args): v(T(std::forward<Args>(args)...))
            {}
            
            template<class U, class... Args, std::enable_if_t<std::is_constructible_v<T, std::initializer_list<U>&, Args...>, bool> = true>
            constexpr explicit expected(std::in_place_t, std::initializer_list<U> il, Args&& ... args): v(T(il, std::forward<Args>(args)...))
            {}

//            template<class... Args, std::enable_if_t<std::is_same_v<remove_cvref_t<T>, void>, bool> = true>
//            constexpr explicit expected(std::in_place_t) noexcept: v(T())
//            {}
            
            template<class... Args, std::enable_if_t<std::is_constructible_v<E, Args...>, bool> = true>
            constexpr explicit expected(unexpect_t, Args&& ... args): v(E(std::forward<Args>(args)...))
            {}
            
            template<class U, class... Args, std::enable_if_t<std::is_constructible_v<E, std::initializer_list<U>&, Args...>, bool> = true>
            constexpr explicit expected(unexpect_t, std::initializer_list<U> il, Args&& ... args): v(E(il, std::forward<Args>(args)...))
            {}
            
            [[nodiscard]] constexpr explicit operator bool() const noexcept
            {
                return std::holds_alternative<T>(v);
            }
            
            [[nodiscard]] constexpr inline bool has_value() const noexcept
            {
                return std::holds_alternative<T>(v);
            }
            
            constexpr T& value()&
            {
                if (*this)
                    return std::get<T>(v);
                else
                    throw bad_expected_access(std::as_const(error()));
            }
            
            constexpr const T& value() const&
            {
                if (*this)
                    return std::get<T>(v);
                else
                    throw bad_expected_access(std::as_const(error()));
            }
            
            constexpr T&& value()&&
            {
                if (*this)
                    return std::get<T>(v);
                else
                    throw bad_expected_access(std::move(error()));
            }
            
            constexpr const T&& value() const&&
            {
                if (*this)
                    return std::get<T>(v);
                else
                    throw bad_expected_access(std::move(error()));
            }
            
            constexpr const E& error() const& noexcept
            {
                return std::get<E>(v);
            }
            
            constexpr E& error()& noexcept
            {
                return std::get<E>(v);
            }
            
            constexpr const E&& error() const&& noexcept
            {
                return std::get<E>(v);
            }
            
            constexpr E&& error()&& noexcept
            {
                return std::get<E>(v);
            }
            
            template<class U, std::enable_if_t<std::is_convertible_v<U, T> && std::is_copy_constructible_v<T>, bool> = true>
            constexpr T value_or(U&& default_value) const&
            {
                return bool(*this) ? **this : static_cast<T>(std::forward<U>(default_value));
            }
            
            template<class U, std::enable_if_t<std::is_convertible_v<U, T> && std::is_move_constructible_v<T>, bool> = true>
            constexpr T value_or(U&& default_value)&&
            {
                return bool(*this) ? std::move(**this) : static_cast<T>(std::forward<U>(default_value));
            }
            
            constexpr inline const T* operator->() const noexcept
            {
                return &std::get<T>(v);
            }
            
            constexpr inline T* operator->() noexcept
            {
                return &std::get<T>(v);
            }
            
            constexpr inline const T& operator*() const& noexcept
            {
                return std::get<T>(v);
            }
            
            constexpr inline T& operator*()& noexcept
            {
                return std::get<T>(v);
            }
            
            constexpr inline const T&& operator*() const&& noexcept
            {
                return std::move(std::get<T>(v));
            }
            
            constexpr inline T&& operator*()&& noexcept
            {
                return std::move(std::get<T>(v));
            }
    };
    
    template<typename T, typename E>
    class expected<T, E, false> : public expected<T, E, true>
    {
        public:
            using expected<T, E, true>::expected;
            
            constexpr expected(const expected& copy) = delete;
            
            expected& operator=(const expected& copy) = delete;
    };
}

#endif //BLT_EXPECTED_H
