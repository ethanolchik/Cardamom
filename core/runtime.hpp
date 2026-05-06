#pragma once

#include <cstddef>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

template <typename T>
std::optional<T> __cardamom_array_get(const std::vector<T>& values, int index) {
    if (index < 0 || static_cast<std::size_t>(index) >= values.size()) {
        return std::nullopt;
    }
    return values.at(index);
}

template <typename T, typename F>
auto __cardamom_option_map(const std::optional<T>& value, F mapper)
    -> std::optional<std::decay_t<decltype(mapper(*value))>> {
    if (!value.has_value()) {
        return std::nullopt;
    }
    return std::make_optional(mapper(*value));
}

template <typename T>
struct __cardamom_ok {
    T value;
};

template <typename E>
struct __cardamom_err {
    E error;
};

template <typename T, typename E>
using __cardamom_result = std::variant<__cardamom_ok<T>, __cardamom_err<E>>;

template <typename T>
__cardamom_ok<std::decay_t<T>> __cardamom_make_ok(T&& value) {
    return __cardamom_ok<std::decay_t<T>>{std::forward<T>(value)};
}

template <typename E>
__cardamom_err<std::decay_t<E>> __cardamom_make_err(E&& error) {
    return __cardamom_err<std::decay_t<E>>{std::forward<E>(error)};
}

template <typename T, typename E, typename F>
T __cardamom_result_value_or(const __cardamom_result<T, E>& value, F fallback) {
    if (const auto* ok = std::get_if<__cardamom_ok<T>>(&value)) {
        return ok->value;
    }
    return fallback;
}

template <typename T, typename E>
bool __cardamom_result_is_ok(const __cardamom_result<T, E>& value) {
    return std::holds_alternative<__cardamom_ok<T>>(value);
}

template <typename T, typename E>
bool __cardamom_result_is_err(const __cardamom_result<T, E>& value) {
    return std::holds_alternative<__cardamom_err<E>>(value);
}

template <typename T, typename E, typename F>
auto __cardamom_result_map(const __cardamom_result<T, E>& value, F mapper)
    -> __cardamom_result<std::decay_t<decltype(mapper(std::declval<T>()))>, E> {
    using R = std::decay_t<decltype(mapper(std::declval<T>()))>;
    if (const auto* ok = std::get_if<__cardamom_ok<T>>(&value)) {
        return __cardamom_ok<R>{mapper(ok->value)};
    }
    return __cardamom_err<E>{std::get<__cardamom_err<E>>(value).error};
}
