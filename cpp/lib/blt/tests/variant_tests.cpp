/*
 *  <Short Description>
 *  Copyright (C) 2025  Brett Terpstra
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
#include <blt/logging/logging.h>
#include <blt/std/assert.h>
#include <blt/std/variant.h>

struct base_type
{
	[[nodiscard]] virtual int simple() const = 0;
	[[nodiscard]] virtual std::string to_string() const = 0;

	virtual ~base_type() = default;
};

struct mutate_type : base_type
{
	virtual void mutate(int i) = 0;
};

struct type1 final : base_type
{
	[[nodiscard]] int simple() const final // NOLINT
	{
		return 1;
	}

	[[nodiscard]] std::string to_string() const final // NOLINT
	{
		return "Type1";
	}
};

struct type2 final : base_type
{
	[[nodiscard]] int simple() const final // NOLINT
	{
		return 2;
	}

	[[nodiscard]] std::string to_string() const final // NOLINT
	{
		return "Type2";
	}
};

struct type3 final : base_type
{
	[[nodiscard]] int simple() const final // NOLINT
	{
		return 3;
	}

	[[nodiscard]] std::string to_string() const final // NOLINT
	{
		return "Type3";
	}
};

struct storing_type1 final : mutate_type
{
	explicit storing_type1(const int i): internal(i)
	{}

	[[nodiscard]] int simple() const final // NOLINT
	{
		return internal;
	}

	void mutate(const int i) final
	{
		internal = i;
	}

	[[nodiscard]] std::string to_string() const final // NOLINT
	{
		return "Storing Type: {" + std::to_string(internal) + "}";
	}

	int internal;
};

struct storing_type2 final : mutate_type
{
	explicit storing_type2(const float i): internal(i * 2.2534f)
	{}

	[[nodiscard]] int simple() const final // NOLINT
	{
		return static_cast<int>(internal);
	}

	void mutate(const int i) final
	{
		internal = static_cast<float>(i) * 2.2534f;
	}

	[[nodiscard]] std::string to_string() const final // NOLINT
	{
		return "Storing Type: {" + std::to_string(internal) + "}";
	}

	float internal;
};

struct no_members
{
	int hello;
};

struct concrete_visitor
{
	[[nodiscard]] std::string operator()(const type1& t1) const
	{
		return t1.to_string();
	}

	[[nodiscard]] std::string operator()(const type2& t2) const
	{
		return t2.to_string();
	}

	[[nodiscard]] std::string operator()(const type3& t3) const
	{
		return t3.to_string();
	}
};

struct concrete_visitor_with_state
{
	concrete_visitor_with_state(std::function<std::string(const type1&)> func1, std::function<std::string(const type2&)> func2,
								std::function<std::string(const type3&)> func3): func1(std::move(func1)), func2(std::move(func2)),
																				func3(std::move(func3))
	{}

	[[nodiscard]] std::string operator()(const type1& t1) const
	{
		return func1(t1);
	}

	[[nodiscard]] std::string operator()(const type2& t2) const
	{
		return func2(t2);
	}

	[[nodiscard]] std::string operator()(const type3& t3) const
	{
		return func3(t3);
	}

private:
	std::function<std::string(const type1&)> func1;
	std::function<std::string(const type2&)> func2;
	std::function<std::string(const type3&)> func3;
};

/*
 * **********
 * This is not allowed! BLT's visitor is only able to change return types if you provide functions as lambdas,
 * otherwise it is not possible to change the return of these functions
 * **********
 */
struct concrete_void
{
	[[nodiscard]] std::string operator()(const type1& t1) const
	{
		return t1.to_string();
	}

	[[nodiscard]] std::string operator()(const type2& t2) const
	{
		return t2.to_string();
	}

	void operator()(const type3&) const
	{

	}
};

int main()
{
	blt::variant_t<type1, type2, type3> v1{type1{}};
	blt::variant_t<type1, type2, type3> v2{type2{}};
	blt::variant_t<type1, type2, type3> v3{type3{}};

	BLT_TRACE("Variants to_string():");

	auto v1_result = v1.call_member(&base_type::to_string);
	BLT_ASSERT_MSG(v1_result == type1{}.to_string(), ("Expected result to be " + type1{}.to_string() + " but found " + v1_result).c_str());
	static_assert(std::is_same_v<decltype(v1_result), std::string>, "Result type expected to be string!");
	BLT_TRACE("V1: {}", v1_result);

	auto v2_result = v2.call_member(&base_type::to_string);
	BLT_ASSERT_MSG(v2_result == type2{}.to_string(), ("Expected result to be " + type2{}.to_string() + " but found " + v2_result).c_str());
	static_assert(std::is_same_v<decltype(v2_result), std::string>, "Result type expected to be string!");
	BLT_TRACE("V2: {}", v2_result);

	auto v3_result = v3.call_member(&base_type::to_string);
	BLT_ASSERT_MSG(v3_result == type3{}.to_string(), ("Expected result to be " + type3{}.to_string() + " but found " + v3_result).c_str());
	static_assert(std::is_same_v<decltype(v3_result), std::string>, "Result type expected to be string!");
	BLT_TRACE("V3: {}", v3.call_member(&base_type::to_string));

	blt::variant_t<type1, type2, no_members> member_missing_stored_member{type1{}};
	blt::variant_t<type1, type2, no_members> member_missing_stored_no_member{no_members{50}};

	auto stored_member_result = member_missing_stored_member.call_member(&base_type::to_string);
	auto no_member_result = member_missing_stored_no_member.call_member(&base_type::to_string);

	static_assert(std::is_same_v<decltype(stored_member_result), std::optional<std::string>>);
	BLT_ASSERT(stored_member_result.has_value());
	static_assert(std::is_same_v<decltype(no_member_result), std::optional<std::string>>);
	BLT_ASSERT(!no_member_result.has_value());

	BLT_TRACE("Stored: has value? '{}' value: '{}'", stored_member_result.has_value(), *stored_member_result);
	BLT_TRACE("No Member: {}", no_member_result.has_value());

	auto visit_result_v1 = v1.visit([](const type1& t1) {
		return t1.simple();
	}, [](const type2& t2) {
		return t2.simple();
	}, [](const type3&) {});

	static_assert(std::is_same_v<decltype(visit_result_v1), std::optional<int>>, "Result type expected to be optional<int>!");
	BLT_ASSERT(visit_result_v1.has_value());
	BLT_ASSERT(*visit_result_v1 == 1);
	BLT_TRACE("Visit optional int: {}", *visit_result_v1);

	auto visit_result_v2 = v2.visit([](const type1& t1) {
		return static_cast<float>(t1.simple());
	}, [](const type2& t2) {
		return std::to_string(t2.simple());
	}, [](const type3& t3) {
		return t3.simple();
	});

	static_assert(std::is_same_v<decltype(visit_result_v2), blt::variant_t<int, std::string, float>>,
				"Result type expected to be variant_t<int, std::string, float>!");
	BLT_ASSERT(visit_result_v2.index() == 1);
	BLT_ASSERT(visit_result_v2.get<std::string>() == "2");
	BLT_TRACE("Visit variant result: {}", visit_result_v2.get<std::string>());

	auto visit_result_v3 = v2.visit([](const type1&) {}, [](const type2& t2) {
		return std::to_string(t2.simple());
	}, [](const type3& t3) {
		return t3.simple();
	});

	static_assert(std::is_same_v<decltype(visit_result_v3), std::optional<blt::variant_t<int, std::string>>>,
				"Result type expected to be optional<variant_t<int, std::string>>!");
	BLT_ASSERT(visit_result_v3.has_value());
	BLT_ASSERT(visit_result_v3.value().index() == 1);
	BLT_ASSERT(visit_result_v3.value().get<std::string>() == "2");
	BLT_TRACE("Visit optional variant result: {}", visit_result_v3.value().get<std::string>());

	auto single_visitee = v3.visit([](auto&& a) {
		return a.to_string();
	});

	static_assert(std::is_same_v<decltype(single_visitee), std::string>, "Result type expected to be string!");
	BLT_ASSERT(single_visitee == type3{}.to_string());
	BLT_TRACE("Single visitee: {}", single_visitee);

	auto provided_visitor = v3.visit(blt::lambda_visitor{
		[](const type1& t1) {
			return t1.to_string();
		},
		[](const type2& t2) {
			return t2.to_string();
		},
		[](const type3& t3) {
			return t3.to_string();
		}
	});

	static_assert(std::is_same_v<decltype(provided_visitor), std::string>, "Result type expected to be string!");
	BLT_ASSERT(provided_visitor == type3{}.to_string());
	BLT_TRACE("Provided visitor: {}", provided_visitor);

	concrete_visitor visit{};
	auto concrete_visitor_result = v3.visit(visit);
	BLT_TRACE("Concrete Result: {}", concrete_visitor_result);
	BLT_ASSERT(concrete_visitor_result == type3{}.to_string());

	auto concrete_visitor_result2 = v2.visit(visit);
	BLT_TRACE("Concrete Result: {}", concrete_visitor_result2);
	BLT_ASSERT(concrete_visitor_result2 == type2{}.to_string());

	concrete_visitor_with_state concrete_visitor_state{[](const auto& type) {
		return type.to_string();
	}, [](const auto& type) {
		return type.to_string();
	}, [](const auto& type) {
		return type.to_string();
	}};

	auto concrete_visitor_state_result = v1.visit(blt::black_box_ret(concrete_visitor_state));
	BLT_TRACE("Concrete State Result: {}", concrete_visitor_state_result);
	BLT_ASSERT(concrete_visitor_state_result == type1{}.to_string());

	BLT_INFO("Variant tests passed!");
}
