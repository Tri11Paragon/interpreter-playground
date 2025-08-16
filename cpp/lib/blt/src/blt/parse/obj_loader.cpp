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
#define BLT_DISABLE_TRACE
#define BLT_DISABLE_DEBUG

#include <blt/parse/obj_loader.h>
#include <blt/fs/loader.h>
#include <blt/std/string.h>
#include <blt/iterator/iterator.h>
#include <cctype>
#include <charconv>
#include "blt/std/assert.h"
#include "blt/std/utility.h"
#include <blt/logging/logging.h>

namespace blt::parse
{
	class char_tokenizer
	{
	private:
		std::string_view string;
		std::size_t current_pos = 0;

	public:
		explicit char_tokenizer(std::string_view view): string(view)
		{}

		inline char advance()
		{
			return string[current_pos++];
		}

		inline bool has_next(size_t offset = 0)
		{
			return current_pos + offset < string.size();
		}

		inline std::string_view read_fully()
		{
			return blt::string::trim(string.substr(current_pos));
		}
	};

	template <typename T = float>
	T get(std::string_view str)
	{
		T x;
		// TODO: GCC version. C++17 supports from_chars but GCC8.5 doesn't have floating point.
		#if __cplusplus >= BLT_CPP20
        const auto [ptr, ec] = std::from_chars(str.data(), str.data() + str.size(), x);
		#else
		auto ec = std::errc();
		if constexpr (std::is_floating_point_v<T>)
		{
			x = static_cast<T>(std::stod(std::string(str)));
		} else if constexpr (std::is_integral_v<T>)
		{
			x = static_cast<T>(std::stoll(std::string(str)));
		} else
			static_assert(
				"You are using a c++ version which does not support the required std::from_chars, manual conversion has failed to find a type!");
		#endif
		// probably not needed.
		if (ec != std::errc())
		{
			//            int i;
			//            const auto [ptr2, ec2] = std::from_chars(str.data(), str.data() + str.size(), i);
			//            if (ec2 == std::errc())
			//            {
			//                x = static_cast<float>(i);
			//            } else
			//            {
			BLT_WARN("Unable to parse string '{}' into number!", std::string(str).c_str());
			x = 0;
			//            }
		}
		return x;
	}

	void obj_loader::parse_vertex_line(char_tokenizer& tokenizer)
	{
		char type = tokenizer.advance();

		if (type == 'p')
		{
			BLT_WARN("Unexpected type '{:c}' (not supported)", type);
			return;
		}

		auto elements = string::split(tokenizer.read_fully(), " ");
		BLT_ASSERT(elements.size() >= 2 && "Current line doesn't have enough arguments to process!");
		float x = get(elements[0]), y = get(elements[1]);
		BLT_DEBUG("Loaded value of ({}, {})", x, y);
		if (elements.size() < 3)
		{
			if (type == 't')
				uvs.push_back(uv_t{x, y});
			else
				BLT_ERROR("Unable to parse line '{}' type '{:c}' not recognized for arg count", tokenizer.read_fully(), type);
		} else
		{
			float z = get(elements[2]);
			BLT_DEBUG(" with z: {}", z);
			if (!handle_vertex_and_normals(x, y, z, type))
				BLT_ERROR("Unable to parse line '{}' type '{:c}' not recognized", tokenizer.read_fully(), type);
		}
	}

	bool obj_loader::handle_vertex_and_normals(float x, float y, float z, char type)
	{
		if (std::isspace(type))
		{
			vertices.push_back(vertex_t{x, y, z});
		} else if (type == 'n')
		{
			normals.push_back(normal_t{x, y, z});
		} else
			return false;
		return true;
	}

	obj_model_t quick_load(std::string_view file)
	{
		return obj_loader().parseFile(file);
	}

	obj_model_t obj_loader::parseFile(std::string_view file)
	{
		auto lines = blt::fs::getLinesFromFile(std::string(file));
		for (const auto& [index, line] : blt::enumerate(lines))
		{
			current_line = index;
			char_tokenizer token(line);
			if (!token.has_next() || token.read_fully().empty())
				continue;
			switch (token.advance())
			{
				case '#':
					continue;
				case 'f':
					parse_face(token);
					break;
				case 'v':
					parse_vertex_line(token);
					break;
				case 'o':
				{
					current_object.object_names.emplace_back(token.read_fully());
					BLT_TRACE("Setting object '{}'", std::string(current_object.object_name).c_str());
					break;
				}
				case 'm':
				{
					while (token.has_next() && token.advance() != ' ')
					{}
					BLT_WARN("Material '{}' needs to be loaded!", std::string(token.read_fully()).c_str());
					break;
				}
				case 'u':
				{
					if (!current_object.indices.empty())
						data.push_back(current_object);
					current_object = {};
					while (token.has_next() && token.advance() != ' ')
					{}
					current_object.material = token.read_fully();
					//BLT_WARN("Using material '{}'", std::string(token.read_fully()).c_str());
					break;
				}
				case 's':
					//BLT_WARN("Using shading: {}", std::string(token.read_fully()).c_str());
					break;
			}
		}
		data.push_back(current_object);
		return {std::move(vertex_data), std::move(data), std::move(materials)};
	}

	void obj_loader::parse_face(char_tokenizer& tokenizer)
	{
		auto faces = blt::string::split(std::string(tokenizer.read_fully()), ' ');
		if (faces.size() == 3)
		{
			triangle_t triangle{};
			handle_face_vertex(faces, triangle.v);
			current_object.indices.push_back(triangle);
		} else if (faces.size() == 4)
		{
			quad_t quad{};
			handle_face_vertex(faces, quad.v);
			triangle_t t1{};
			triangle_t t2{};

			for (int i = 0; i < 3; i++)
				t1.v[i] = quad.v[i];
			t2.v[0] = quad.v[0];
			t2.v[1] = quad.v[2];
			t2.v[2] = quad.v[3];

			current_object.indices.push_back(t1);
			current_object.indices.push_back(t2);
		} else
			BLT_WARN("Unsupported face vertex count of {:d} on line {:d}!", faces.size(), current_line);
	}

	void obj_loader::handle_face_vertex(const std::vector<std::string>& face_list, int32_t* arr)
	{
		for (const auto& [e_index, value] : blt::enumerate(face_list))
		{
			auto indices = blt::string::split(value, '/');
			BLT_ASSERT(indices.size() == 3 && "Must have vertex, uv, and normal indices!!");

			auto vi = get<std::int32_t>(indices[0]) - 1;
			auto ui = get<std::int32_t>(indices[1]) - 1;
			auto ni = get<std::int32_t>(indices[2]) - 1;

			BLT_DEBUG("Found vertex: {:d}, UV: {:d}, and normal: {:d}", vi, ui, ni);

			face_t face{vi, ui, ni};

			auto loc = vertex_map.find(face);
			if (loc == vertex_map.end())
			{
				BLT_DEBUG("{}", "DID NOT FIND FACE!");
				auto index = static_cast<std::int32_t>(vertex_data.size());
				vertex_data.push_back({vertices[vi], uvs[ui], normals[ni]});
				BLT_DEBUG("Vertex: ({.4f}, {.4f}, {.4f}), UV: ({.4f}, {.4f}), Normal: ({.4f}, {.4f}, {:.4f})", vertices[vi].x(), vertices[vi].y(),
						vertices[vi].z(), uvs[ui].x(), uvs[ui].y(), normals[ni].x(), normals[ni].y(), normals[ni].z());
				vertex_map.insert({face, index});
				arr[e_index] = index;
			} else
			{
				BLT_TRACE("Using cached data; {:d}; map size: {:d}", loc->second, vertex_data.size());
				//const auto& d = vertex_data[loc->second];
				BLT_TRACE("Vertex: ({.4f}, {.4f}, {.4f}), UV: ({.4f}, {.4f}), Normal: ({.4f}, {.4f}, {:.4f})", d.vertex.x(), d.vertex.y(),
						d.vertex.z(), d.uv.x(), d.uv.y(), d.normal.x(), d.normal.y(), d.normal.z());
				arr[e_index] = loc->second;
			}
		}
	}
}
