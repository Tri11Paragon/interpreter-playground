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

#ifndef BLT_WITH_GRAPHICS_OBJ_LOADER_H
#define BLT_WITH_GRAPHICS_OBJ_LOADER_H

#include "blt/math/vectors.h"
#include "blt/std/hashmap.h"
#include <utility>
#include <vector>
#include <string_view>
#include <string>

namespace blt::parse
{
    
    typedef blt::vec3f vertex_t;
    typedef blt::vec2f uv_t;
    typedef blt::vec3f normal_t;
    typedef blt::vec3f color_t;
    
    class model_data
    {
        public:
        private:
            std::vector<vertex_t> vertices;
            std::vector<uv_t> uvs;
            std::vector<normal_t> normals;
    };
    
    struct face_t
    {
        std::int32_t vertex, uv, normal;
    };
    
    static inline bool operator==(const face_t& f1, const face_t& f2)
    {
        return f1.vertex == f2.vertex && f1.uv == f2.uv && f1.normal == f2.normal;
    }
    
    struct face_hash
    {
        size_t operator()(const face_t& face) const
        {
            std::hash<std::int32_t> hasher;
            return hasher(face.vertex) ^ hasher(face.uv) ^ hasher(face.normal);
        }
    };
    
    struct face_eq
    {
        bool operator()(const face_t& f1, const face_t& f2) const
        {
            return f1 == f2;
        }
    };
    
    struct constructed_vertex_t
    {
        vertex_t vertex;
        uv_t uv;
        normal_t normal;
    };
    
    struct triangle_t
    {
        std::int32_t v[3];
    };
    
    struct quad_t
    {
        std::int32_t v[4];
    };
    
    struct material_t
    {
        std::string material_name;
        color_t ambient;
        color_t diffuse;
        color_t specular;
        float specular_exponent = 0.0f;
        float transparency = 1.0f;
        color_t transmission_filter_color{1, 1, 1};
        std::string texture_ambient;
        std::string texture_diffuse;
        std::string map_spec_color;
        std::string map_spec_highlight;
        std::string map_bump;
        std::string map_displacement;
    };
    
    struct object_data
    {
        std::vector<std::string> object_names;
        std::string material;
        std::vector<triangle_t> indices;
    };
    
    class obj_model_t
    {
        private:
            std::vector<constructed_vertex_t> vertex_data_;
            std::vector<object_data> objects_;
            hashmap_t<std::string, material_t> materials_;
        public:
            obj_model_t(std::vector<constructed_vertex_t>&& vertex_data, std::vector<object_data>&& objects, hashmap_t<std::string, material_t>&& mats):
                    vertex_data_(vertex_data), objects_(objects), materials_(mats)
            {}
            
            inline const auto& vertex_data()
            {
                return vertex_data_;
            };
            
            inline const auto& objects()
            {
                return objects_;
            };
            
            inline const auto& materials()
            {
                return materials_;
            }
    };
    
    class char_tokenizer;
    
    class obj_loader
    {
        private:
            std::vector<vertex_t> vertices;
            std::vector<uv_t> uvs;
            std::vector<normal_t> normals;
            
            // maps between face (constructed vertex) -> vertex indices
            hashmap_t<face_t, std::int32_t, face_hash, face_eq> vertex_map;
            std::vector<constructed_vertex_t> vertex_data;
            object_data current_object;
            std::vector<object_data> data;
            hashmap_t<std::string, material_t> materials;
            
            size_t current_line = 0;
        private:
            bool handle_vertex_and_normals(float x, float y, float z, char type);
            
            void parse_vertex_line(char_tokenizer& tokenizer);
            
            void parse_face(char_tokenizer& tokenizer);
            
            void handle_face_vertex(const std::vector<std::string>& face_list, std::int32_t* arr);
        
        public:
            obj_model_t parseFile(std::string_view file);
    };
    
    obj_model_t quick_load(std::string_view file);
    
}

#endif //BLT_WITH_GRAPHICS_OBJ_LOADER_H
