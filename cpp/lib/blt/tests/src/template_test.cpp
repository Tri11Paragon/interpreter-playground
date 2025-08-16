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
#include <templating_test.h>
#include <string>
#include <blt/std/logging.h>
#include <blt/parse/templating.h>

const std::string shader_test_string = R"("
#version 300 es
precision mediump float;

${LAYOUT_STRING} out vec4 FragColor;
in vec2 uv;
in vec2 pos;

uniform sampler2D tex;

vec4 linear_iter(vec4 i, vec4 p, float factor){
    return (i + p) * factor;
}

void main() {
    FragColor = texture(tex, uv);
}

")";

void process_string(const std::string& str)
{
    BLT_DEBUG(str);
    auto results = blt::template_engine_t::process_string(str);
    if (results)
    {
        auto val = results.value();
        for (auto& v : val)
        {
            BLT_TRACE_STREAM << (blt::template_token_to_string(v.type));
        }
        BLT_TRACE_STREAM << "\n";
        for (auto& v : val)
        {
            BLT_TRACE("{%s: %s}", blt::template_token_to_string(v.type).c_str(), std::string(v.token).c_str());
        }
    } else
    {
        auto error = results.error();
        switch (error)
        {
            case blt::template_tokenizer_failure_t::MISMATCHED_CURLY:
                BLT_ERROR("Tokenizer Failure: Mismatched curly");
                break;
            case blt::template_tokenizer_failure_t::MISMATCHED_PAREN:
                BLT_ERROR("Tokenizer Failure: Mismatched parenthesis");
                break;
            case blt::template_tokenizer_failure_t::MISMATCHED_QUOTE:
                BLT_ERROR("Tokenizer Failure: Mismatched Quotes");
                break;
        }
        
    }
    BLT_DEBUG("--------------------------");
}

namespace blt::test
{
    void template_test()
    {
        process_string(shader_test_string);
        process_string("~hello");
        process_string("hello");
        process_string("hello ${WORLD}");
        process_string("layout (location = ${IF(LAYOUT_LOCATION) LAYOUT_LOCATION ELSE ~DISCARD})");
    }
}