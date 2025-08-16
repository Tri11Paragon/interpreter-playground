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
#include <stdexcept>
#include <blt/std/requests.h>

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#endif

#if defined(__has_include) && __has_include(<curl/curl.h>)
#include <curl/curl.h>
#define BLT_HAS_CURL
#endif

namespace blt::requests
{
#ifdef BLT_HAS_CURL
    struct curl_init_t
    {
        curl_init_t()
        {
            const auto version_data = curl_version_info(CURLVERSION_NOW);
            if (!(version_data->features & CURL_VERSION_THREADSAFE))
            {
                thread_safe = false;
            }
            curl_global_init(CURL_GLOBAL_ALL);
        }

        ~curl_init_t()
        {
            curl_global_cleanup();
        }

        bool thread_safe = true;
    };

    struct curl_easy_init_t
    {
        curl_easy_init_t(): curl(curl_easy_init())
        {
        }

        ~curl_easy_init_t()
        {
            curl_easy_cleanup(curl);
        }

        CURL* curl;
    };

    void init()
    {
        static curl_init_t curl_init_obj;
    }

    CURL* easy_init()
    {
        thread_local curl_easy_init_t curl_easy_init_obj;
        return curl_easy_init_obj.curl;
    }

    size_t write_to_string_func(const void* data, const size_t size, const size_t nmemb, void* user_data)
    {
        auto& str = *static_cast<std::string*>(user_data);
        str.append(static_cast<const char*>(data), size * nmemb);
        return size * nmemb;
    };
#endif

    std::string send_get_request(const std::string& url)
    {
#ifdef __EMSCRIPTEN__
		auto* str = static_cast<char*>(EM_ASM_PTR({
				var xhr = new XMLHttpRequest();
				xhr.open("GET", $0);
				xhr.send();
				return stringToNewUTF8(xhr.responseText);
			}, url.c_str()));
		std::string str_obj{str};
		free(str);
		return str_obj;
#else
#ifdef BLT_HAS_CURL
        init();
        auto curl = easy_init();
        if (!curl)
            throw std::runtime_error("Failed to initialize curl");
        std::string response_string;

        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, static_cast<void*>(&response_string));
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, &write_to_string_func);

        const auto res = curl_easy_perform(curl);
        if (res != CURLE_OK)
            throw std::runtime_error(curl_easy_strerror(res));

        return response_string;
#else
        return "Missing cURL! Unable to fetch URL: '" + url + "'";
#endif
#endif
    }
}
