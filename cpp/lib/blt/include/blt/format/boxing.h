#pragma once
/*
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

#ifndef BLT_BOXING_H
#define BLT_BOXING_H

#include <blt/std/types.h>
#include <blt/logging/logging.h>
#include <string>

namespace blt
{
    namespace detail
    {
        class log_box_base_t
        {
            public:
                explicit log_box_base_t(std::string_view title, blt::size_t padding = 0): padding(padding), title(title)
                {}
                
                template<typename Logger>
                void make_padding(Logger& logger)
                {
                    for (blt::size_t i = 0; i < padding; i++)
                        logger << '-';
                }
                
                template<typename Logger>
                void make_full_width_line(Logger& logger)
                {
                    for (blt::size_t i = 0; i < padding * 2 + 2 + title.size(); i++)
                        logger << '-';
                }
                
                template<typename Logger>
                void make_full_title(Logger& logger)
                {
                    make_padding(logger);
                    if (padding > 0)
                        logger << "{";
                    logger << title;
                    if (padding > 0)
                        logger << "}";
                    make_padding(logger);
                }
            
            protected:
                blt::size_t padding;
                std::string title;
        };
    }
    
    template<typename Logger>
    class log_box_t : detail::log_box_base_t
    {
        public:
            log_box_t(Logger& logger, std::string_view title, blt::size_t padding = 0): detail::log_box_base_t(title, padding), logger(logger)
            {
                make_full_title(logger);
                logger << '\n';
            }
            
            ~log_box_t()
            {
                make_full_width_line(logger);
                logger << '\n';
            }
        private:
            Logger& logger;
    };
    
    template<typename Logger>
    log_box_t(Logger&& logger, std::string_view, blt::size_t) -> log_box_t<Logger>;
    
}

#endif //BLT_BOXING_H
