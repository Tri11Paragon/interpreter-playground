/*
 * Created by Brett on 26/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_TESTS_FORMAT_H
#define BLT_TESTS_FORMAT_H

#include <string>
#include <utility>
#include <vector>
#include <blt/math/math.h>
#include <blt/std/types.h>
#include <algorithm>
#include <string_view>
#include "memory.h"
#include <blt/std/vector.h>
#include <variant>

namespace blt::string
{
    template<typename T>
    static inline std::string withGrouping(T t, size_t group = 3)
    {
        // TODO: all this + make it faster
        static_assert(std::is_arithmetic_v<T> && "Must be arithmetic type!");
        auto str = std::to_string(t);
        std::string ret;
        ret.reserve(str.size());
        blt::size_t count = 0;
        auto start_pos = static_cast<blt::i64>(str.size() - 1);
        for (auto i = start_pos; i >= 0; i--)
        {
            if (str[i] == '.')
            {
                start_pos = i - 1;
                break;
            }
        }
        for (auto i = static_cast<blt::i64>(str.size() - 1); i > start_pos; i--)
            ret += str[i];
        for (auto i = start_pos; i >= 0; i--)
        {
            ret += str[i];
            if (count++ % (group) == group - 1 && i != 0)
                ret += ',';
        }
        std::reverse(ret.begin(), ret.end());
        return ret;
    }
}

namespace blt
{
    class byte_convert_t
    {
        public:
            enum class byte_t : blt::u64
            {
                Bytes = 1,
                Kilobyte = 1024,
                Megabyte = 1024 * 1024,
                Gigabyte = 1024 * 1024 * 1024,
            };
            
            explicit byte_convert_t(blt::u64 bytes): bytes(bytes)
            {}
            
            byte_convert_t(blt::u64 bytes, byte_t convert_type): bytes(bytes), type(convert_type)
            {
                converted = static_cast<double>(bytes) / static_cast<double>(static_cast<blt::u64>(type));
            }
            
            byte_convert_t& convert_to_nearest_type()
            {
                if (bytes > 1073741824)
                {
                    // gigabyte
                    type = byte_t::Gigabyte;
                } else if (bytes > 1048576)
                {
                    // megabyte
                    type = byte_t::Megabyte;
                } else if (bytes > 1024)
                {
                    // kilobyte
                    type = byte_t::Kilobyte;
                } else
                {
                    type = byte_t::Bytes;
                }
                converted = static_cast<double>(bytes) / static_cast<double>(static_cast<blt::u64>(type));
                return *this;
            }
            
            [[nodiscard]] std::string_view type_string() const
            {
                switch (type)
                {
                    case byte_t::Bytes:
                        return "b";
                    case byte_t::Kilobyte:
                        return "KiB";
                    case byte_t::Megabyte:
                        return "MiB";
                    case byte_t::Gigabyte:
                        return "GiB";
                }
                return "NotPossible!";
            }
            
            [[nodiscard]] double getConverted() const
            {
                return converted;
            }
            
            template<blt::i64 decimal_places = -1, template<blt::i64> typename round_function = round_up_t>
            [[nodiscard]] double getConvertedRound() const
            {
                round_function<decimal_places> convert{};
                return convert(converted);
            }
            
            template<blt::i64 decimal_places = -1, template<blt::i64> typename round_function = round_up_t>
            [[nodiscard]] std::string to_pretty_string() const
            {
                auto str = string::withGrouping(getConvertedRound<decimal_places, round_function>(), 3);
                str += type_string();
                return str;
            }
            
            [[nodiscard]] blt::u64 getBytes() const
            {
                return bytes;
            }
            
            [[nodiscard]] byte_t getType() const
            {
                return type;
            }
        
        private:
            blt::u64 bytes = 0;
            byte_t type = byte_t::Bytes;
            double converted = 0;
    };
}

namespace blt::string
{
    // negative decimal places will not round.
    template<blt::i64 decimal_places = -1>
    static inline std::string fromBytes(blt::u64 bytes)
    {
        byte_convert_t convert(bytes);
        convert.convert_to_nearest_type();
        return std::to_string(convert.getConvertedRound<decimal_places>()) + convert.type_string();
    }
    
    static inline std::string bytes_to_pretty(blt::u64 bytes)
    {
        return byte_convert_t(bytes).convert_to_nearest_type().to_pretty_string();
    }
    
    // TODO: update table formatter to use these!
    /**
     * creates a line starting/ending with ending char filled between with spacing char
     * @param totalLength total length to generate
     * @param endingChar beginning and ending char to use
     * @param spacingChar char to use for spacing
     * @return a generated line string eg: +--------+
     */
    std::string createLine(size_t totalLength, char endingChar, char spacingChar);
    
    /**
     * Create a padding string using length and spacing char
     * @param length length of string to generate
     * @param spacing char to use to generate padding
     * @return a padding string
     */
    std::string createPadding(size_t length, char spacing = ' ');
    
    // TODO template the padding functions:
    
    /**
     * Ensure that string str has expected length, pad after the string otherwise.
     * @param str string to pad
     * @param expectedLength expected length of the string.
     * @return a space padded string
     */
    static inline std::string postPadWithSpaces(const std::string& str, size_t expectedLength)
    {
        auto currentSize = (int) (str.length() - 1);
        if ((int) expectedLength - currentSize <= 0)
            return str;
        auto paddedString = str;
        paddedString += createPadding(expectedLength - currentSize);
        return paddedString;
    }
    
    /**
     * Ensure that string str has expected length, pad before the string otherwise.
     * @param str string to pad
     * @param expectedLength expected length of the string.
     * @return a space padded string
     */
    static inline std::string prePadWithSpaces(const std::string& str, size_t expectedLength)
    {
        auto currentSize = str.length() - 1;
        auto paddedString = std::string();
        paddedString += createPadding(expectedLength - currentSize);
        paddedString += str;
        return paddedString;
    }
    
    struct utf8_string
    {
        char* characters;
        unsigned int size;
    };
    
    // taken from java, adapted for c++.
    static inline utf8_string createUTFString(const std::string& str)
    {
        const auto strlen = (unsigned int) str.size();
        unsigned int utflen = strlen;
        
        for (unsigned int i = 0; i < strlen; i++)
        {
            unsigned char c = str[i];
            if (c >= 0x80 || c == 0)
                utflen += 1;
        }
        
        if (utflen > 65535 || /* overflow */ utflen < strlen)
            throw "UTF Error";
        
        utf8_string chars{};
        chars.size = utflen + 2;
        chars.characters = new char[chars.size];
        
        int count = 0;
        chars.characters[count++] = (char) ((utflen >> 8) & 0xFF);
        chars.characters[count++] = (char) ((utflen >> 0) & 0xFF);
        
        unsigned int i = 0;
        for (i = 0; i < strlen; i++)
        { // optimized for initial run of ASCII
            int c = (unsigned char) str[i];
            if (c >= 0x80 || c == 0) break;
            chars.characters[count++] = (char) c;
        }
        
        for (; i < strlen; i++)
        {
            int c = (unsigned char) str[i];
            if (c < 0x80 && c != 0)
            {
                chars.characters[count++] = (char) c;
            } else if (c >= 0x800)
            {
                chars.characters[count++] = (char) (0xE0 | ((c >> 12) & 0x0F));
                chars.characters[count++] = (char) (0x80 | ((c >> 6) & 0x3F));
                chars.characters[count++] = (char) (0x80 | ((c >> 0) & 0x3F));
            } else
            {
                chars.characters[count++] = (char) (0xC0 | ((c >> 6) & 0x1F));
                chars.characters[count++] = (char) (0x80 | ((c >> 0) & 0x3F));
            }
        }
        return chars;
    }
    
    static inline std::string getStringFromUTF8(const utf8_string& str)
    {
        int utflen = (int) str.size;
        int c, char2, char3;
        int count = 0;
        int chararr_count = 0;
        
        auto chararr = new char[utflen + 1];
        
        while (count < utflen)
        {
            c = (int) str.characters[count] & 0xff;
            if (c > 127) break;
            count++;
            chararr[chararr_count++] = (char) c;
        }
        
        while (count < utflen)
        {
            c = (int) str.characters[count] & 0xff;
            switch (c >> 4)
            {
                case 0:
                case 1:
                case 2:
                case 3:
                case 4:
                case 5:
                case 6:
                case 7:
                    /* 0xxxxxxx*/
                    count++;
                    chararr[chararr_count++] = (char) c;
                    break;
                case 12:
                case 13:
                    /* 110x xxxx   10xx xxxx*/
                    count += 2;
                    if (count > utflen)
                        throw "malformed input: partial character at end";
                    char2 = (int) str.characters[count - 1];
                    if ((char2 & 0xC0) != 0x80)
                        throw "malformed input around byte " + std::to_string(count);
                    chararr[chararr_count++] = (char) (((c & 0x1F) << 6) |
                                                       (char2 & 0x3F));
                    break;
                case 14:
                    /* 1110 xxxx  10xx xxxx  10xx xxxx */
                    count += 3;
                    if (count > utflen)
                        throw "malformed input: partial character at end";
                    char2 = (int) str.characters[count - 2];
                    char3 = (int) str.characters[count - 1];
                    if (((char2 & 0xC0) != 0x80) || ((char3 & 0xC0) != 0x80))
                        throw "malformed input around byte " + std::to_string(count - 1);
                    chararr[chararr_count++] = (char) (((c & 0x0F) << 12) |
                                                       ((char2 & 0x3F) << 6) |
                                                       ((char3 & 0x3F) << 0));
                    break;
                default:
                    /* 10xx xxxx,  1111 xxxx */
                    throw "malformed input around byte " + std::to_string(count);
                    break;
            }
        }
        chararr[utflen] = '\0';
        std::string strs{chararr};
        delete[] chararr;
        return strs;
    }
    
    struct TableColumn
    {
        std::string columnName;
        size_t maxColumnLength = 0;
        
        explicit TableColumn(std::string columnName): columnName(std::move(columnName))
        {}
    };
    
    struct TableRow
    {
        std::vector<std::string> rowValues;
    };
    
    class TableFormatter
    {
        private:
            std::string m_tableName;
            int m_columnPadding;
            [[maybe_unused]]
            int m_maxColumnWidth;
            std::vector<TableColumn> columns;
            std::vector<TableRow> rows;
            
            std::string generateTopSeparator(size_t size);
            
            std::string generateColumnHeader();
            
            std::string generateSeparator(size_t size);
            
            void updateMaxColumnLengths();
            
            [[nodiscard]] inline size_t columnSize(const TableColumn& column) const
            {
                return column.columnName.size() + m_columnPadding * 2;
            }
        
        public:
            explicit TableFormatter(std::string tableName = "", int columnPadding = 2, int maxColumnWidth = 500):
                    m_tableName(std::move(tableName)), m_columnPadding(columnPadding), m_maxColumnWidth(maxColumnWidth)
            {}
            
            inline void addColumn(const TableColumn& column)
            {
                columns.push_back(column);
            }
            
            inline void addColumn(std::string column)
            {
                columns.emplace_back(std::move(column));
            }
            
            inline void addRow(TableRow row)
            {
                if (row.rowValues.size() > columns.size())
                    throw "Cannot insert more rows than columns!\n";
                // ensure every row populates every column. This is important as the table generator assumes that all rows are complete!
                if (row.rowValues.size() < columns.size())
                    for (auto i = row.rowValues.size(); i < columns.size(); i++)
                        row.rowValues.emplace_back(" ");
                rows.push_back(std::move(row));
            }
            
            inline void addRow(const std::initializer_list<std::string>& values)
            {
                TableRow row;
                for (const auto& value : values)
                    row.rowValues.push_back(value);
                addRow(row);
            }
            
            std::vector<std::string> createTable(bool top = false, bool bottom = false);
    };
    
    static inline constexpr size_t MAX_CHILDREN = 16;
    
    /**
     * Structure which provides variables for the internal spacing of ASCII objects
     */
    struct ascii_padding_format
    {
        size_t horizontalPadding = 1;
        size_t verticalPadding = 1;
    };
    
    struct tree_format
    {
        ascii_padding_format boxFormat;
        
        int verticalPadding;
        int horizontalPadding;
        
        // should we remove preceding spaces?
        bool collapse = true;
        
        tree_format(): boxFormat{}, verticalPadding(1), horizontalPadding(4)
        {}
    };
    
    struct tree_node
    {
        std::string data;
        std::string title;
        blt::static_vector<tree_node*, 16> children;
    };
    
    class ascii_data
    {
        private:
            char* data_;
            size_t width_ = 0;
            size_t height_ = 0;
            size_t size_ = 0;
        public:
            ascii_data(size_t width, size_t height): data_(new char[width * height]), width_(width), height_(height), size_(width * height)
            {
                // he he he
                memset(data_, ' ', width * height);
            }
            
            ascii_data(const ascii_data& copy) = delete;
            
            ascii_data(ascii_data&& move) noexcept
            {
                data_ = move.data_;
                width_ = move.width_;
                height_ = move.height_;
                size_ = move.size_;
            }
            
            ascii_data& operator=(const ascii_data& copy) = delete;
            
            ascii_data& operator=(ascii_data&& move) noexcept
            {
                delete[] data_;
                data_ = move.data_;
                width_ = move.width_;
                height_ = move.height_;
                size_ = move.size_;
                return *this;
            };
            
            inline char& at(size_t x, size_t y)
            {
                return data_[x * height_ + y];
            }
            
            inline char* data()
            {
                return data_;
            }
            
            std::vector<std::string> toVec()
            {
                std::vector<std::string> vec;
                for (size_t j = 0; j < height(); j++)
                {
                    std::string line;
                    line.reserve(width());
                    for (size_t i = 0; i < width(); i++)
                    {
                        line += at(i, j);
                    }
                    vec.push_back(std::move(line));
                }
                return vec;
            }
            
            [[nodiscard]] char* data() const
            {
                return data_;
            }
            
            [[nodiscard]] inline size_t width() const
            {
                return width_;
            }
            
            [[nodiscard]] inline size_t height() const
            {
                return height_;
            }
            
            [[nodiscard]] inline size_t size() const
            {
                return size_;
            }
            
            ~ascii_data()
            {
                delete[] data_;
            }
    };
    
    class ascii_object
    {
        protected:
            size_t width_;
            size_t height_;
        public:
            ascii_object(size_t width, size_t height): width_(width), height_(height)
            {}
            
            /**
             * @return Internal width of the ascii object. This does not include the bordering box
             */
            [[nodiscard]] inline size_t width() const
            {
                return width_;
            }
            
            /**
             * @return Internal height of the ascii object. This does not include the border.
             */
            [[nodiscard]] inline size_t height() const
            {
                return height_;
            }
            
            /**
             * @return full height of the ascii box, includes the expected border around the box
             */
            [[nodiscard]] inline size_t full_height() const
            {
                return height_ + 2;
            }
            
            /**
             * @return full width of the ascii box, includes the expected border around the box.
             */
            [[nodiscard]] inline size_t full_width() const
            {
                return width_ + 2;
            }
    };
    
    class ascii_box : public ascii_object
    {
        public:
            std::string_view data;
            const ascii_padding_format& format;
        public:
            explicit ascii_box(std::string_view data,
                               const ascii_padding_format& format = {}): ascii_object(data.length() + (format.horizontalPadding * 2),
                                                                                      1 + (format.verticalPadding * 2)), data(data), format(format)
            {}
    };
    
    class ascii_titled_box : public ascii_object
    {
        public:
            std::string_view title;
            std::string_view data;
            const ascii_padding_format& format;
        public:
            ascii_titled_box(std::string_view title, std::string_view data,
                             const ascii_padding_format& format = {}):
                    ascii_object(std::max(data.length(), title.length()) + (format.horizontalPadding * 2),
                                 3 + (format.verticalPadding * 2)), title(title), data(data), format(format)
            {}
    };
    
    typedef std::variant<ascii_box, ascii_titled_box> box_type;
    
    class ascii_boxes
    {
        private:
            std::vector<box_type> boxes_;
            size_t width_ = 1;
            size_t height_ = 0;
            enum class STORED_TYPE
            {
                NONE, BOX, BOX_WITH_TITLE, BOTH
            };
            STORED_TYPE type = STORED_TYPE::NONE;
        public:
            ascii_boxes() = default;
            
            ascii_boxes(std::initializer_list<box_type> boxes)
            {
                for (const auto& b : boxes)
                    push_back(b);
            }
            
            void push_back(box_type&& box);
            
            inline void push_back(const box_type& box)
            {
                push_back(box_type(box));
            }
            
            inline std::vector<box_type>& boxes()
            {
                return boxes_;
            }
            
            [[nodiscard]] inline size_t width() const
            {
                return width_;
            }
            
            [[nodiscard]] inline size_t height() const
            {
                return height_;
            }
            
            [[nodiscard]] inline bool is_mixed() const
            {
                return type == STORED_TYPE::BOTH;
            }
    };
    
    typedef std::variant<box_type, ascii_boxes> box_container;
    
    ascii_data constructBox(const box_container& box, bool normalize_mixed_types = true);
    
    class BinaryTreeFormatter
    {
        public:
            // data classes
            
            
            struct Node
            {
                std::string data;
                Node* left = nullptr;
                Node* right = nullptr;
                
                explicit Node(std::string data): data(std::move(data))
                {}
                
                Node* with(Node* l, Node* r = nullptr)
                {
                    left = l;
                    right = r;
                    return this;
                }
                
                ~Node()
                {
                    delete left;
                    delete right;
                }
            };
        
        private:
            tree_format format;
            
            Node* root = nullptr;
        public:
            explicit BinaryTreeFormatter(std::string rootData, const tree_format& format = {}):
                    format(format), root(new Node(std::move(rootData)))
            {}
            
            std::vector<std::string> generateBox(Node* node) const;
            
            inline Node* getRoot()
            {
                return root;
            }
            
            std::vector<std::string> construct();
            
            BinaryTreeFormatter()
            {
                delete root;
            }
    };
    
}
#endif //BLT_TESTS_FORMAT_H
