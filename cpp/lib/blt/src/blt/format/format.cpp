/*
 * Created by Brett on 26/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */
#include <blt/format/format.h>
#include <blt/std/string.h>
#include <cmath>
#include "blt/logging/logging.h"
#include "blt/std/assert.h"
#include "blt/std/utility.h"
#include <stack>
#include <queue>
#include <algorithm>
#include <blt/std/variant.h>

inline constexpr char SEPARATOR = '-';
inline constexpr char CONNECTOR = '+';
inline constexpr char BAR = '|';

/**
 * Returns information from a box_type variant, useful for getting width()/height()
 */
#define getBoxData(variant, getter)                                                             \
    std::visit(blt::lambda_visitor{                                                             \
        [](const blt::string::ascii_box& box) -> decltype(auto) { return box.getter; },                   \
        [](const blt::string::ascii_titled_box& box) -> decltype(auto) { return box.getter; }             \
    }, variant)

std::vector<std::string> blt::string::TableFormatter::createTable(bool top, bool bottom)
{
    std::vector<std::string> table;
    const auto& tableHeader = generateColumnHeader();
    const auto& topSeparator = generateTopSeparator(tableHeader.size());
    const auto& lineSeparator = generateSeparator(tableHeader.size() - 1);
    
    if (top)
        table.push_back(topSeparator);
    
    table.push_back(tableHeader);
    table.push_back(lineSeparator);
    
    for (const auto& row : rows)
    {
        std::string rowString = "|";
        for (unsigned int i = 0; i < row.rowValues.size(); i++)
        {
            const auto& rowValue = row.rowValues[i];
            const auto& column = columns[i];
            const int spaceLeft = int(column.maxColumnLength) - int(rowValue.size());
            // we want to prefer putting the space on the right size, flooring left and ceiling right ensures this.
            rowString += createPadding((int) std::floor(spaceLeft / 2.0) + m_columnPadding);
            rowString += rowValue;
            rowString += createPadding((int) std::ceil(spaceLeft / 2.0) + m_columnPadding);
            rowString += "|";
        }
        table.push_back(rowString);
    }
    
    if (bottom)
        table.push_back(lineSeparator);
    
    return table;
}

std::string blt::string::TableFormatter::generateColumnHeader()
{
    updateMaxColumnLengths();
    std::string header = "|";
    
    for (unsigned int i = 0; i < columns.size(); i++)
    {
        const auto& column = columns[i];
        auto columnPaddingLength = (int(column.maxColumnLength) - int(column.columnName.size())) / 2.0;
        header += createPadding(int(m_columnPadding + (int) std::floor(columnPaddingLength)));
        
        header += column.columnName;
        
        header += createPadding(int(m_columnPadding + (int) std::ceil(columnPaddingLength)));
        if (i < columns.size() - 1)
            header += "|";
    }
    
    header += "|";
    return header;
}

std::string blt::string::TableFormatter::generateTopSeparator(size_t size)
{
    auto sizeOfName = m_tableName.empty() ? 0 : m_tableName.size() + 4;
    auto sizeNameRemoved = size - sizeOfName;
    
    std::string halfWidthLeftSeparator;
    std::string halfWidthRightSeparator;
    
    auto sizeNameFloor = (size_t) std::floor((double) sizeNameRemoved / 2.0);
    auto sizeNameCeil = (size_t) std::ceil((double) sizeNameRemoved / 2.0);
    
    halfWidthLeftSeparator.reserve(sizeNameCeil);
    halfWidthRightSeparator.reserve(sizeNameFloor);
    
    halfWidthLeftSeparator += "+";
    
    for (unsigned int i = 0; i < sizeNameFloor - 1; i++)
        halfWidthLeftSeparator += "-";
    
    for (unsigned int i = 0; i < sizeNameCeil - 1; i++)
        halfWidthRightSeparator += "-";
    
    halfWidthRightSeparator += "+";
    
    std::string separator;
    separator += halfWidthLeftSeparator;
    if (sizeOfName != 0)
    {
        separator += "{ ";
        separator += m_tableName;
        separator += " }";
    }
    separator += halfWidthRightSeparator;
    return separator;
}

std::string blt::string::TableFormatter::generateSeparator(size_t size)
{
    size_t nextIndex = 0;
    size_t currentColumnIndex = 0;
    std::string wholeWidthSeparator;
    for (unsigned int i = 0; i < size; i++)
    {
        if (i == nextIndex)
        {
            auto currentColumnSize = columns[currentColumnIndex++].maxColumnLength + m_columnPadding * 2;
            nextIndex += currentColumnSize + 1;
            wholeWidthSeparator += "+";
        } else
            wholeWidthSeparator += "-";
    }
    wholeWidthSeparator += "+";
    return wholeWidthSeparator;
}

void blt::string::TableFormatter::updateMaxColumnLengths()
{
    for (unsigned int i = 0; i < columns.size(); i++)
    {
        auto& column = columns[i];
        column.maxColumnLength = column.columnName.size();
        for (const auto& row : rows)
        {
            column.maxColumnLength = std::max(column.maxColumnLength, row.rowValues[i].size());
        }
    }
}

/*
 * -----------------------
 *     Tree Formatter
 * -----------------------
 */

struct node_data
{
    blt::string::BinaryTreeFormatter::Node* node;
    std::vector<std::string> box;
    size_t level;
    bool hasHomosexuality = true;
    
    node_data(blt::string::BinaryTreeFormatter::Node* node, size_t level, bool homoness): node(node), level(level), hasHomosexuality(homoness)
    {}
};

struct level_data
{
    std::vector<node_data> level;
    size_t count = 0;
    size_t depth = 0;
    size_t max_horizontal_length = 0;
};

std::vector<std::string> blt::string::BinaryTreeFormatter::construct()
{
    std::stack<level_data> levels;
    std::queue<node_data> bfs;
    bfs.emplace(root, 0, false);
    // construct a stack of the highest node -> the lowest node.
    level_data currentLevel;
    currentLevel.depth = 0;
    size_t maxLineLength = 0;
    while (!bfs.empty())
    {
        auto n = bfs.front();
        if (currentLevel.depth != n.level)
        {
            levels.push(currentLevel);
            currentLevel = {};
        }
        bool isAHomo = false;
        currentLevel.count++;
        if (n.node != nullptr)
        {
            auto box = generateBox(n.node);
            currentLevel.max_horizontal_length = std::max(currentLevel.max_horizontal_length, box[0].size());
            std::string replacement = "-";
            // UGLY TODO: fix this
            if (n.node->left != nullptr)
                replacement = "$";
            else if (n.node->right != nullptr)
                replacement = "#";
            if (replacement[0] == '$' && n.node->right != nullptr)
                replacement = "@";
            isAHomo = n.node->left && !n.node->right;
            blt::string::replaceAll(box.front(), "%", replacement);
            n.box = std::move(box);
        }
        currentLevel.level.push_back(n);
        currentLevel.depth = n.level;
        bfs.pop();
        //std::cout << "Node at level " << n.level << " " << n.node << "\n";
        if (n.node == nullptr)
            continue;
        if (n.node->left != nullptr)
            bfs.emplace(n.node->left, n.level + 1, isAHomo);
        else
            bfs.emplace(nullptr, n.level + 1, isAHomo);
        
        if (n.node->right != nullptr)
            bfs.emplace(n.node->right, n.level + 1, isAHomo);
        else
            bfs.emplace(nullptr, n.level + 1, isAHomo);
    }
    std::vector<std::string> lines;
    size_t lineLength = 0;
    //size_t lastLineLength = 0;
    const size_t lineHeight = format.verticalPadding * 2 + 3;
    //std::cout << levels.size() << "\n";
    const size_t verticalSpacing =
            format.boxFormat.horizontalPadding % 2 == 0 ? format.boxFormat.horizontalPadding + 1 : format.boxFormat.horizontalPadding;
    while (!levels.empty())
    {
        std::vector<std::string> currentLines;
        const auto& n = levels.top();
        
        for (const auto& b : n.level)
        {
            std::vector<std::string> box = b.box;
            if (b.node == nullptr || box.empty())
            {
                for (size_t i = 0; i < lineHeight; i++)
                    box.push_back(createPadding(n.max_horizontal_length));
            }
            if (currentLines.empty())
            {
                for (const auto& v : box)
                    currentLines.push_back(v);
            } else
            {
                BLT_ASSERT(currentLines.size() == box.size() && "Box lines should match current lines!");
                for (size_t i = 0; i < currentLines.size(); i++)
                {
                    currentLines[i] += createPadding(format.boxFormat.verticalPadding);
                    //currentLines[i] += createPadding(format.horizontalSpacing + static_cast<std::int64_t>(lineLength / (n.level.size() + 1)));
                    currentLines[i] += box[i];
                }
            }
        }
        // TODO:
        //std::int64_t padLength = 0;
        //if (n.level.size() == 1)
        //    padLength = ((static_cast<std::int64_t>(lineLength) - static_cast<std::int64_t>(currentLines[0].length())) / 2);
        //else
        //    padLength = ((static_cast<std::int64_t>(lineLength) - static_cast<std::int64_t>(lineLength / (n.level.size()))) / 2);
        std::int64_t padLength = ((static_cast<std::int64_t>(lineLength) - static_cast<std::int64_t>(currentLines[0].length())) / 2);
        //std::int64_t padLength = ((static_cast<std::int64_t>(lineLength) - static_cast<std::int64_t>(lineLength / (n.level.size()))) / 2);
        if (padLength < 0)
            padLength = 0;
        for (const auto& v : currentLines)
        {
            lineLength = std::max(lineLength, v.length());
            lines.push_back(createPadding(padLength) + v);
            maxLineLength = std::max(maxLineLength, lines.back().length());
            //lastLineLength = lines.back().length();
        }
        levels.pop();
        if (!levels.empty())
            for (size_t i = 0; i < verticalSpacing; i++)
                lines.emplace_back(" ");
    }
    
    for (auto& line : lines)
        if (line.length() < maxLineLength)
            line += createPadding(maxLineLength - line.length());
    
    std::reverse(lines.begin(), lines.end());
    
    size_t index = 1;
    size_t minimal = std::numeric_limits<size_t>::max();
    while (index < lines.size())
    {
        if (auto poses = blt::string::containsAll(lines[index], std::unordered_set{'$', '#', '@'}))
        {
            const auto& nextLine = lines[index + verticalSpacing + 1];
            auto poses2 = blt::string::containsAll(nextLine, std::unordered_set{'%'}).value();
            size_t consume = 0;
            for (auto p : poses.value())
            {
                char type = lines[index][p];
                for (size_t n = 0; n < verticalSpacing / 2; n++)
                    lines[index + n + 1][p] = '|';
                auto start = index + (verticalSpacing / 2) + 1;
                switch (type)
                {
                    case '@':
                        for (size_t i = poses2[consume] + 1; i < poses2[consume + 1]; i++)
                            lines[start][i] = '-';
                        lines[start][poses2[consume]] = '+';
                        lines[start][poses2[consume + 1]] = '+';
                        for (size_t n = 0; n < verticalSpacing / 2; n++)
                        {
                            lines[start + n + 1][poses2[consume]] = '|';
                            lines[start + n + 1][poses2[consume + 1]] = '|';
                        }
                        consume += 2;
                        break;
                    case '$':
                        for (size_t i = poses2[consume] + 1; i < p; i++)
                            lines[start][i] = '-';
                        lines[start][poses2[consume]] = '+';
                        for (size_t n = 0; n < verticalSpacing / 2; n++)
                            lines[start + n + 1][poses2[consume]] = '|';
                        consume++;
                        break;
                    case '#':
                        for (size_t i = p; i < poses2[consume]; i++)
                            lines[start][i] = '-';
                        lines[start][poses2[consume]] = '+';
                        for (size_t n = 0; n < verticalSpacing / 2; n++)
                            lines[start + n + 1][poses2[consume]] = '|';
                        consume++;
                        break;
                    default:
                        break;
                }
                lines[start][p] = '+';
            }
        }
        blt::string::replaceAll(lines[index], "%", "+");
        blt::string::replaceAll(lines[index], "#", "+");
        blt::string::replaceAll(lines[index], "@", "+");
        blt::string::replaceAll(lines[index], "$", "+");
        if (format.collapse)
        {
            for (size_t i = 0; i < lines[index].size(); i++)
            {
                if (lines[index][i] != ' ')
                {
                    minimal = std::min(minimal, i);
                    break;
                }
            }
        }
        index++;
    }
    blt::string::replaceAll(lines.front(), "%", "-");
    if (format.collapse)
    {
        for (auto& line : lines)
            line = line.substr(minimal);
    }
    return lines;
}

std::vector<std::string> blt::string::BinaryTreeFormatter::generateBox(blt::string::BinaryTreeFormatter::Node* node) const
{
    if (node == nullptr)
        return {};
    std::vector<std::string> lines;
    
    auto& data = node->data;
    auto dataLength = data.length();
    auto paddingLeft = format.horizontalPadding;
    auto paddingRight = format.horizontalPadding;
    auto totalLength = paddingLeft + paddingRight + dataLength;
    
    if (totalLength % 2 != 0)
    {
        paddingRight += 1;
        totalLength += 1;
    }
    
    // create horizontal line based on the calculated length
    std::string hline = createLine(totalLength, '+', '-');
    hline[(hline.size() - 1) / 2] = '%';
    std::string paddedLine = createLine(totalLength, '|', ' ');
    std::string dataStr;
    dataStr.reserve(totalLength);
    dataStr += '|';
    dataStr += createPadding(std::max(paddingLeft, 1) - 1);
    dataStr += data;
    dataStr += createPadding(std::max(paddingRight, 1) - 1);
    dataStr += '|';
    
    lines.push_back(hline);
    for (int i = 0; i < format.verticalPadding; i++)
        lines.push_back(paddedLine);
    lines.push_back(std::move(dataStr));
    for (int i = 0; i < format.verticalPadding; i++)
        lines.push_back(paddedLine);
    
    lines.push_back(std::move(hline));
    
    return lines;
}

std::string blt::string::createLine(size_t totalLength, char endingChar, char spacingChar)
{
    std::string line;
    line.reserve(totalLength);
    line += endingChar;
    line += createPadding(totalLength - 2, spacingChar);
    line += endingChar;
    return line;
}

std::string blt::string::createPadding(size_t length, char spacing)
{
    std::string padding;
    padding.reserve(length);
    for (size_t i = 0; i < length; i++)
        padding += spacing;
    return padding;
}

namespace blt::string
{
    
    void constructVerticalSeparator(blt::string::ascii_data& data, size_t offset, size_t height)
    {
        for (size_t i = 0; i < height; i++)
        {
            if (data.at(offset, i) != '+')
                data.at(offset, i) = BAR;
        }
    }
    
    void addBox(blt::string::ascii_data& data, const blt::string::box_type& box, size_t width_offset, bool has_titled_friends = false,
                size_t height_offset = 1)
    {
        // get box data
        auto width = getBoxData(box, width());
        auto full_width = getBoxData(box, full_width());
        auto full_height = getBoxData(box, full_height());
        const auto& box_format = getBoxData(box, format);
        const auto& box_data = getBoxData(box, data);
        
        size_t titlePad = box_format.horizontalPadding + 1;
        size_t dataPad = box_format.horizontalPadding + 1;
        
        // construct titled box's title
        if (has_titled_friends || std::holds_alternative<ascii_titled_box>(box))
        {
            for (size_t i = 0; i < full_width; i++)
            {
                char c = SEPARATOR;
                if (i == 0 || i == full_width - 1)
                    c = CONNECTOR;
                data.at(width_offset + i, 2) = c;
            }
            // TODO: this is ugly
            if (std::holds_alternative<ascii_titled_box>(box))
            {
                const auto& box_title = std::get<ascii_titled_box>(box).title;
                
                // if one of the strings are larger than there will be a misalignment as the width of the box is based on the largest string,
                // so we need to add an offset to the smallest string for centering.
                if (box_data.length() > box_title.length())
                    titlePad += (box_data.length() - box_title.length()) / 2;
                else
                    dataPad += (box_title.length() - box_data.length()) / 2;
                
                // copy in the title and data string
                for (size_t i = 0; i < box_title.size(); i++)
                    data.at(width_offset + titlePad + i, 1) = box_title[i];
            } else
                full_height += 2;
            height_offset = 3;
        }
        
        // create the horizontal separators
        for (size_t i = 0; i < full_width; i++)
        {
            char c = SEPARATOR;
            if (i == 0 || i == full_width - 1)
                c = CONNECTOR;
            data.at(width_offset + i, 0) = c;
            data.at(width_offset + i, full_height - 1) = c;
        }
        
        for (size_t i = 0; i < box_data.size(); i++)
            data.at(width_offset + dataPad + i, height_offset + box_format.verticalPadding) = box_data[i];
        // add the vertical separator
        constructVerticalSeparator(data, width_offset + width + 1, full_height);
    }
    
    blt::string::ascii_data constructBox(const blt::string::box_container& box, bool normalize_mixed_types)
    {
        auto width = std::visit(blt::lambda_visitor{
                [](const blt::string::box_type& box) -> size_t { return getBoxData(box, full_width()); },
                [](const blt::string::ascii_boxes& boxes) -> size_t { return boxes.width(); }
        }, box);
        auto height = std::visit(blt::lambda_visitor{
                [](const blt::string::box_type& box) -> size_t { return getBoxData(box, full_height()); },
                [](const blt::string::ascii_boxes& boxes) -> size_t { return boxes.height(); }
        }, box);
        
        string::ascii_data data(width, height);
        
        constructVerticalSeparator(data, 0, height);
        
        if (std::holds_alternative<blt::string::box_type>(box))
            addBox(data, std::get<blt::string::box_type>(box), 0);
        else
        {
            auto bv = std::get<blt::string::ascii_boxes>(box);
            size_t offset = 0;
            for (const auto& b : bv.boxes())
            {
                addBox(data, b, offset, bv.is_mixed() && normalize_mixed_types);
                offset += getBoxData(b, width()) + 1;
            }
        }
        
        return data;
    }
    
    void string::ascii_boxes::push_back(string::box_type&& box)
    {
        // I don't like this.
        switch (type)
        {
            case STORED_TYPE::NONE:
                if (std::holds_alternative<ascii_box>(box))
                    type = STORED_TYPE::BOX;
                if (std::holds_alternative<ascii_titled_box>(box))
                    type = STORED_TYPE::BOX_WITH_TITLE;
                break;
            case STORED_TYPE::BOX:
                if (std::holds_alternative<ascii_titled_box>(box))
                    type = STORED_TYPE::BOTH;
                break;
            case STORED_TYPE::BOX_WITH_TITLE:
                if (std::holds_alternative<ascii_box>(box))
                    type = STORED_TYPE::BOTH;
                break;
            case STORED_TYPE::BOTH:
                break;
        }
        width_ += getBoxData(box, width()) + 1;
        // should all be the same
        height_ = std::max(getBoxData(box, full_height()), height_);
        boxes_.push_back(box);
    }
}