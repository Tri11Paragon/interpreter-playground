/*
 * Created by Brett on 09/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_BINARY_TREE_H
#define BLT_BINARY_TREE_H

#include <stdexcept>
#include <vector>
#include <blt/std/allocator.h>
#include <blt/std/types.h>
#include <iostream>
#include <memory>
// TODO: blt::queue
#include <queue>
#include <stack>

namespace blt
{
    
    class binary_search_tree_error : public std::runtime_error
    {
        public:
            explicit binary_search_tree_error(const std::string& string): runtime_error(string)
            {}
    };
    
    template<typename T, typename ALLOC = blt::area_allocator<T>>
    class AVL_node_tree
    {
        private:
            ALLOC alloc;
            
            struct node
            {
                T val;
                node* left;
                node* right;
                ALLOC& alloc;
                
                node(const T& t, ALLOC& alloc): val(t), alloc(alloc)
                {}
                
                node(T&& m, ALLOC alloc): val(m), alloc(alloc)
                {}
                
                node(const node& copy) = delete;
                
                node(node&& move) = delete;
                
                node& operator=(const node& copy) = delete;
                
                node& operator=(node&& move) = delete;
                
                ~node()
                {
                    left->~node();
                    alloc.deallocate(left);
                    right->~node();
                    alloc.deallocate(right);
                }
            };
            
            inline node* newNode(T&& t)
            {
                return new(alloc.allocate(1)) node(t);
            }
            
            node* root = nullptr;
        public:
            AVL_node_tree() = default;
            
            AVL_node_tree(const AVL_node_tree& copy) = delete;
            
            AVL_node_tree(AVL_node_tree&& move) = delete;
            
            AVL_node_tree& operator=(const AVL_node_tree& copy) = delete;
            
            AVL_node_tree& operator=(AVL_node_tree&& move) = delete;
            
            size_t height(node* start = nullptr)
            {
                if (start == nullptr)
                    start = root;
                if (start == nullptr)
                    return 0;
                std::queue<node*> nodes;
                nodes.push(start);
                size_t height = 0;
                while (!nodes.empty())
                {
                    height++;
                    size_t level_count = nodes.size();
                    while (level_count-- > 0)
                    {
                        if (nodes.front()->left != nullptr)
                            nodes.push(nodes.front()->left);
                        if (nodes.front()->right != nullptr)
                            nodes.push(nodes.front()->right);
                        nodes.pop();
                    }
                }
                return height;
            }
            
            void insert(const T& t)
            {
                if (root == nullptr)
                {
                    root = newNode(t);
                    return;
                }
                node* search = root;
                node* parent = nullptr;
                while (true)
                {
                    if (t < search->val)
                    {
                        if (search->left == nullptr)
                        {
                            search->left = newNode(t);
                            break;
                        }
                        search = search->left;
                    } else
                    {
                        if (search->right == nullptr)
                        {
                            search->right = newNode(t);
                            break;
                        }
                        search = search->right;
                    }
                    parent = search;
                }
            }
            
            ~AVL_node_tree()
            {
                root->~node();
                alloc.deallocate(root);
            }
    };
    
    template<typename K, typename V>
    class range_tree_t
    {
        public:
            struct node_t
            {
                K k;
                V v;
                blt::i64 children = 0;
                
                node_t(K k, V v): k(std::move(k)), v(std::move(v))
                {}
            };
            
            void insert(K k, V v)
            {
                auto insert_point = nodes.begin();
                auto insert_parent = insert_point;
                
                while (insert_point != nodes.end())
                {
                    // no children
                    if (insert_point->children == 0)
                    {
                        ++insert_point->children;
                        ++insert_point;
                        break;
                    } else if (insert_point->children == 1)
                    {
                        // 1 child case
                        insert_parent = insert_point;
                        // find if child is min & move to it
                        ++insert_point;
                        bool min = insert_point->k < insert_parent->k;
                        if (k < insert_parent->k)
                        {
                            // if the parent's child is a min value, then we can safely move towards it
                            if (min)
                                continue;
                            else
                            {
                                // otherwise we can break and this will insert the new node as the new min.
                                ++insert_parent->children;
                                break;
                            }
                        } else
                        {
                            // parents child is min, so we move past it
                            if (min)
                            {
                                insert_point = skip_children(insert_point);
                                // can break as we insert here
                                ++insert_parent->children;
                                break;
                            } else
                            {
                                // parents child is max, we can safely move towards it
                                continue;
                            }
                        }
                    } else
                    {
                        insert_parent = insert_point;
                        ++insert_point;
                        if (k < insert_parent->k)
                            continue;
                        else
                            insert_point = skip_children(insert_point);
                    }
                }
                
                nodes.insert(insert_point, {std::move(k), std::move(v)});
            }
            
            void print(std::ostream& out, bool pretty_print)
            {
                std::stack<blt::size_t> left;
                blt::size_t indent = 0;
                for (auto& v : nodes)
                {
                    if (v.children > 0)
                    {
                        create_indent(out, indent, pretty_print) << "(";
                        indent++;
                        left.emplace(v.children);
                        out << v.k << ": " << v.v << end_indent(pretty_print);
                    } else
                        create_indent(out, indent, pretty_print) << v.k << ": " << v.v << end_indent(pretty_print);
                    while (!left.empty())
                    {
                        auto top = left.top();
                        left.pop();
                        if (top == 0)
                        {
                            indent--;
                            create_indent(out, indent, pretty_print) << ")" << end_indent(pretty_print);
                            continue;
                        } else
                        {
                            if (!pretty_print)
                                out << " ";
                            left.push(top - 1);
                            break;
                        }
                    }
                }
                while (!left.empty())
                {
                    auto top = left.top();
                    left.pop();
                    if (top == 0)
                    {
                        indent--;
                        create_indent(out, indent, pretty_print) << ")" << end_indent(pretty_print);
                        continue;
                    } else
                    {
                        out << "TREE MISMATCH";
                        break;
                    }
                }
                out << '\n';
            }
            
            std::optional<V> search(const K& k)
            {
                auto point = nodes.begin();
                while (point != nodes.end())
                {
                    if (k == point->k)
                        return point->v;
                    if (point->children == 0)
                        return {};
                    auto parent = point;
                    ++point;
                    auto min = point->k < parent->k;
                    if (k >= parent->k)
                    {
                        if (min)
                            point = skip_children(point);
                    }
                }
                return {};
            }
        
        private:
            auto skip_children(typename std::vector<node_t>::iterator begin)
            {
                blt::i64 children_left = 0;
                
                do
                {
                    if (children_left != 0)
                        children_left--;
                    if (begin->children > 0)
                        children_left += begin->children;
                    ++begin;
                } while (children_left > 0);
                
                return begin;
            }
            
            std::ostream& create_indent(std::ostream& out, blt::size_t amount, bool pretty_print)
            {
                if (!pretty_print)
                    return out;
                for (blt::size_t i = 0; i < amount; i++)
                    out << '\t';
                return out;
            }
            
            std::string_view end_indent(bool pretty_print)
            {
                return pretty_print ? "\n" : "";
            }
            
            std::vector<node_t> nodes;
    };
    
}

#endif //BLT_BINARY_TREE_H
