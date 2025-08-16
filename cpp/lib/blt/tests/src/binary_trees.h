#pragma once

#include <blt/std/binary_tree.h>
#include <blt/std/random.h>
#include <blt/std/time.h>
#include <blt/std/string.h>
#include <iostream>

void printBinaryTree(blt::node_binary_search_tree<long>& tree) {
    auto nodes = tree.inOrderTraverse();
    for (auto n : nodes)
        std::cout << n->payload << " ";
    std::cout << "\n";
}

void binaryTreeTest(){
    
    using namespace blt;

    node_binary_search_tree<long> dataTree;
    dataTree.insert(6);
    dataTree.insert(3);
    dataTree.insert(2);
    dataTree.insert(4);
    dataTree.insert(10);
    dataTree.insert(13);
    dataTree.insert(8);
    dataTree.insert(16);

    printBinaryTree(dataTree);

    auto searchedNode = dataTree.search(10);
    std::cout << "10's children: "<< searchedNode->left->payload << ", " << searchedNode->right->payload << "\n";

    dataTree.remove(6);

    printBinaryTree(dataTree);

    //searchedNode = dataTree.search(8);
    //std::cout << "8's children: "<< searchedNode->left->payload << ", " << searchedNode->right->payload << "\n";
    

}