#!/bin/bash
mkdir debug release debug-addr reldebug-addr clang-debug clang-release clang-debug-addr clang-reldebug-addr

# GNU
cmake -S ./ -B debug -DCMAKE_BUILD_TYPE=Debug -DENABLE_ADDRSAN=OFF -DENABLE_UBSAN=OFF -G Ninja
cmake -S ./ -B release -DCMAKE_BUILD_TYPE=Release -DENABLE_ADDRSAN=OFF -DENABLE_UBSAN=OFF -G Ninja
cmake -S ./ -B debug-addr -DCMAKE_BUILD_TYPE=Debug -DENABLE_ADDRSAN=ON -DENABLE_UBSAN=ON -G Ninja
cmake -S ./ -B reldebug-addr -DCMAKE_BUILD_TYPE=RelWithDebInfo -DENABLE_ADDRSAN=ON -DENABLE_UBSAN=ON -G Ninja

# Clang
cmake -S ./ -B clang-debug -DCMAKE_BUILD_TYPE=Debug -DENABLE_ADDRSAN=OFF -DENABLE_UBSAN=OFF -G Ninja
cmake -S ./ -B clang-release -DCMAKE_BUILD_TYPE=Release -DENABLE_ADDRSAN=OFF -DENABLE_UBSAN=OFF -G Ninja
cmake -S ./ -B clang-debug-addr -DCMAKE_BUILD_TYPE=Debug -DENABLE_ADDRSAN=ON -DENABLE_UBSAN=ON -G Ninja
cmake -S ./ -B clang-reldebug-addr -DCMAKE_BUILD_TYPE=RelWithDebInfo -DENABLE_ADDRSAN=ON -DENABLE_UBSAN=ON -G Ninja

ninja -j 30 -C debug
ninja -j 30 -C release
ninja -j 30 -C debug-addr
ninja -j 30 -C reldebug-addr

ninja -j 30 -C clang-debug
ninja -j 30 -C clang-release
ninja -j 30 -C clang-debug-addr
ninja -j 30 -C clang-reldebug-addr

./release/BLT_TESTS
