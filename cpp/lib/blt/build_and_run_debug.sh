#!/bin/bash
cd cmake-build-debug
cmake -DBUILD_TESTS=ON -G Ninja ../ && ninja -j 16 && ./BLT_TESTS
cd ..
