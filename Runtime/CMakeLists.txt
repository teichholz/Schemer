cmake_minimum_required(VERSION 3.15)
project(Runtime)

set(CMAKE_CXX_STANDARD 17)

add_library(Runtime STATIC library.cpp test.cpp)
find_library(GC gc)
message(${GC})
target_link_libraries(Runtime PUBLIC ${GC})
