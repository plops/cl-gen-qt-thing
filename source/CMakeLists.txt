cmake_minimum_required(VERSION 3.0.2)

project(qt_thing)
SET (CMAKE_CXX_COMPILER             "/usr/bin/clang++")
set(CMAKE_CXX_FLAGS "-O3 -std=c++1z -fstack-protector-strong -fident -fasynchronous-unwind-tables -Wall -Wextra -pedantic-errors -Wsign-promo -Wnon-virtual-dtor -Winit-self -Wno-deprecated-declarations -pipe -D_FILE_OFFSET_BITS=64 -march=native   -ffast-math -Wno-nested-anon-types -ggdb") # -flto

find_package(Qt5Widgets)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)
#set(CMAKE_AUTOUIC ON)


add_library(main_win main_win.cpp)
target_link_libraries(main_win Qt5::Widgets)


add_executable(qt_thing_app main.cpp)
target_link_libraries(qt_thing_app main_win)


