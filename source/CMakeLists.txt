cmake_minimum_required(VERSION 3.0.2)

project(qt_thing)
SET (CMAKE_CXX_COMPILER             "/usr/bin/clang++")
set(CMAKE_CXX_FLAGS "-ggdb -std=c++14")

find_package(Qt5Widgets)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)


add_library(main_win main_win.cpp)
target_link_libraries(main_win Qt5::Widgets)


add_executable(qt_thing_app main.cpp)
target_link_libraries(qt_thing_app main_win)


