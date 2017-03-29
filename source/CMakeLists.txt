cmake_minimum_required(VERSION 3.0.2)

project(qt_thing)

# Find the QtWidgets library
find_package(Qt5Widgets)

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
# Instruct CMake to run moc automatically when needed.
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)


# Tell CMake to create the helloworld executable
add_executable(qt_thing_app main.cpp)

# Use the Widgets module from Qt 5.
target_link_libraries(qt_thing_app Qt5::Widgets)
