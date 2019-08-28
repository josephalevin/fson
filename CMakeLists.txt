# Copyright 2019 Keith F. Prussing
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

cmake_minimum_required(VERSION 3.6.1)

cmake_policy(SET CMP0048 NEW) # To get package versioning working
project(FSON VERSION 1.0.0 LANGUAGES Fortran C)

set(CMAKE_MACOSX_RPATH 1)

add_library(FSON SHARED
    src/fson_string_m.f90
    src/fson_value_m.f90
    src/fson_path_m.f90
    src/fson.f90
)

add_library(FSON::FSON ALIAS FSON)

target_include_directories(FSON
    PUBLIC
        $<INSTALL_INTERFACE:include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}
)

target_compile_options(FSON
    PUBLIC
    PRIVATE
    $<$<OR:$<Fortran_COMPILER_ID:GNU>,$<Fortran_COMPILER_ID:G95>>:
        -Wno-maybe-uninitialized
    >
)

#
# Installation details
#
include(GNUInstallDirs)
set(INSTALL_CONFIGDIR ${CMAKE_INSTALL_LIBDIR}/cmake/fson)

install(TARGETS FSON
    EXPORT fson-targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

# Add the correct module for `use` here.
install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/fson.mod
    ${CMAKE_CURRENT_BINARY_DIR}/fson_path_m.mod
    ${CMAKE_CURRENT_BINARY_DIR}/fson_value_m.mod
    ${CMAKE_CURRENT_BINARY_DIR}/fson_string_m.mod
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

install(EXPORT fson-targets
    FILE
        FSONTargets.cmake
    NAMESPACE
        FSON::
    DESTINATION
        ${INSTALL_CONFIGDIR}
)

include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/FSONConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY AnyNewerVersion
)

configure_package_config_file(
    ${CMAKE_CURRENT_LIST_DIR}/cmake/FSONConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/FSONConfig.cmake
    INSTALL_DESTINATION ${INSTALL_CONFIGDIR}
)

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/FSONConfig.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/FSONConfigVersion.cmake
    DESTINATION ${INSTALL_CONFIGDIR}
)

#
# Export for testing
#
export(EXPORT fson-targets
    FILE
        ${CMAKE_CURRENT_BINARY_DIR}/FSONTargets.cmake
    NAMESPACE
        FSON::
)

#
# Define the test routines
#

include(FetchContent OPTIONAL RESULT_VARIABLE fc_LOADED)
if (${fc_LOADED} MATCHES NOTFOUND)
    include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/FetchContent.cmake)
endif()
FetchContent_Declare(zofu
    GIT_REPOSITORY https://github.com/acroucher/zofu.git
)
FetchContent_GetProperties(zofu)
if (NOT zofu_POPULATED)
    FetchContent_Populate(zofu)
endif()

# Building zofu could be replaced with the proper target if it had a
# CMake build.
add_library(zofu SHARED
    ${zofu_SOURCE_DIR}/src/zofu.F90
    ${zofu_SOURCE_DIR}/src/zofu_kinds.F90
    ${zofu_SOURCE_DIR}/src/zofu_scan.F90
    ${zofu_SOURCE_DIR}/src/zofu_str_utils.F90
)
add_executable(zofu-driver ${zofu_SOURCE_DIR}/src/zofu_driver.F90)
target_link_libraries(zofu-driver zofu)

# Defining each test is a matter of generating the source, building the
# executable, and adding the test.
enable_testing()
foreach(test_name fson_test fson_test2)
    add_library(${test_name}_lib
        src/tests/${test_name}_zofu.f90
    )
    target_link_libraries(${test_name}_lib FSON::FSON zofu)

    add_custom_command(OUTPUT
        ${CMAKE_CURRENT_BINARY_DIR}/${test_name}_driver.f90
        COMMAND ${CMAKE_CURRENT_BINARY_DIR}/zofu-driver
            ${CMAKE_CURRENT_SOURCE_DIR}/src/tests/${test_name}_zofu.f90
            ${CMAKE_CURRENT_BINARY_DIR}/${test_name}_driver.f90
            DEPENDS zofu-driver
        )

    add_executable(${test_name}
        ${CMAKE_CURRENT_SOURCE_DIR}/src/tests/${test_name}_zofu.f90
        ${CMAKE_CURRENT_BINARY_DIR}/${test_name}_driver.f90)
    target_link_libraries(${test_name} FSON::FSON zofu)
    add_test(NAME ${test_name} COMMAND ${test_name}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src/tests)
endforeach()

# Register the package in the user's registry
export(PACKAGE FSON)

