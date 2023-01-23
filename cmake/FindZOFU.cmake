# Summary:
# Implementation of find_package() for zofu Fortran unit testing
# library from https://github.com/acroucher/zofu
#
# Input:
# These variables are unconfirmed, set by the user
# ZOFU_BINARY_PATH - path containing zofu-driver. Windows also contains libzofu.dll
# ZOFU_LIBRARY_PATH - path containing libzofu.so, libzofu.dll.a
# ZOFU_MODULE_PATH - path containing zofu.mod, zofu_kinds.mod, zofu_scan.mod, zofu_str_utils.mod
#
# Output:
# These variables are confirmed or set by CMake
# ZOFU_FOUND - boolean; status indicating output variables are set
# ZOFU_DRIVER - full path to zofu-driver
# ZOFU_LIBRARY_NAME - common name of library ("zofu"; as passed to -l)
# ZOFU_LIBRARY - full path to libzofu.so (or .a or .dll.a)
# ZOFU_LIBRARY_DIR - confirmed library path (should equal ZOFU_LIBRARY_PATH) as passed to -L
# ZOFU_MODULE_DIR - confirmed Fortran module path (should equal ZOFU_MODULE_PATH) as passed to -I

### Initial conditions ###

set(ZOFU_FOUND OFF)

set(ZOFU_LIBRARY_NAME zofu)

set(ZOFU_BINARY_PATH  "" CACHE PATH "zofu binary directory path")
set(ZOFU_LIBRARY_PATH "" CACHE PATH "zofu library directory path")
set(ZOFU_MODULE_PATH  "" CACHE PATH "zofu Fortran module directory path")

### Search for artifacts ###

# Find zofu-driver in bindir
find_program(ZOFU_DRIVER zofu-driver
    PATHS "${ZOFU_BINARY_PATH}" ~/.local/bin
)

# Find zofu library in libdir (let CMake guess at actual file name
# based on common library name)
find_library(ZOFU_LIBRARY ${ZOFU_LIBRARY_NAME}
    PATHS "${ZOFU_LIBRARY_PATH}" ~/.local/lib
)

if(ZOFU_LIBRARY)
    get_filename_component(ZOFU_LIBRARY_DIR "${ZOFU_LIBRARY}" DIRECTORY)
    set(ZOFU_FOUND ON)
# else()
#     # ZOFU_FOUND is already OFF - just PASS
endif()

# Find zofu module directory based on single check for zofu.mod
find_file(ZOFU_MODULE ${ZOFU_LIBRARY_NAME}.mod
    PATHS "${ZOFU_MODULE_PATH}" ~/.local/finclude/zofu ~/Documents/Projects/git-proj/zofu/build/libzofu.so.p
)

if(ZOFU_MODULE)
    get_filename_component(ZOFU_MODULE_DIR "${ZOFU_MODULE}" DIRECTORY)
    # If ZOFU_FOUND is ON, leave it ON - just PASS
else()
    # If ZOFU_FOUND is ON, turn it OFF
    set(ZOFU_FOUND OFF)
endif()

### Display results ###

# Diagnostics
if(ZOFU_FOUND)
    message(STATUS "Zofu Fortran testing library is available")
    # message(STATUS "ZOFU_DRIVER set to ${ZOFU_DRIVER}")
    # message(STATUS "ZOFU_LIBRARY_NAME set to ${ZOFU_LIBRARY_NAME}")
    # message(STATUS "ZOFU_LIBRARY set to ${ZOFU_LIBRARY}")
    # message(STATUS "ZOFU_LIBRARY_DIR set to ${ZOFU_LIBRARY_DIR}")
    # message(STATUS "ZOFU_MODULE (${ZOFU_LIBRARY_NAME}.mod) set to ${ZOFU_MODULE}")
    # message(STATUS "ZOFU_MODULE_DIR set to ${ZOFU_MODULE_DIR}")
else()
    message(STATUS "Zofu Fortran testing library is not available")
    # message(STATUS "ZOFU_DRIVER set to ${ZOFU_DRIVER}")
    # message(STATUS "ZOFU_LIBRARY_NAME set to ${ZOFU_LIBRARY_NAME}")
    # message(STATUS "ZOFU_LIBRARY set to ${ZOFU_LIBRARY}")
    # message(STATUS "ZOFU_LIBRARY_DIR set to ${ZOFU_LIBRARY_DIR}")
    # message(STATUS "ZOFU_MODULE (${ZOFU_LIBRARY_NAME}.mod) set to ${ZOFU_MODULE}")
    # message(STATUS "ZOFU_MODULE_DIR set to ${ZOFU_MODULE_DIR}")
endif()

# if(ZOFU_BINARY_PATH)
#     message(STATUS "Input ZOFU_BINARY_PATH specified as ${ZOFU_BINARY_PATH}")
# else()
#     message(STATUS "Input ZOFU_BINARY_PATH not specified")
# endif()

# if(ZOFU_LIBRARY_PATH)
#     message(STATUS "Input ZOFU_LIBRARY_PATH specified as ${ZOFU_LIBRARY_PATH}")
# else()
#     message(STATUS "Input ZOFU_LIBRARY_PATH not specified")
# endif()

# if(ZOFU_MODULE_PATH)
#     message(STATUS "Input ZOFU_MODULE_PATH specified as ${ZOFU_MODULE_PATH}")
# else()
#     message(STATUS "Input ZOFU_MODULE_PATH not specified")
# endif()