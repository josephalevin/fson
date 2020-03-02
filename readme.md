![Unit tests](https://github.com/josephalevin/fson/workflows/Unit%20tests/badge.svg) ![Unit tests (CMake)](https://github.com/josephalevin/fson/workflows/Unit%20tests%20(CMake)/badge.svg)

Fortran 95 JSON Parser
======================

Getting Started
---------------
```fortran
program example1

    ! Typical usage should only require an explicit use of the fson module.
    ! The other modules will be used privately by fson as required.  
    use fson

    ! declare a pointer variable.  Always use a pointer with fson_value.
    type(fson_value), pointer :: value

    ! parse the json file
    value => fson_parse("test1.json")

    ! print the parsed data to the console
    call fson_print(value)    

    ! extract data from the parsed value        

    ! clean up
    call fson_destroy(value)

end program example1
```

Example JSON
------------
This JSON will serve as a reference for the following examples.  
If you are not already familiar with JSON you can read more at:
<http://www.json.org/> and <http://en.wikipedia.org/wiki/JSON>.
```
{
     "name"       : {"first": "John", "last" : "Smith"},
     "age"        : 25,
     "address"    :
       { "streetAddress": "21 2nd Street",
         "city"         : "New York",
         "state"        : "NY",
         "postalCode"   : "10021"},
     "PhoneNumber":
       [ {"type"  : "home", "number": "212 555-1234"},
         {"type"  : "fax",  "number": "646 555-4567"} ]
}
```

Extracting Data
---------------
Getting the data from the parsed fson_value to your variable is easy.  All extraction is performed through a call to fson_get().  
This subroutine is overloaded for different target value types.

```fortran
program example1

! The fson mudule has the basic parser and lookup 
use fson

! Functions for accessing data as an array
use fson_value_m, only: fson_value_count, fson_value_get
character(len=1024) :: strval, strval2
integer i

! Declare a pointer variables.  Always use a pointer with fson_value.
type(fson_value), pointer :: json_data, array, item

! Parse the json file
json_data => fson_parse("test1.json")

! Get the first and last name and print them
call fson_get(json_data, "name.first", strval)
call fson_get(json_data, "name.last",  strval2)
print *, "name.first = ", trim(strval)
print *, "name.last  = ", trim(strval2)

! Use a lookup string to get the first phone number
call fson_get(json_data, "PhoneNumber[1].number", strval)     
print *, "PhoneNumber[1].number = ", trim(strval)
print *, ""

! Get the phone numbers as an array
call fson_get(json_data, "PhoneNumber", array)

! Loop through each array item
do i = 1, fson_value_count(array)
  ! Get the array item (this is an associative array)
  item => fson_value_get(array, i)
  
  ! Lookup the values from the array
  call fson_get(item, "type", strval)
  call fson_get(item, "number", strval2)
  
  ! Print out the values
  print *, "Phone Number:"
  print *, "type = ", trim(strval), ", number = ", trim(strval2)
end do

! clean up
call fson_destroy(json_data)

end program example1
```
    
The program output is the following:

```
name.first = John
name.last  = Smith
PhoneNumber[1].number = 212555-1234

Phone Number:
type = home, number = 212555-1234
Phone Number:
type = fax, number = 646555-4567
```

Extracting arrays
-----------------
You can also extract entire arrays, as Fortran allocatable arrays, using fson_get(). This assumes the array's elements are all of the same type (integer, real, double precision, logical or character). Rank-1 (vector) or rank-2 (matrix) arrays are supported.

The following example parses a JSON file containing an vector of integers called "v" and a matrix of real numbers called "m", and prints them:

```fortran
program extract_array

  use fson
  implicit none
  type(fson_value), pointer :: data
  integer, allocatable :: vec(:)
  real, allocatable :: mat(:,:)
    
  data => fson_parse("data.json")

  call fson_get(data, "v", vec)
  call fson_get(data, "m", mat)
  print *, vec
  print *, mat

  call fson_destroy(data)
  deallocate(vec, mat)

end program extract_array
```

When extracting character arrays, it is necessary to specify a string length when the array is declared, e.g.:

```fortran
integer, parameter :: str_len = 8
character(len = str_len), allocatable :: m(:)

data => fson_parse("data.json")
call fson_get(data, "char_array", m)
```

JSON Path
---------

```
| Operator | Description              | 
|----------|--------------------------|
|    $     | Root object/value        | 
|    .     | Child operator           |
|    []    | Array element            |
```

Installation
------------

Three methods for installing FSON are provided: a regular Makefile, a
[Meson](https://mesonbuild.com) build, and a [CMake](https://cmake.org)
build.  Below we give a brief overview of how to use each one.

### Makefile

Installing via Makefile is simply a matter of opening the provided
`makefile`, updating the appropriate variables near the top, and running

```bash
make
make install
```

For the Makefile build, unit tests can be run using the
[FRUIT](https://sourceforge.net/projects/fortranxunit/) library,
together with [FRUITPy](https://github.com/acroucher/FRUITPy) which is
a Python interface to FRUIT. When these are installed, the
`fson_test.py` script in the FSON base directory can be run to execute
the unit tests.

For a more automated installation, see one of the following installation methods.

### Meson

Once [Meson](https://mesonbuild.com) and
[Ninja](https://ninja-build.org/) are installed on your system, FSON
may be built, tested and installed as follows:

```bash
meson build
cd build
ninja
ninja test # optional unit tests
ninja install
```

For the Meson build, the unit tests use the
[Zofu](https://github.com/acroucher/zofu) library. If this is not
already installed on your system, it will be built as a Meson
subproject of FSON.

See the [Meson](https://mesonbuild.com) documentation for more
information on customizing the build (e.g. specifying the install
directory, build optimization etc.).

### CMake

The provided `CMakeLists.txt` runs with CMake 3 to configure, build and
install FSON.  The simplest means to build and install using CMake is to
run

```bash
mkdir bld
cd bld
cmake3 ..
make
make test # optional unit tests
make install
```

However, all of the command options available to CMake are supported.
For example, to enable building shared libraries use the
`-DBUILD_SHARED_LIBS:BOOL=ON` flag and to override the installation
prefix use `-DCMAKE_INSTALL_PREFIX=...`.  Thus the command

```bash
cmake3 -DBUILD_SHARED_LIBS:BOOL=ON -DCMAKE_INSTALL_PREFIX=$HOME
```

would configure FSON to build a shared library and install under your
home directory.

Once FSON has been built and installed, you can import it into your
CMake supported project using

```cmake
find_package(FSON)
...
add_executable(mytarget
...
target_link_libraries(mytarget FSON::FSON)
```

The CMake build require CMake version 3.6.1 or higher and `git` version
1.6.5 or newer.  It may work with older versions of CMake, but this is
what has been tested.

Some platforms are unable to build the [Zofu](https://github.com/acroucher/zofu)
dependency, but the library builds correctly.  On these platforms, use

```bash
cmake3 -DFSON_ENABLE_TESTS:BOOL=OFF ...
```

to disable the unit tests.

