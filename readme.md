Fortran 95 JSON Parser
======================

Getting Started
---------------
```fortran
program example1

    ! Typical usage should only require an explicit use of the fson module.
    ! The other modules will be used privatley by fson as required.  
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
You can also extract entire arrays, as Fortran allocatable arrays, using fson_get(). The following example parses a JSON file containing an array of real numbers called "arr", and prints the array:

```fortran
program extract_array

  use fson
  implicit none
  type(fson_value), pointer :: data
  real, allocatable :: vals(:)
    
  data => fson_parse("data.json")

  call fson_get(data, "arr", vals)
  print *, vals

  call fson_destroy(data)
  deallocate(vals)

end program extract_array
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
