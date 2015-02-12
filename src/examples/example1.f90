! Copyright (c) 2012 Joseph A. Levin
!
! Permission is hereby granted, free of charge, to any person obtaining a copy of this
! software and associated documentation files (the "Software"), to deal in the Software
! without restriction, including without limitation the rights to use, copy, modify, merge,
! publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
! persons to whom the Software is furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all copies or 
! substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
! INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
! OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
! DEALINGS IN THE SOFTWARE.


! FSON MODULE 
!
! File:   fson.f95
! Author: Joseph A. Levin
!
! Created on March 10, 2012, 5:24 AM
!

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