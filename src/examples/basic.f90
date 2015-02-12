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
! Created on January 8, 2013
!

program example_basic 

    ! Typical usage should only require an explicit use of the fson module.
    ! The other modules will be used privatley by fson as required.  
!    use fson

    ! declare a pointer variable.  Always use a pointer with fson_value.
!    type(fson_value), pointer :: value

    ! parse the json file
!    value => fson_parse("test1.json")

    ! print the parsed data to the console
!    call fson_print(value)

    ! extract data from the parsed value        

    ! clean up
!    call fson_destroy(value)

end program example_basic
