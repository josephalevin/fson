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
! Created on March 6, 2012, 7:48 PM
!

module fson
    use fson_value_m, fson_print => fson_value_print, fson_destroy => fson_value_destroy
    use fson_string_m
    use fson_path_m, fson_get => fson_path_get

    implicit none

    private

    public :: fson_parse, fson_value, fson_get, fson_print, fson_destroy

    ! FILE IOSTAT CODES
    integer, parameter :: end_of_file = -1
    integer, parameter :: end_of_record = -2

    ! PARSING STATES
    integer, parameter :: STATE_LOOKING_FOR_VALUE = 1
    integer, parameter :: STATE_IN_OBJECT = 2
    integer, parameter :: STATE_IN_PAIR_NAME = 3
    integer, parameter :: STATE_IN_PAIR_VALUE = 4

    ! POP/PUSH CHARACTER
    integer :: pushed_index = 0
    character (len = 10) :: pushed_char

contains

    !
    ! FSON PARSE
    !
    function fson_parse(file, unit) result(p)
        type(fson_value), pointer :: p
        integer, optional, intent(inout) :: unit
        character(len = *), intent(in) :: file
        logical :: unit_available
        integer :: u
        ! init the pointer to null
        nullify(p)

        ! select the file unit to use
        if (present(unit)) then
            u = unit
        else
            ! find the first available unit
            unit_available = .false.
            u = 20

            do while (.not.unit_available)
                inquire(unit = u, exist = unit_available)
                u = u + 1
            end do
        end if

        ! open the file
        open (unit = u, file = file, status = "old", action = "read", form = "formatted", position = "rewind")

        ! create the value and associate the pointer        
        p => fson_value_create()

        ! parse as a value
        call parse_value(unit = u, value = p)

        ! close the file
        if( .not. present(unit)) then
            close (u)
        end if

    end function fson_parse

    !
    ! PARSE_VALUE
    !
    recursive subroutine parse_value(unit, value)
        integer, intent(inout) :: unit
        type(fson_value), pointer :: value
        logical :: eof
        character :: c

        ! for some unknown reason the next pointer is getting messed with the pop
        type(fson_value), pointer :: hack

        ! start the hack                  
        hack => value % next

        ! pop the next non whitespace character off the file
        c = pop_char(unit, eof = eof, skip_ws = .true.)

        ! finish the hack; set the next pointer to whatever it was before the pop
        value % next => hack

        if (eof) then
            return
        else
            select case (c)
            case ("{")
                ! start object                
                value % value_type = TYPE_OBJECT
                call parse_object(unit, value)
            case ("[")
                ! start array
                value % value_type = TYPE_ARRAY
                call parse_array(unit, value)
            case ("]")
                ! end an empty array
                nullify(value)
            case ('"')
                ! string                                      
                value % value_type = TYPE_STRING
                value % value_string => parse_string(unit)
            case ("t")
                !true
                value % value_type = TYPE_LOGICAL
                call parse_for_chars(unit, "rue")
                value % value_logical = .true.
            case ("f")
                !false
                value % value_type = TYPE_LOGICAL
                value % value_logical = .false.
                call parse_for_chars(unit, "alse")
            case ("n")
                value % value_type = TYPE_NULL
                call parse_for_chars(unit, "ull")
            case("-", "0": "9")
                call push_char(c)
                call parse_number(unit, value)
            case default
                print *, "ERROR: Unexpected character while parsing value. '", c, "' ASCII=", iachar(c)
                call exit (1)
            end select
        end if

    end subroutine parse_value

    !
    ! PARSE OBJECT
    !    
    recursive subroutine parse_object(unit, parent)
        integer, intent(inout) :: unit
        type(fson_value), pointer :: parent, pair


        logical :: eof
        character :: c

        ! pair name
        c = pop_char(unit, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing start of object."
            call exit (1)
        else if ("}" == c) then
            ! end of an empty object
            return
        else if ('"' == c) then
            pair => fson_value_create()
            pair % name => parse_string(unit)
        else
            print *, "ERROR: Expecting string: '", c, "'"
            call exit (1)
        end if

        ! pair value
        c = pop_char(unit, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing object member. 1"
            call exit (1)
        else if (":" == c) then
            ! parse the value                       
            call parse_value(unit, pair)
            call fson_value_add(parent, pair)
        else
            print *, "ERROR: Expecting : and then a value. ", c
            call exit (1)
        end if

        ! another possible pair
        c = pop_char(unit, eof = eof, skip_ws = .true.)
        if (eof) then
            return
        else if ("," == c) then
            ! read the next member            
            call parse_object(unit = unit, parent = parent)
        else if ("}" == c) then
            return
        else
            print *, "ERROR: Expecting end of object.", c
            call exit (1)
        end if

    end subroutine parse_object

    !
    ! PARSE ARRAY
    !    
    recursive subroutine parse_array(unit, array)
        integer, intent(inout) :: unit
        type(fson_value), pointer :: array, element

        logical :: eof
        character :: c


        ! try to parse an element value
        element => fson_value_create()
        call parse_value(unit, element)

        ! parse value will disassociate an empty array value
        if (associated(element)) then
            call fson_value_add(array, element)
        end if


        ! popped the next character
        c = pop_char(unit, eof = eof, skip_ws = .true.)

        if (eof) then
            return
        else if ("," == c) then
            ! parse the next element
            call parse_array(unit, array)
        else if ("]" == c) then
            ! end of array
            return
        end if

    end subroutine parse_array

    !
    ! PARSE STRING
    !
    function parse_string(unit) result(string)
        integer, intent(inout) :: unit
        type(fson_string), pointer :: string

        logical :: eof
        character :: c, last

        string => fson_string_create()

        do
            c = pop_char(unit, eof = eof, skip_ws = .false.)
            if (eof) then
                print *, "Expecting end of string"
                call exit(1)!
            else if ('"' == c .and. last .ne. '\') then
                exit
            else
                last = c
                call fson_string_append(string, c)
            end if
        end do
    end function parse_string

    !
    ! PARSE FOR CHARACTERS
    !
    subroutine parse_for_chars(unit, chars)
        integer, intent(in) :: unit
        character(len = *), intent(in) :: chars
        integer :: i, length
        logical :: eof
        character :: c

        length = len_trim(chars)

        do i = 1, length
            c = pop_char(unit, eof = eof, skip_ws = .true.)
            if (eof) then
                print *, "ERROR: Unexpected end of file while parsing array."
                call exit (1)
            else if (c .ne. chars(i:i)) then
                print *, "ERROR: Unexpected character.'", c,"'", chars(i:i)
                call exit (1)
            end if
        end do

    end subroutine parse_for_chars

    !
    ! PARSE NUMBER
    !
    subroutine parse_number(unit, value)
        integer, intent(inout) :: unit
        type(fson_value), pointer :: value
        logical :: eof, negative, decimal, scientific
        character :: c
        integer :: integral, exp, digit_count
        real :: frac


        ! first character is either - or a digit        
        c = pop_char(unit, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing number."
            call exit (1)
        else if ("-" == c) then
            negative = .true.
        else
            negative = .false.
            call push_char(c)
        end if


        ! parse the integral
        integral = parse_integer(unit)

        decimal = .false.
        scientific = .false.

        do
            ! first character is either - or a digit        
            c = pop_char(unit, eof = eof, skip_ws = .true.)
            if (eof) then
                print *, "ERROR: Unexpected end of file while parsing number."
                call exit (1)
            else
                select case (c)
                case (".")
                    ! this is already fractional number
                    if (decimal) then
                        ! already found a decimal place
                        print *, "ERROR: Unexpected second decimal place while parsing number."
                        call exit(1)
                    end if
                    decimal = .true.
                    frac = parse_integer(unit, digit_count)
                    frac = frac / (10 ** digit_count)
                case ("e", "E")
                    ! this is already an exponent number
                    if (scientific) then
                        ! already found a e place
                        print *, "ERROR: Unexpected second exponent while parsing number."
                        call exit(1)
                    end if
                    scientific = .true.
                    ! this number has an exponent
                    exp = parse_integer(unit)

                case default
                    ! this is a integer
                    if (decimal) then

                        ! add the integral
                        frac = frac + integral

                        if (scientific) then
                            ! apply exponent
                            frac = frac * (10 ** exp)
                        end if

                        ! apply negative
                        if (negative) then
                            frac = frac * (-1)
                        end if


                        value % value_type = TYPE_REAL
                        value % value_real = frac
                    else
                        if (scientific) then
                        ! apply exponent
                        integral = integral * (10 ** exp)
                        end if

                        ! apply negative
                        if (negative) then
                        integral = integral * (-1)
                        end if

                        value % value_type = TYPE_INTEGER
                        value % value_integer = integral
                    end if
                    call push_char(c)
                    exit
                end select
            end if
        end do



    end subroutine

    !
    ! PARSE INTEGER    
    !
    integer function parse_integer(unit, digit_count) result(integral)
        integer, intent(in) :: unit
        integer, optional, intent(inout) :: digit_count
        logical :: eof
        character :: c
        integer :: tmp, count

        count = 0
        integral = 0
        do
            c = pop_char(unit, eof = eof, skip_ws = .true.)
            if (eof) then
                print *, "ERROR: Unexpected end of file while parsing digit."
                call exit (1)
            else
                select case(c)
                case ("0":"9")
                    ! digit        
                    read (c, '(i1)') tmp
                    ! shift
                    if (count > 0) then
                        integral = integral * 10
                    end if
                    ! add
                    integral = integral + tmp

                    ! increase the count
                    count = count + 1
                case default
                    if (present(digit_count)) then
                        digit_count = count
                    end if
                    call push_char(c)
                    return
                end select
            end if
        end do

    end function parse_integer

    !
    ! POP CHAR
    !
    recursive character function pop_char(unit, eof, skip_ws) result(popped)
        integer, intent(in) :: unit
        logical, intent(out) :: eof
        logical, intent(in), optional :: skip_ws

        integer :: ios
        character :: c
        logical :: ignore

        eof = .false.
        if (.not.present(skip_ws)) then
            ignore = .false.
        else
            ignore = skip_ws
        end if

        do
            if (pushed_index > 0) then
                ! there is a character pushed back on, most likely from the number parsing                
                c = pushed_char(pushed_index:pushed_index)
                pushed_index = pushed_index - 1
            else
                read (unit = unit, fmt = "(a)", advance = "no", iostat = ios) c
            end if
            if (ios == end_of_record) then
                cycle            
            else if (ios == end_of_file) then
                eof = .true.
                exit
            else if (iachar(c) <= 32) then
                ! non printing ascii characters
                cycle
            else if (ignore .and. c == " ") then
                cycle
            else
                popped = c
                exit
            end if
        end do

    end function pop_char

    !
    ! PUSH CHAR
    !
    subroutine push_char(c)
        character, intent(inout) :: c
        pushed_index = pushed_index + 1
        pushed_char(pushed_index:pushed_index) = c

    end subroutine push_char

end module fson
