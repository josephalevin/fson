!     
! FSON MODULE 
!
! File:   fson.f95
! Author: Joseph A. Levin
!
! Created on March 6, 2012, 7:48 PM
!

module fson
    use fson_value_m
    use fson_string_m

    implicit none
    
    private
    
    public :: fson_parse_file, fson_value

    ! FILE IOSTAT CODES
    integer, parameter :: end_of_file = -1
    integer, parameter :: end_of_record = -2

    ! PARSING STATES
    integer, parameter :: STATE_LOOKING_FOR_VALUE = 1
    integer, parameter :: STATE_IN_OBJECT = 2
    integer, parameter :: STATE_IN_PAIR_NAME = 3
    integer, parameter :: STATE_IN_PAIR_VALUE = 4


contains

    !
    ! FSON PARSE FILE
    !
    function fson_parse_file(file) result(p)
        type(fson_value), pointer :: p
        integer :: unit
        character(len = *), intent(in) :: file
        
        ! init the pointer to null
        nullify(p)

        ! select the file unit to use
        unit = 42
        
        ! open the file
        open (unit = unit, file = file, status = "old", action = "read", form = "formatted", position = "rewind")
        
        ! create the value and associate the pointer        
        p => fson_value_create()
        
        ! parse as a value
        call parse_value(unit = unit, value = p)
        
        ! close the file
        close (unit)

    end function fson_parse_file

    !
    ! PARSE_VALUE
    !
    recursive subroutine parse_value(unit, value)
        integer, intent(inout) :: unit
        type(fson_value), pointer, intent(inout) :: value       
        logical :: eof
        character :: c
        
        ! for some unknown reason the next pointer is getting messed with the pop
        type(fson_value), pointer :: hack                    
        hack=> value%next
                
        ! pop the next non whitespace character off the file
        c = pop_char(unit, eof = eof, skip_ws = .true.)        
                
        ! finish the hack; set the next pointer to whatever it was before the pop
        value % next => hack                

        if (eof) then
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
            case default
                print *, "ERROR: Unexpected character while parsing for value: '", c, "'"
                call exit (1)
            end select
        end if

    end subroutine parse_value

    !
    ! PARSE OBJECT
    !    
    recursive subroutine parse_object(unit, parent)
        integer, intent(inout) :: unit
        type(fson_value), pointer, intent(inout) :: parent

        type(fson_value), pointer :: pair

        logical :: eof
        character :: c

        if (.not.associated(parent)) then
            parent => fson_value_create()
        end if

        ! pair name
        c = pop_char(unit, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing object member."
            call exit (1)
        else if ('"' == c) then
            pair => fson_value_create()
            pair % name = parse_string(unit)            
        else
            print *, "ERROR: Expecting string: '", c, "'"
            call exit (1)
        end if
        
        ! pair value
        c = pop_char(unit, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing object member."
            call exit (1)
        else if (":" == c) then
            ! parse the value
            print *, "parse value"
            call parse_value(unit, pair)
            print *, "after value", associated(pair % next)
        else
            print *, "ERROR: Expecting :", c
            call exit (1)
        end if

        ! another possible pair
        c = pop_char(unit, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing object member."
            call exit (1)
        else if ("," == c) then
            ! read the next member
            call parse_object(unit = unit, parent = parent)
            print *, "after object", associated(pair % next)
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
    recursive subroutine parse_array(unit, parent)
        integer, intent(inout) :: unit
        type(fson_value), pointer, intent(inout) :: parent

        type(fson_value), pointer :: element

        logical :: eof
        character :: c

        if (.not.associated(parent)) then
            parent => fson_value_create()
        end if


        ! try to parse an element value
        call parse_value(unit, element)
        if (associated(element)) then
            call fson_value_add(parent, element)
        end if


        ! popped the next character
        c = pop_char(unit, eof = eof, skip_ws = .true.)

        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing array."
            call exit (1)
        else if ("," == c) then
            ! parse the next element
            call parse_array(unit, parent)
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
            else if ('"' == c .and. last .ne. "\") then
                exit
            else
                last = c
                call string_append(string, c)
            end if
        end do
    end function parse_string
    
    !
    ! PARSE FOR CHARACTERS
    !
    subroutine parse_for_chars(unit, chars)
        integer, intent(in) :: unit       
        character(len=*), intent(in) :: chars
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
                print *, "ERROR: Unexpected character.", c, chars(i:i)
                call exit (1)            
            end if
        end do
        
    end subroutine parse_for_chars

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
            read (unit = unit, fmt = "(a)", advance = "no", iostat = ios) c
            if (ios == end_of_record) then
                cycle
            else if (ios == end_of_file) then
                eof = .true.
                exit
            else if (ignore .and. c == " ") then
                cycle
            else
                popped = c
                exit
            end if
        end do

    end function
   


end module fson

!
! MAIN PROGRAM
! 
! Reads in a json file and prints the contents to stdout.
!
program main
    use fson
    use fson_value_m
    implicit none

    type(fson_value), pointer :: parsed

    parsed => fson_parse_file(file = "test1.json")

    print *, fson_value_count(parsed)
    !    call fson_value_print(parsed)


end program main

