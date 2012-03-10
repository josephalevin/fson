!     
! File:   fson.f95
! Author: josephalevin
!
! Created on March 6, 2012, 7:48 PM
!

module fson
    use fson_value_m
    use fson_string_m

    implicit none

    ! FILE IOSTAT CODES
    integer, parameter :: end_of_file = -1
    integer, parameter :: end_of_record = -2

    ! PARSING STATES
    integer, parameter :: STATE_LOOKING_FOR_VALUE = 1
    integer, parameter :: STATE_IN_OBJECT = 2
    integer, parameter :: STATE_IN_PAIR_NAME = 3
    integer, parameter :: STATE_IN_PAIR_VALUE = 4

    !    private
    !
    !    public :: test





contains

    function fson_parse_file(file)
        type(fson_value), allocatable :: fson_parse_file
        integer :: unit
        character(len = *), intent(in) :: file

        unit = 10
        ! open the file
        open (unit = unit, file = file, status = "old", action = "read", form = "formatted", position = "rewind")

        allocate(fson_parse_file)

        call parse_value(unit = unit, value = fson_parse_file)


    end function fson_parse_file

    !
    ! parse value
    !
    recursive subroutine parse_value(unit, value)
        integer, intent(inout) :: unit
        type(fson_value), allocatable, intent(inout) :: value

        logical :: eof
        character :: c

        c = pop_char(unit, eof = eof, skip_ws = .true.)

        if (eof) then
        else
            select case (c)
            case ("{")
                ! start object
                value%value_type = TYPE_OBJECT
                call parse_object(unit, value)
            case ("[")
                ! start array
                value%value_type = TYPE_ARRAY
                call parse_array(unit, value)
            case ('"')
                ! string
                value%value_type = TYPE_STRING
                value % value_string = parse_string(unit)
            end select
        end if

    end subroutine parse_value

    !
    ! parse object
    !    
    recursive subroutine parse_object(unit, parent)
        integer, intent(inout) :: unit
        type(fson_value), allocatable, intent(inout) :: parent

        type(fson_value), allocatable :: pair

        logical :: eof
        character :: c

        if (.not.allocated(parent)) then
            allocate(parent)
        end if

        do
            ! popped the next character
            c = pop_char(unit, eof = eof, skip_ws = .true.)

            if (eof) then
                print *, "ERROR: Unexpected end of file while parsing object member."
                call exit (1)
            else
                select case (c)
                case ('"')
                    allocate(pair)
                    pair % name = parse_string(unit)
                case (":")
                    ! parse the value
                    call parse_value(unit = unit, value = pair)
                case (",")
                    ! read the next member
                    call parse_object(unit = unit, parent = parent)
                    exit
                case ("}")
                    ! end of object
                    return
                case default
                    print *, "ERROR: Unexpected character while parsing object member.", c
                    call exit(1)
                end select
            end if


        end do

    end subroutine parse_object

    !
    ! parse array
    !    
    recursive subroutine parse_array(unit, parent)
        integer, intent(inout) :: unit
        type(fson_value), allocatable, intent(inout) :: parent

        type(fson_value), allocatable :: element

        logical :: eof
        character :: c

        if (.not.allocated(parent)) then
            allocate(parent)
        end if


        ! try to parse an element value
        call parse_value(unit, element)
        if (allocated(element)) then
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
        type(fson_string), allocatable :: string

        logical :: eof
        character :: c, last

        allocate (string)


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


    ! pop the next character off the stream
    character function pop_char(unit, eof, skip_ws)
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
                pop_char = c
                exit
            end if
        end do

    end function




end module fson

program main
    use fson
    implicit none

    type(fson_value) :: parsed


    parsed = fson_parse_file(file = "test1.json")

    call fson_value_print(parsed)

end program main

