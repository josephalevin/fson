!     
! File:   string.f95
! Author: josephalevin
!
! Created on March 7, 2012, 7:40 PM
!

module string_m

    private

    public :: string, string_length, string_append, string_clear, string_copy

    integer, parameter :: BLOCK_SIZE = 5

    type string
        character (len = BLOCK_SIZE) :: chars
        integer :: index = 0
        type(string), pointer :: next => null()
    end type string

    interface string_append
        module procedure append_chars, append_string
    end interface string_append

    interface string_copy
        module procedure copy_chars
    end interface string_copy

contains

    !
    ! ALLOCATE BLOCK
    !
    subroutine allocate_block(this)
        type(string), intent(inout) :: this
        type(string), pointer :: new

        if (.not.associated(this % next)) then
            allocate(new)
            this % next => new
        end if

    end subroutine allocate_block


    !
    ! APPEND_STRING
    !
    subroutine append_string(str1, str2)
        type(string), intent(inout) :: str1, str2
        integer length, i

        length = string_length(str2)

        do i = 1, length
            call append_char(str1, get_char_at(str2, i))
        end do


    end subroutine append_string

    !
    ! APPEND_CHARS
    !
    subroutine append_chars(str, c)
        type(string), intent(inout) :: str
        character (len = *), intent(in) :: c
        integer length, i

        length = len_trim(c)

        do i = 1, length
            call append_char(str, c(i:i))
        end do

    end subroutine append_chars

    !
    ! APPEND_CHAR
    !
    recursive subroutine append_char(str, c)
        type(string), intent(inout) :: str
        character, intent(in) :: c

        if (str % index .GE. BLOCK_SIZE) then
            !set down the chain
            call allocate_block(str)
            call append_char(str % next, c)

        else
            ! set local
            str % index = str % index + 1
            str % chars(str % index:str % index) = c
        end if

    end subroutine append_char

    !
    ! COPY CHARS
    !
    subroutine copy_chars(this, to)
        type(string), intent(in) :: this
        character(len = *), intent(inout) :: to
        integer :: length
        
        length = min(string_length(this), len(to))
        
        do i = 1, length
            to(i:i) = get_char_at(this, i)
        end do
        
        ! pad with nothing
        do i = length + 1, len(to)
            to(i:i) = ""
        end do


    end subroutine copy_chars



    !
    ! CLEAR
    !
    recursive subroutine string_clear(this)
        type(string), intent(inout) :: this

        if (associated(this % next)) then
            call string_clear(this % next)
            deallocate(this % next)
            nullify (this % next)
        end if

        this % index = 0

    end subroutine string_clear

    !
    ! SIZE    
    !
    recursive integer function string_length(str) result(count)
        type(string), intent(in) :: str

        count = str % index

        if (str % index == BLOCK_SIZE .AND. associated(str % next)) then
            count = count + string_length(str % next)
        end if

    end function string_length


    !
    ! GET CHAR AT
    !
    recursive character function get_char_at(this, i) result(c)
        type(string), intent(in) :: this
        integer, intent(in) :: i

        if (i .LE. this % index) then
            c = this % chars(i:i)
        else
            c = get_char_at(this % next, i - this % index)
        end if

    end function get_char_at
end module string_m
