!     
! File:   string.f95
! Author: josephalevin
!
! Created on March 7, 2012, 7:40 PM
!

module string_m

    private

    public :: string, size, append

    integer, parameter :: BLOCK_SIZE = 5

    type string
        character (len = BLOCK_SIZE) :: chars
        integer :: index = 0
        type(string), pointer :: next => null()
    end type string

    interface append
        module procedure append_chars, append_string
    end interface append

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
    ! APPEND_CHARS
    !
    subroutine append_string(str1, str2)
        type(string), intent(inout) :: str1, str2
        integer length

        length = size(str2)



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
    ! CLEAR
    !
    recursive subroutine clear(b)
        type(string), intent(inout) :: b

        if (associated(b % next)) then
            call clear(b % next)
        end if

        b % index = 0
        nullify (b % next)

    end subroutine clear

    !
    ! SIZE    
    !
    recursive integer function size(str) result(count)
        type(string), intent(in) :: str

        count = str % index

        if (str % index == BLOCK_SIZE .AND. associated(str % next)) then
            count = count + size(str % next)
        end if
        
    end function size


end module string_m
