!     
! File:   fjson.f95
! Author: josephalevin
!
! Created on March 6, 2012, 7:48 PM
!

module fson

    implicit none

    private

    public :: test





contains


    subroutine test()
        integer :: a
    end subroutine test


end module fson

program main
    use fson
    use string_m
    implicit none
    integer, parameter :: end_of_file = -1
    integer, parameter :: end_of_record = -2
    integer :: i, n, ios, kount
    character (len = 1) :: c
    character (len = 30) :: x
    type(string) :: str
    
    x = "Hello, world!"


    open (unit = 10, file = "test1.json", status = "old", action = "read", form = "formatted", position = "rewind")
        

    kount = 0
    do
        read (unit = 10, fmt = "(a)", advance = "no", iostat = ios) c
        if (ios == end_of_record) then
            cycle
        else if (ios == end_of_file) then
            exit
        else                        
            select case (c)
                case ("{")
                    print *, "START OBJECT"
                case ("}")
                    print *, "END OBJECT"
                case ("[")
                    print *, "START ARRAY"
                case ("]")
                    print *, "END ARRAY"
                case default
                    print *, c
            end select
        end if
    end do

    
    print *, size(str)
    call append(str, "Hello, world!")
    
  
    print *, size(str)

end program main

