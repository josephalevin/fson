!     
! File:   value_m.f95
! Author: josephalevin
!
! Created on March 7, 2012, 10:14 PM
!

module fson_value_m

    use fson_string_m

    implicit none

    private

    public :: fson_value, fson_value_add, fson_value_print, fson_value_count, fson_value_create

    !constants for the value types
    integer, public, parameter :: TYPE_NULL = 0
    integer, public, parameter :: TYPE_OBJECT = 1
    integer, public, parameter :: TYPE_ARRAY = 2
    integer, public, parameter :: TYPE_STRING = 3
    integer, public, parameter :: TYPE_INTEGER = 4
    integer, public, parameter :: TYPE_REAL = 5
    integer, public, parameter :: TYPE_LOGICAL = 6


    !
    ! fson value
    !
    type fson_value
        type(fson_string), pointer :: name
        integer :: value_type = TYPE_NULL
        logical :: value_logical
        integer :: value_integer
        real :: value_real
        type(fson_string), pointer :: value_string
        type(fson_value), pointer :: next
    end type fson_value

contains

    !
    ! fson value create
    !
    function fson_value_create() result(p)
        type(fson_value), pointer :: p
        type(fson_value), allocatable, target :: new
        
        allocate(new)  
        new%next => null() 

        print *, "create", associated(new%next)
        
        p => new
        
    end function fson_value_create

    !
    ! fson value add
    !
    subroutine fson_value_add(this, member)
        type(fson_value), pointer, intent(in) :: this
        type(fson_value), pointer, intent(in) :: member
        type(fson_value), pointer :: p

        
        print *, "Add"
        
        
        p => this        
        
        do while (associated(p%next))
            print *, "2"
            p => p%next
        end do
        
        print *, "3"
        
       

    end subroutine

    !
    ! fson value count
    !
    integer function fson_value_count(this) result(count)
        type(fson_value), pointer, intent(in) :: this        
        type(fson_value), pointer :: p
        
        p => null()
        count = 0
        
        p => this%next       
        print *, "p",  associated(p)
        
        do while (associated(p))            
            count = count + 1                     
            p = p%next
        end do                
        
    end function

    !
    ! fson value print
    !
    subroutine fson_value_print(this, indent)
        type(fson_value), intent(in) :: this
        integer, optional, intent(in) :: indent

        integer :: tab
        if (present(indent)) then
            tab = indent
        else
            tab = 0
        end if

        select case (this % value_type)
        case(TYPE_OBJECT)
            print *, "{"
            print *, "}"
        case (TYPE_NULL)
            print *, "NULL"
        end select
    end subroutine fson_value_print

    !
    ! tabs
    !
    function tabs(n) result(chars)
        integer, intent(in) :: n
        character(len = 100) :: chars

        chars = repeat(" ", n)

    end function tabs

end module fson_value_m
