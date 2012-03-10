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
    
    public :: fson_value, fson_value_add, fson_value_print

    !constants for the value types
    integer, public, parameter :: TYPE_NULL = 0
    integer, public, parameter :: TYPE_OBJECT = 1
    integer, public, parameter :: TYPE_ARRAY = 2
    integer, public, parameter :: TYPE_STRING = 3
    integer, public, parameter :: TYPE_INTEGER = 4
    integer, public, parameter :: TYPE_REAL = 5
    integer, public, parameter :: TYPE_LOGICAL = 6



    type fson_value
        type(fson_string) :: name
        integer :: value_type = TYPE_NULL
        logical :: value_logical
        integer :: value_integer
        real :: value_real
        type(fson_string) :: value_string
        ! type (json), dimension (:), pointer :: value_array   
        type(fson_value), pointer :: parent
        type(fson_value), pointer :: next
    end type fson_value
    
    contains
    
    subroutine fson_value_add(this, member)
        type(fson_value), intent(in) :: this
        type(fson_value), intent(in) :: member
        
        
    end subroutine
    
    subroutine fson_value_print(this)
        type(fson_value), intent(in) :: this
        
        select case (this%value_type)
            case(TYPE_OBJECT)
                print *, "{"
                print *, "}"
            case (TYPE_NULL)
                print *, "NULL"
        end select
    end subroutine


end module fson_value_m
