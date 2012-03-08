!     
! File:   value_m.f95
! Author: josephalevin
!
! Created on March 7, 2012, 10:14 PM
!

module value_m
    
    use string_m
    
    implicit none
    
    private
    
    public :: value

    !constants for the value types
    integer, parameter :: TYPE_NULL = 0
    integer, parameter :: TYPE_OBJECT = 1
    integer, parameter :: TYPE_ARRAY = 2
    integer, parameter :: TYPE_STRING = 3
    integer, parameter :: TYPE_INTEGER = 4
    integer, parameter :: TYPE_REAL = 5
    integer, parameter :: TYPE_BOOLEAN = 6



    type value
        type(string) :: name
        integer :: value_type = TYPE_NULL
        logical :: value_boolean
        integer :: value_integer
        real :: value_real
        type(string) :: value_string
        ! type (json), dimension (:), pointer :: value_array   
        type(value), pointer :: parent
        type(value), pointer :: next
    end type value


end module value_m
