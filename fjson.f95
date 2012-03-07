!     
! File:   fjson.f95
! Author: josephalevin
!
! Created on March 6, 2012, 7:48 PM
!

module fjson

    implicit none
    
    private
    
    public :: json, open_json_file, json_write, is_object
    
    !constants for the value types
    integer, parameter :: TYPE_NULL = 0
    integer, parameter :: TYPE_OBJECT = 1
    integer, parameter :: TYPE_ARRAY = 2
    integer, parameter :: TYPE_STRING = 3
    integer, parameter :: TYPE_INTEGER = 4
    integer, parameter :: TYPE_REAL = 5
    integer, parameter :: TYPE_BOOLEAN = 6
    
    
    
    type json
        character (len=25) ::name
        integer :: value_type = TYPE_NULL
        logical :: value_boolean
        integer :: value_integer
        real    :: value_real
        character (len=100) :: value_string        
        type (json), dimension (:), pointer :: value_array        
    end type json
        
        
        
        contains
    
    subroutine open_json_file (j, file)
        implicit none
        
        type(json), intent(out) :: j
        character(len=*), intent(in) :: file
                
        write (*,*) "Hello, World"
        write (*,*) TYPE_ARRAY
    end subroutine
    
    subroutine json_write( )
        implicit none
    end subroutine
    
    function is_null(this)
        type (json), intent(in) :: this
        logical :: is_null
        is_null = TYPE_NULL .eq. this%value_type
    end function
    
    function is_object(this)
        type (json), intent(in) :: this
        logical :: is_object
        is_object = TYPE_OBJECT .eq. this%value_type
    end function
    
    function is_array(this)
        type (json), intent(in) :: this
        logical :: is_array
        is_array = TYPE_ARRAY .eq. this%value_type
    end function
    
    

end module

program main
    use fjson
    implicit none
    
    type(json) :: j
    
    j%value_type=1
    
    write (*,*) is_object(j)
        
    
end program
    
    