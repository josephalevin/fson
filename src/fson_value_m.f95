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

    public :: fson_value, fson_value_create, fson_value_destroy, fson_value_add, fson_value_get, fson_value_count, fson_value_print

    !constants for the value types
    integer, public, parameter :: TYPE_UNKNOWN = -1
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
        type(fson_string), pointer :: name => null()
        integer :: value_type = TYPE_UNKNOWN
        logical :: value_logical
        integer :: value_integer
        real :: value_real
        type(fson_string), pointer :: value_string => null()
        type(fson_value), pointer :: next => null()
    end type fson_value
    
    !
    ! FSON VALUE GET
    !
    ! Use either a 1 based index or member name to get the value.
    interface fson_value_get
        module procedure get_by_index
    end interface fson_value_get

contains

    !
    ! FSON VALUE CREATE
    !
    function fson_value_create() result(new)
        type(fson_value), pointer :: new        
        
        allocate(new)                                  
        
    end function fson_value_create
    
    !
    ! FSON VALUE DESTROY
    !
    subroutine fson_value_destroy(this)
        type(fson_value), pointer :: this
        
    end subroutine fson_value_destroy

    !
    ! FSON VALUE ADD
    !
    ! Adds the memeber to the linked list
    subroutine fson_value_add(this, member)
        type(fson_value), pointer :: this, member, p

        ! get to the tail of the linked list                       
        p => this        
        
        do while (associated(p%next))            
            p => p%next
        end do
        
        p%next => member 
        
        nullify(member%next)
               
    end subroutine

    !
    ! fson value count
    !
    integer function fson_value_count(this) result(count)
        type(fson_value), pointer :: this, p        
                
        count = 0
        
        p => this%next               
        
        do while (associated(p))            
            count = count + 1                     
            p => p%next
        end do                
        
    end function
    
    function get_by_index(this, index) result(p)
        type(fson_value), pointer :: this, p
        integer, intent(in) :: index
        integer :: i
        
        p => this
        
        do i = 1, index
            p => p%next            
        end do
        
    end function get_by_index

    !
    ! FSON VALUE PRINT
    !
    recursive subroutine fson_value_print(this, indent)
        type(fson_value), pointer :: this, element
        integer, optional, intent(in) :: indent
        character (len=1024) :: tmp_chars
        integer :: tab, i, count
        if (present(indent)) then
            tab = indent
        else
            tab = 0
        end if

        select case (this % value_type)
        case(TYPE_OBJECT)
            print *, "{"
            count = fson_value_count(this)            
            do i=1, count                
                ! get the element
                element => fson_value_get(this, i)                     
                ! get the name
                call fson_string_copy(element%name, tmp_chars)
                ! print the name
                print *, '"', trim(tmp_chars), '":'
                ! recursive print of the element
                call fson_value_print(element)
                ! print the separator if required
                if( i < count) then
                    print *,  ","
                end if
            end do
            
            print *, "}"
        case (TYPE_ARRAY)
            print *, "["
            count = fson_value_count(this)            
            do i=1, count
                ! get the element
                element => fson_value_get(this, i)                                                                
                ! recursive print of the element
                call fson_value_print(element)
                ! print the separator if required
                if( i < count) then
                    print *,  ","
                end if
            end do
            print *, "]"
        case (TYPE_NULL)            
            print *, "null"
        case (TYPE_STRING)
            call fson_string_copy(this%value_string, tmp_chars)            
            print *, '"', trim(tmp_chars), '"'
        case (TYPE_LOGICAL)
            if (this % value_logical) then
                print *, "true"
            else
                print *, "false"
            end if            
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
