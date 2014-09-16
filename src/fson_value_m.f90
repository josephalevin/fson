! Copyright (c) 2012 Joseph A. Levin
!
! Permission is hereby granted, free of charge, to any person obtaining a copy of this
! software and associated documentation files (the "Software"), to deal in the Software
! without restriction, including without limitation the rights to use, copy, modify, merge,
! publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
! persons to whom the Software is furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all copies or 
! substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
! INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
! OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
! DEALINGS IN THE SOFTWARE.

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
    ! FSON VALUE
    !
    type fson_value
        type(fson_string), pointer :: name => null()
        integer :: value_type = TYPE_UNKNOWN
        logical :: value_logical
        integer :: value_integer
        real :: value_real
        double precision :: value_double
        integer, private :: count = 0
        type(fson_string), pointer :: value_string => null()
        type(fson_value), pointer :: next => null()
        type(fson_value), pointer :: parent => null()
        type(fson_value), pointer :: children => null()
        type(fson_value), pointer :: tail => null()
    end type fson_value

    !
    ! FSON VALUE GET
    !
    ! Use either a 1 based index or member name to get the value.
    interface fson_value_get
        module procedure get_by_index
        module procedure get_by_name_chars
        module procedure get_by_name_string
    end interface fson_value_get

contains

    !
    ! FSON VALUE CREATE
    !
    function fson_value_create() result(new)
        type(fson_value), pointer :: new

        nullify(new)
        allocate(new)

    end function fson_value_create

    !
    ! FSON VALUE DESTROY
    !
    recursive subroutine fson_value_destroy(this, destroy_next)

      implicit none
      type(fson_value), pointer :: this
      logical, intent(in), optional :: destroy_next

      type(fson_value), pointer :: p
      integer :: count
      logical :: donext

      if (present(destroy_next)) then
         donext = destroy_next
      else
         donext = .true.
      end if

      if (associated(this)) then

         if(associated(this % name)) then
            call fson_string_destroy(this % name)
            nullify (this % name)
         end if

         if(associated(this % value_string)) then
            call fson_string_destroy(this % value_string)
            nullify (this % value_string)
         end if

         if(associated(this % children)) then
            do while (this % count > 0)
               p => this % children
               this % children => this % children % next
               this % count = this % count - 1
               call fson_value_destroy(p, .false.)
            end do
            nullify(this % children)
         end if

         if ((associated(this % next)) .and. (donext)) then
            call fson_value_destroy(this % next)
            nullify (this % next)
         end if

         if(associated(this % tail)) then
            nullify (this % tail)
         end if

         deallocate(this)
         nullify(this)

      end if

    end subroutine fson_value_destroy

    !
    ! FSON VALUE ADD
    !
    ! Adds the member to the linked list

    subroutine fson_value_add(this, member)

      implicit none
      type(fson_value), pointer :: this, member

      ! associate the parent
      member % parent => this

      ! add to linked list
      if (associated(this % children)) then
         this % tail % next => member
      else
         this % children => member
      end if

      this % tail => member
      this % count = this % count + 1

    end subroutine fson_value_add

    !
    ! FSON_VALUE_COUNT
    !
    integer function fson_value_count(this) result(count)
        type(fson_value), pointer :: this, p

        count = this % count

    end function

    !
    ! GET BY INDEX
    !
    function get_by_index(this, index) result(p)
        type(fson_value), pointer :: this, p
        integer, intent(in) :: index
        integer :: i

        p => this % children

        do i = 1, index - 1
            p => p % next
        end do

    end function get_by_index

    !
    ! GET BY NAME CHARS
    !
    function get_by_name_chars(this, name) result(p)
        type(fson_value), pointer :: this, p
        character(len=*), intent(in) :: name
        
        type(fson_string), pointer :: string
        
        ! convert the char array into a string
        string => fson_string_create(name)
        
        p => get_by_name_string(this, string)

        call fson_string_destroy(string)
        
    end function get_by_name_chars
    
    !
    ! GET BY NAME STRING
    !
    function get_by_name_string(this, name) result(p)
        type(fson_value), pointer :: this, p
        type(fson_string), pointer :: name
        integer :: i                
        
        if(this % value_type .ne. TYPE_OBJECT) then
            nullify(p)
            return 
        end if
        
        do i=1, fson_value_count(this)
            p => fson_value_get(this, i)
            if (fson_string_equals(p%name, name)) then                
                return
            end if
        end do
        
        ! didn't find anything
        nullify(p)
        
        
    end function get_by_name_string
    
    !
    ! FSON VALUE PRINT
    !
    recursive subroutine fson_value_print(this, indent)
        type(fson_value), pointer :: this, element
        integer, optional, intent(in) :: indent
        character (len = 1024) :: tmp_chars
        integer :: tab, i, count, spaces
                
        if (present(indent)) then
            tab = indent
        else
            tab = 0
        end if
        
        spaces = tab * 2

        select case (this % value_type)
        case(TYPE_OBJECT)
            print *, repeat(" ", spaces), "{"
            count = fson_value_count(this)
            do i = 1, count
                ! get the element
                element => fson_value_get(this, i)
                ! get the name
                call fson_string_copy(element % name, tmp_chars)
                ! print the name
                print *, repeat(" ", spaces), '"', trim(tmp_chars), '":'
                ! recursive print of the element
                call fson_value_print(element, tab + 1)
                ! print the separator if required
                if (i < count) then
                    print *, repeat(" ", spaces), ","
                end if
            end do

            print *, repeat(" ", spaces), "}"
        case (TYPE_ARRAY)
            print *, repeat(" ", spaces), "["
            count = fson_value_count(this)
            do i = 1, count
                ! get the element
                element => fson_value_get(this, i)
                ! recursive print of the element
                call fson_value_print(element, tab + 1)
                ! print the separator if required
                if (i < count) then
                    print *, ","
                end if
            end do
            print *, repeat(" ", spaces), "]"
        case (TYPE_NULL)
            print *, repeat(" ", spaces), "null"
        case (TYPE_STRING)
            call fson_string_copy(this % value_string, tmp_chars)
            print *, repeat(" ", spaces), '"', trim(tmp_chars), '"'
        case (TYPE_LOGICAL)
            if (this % value_logical) then
                print *, repeat(" ", spaces), "true"
            else
                print *, repeat(" ", spaces), "false"
            end if
        case (TYPE_INTEGER)
            print *, repeat(" ", spaces), this % value_integer
        case (TYPE_REAL)
            print *, repeat(" ", spaces), this % value_double
        end select
    end subroutine fson_value_print
       

end module fson_value_m
