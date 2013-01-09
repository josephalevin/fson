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
! File:   fson_path_m.f95
! Author: Joseph A. Levin
!
! Created on March 10, 2012, 11:01 PM
!

module fson_path_m
    
    use fson_value_m 
    use fson_string_m

    private
    
    public :: fson_path_get, array_callback
    
    interface fson_path_get
        module procedure get_by_path
        module procedure get_integer
        module procedure get_real
        module procedure get_double
        module procedure get_logical
        module procedure get_chars
        module procedure get_array
    end interface fson_path_get

contains
    !
    ! GET BY PATH
    !
    ! $     = root 
    ! @     = this
    ! .     = child object member
    ! []    = child array element
    !
    recursive subroutine get_by_path(this, path, p)
        type(fson_value), pointer :: this, p        
        character(len=*), intent(inout) :: path
        integer :: i, length, child_i
        character :: c
        logical :: array        
                
        ! default to assuming relative to this
        p => this
        
        child_i = 1          
        
        array = .false.
        
        length = len_trim(path)
        
        do i=1, length
            c = path(i:i)    
            select case (c)
                case ("$")
                    ! root
                    do while (associated (p % parent))
                        p => p % parent
                    end do
                    child_i = i + 1
                case ("@")
                    ! this                    
                    p => this
                    child_i = i + 1
                case (".")                    
                    ! get child member from p                          
                    if (child_i < i) then                          
                        p => fson_value_get(p, path(child_i:i-1))
                    else
                        child_i = i + 1
                        cycle
                    end if
                    
                    if(.not.associated(p)) then
                        return                                        
                    end if
                    
                    child_i = i+1
                case ("[")                    
                    ! start looking for the array element index
                    array = .true.
                    child_i = i + 1
                case ("]")
                    if (.not.array) then
                        print *, "ERROR: Unexpected ], not missing preceding ["
                        call exit(1)
                    end if
                    array = .false.
                    child_i = parse_integer(path(child_i:i-1))                                                
                    p => fson_value_get(p, child_i)                                                                                                                    
                    
                    child_i= i + 1                                     
            end select            
        end do
                
        ! grab the last child if present in the path
        if (child_i <= length) then            
            p => fson_value_get(p, path(child_i:i-1))                    
            if(.not.associated(p)) then
                return
            else                
            end if
        end if
                
        
    end subroutine get_by_path
    
    !
    ! PARSE INTEGER
    !
    integer function parse_integer(chars) result(integral)
        character(len=*) :: chars
        character :: c
        integer :: tmp, i
                
        integral = 0        
        do i=1, len_trim(chars)
            c = chars(i:i)            
            select case(c)
                case ("0":"9")
                    ! digit        
                    read (c, '(i1)') tmp                                               
                    
                    ! shift
                    if(i > 1) then
                        integral = integral * 10
                    end if
                    ! add
                    integral = integral + tmp
                                                    
                case default                          
                    return
            end select            
        end do
    
    end function parse_integer    
    
    !
    ! GET INTEGER
    !
    subroutine get_integer(this, path, value)
        type(fson_value), pointer :: this, p
        character(len=*), optional :: path
        integer :: value        
        
        
        nullify(p)                
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = p % value_integer
        else if (p % value_type == TYPE_REAL) then
            value = p % value_real
        else if (p % value_type == TYPE_LOGICAL) then
            if (p % value_logical) then
                value = 1
            else
                value = 0
            end if
        else
            print *, "Unable to resolve value to integer: ", path
            call exit(1)
        end if
        
    end subroutine get_integer
    
    !
    ! GET REAL
    !
    subroutine get_real(this, path, value)
        type(fson_value), pointer :: this, p
        character(len=*), optional :: path
        real :: value        
        
        
        nullify(p)                
        
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = p % value_integer
        else if (p % value_type == TYPE_REAL) then
            value = p % value_real
        else if (p % value_type == TYPE_LOGICAL) then
            if (p % value_logical) then
                value = 1
            else
                value = 0
            end if
        else
            print *, "Unable to resolve value to real: ", path
            call exit(1)
        end if
        
    end subroutine get_real
    
    !
    ! GET DOUBLE
    !
    subroutine get_double(this, path, value)
        type(fson_value), pointer :: this, p
        character(len=*), optional :: path
        double precision :: value        
        
        
        nullify(p)                
        
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = p % value_integer
        else if (p % value_type == TYPE_REAL) then
            value = p % value_real
        else if (p % value_type == TYPE_LOGICAL) then
            if (p % value_logical) then
                value = 1
            else
                value = 0
            end if
        else
            print *, "Unable to resolve value to double: ", path
            call exit(1)
        end if
        
    end subroutine get_double
    
    
    !
    ! GET LOGICAL
    !
    subroutine get_logical(this, path, value)
        type(fson_value), pointer :: this, p
        character(len=*), optional :: path
        logical :: value        
        
        
        nullify(p)                
        
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = (p % value_integer > 0)       
        else if (p % value_type == TYPE_LOGICAL) then
            value = p % value_logical
        else
            print *, "Unable to resolve value to real: ", path
            call exit(1)
        end if
        
    end subroutine get_logical
    
    !
    ! GET CHARS
    !
    subroutine get_chars(this, path, value)
        type(fson_value), pointer :: this, p
        character(len=*), optional :: path
        character(len=*) :: value  
        
        nullify(p)                
        
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
                
        
        if(p % value_type == TYPE_STRING) then            
            call fson_string_copy(p % value_string, value)          
        else
            print *, "Unable to resolve value to characters: ", path
            call exit(1)
        end if
        
    end subroutine get_chars
    
    !
    ! GET ARRAY
    !
    
    subroutine get_array(this, path, array_callback)
        type(fson_value), pointer :: this, p, element
        character(len=*), optional :: path   
        integer :: index, count
        
        ! ELEMENT CALLBACK
        interface
            subroutine array_callback(element, index, count)
                use fson_value_m
                type(fson_value), pointer :: element
                integer :: index, count
            end subroutine array_callback
        end interface
        
        
        nullify(p)                
        
        ! resolve the path to the value
        if(present(path)) then
            call get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
            
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            call exit(1)
        end if
                
        
        if(p % value_type == TYPE_ARRAY) then            
            count = fson_value_count(p)
            do index=1, count
                element => fson_value_get(p, index)
                call array_callback(element, index, count)
            end do
        else
            print *, "Resolved value is not an array. ", path
            call exit(1)
        end if
        
    end subroutine get_array
    

end module fson_path_m
