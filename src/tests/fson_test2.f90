module fson_test2

  use fruit
  use fson
  use fson_value_m

  implicit none

contains

!------------------------------------------------------------------------

  subroutine test_array()

    implicit none

    real, parameter :: vals(7) = [-3.0, -2.1, -1.2, 0.0, 1.9, 2.8, 3.7]
    type(fson_value), pointer :: data, arr, item
    integer :: i
    real :: val
    real, parameter :: tol = 1.e-9
    
    data => fson_parse("test2.json")

    call fson_get(data, "array1", arr)

    do i = 1, fson_value_count(arr)
       item => fson_value_get(arr, i)
       call fson_get(item, "", val)
       call assert_equals(vals(i), val, tol, "test_array")
    end do

    call fson_destroy(data)

  end subroutine test_array

!------------------------------------------------------------------------

  subroutine test_dict()

    implicit none
    type(fson_value), pointer :: data, dict, item
    integer :: count, val_int
    integer, parameter :: expected_count = 4, expected_val_int = -3
    real, parameter :: expected_val_real = 2.718281828459
    logical, parameter :: expected_val_logical = .true.
    character(len = 11), parameter :: expected_val_string = "Hello world"
    real :: val_real
    logical :: val_logical
    character(len = len(expected_val_string)) :: val_string

    data => fson_parse("test2.json")

    call fson_get(data, "dict1", dict)

    count = fson_value_count(dict)
    call assert_equals(expected_count, count, "dict count")

    item => fson_value_get(dict, "int")
    call fson_get(item, "", val_int)
    call assert_equals(expected_val_int, val_int, "dict value integer")

    item => fson_value_get(dict, "real")
    call fson_get(item, "", val_real)
    call assert_equals(expected_val_real, val_real, "dict value real")

    item => fson_value_get(dict, "logical")
    call fson_get(item, "", val_logical)
    call assert_equals(expected_val_logical, val_logical, "dict value logical")

    item => fson_value_get(dict, "string")
    call fson_get(item, "", val_string) 
    call assert_equals(expected_val_string, val_string, "dict value string")
   
    call fson_destroy(data)

  end subroutine test_dict

!------------------------------------------------------------------------

  subroutine test_array_int()

    implicit none

    integer, parameter :: expected(5) = [7, 1, -2, 4, -10]
    type(fson_value), pointer :: data
    integer, allocatable :: vals(:)
    
    data => fson_parse("test2.json")

    call fson_get(data, "array2", vals)
    call assert_equals(expected, vals, size(expected), "test_array_int")

    call fson_destroy(data)
    deallocate(vals)

  end subroutine test_array_int

!------------------------------------------------------------------------

end module fson_test2
