module fson_test2

  use fruit
  use fson
  use fson_value_m

  implicit none

  integer, parameter :: dp = kind(0.d0)

contains

!------------------------------------------------------------------------

  subroutine test_array_int()

    implicit none

    integer, parameter :: expected(5) = [7, 1, -2, 4, -10]
    integer :: expected_2d(3,2) = transpose(reshape([1, 2, 3, 4, 5, 6], [2,3]))
    type(fson_value), pointer :: data
    integer, allocatable :: vals(:), vals2(:,:)
    
    data => fson_parse("test2.json")

    call fson_get(data, "array_int", vals)
    call assert_equals(expected, vals, size(expected), "test_array_int")

    call fson_get(data, "array_int_2d", vals2)
    call assert_equals(expected_2d, vals2, size(expected_2d,1), size(expected_2d,2), &
         "test_array_int_2d")

    call fson_destroy(data)
    deallocate(vals, vals2)

  end subroutine test_array_int

!------------------------------------------------------------------------

  subroutine test_array_real()

    implicit none

    real, parameter :: expected(7) = [-3.0, -2.1, -1.2, 0.0, 1.9, 2.8, 3.7]
    real, parameter :: expected_2d(2,3) = &
         transpose(reshape([2., -1., 5., -2.718, 3.142, 1.618], [3,2]))
    real, parameter :: tol = 1.e-5
    type(fson_value), pointer :: data
    real, allocatable :: vals(:), vals2(:,:)

    data => fson_parse("test2.json")

    call fson_get(data, "array_real", vals)
    call assert_equals(expected, vals, size(expected), tol, "test_array_real")

    call fson_get(data, "array_real_2d", vals2)
    call assert_equals(expected_2d, vals2, size(expected_2d,1), size(expected_2d,2), &
         tol, "test_array_real_2d")

    call fson_destroy(data)
    deallocate(vals, vals2)

  end subroutine test_array_real

!------------------------------------------------------------------------

  subroutine test_array_double()

    implicit none

    real(dp), parameter :: expected(7) = &
         [-3.0_dp, -2.1_dp, -1.2_dp, 0.0_dp, 1.9_dp, 2.8_dp, 3.7_dp]
    real(dp), parameter :: expected_2d(2,3) = &
         transpose(reshape([ &
         2._dp, -1._dp, 5._dp, -2.718_dp, 3.142_dp, 1.618_dp], [3,2]))
    real(dp), parameter :: tol = 1.e-5_dp
    type(fson_value), pointer :: data
    real(dp), allocatable :: vals(:), vals2(:,:)

    data => fson_parse("test2.json")

    call fson_get(data, "array_real", vals)
    call assert_equals(expected, vals, size(expected), tol, "test_array_double")

    call fson_get(data, "array_real_2d", vals2)
    call assert_equals(expected_2d, vals2, size(expected_2d,1), size(expected_2d,2), &
         tol, "test_array_double_2d")

    call fson_destroy(data)
    deallocate(vals, vals2)

  end subroutine test_array_double

!------------------------------------------------------------------------

  subroutine test_array_logical()

    implicit none

    logical, parameter :: expected(4) = [.true., .false., .true., .false.]
    logical, parameter :: expected_2d(3,3) = transpose(reshape(&
         [.true., .false., .true., .false., .true., .false., .true., .false., .true.],&
         [3,3]))
    type(fson_value), pointer :: data
    logical, allocatable :: vals(:), vals2(:,:)

    data => fson_parse("test2.json")

    call fson_get(data, "array_logical", vals)
    call assert_equals(expected, vals, size(expected), "test_array_logical")

    call fson_get(data, "array_logical_2d", vals2)
    call assert_equals(expected_2d, vals2, size(expected_2d,1), size(expected_2d,2), &
         "test_array_logical_2d")

    call fson_destroy(data)
    deallocate(vals, vals2)

  end subroutine test_array_logical

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

end module fson_test2
