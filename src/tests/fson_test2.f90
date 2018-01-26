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

    real, parameter :: expected(7) = [-3.0, -2.1, -1.2, 0.0, 1.9, 1.0E-01, 1.0E+01]
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
         [-3.0_dp, -2.1_dp, -1.2_dp, 0.0_dp, 1.9_dp, 0.1_dp, 10.0_dp]
    real(dp), parameter :: expected_exp(2) = &
         [1.4e-14_dp, 2.e-14_dp]
    real(dp), parameter :: expected_exp_round(3) = &
         [6.414e-16_dp, 3.139e-15_dp, 4.052e-15_dp]
    real(dp), parameter :: expected_2d(2,3) = &
         transpose(reshape([ &
         2._dp, -1._dp, 5._dp, -2.718_dp, 3.142_dp, 1.618_dp], [3,2]))
    real(dp), parameter :: tol = 1.e-5_dp, exp_tol = 1.e-24_dp
    type(fson_value), pointer :: data
    real(dp), allocatable :: vals(:), vals2(:,:)

    data => fson_parse("test2.json")

    call fson_get(data, "array_real", vals)
    call assert_equals(expected, vals, size(expected), tol, "test_array_double")

    call fson_get(data, "array_real_exp", vals)
    call assert_equals(expected_exp, vals, size(expected_exp), exp_tol, &
         "test_array_double_exp")

    call fson_get(data, "array_real_exp_round", vals)
    call assert_equals(expected_exp_round, vals, size(expected_exp_round), exp_tol, &
         "test_array_double_exp_round")

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
    double precision, parameter :: expected_val_double = expected_val_real
    double precision, parameter :: double_tol = 1.e-07
    logical, parameter :: expected_val_logical = .true.
    character(len = 11), parameter :: expected_val_string = "Hello world"
    real :: val_real
    double precision :: val_double
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
    call fson_get(item, "", val_double)
    call assert_equals(expected_val_double, val_double, double_tol, "dict value double")

    item => fson_value_get(dict, "logical")
    call fson_get(item, "", val_logical)
    call assert_equals(expected_val_logical, val_logical, "dict value logical")

    item => fson_value_get(dict, "string")
    call fson_get(item, "", val_string) 
    call assert_equals(expected_val_string, val_string, "dict value string")
   
    call fson_destroy(data)

  end subroutine test_dict

!------------------------------------------------------------------------

  subroutine test_empty_string()

    type(fson_value), pointer :: data
    character(len = 1) :: str
    
    data => fson_parse("test2.json")
    call fson_get(data, "empty_string", str)
    call assert_equals("", str, "empty string")

    call fson_destroy(data)

  end subroutine test_empty_string

!------------------------------------------------------------------------

  subroutine test_escape_string()

    type(fson_value), pointer :: data
    character(len = 1) :: str

    data => fson_parse("test2.json")
    call fson_get(data, "escape_string", str)
    call assert_equals("\", str, "escape string")
    call fson_destroy(data)

  end subroutine test_escape_string

!------------------------------------------------------------------------

  subroutine test_integer_mantissa_exponent()

    type(fson_value), pointer :: data
    real :: x
    real, parameter :: expected_positive = 2.e3
    real, parameter :: expected_negative = 2.e-3
    real, parameter :: expected_positive_large = 1.e16
    real, parameter :: expected_negative_large = 2.e-16

    data => fson_parse("test2.json")
    call fson_get(data, "integer_mantissa_positive_exponent", x)
    call assert_equals(expected_positive, x, "integer mantissa positive exponent")
    call fson_get(data, "integer_mantissa_negative_exponent", x)
    call assert_equals(expected_negative, x, "integer mantissa negative exponent")
    call fson_get(data, "integer_mantissa_large_exponent", x)
    call assert_equals(expected_positive_large, x, "integer mantissa large exponent")
    call fson_get(data, "integer_mantissa_large_negative_exponent", x)
    call assert_equals(expected_negative_large, x, &
         "integer mantissa large negative exponent")

    call fson_destroy(data)

  end subroutine test_integer_mantissa_exponent

!------------------------------------------------------------------------

  subroutine test_long_integral_double()

    type(fson_value), pointer :: data
    real(dp) :: x
    real(dp), parameter :: expected = 1.e10_dp, tol = 1.e-3_dp

    data => fson_parse("test2.json")
    call fson_get(data, "long_integral_double", x)
    call assert_equals(expected, x, tol, "long integral double")

    call fson_destroy(data)

  end subroutine test_long_integral_double

!------------------------------------------------------------------------

  subroutine test_long_frac_double()

    type(fson_value), pointer :: data
    real(dp) :: x
    real(dp), parameter :: expected = 0.0002_dp, tol = 1.e-18_dp

    data => fson_parse("test2.json")
    call fson_get(data, "long_frac_double", x)
    call assert_equals(expected, x, tol, "long frac double")

    call fson_destroy(data)

  end subroutine test_long_frac_double

!------------------------------------------------------------------------

  subroutine test_long_integer_double()

    type(fson_value), pointer :: data
    real(dp) :: x
    real(dp), parameter :: expected = 3.155760e9_dp, tol = 1.e-3_dp

    data => fson_parse("test2.json")
    call fson_get(data, "long_int", x)
    call assert_equals(expected, x, tol, "long int to double")

    call fson_destroy(data)

  end subroutine test_long_integer_double

!------------------------------------------------------------------------

  subroutine test_char_array()

    type(fson_value), pointer :: data
    integer :: i, j
    integer, parameter :: string_length = 5
    integer, parameter :: count1 = 3, count21 = 2, count22 = 3
    character(len = string_length), allocatable :: x1(:), x2(:, :)
    character(len = string_length), parameter :: expected1(count1) = &
         ["alpha", "beta ", "foo  "]
    character(len = string_length), parameter :: expected2(count21, &
         count22) = transpose(reshape( &
         ["alpha", "beta ", "gamma", "foo  ", "bar  ", "fubar"], &
         [count22, count21]))
    
    data => fson_parse("test2.json")

    call fson_get(data, "char_array", x1)
    do i = 1, count1
       call assert_equals(expected1(i), x1(i), "1d char array")
    end do

    call fson_get(data, "char_array2", x2)
    do i = 1, count21
       do j = 1, count22
          call assert_equals(expected2(i,j), x2(i,j), "2d char array")
       end do
    end do

    call fson_destroy(data)

  end subroutine test_char_array

!------------------------------------------------------------------------

subroutine test_no_label

    type(fson_value), pointer :: data
    ! Locals:
    integer :: count, i
    type(fson_value), pointer :: dict, item
    character(len = 8) :: str
  
    data => fson_parse("test3.json")

    count = fson_value_count(data)
    call assert_equals(2, count, "count")
    do i = 1, count
       dict => fson_value_get(data, i)
       item => fson_value_get(dict, "a")
       call fson_get(item, "", str) 
       call assert_equals("string a", str, "item a")
       item => fson_value_get(dict, "b")
       call fson_get(item, "", str) 
       call assert_equals("string b", str, "item b")
    end do

    call fson_destroy(data)

end subroutine test_no_label

!------------------------------------------------------------------------

end module fson_test2
