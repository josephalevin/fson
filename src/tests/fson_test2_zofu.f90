module fson_test2

  use zofu
  use fson
  use fson_value_m

  implicit none

  integer, parameter :: dp = kind(0.d0)
  character(len = 512) :: data_path

contains

  subroutine setup()

    ! Locals:
    integer :: ios

    call get_environment_variable('FSON_TEST_DATA_PATH', data_path, status = ios)
    if (ios /= 0) data_path = ''

  end subroutine setup

!------------------------------------------------------------------------

  subroutine test_array_int(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    integer, parameter :: expected(5) = [7, 1, -2, 4, -10]
    integer :: expected_2d(3,2) = transpose(reshape([1, 2, 3, 4, 5, 6], [2,3]))
    type(fson_value), pointer :: data
    integer, allocatable :: vals(:), vals2(:,:)
    
    data => fson_parse(trim(adjustl(data_path)) // "test2.json")

    call fson_get(data, "array_int", vals)
    call test%assert(expected, vals, "test_array_int")

    call fson_get(data, "array_int_2d", vals2)
    call test%assert(expected_2d, vals2, "test_array_int_2d")

    call fson_destroy(data)
    deallocate(vals, vals2)

  end subroutine test_array_int

!------------------------------------------------------------------------

  subroutine test_array_real(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    real, parameter :: expected(7) = [-3.0, -2.1, -1.2, 0.0, 1.9, 1.0E-01, 1.0E+01]
    real, parameter :: expected_2d(2,3) = &
         transpose(reshape([2., -1., 5., -2.718, 3.142, 1.618], [3,2]))
    type(fson_value), pointer :: data
    real, allocatable :: vals(:), vals2(:,:)

    data => fson_parse(trim(adjustl(data_path)) // "test2.json")

    call fson_get(data, "array_real", vals)
    call test%assert(expected, vals, "test_array_real")

    call fson_get(data, "array_real_2d", vals2)
    call test%assert(expected_2d, vals2, "test_array_real_2d")

    call fson_destroy(data)
    deallocate(vals, vals2)

  end subroutine test_array_real

!------------------------------------------------------------------------

  subroutine test_array_double(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    real(dp), parameter :: expected(7) = &
         [-3.0_dp, -2.1_dp, -1.2_dp, 0.0_dp, 1.9_dp, 0.1_dp, 10.0_dp]
    real(dp), parameter :: expected_exp(2) = &
         [1.4e-14_dp, 2.e-14_dp]
    real(dp), parameter :: expected_exp_round(3) = &
         [6.414e-16_dp, 3.139e-15_dp, 4.052e-15_dp]
    real(dp), parameter :: expected_2d(2,3) = &
         transpose(reshape([ &
         2._dp, -1._dp, 5._dp, -2.718_dp, 3.142_dp, 1.618_dp], [3,2]))
    type(fson_value), pointer :: data
    real(dp), allocatable :: vals(:), vals2(:,:)

    data => fson_parse(trim(adjustl(data_path)) // "test2.json")

    call fson_get(data, "array_real", vals)
    call test%assert(expected, vals, "test_array_double")

    call fson_get(data, "array_real_exp", vals)
    call test%assert(expected_exp, vals, "test_array_double_exp")

    call fson_get(data, "array_real_exp_round", vals)
    call test%assert(expected_exp_round, vals, "test_array_double_exp_round")

    call fson_get(data, "array_real_2d", vals2)
    call test%assert(expected_2d, vals2, "test_array_double_2d")

    call fson_destroy(data)
    deallocate(vals, vals2)

  end subroutine test_array_double

!------------------------------------------------------------------------

  subroutine test_array_logical(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    logical, parameter :: expected(4) = [.true., .false., .true., .false.]
    logical, parameter :: expected_2d(3,3) = transpose(reshape(&
         [.true., .false., .true., .false., .true., .false., .true., .false., .true.],&
         [3,3]))
    type(fson_value), pointer :: data
    logical, allocatable :: vals(:), vals2(:,:)

    data => fson_parse(trim(adjustl(data_path)) // "test2.json")

    call fson_get(data, "array_logical", vals)
    call test%assert(expected, vals, "test_array_logical")

    call fson_get(data, "array_logical_2d", vals2)
    call test%assert(expected_2d, vals2, "test_array_logical_2d")

    call fson_destroy(data)
    deallocate(vals, vals2)

  end subroutine test_array_logical

!------------------------------------------------------------------------

  subroutine test_dict(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    type(fson_value), pointer :: data, dict, item
    integer :: count, val_int
    integer, parameter :: expected_count = 4, expected_val_int = -3
    real, parameter :: expected_val_real = 2.718281828459
    double precision, parameter :: expected_val_double = expected_val_real
    logical, parameter :: expected_val_logical = .true.
    character(len = 11), parameter :: expected_val_string = "Hello world"
    real :: val_real
    double precision :: val_double
    logical :: val_logical
    character(len = len(expected_val_string)) :: val_string

    data => fson_parse(trim(adjustl(data_path)) // "test2.json")

    call fson_get(data, "dict1", dict)

    count = fson_value_count(dict)
    call test%assert(expected_count, count, "dict count")

    item => fson_value_get(dict, "int")
    call fson_get(item, "", val_int)
    call test%assert(expected_val_int, val_int, "dict value integer")

    item => fson_value_get(dict, "real")
    call fson_get(item, "", val_real)
    call test%assert(expected_val_real, val_real, "dict value real")
    call fson_get(item, "", val_double)
    call test%assert(expected_val_double, val_double, "dict value double")

    item => fson_value_get(dict, "logical")
    call fson_get(item, "", val_logical)
    call test%assert(expected_val_logical, val_logical, "dict value logical")

    item => fson_value_get(dict, "string")
    call fson_get(item, "", val_string) 
    call test%assert(expected_val_string, val_string, "dict value string")
   
    call fson_destroy(data)

  end subroutine test_dict

!------------------------------------------------------------------------

  subroutine test_empty_string(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    type(fson_value), pointer :: data
    character(len = 1) :: str
    
    data => fson_parse(trim(adjustl(data_path)) // "test2.json")
    call fson_get(data, "empty_string", str)
    call test%assert("", str, "empty string")

    call fson_destroy(data)

  end subroutine test_empty_string

!------------------------------------------------------------------------

  subroutine test_escape_string(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    type(fson_value), pointer :: data
    character(len = 1) :: str

    data => fson_parse(trim(adjustl(data_path)) // "test2.json")
    call fson_get(data, "escape_string", str)
    call test%assert("\", str, "escape string")
    call fson_destroy(data)

  end subroutine test_escape_string

!------------------------------------------------------------------------

  subroutine test_integer_mantissa_exponent(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    type(fson_value), pointer :: data
    real :: x
    real, parameter :: expected_positive = 2.e3
    real, parameter :: expected_negative = 2.e-3
    real, parameter :: expected_positive_large = 1.e16
    real, parameter :: expected_negative_large = 2.e-16

    data => fson_parse(trim(adjustl(data_path)) // "test2.json")
    call fson_get(data, "integer_mantissa_positive_exponent", x)
    call test%assert(expected_positive, x, "integer mantissa positive exponent")
    call fson_get(data, "integer_mantissa_negative_exponent", x)
    call test%assert(expected_negative, x, "integer mantissa negative exponent")
    call fson_get(data, "integer_mantissa_large_exponent", x)
    call test%assert(expected_positive_large, x, "integer mantissa large exponent")
    call fson_get(data, "integer_mantissa_large_negative_exponent", x)
    call test%assert(expected_negative_large, x, &
         "integer mantissa large negative exponent")

    call fson_destroy(data)

  end subroutine test_integer_mantissa_exponent

!------------------------------------------------------------------------

  subroutine test_long_integral_double(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    type(fson_value), pointer :: data
    real(dp) :: x
    real(dp), parameter :: expected = 1.e10_dp

    data => fson_parse(trim(adjustl(data_path)) // "test2.json")
    call fson_get(data, "long_integral_double", x)
    call test%assert(expected, x, "long integral double")

    call fson_destroy(data)

  end subroutine test_long_integral_double

!------------------------------------------------------------------------

  subroutine test_long_frac_double(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    type(fson_value), pointer :: data
    real(dp) :: x
    real(dp), parameter :: expected = 0.0002_dp

    data => fson_parse(trim(adjustl(data_path)) // "test2.json")
    call fson_get(data, "long_frac_double", x)
    call test%assert(expected, x, "long frac double")

    call fson_destroy(data)

  end subroutine test_long_frac_double

!------------------------------------------------------------------------

  subroutine test_long_integer(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    type(fson_value), pointer :: data
    real(dp) :: x
    real(dp), parameter :: expected = 3.155760e9_dp
    integer(kind = 8) :: i
    integer(kind = 8), parameter :: expected_int = 3155760000_8

    data => fson_parse(trim(adjustl(data_path)) // "test2.json")
    call fson_get(data, "long_int", i)
    call test%assert(expected_int, i, "long int")
    call fson_get(data, "long_int", x)
    call test%assert(expected, x, "long int to double")

    call fson_destroy(data)

  end subroutine test_long_integer

!------------------------------------------------------------------------

  subroutine test_char_array(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    type(fson_value), pointer :: data
    integer, parameter :: string_length = 5
    integer, parameter :: count1 = 3, count21 = 2, count22 = 3
    character(len = string_length), allocatable :: x1(:), x2(:, :)
    character(len = string_length), parameter :: expected1(count1) = &
         ["alpha", "beta ", "foo  "]
    character(len = string_length), parameter :: expected2(count21, &
         count22) = transpose(reshape( &
         ["alpha", "beta ", "gamma", "foo  ", "bar  ", "fubar"], &
         [count22, count21]))
    
    data => fson_parse(trim(adjustl(data_path)) // "test2.json")

    call fson_get(data, "char_array", x1)
    call test%assert(expected1, x1, "1d char array")

    call fson_get(data, "char_array2", x2)
    call test%assert(expected2, x2, "2d char array")

    call fson_destroy(data)

  end subroutine test_char_array

!------------------------------------------------------------------------

  subroutine test_no_label(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    type(fson_value), pointer :: data
    ! Locals:
    integer :: count, i
    type(fson_value), pointer :: dict, item
    character(len = 8) :: str
  
    data => fson_parse(trim(adjustl(data_path)) // "test3.json")

    count = fson_value_count(data)
    call test%assert(2, count, "count")
    do i = 1, count
       dict => fson_value_get(data, i)
       item => fson_value_get(dict, "a")
       call fson_get(item, "", str) 
       call test%assert("string a", str, "item a")
       item => fson_value_get(dict, "b")
       call fson_get(item, "", str) 
       call test%assert("string b", str, "item b")
    end do

    call fson_destroy(data)

  end subroutine test_no_label

!------------------------------------------------------------------------

end module fson_test2
