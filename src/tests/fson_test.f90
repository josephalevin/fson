module fson_test
  use fruit
  IMPLICIT NONE

  contains

  function read_file(file) result(r)
    character(:), allocatable :: s
    character(1024) :: r
    character(*) :: file
    character(len=1) :: buf
    integer ios, size
    integer, parameter :: end_of_file = -1    ! Processor dependent
    integer, parameter :: end_of_record = -2  ! Processor dependent
    open (unit = 21, file = file) !, status = "old", action = "read", form = "formatted")
    do
      READ (unit = 21, fmt = "(A)", ADVANCE='NO', SIZE=size, iostat = ios) buf
      if (ios == end_of_record) then
        s = s//CHAR(10)
        cycle
      else if (ios == end_of_file) then
        r = s
        exit
      end if
      s = s//buf
    end do
    close(21)
  end function

  subroutine subtest_fson_test1(json_data)
    use fson
    use fson_value_m
    type(fson_value), pointer :: json_data, array, item
    integer :: age
    REAL :: testReal, testExp, testNegExp
    DOUBLE PRECISION :: testDouble
    character(len=1024) :: strval, strval2
    call fson_get(json_data, "age", age)
    call fson_get(json_data, "testReal", testReal)
    call fson_get(json_data, "testDouble", testDouble)
    call fson_get(json_data, "testExp", testExp)
!    call fson_get(json_data, "testNegExp", testNegExp)

    call assert_equals(25, age, "integer")
    call assert_equals(8.1235, testReal, "real")
    call assert_true(abs(testReal-8.1235).lt.1e-10, "real")
    print *,testDouble
    print *,testExp
    call assert_true(abs(testDouble-8.1234567890123456789).lt.1e-5, "double")
    call assert_equals(1.23E12, testExp, "real exp")
!    call assert_equals(5.43E-21, testNegExp)

    call fson_destroy(json_data)
  end subroutine

  subroutine test_fson_parse_string
    use fson
    type(fson_value), pointer :: json_data
    character(1024)::inputString
    inputString = read_file("test1.json")

    print *, inputString
    json_data => fson_parse(str=inputString)
    call subtest_fson_test1(json_data)
  end subroutine test_fson_parse_string

  subroutine test_fson_parse_file
    use fson
    type(fson_value), pointer :: json_data
    json_data => fson_parse("test1.json")
    call subtest_fson_test1(json_data)
  end subroutine test_fson_parse_file


end module fson_test
