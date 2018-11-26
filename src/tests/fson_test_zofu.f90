module fson_test

  use fson
  use zofu

  implicit none

  character(len = 512) :: data_path

contains

!------------------------------------------------------------------------

  subroutine setup()

    ! Locals:
    integer :: ios

    call get_environment_variable('FSON_TEST_DATA_PATH', data_path, status = ios)
    if (ios /= 0) data_path = ''

  end subroutine setup

!------------------------------------------------------------------------

    function read_file(filename) result(r)
    character(*) :: filename
    character(:), allocatable :: r
    ! Locals:
    integer :: iunit, istat, filesize

    open(newunit = iunit, file = filename, status = 'old', &
         form = 'unformatted', access='stream', iostat = istat)

    if (istat == 0) then
       inquire(file = filename, size = filesize)
       if (filesize > 0) then
          allocate(character(len = filesize) :: r)
          read(iunit, pos = 1) r
          close(iunit)
       else
          write(*,*) 'Error getting file size.'
       end if
    else
       write(*,*) 'Error opening file.'
    end if
    
  end function read_file

!------------------------------------------------------------------------

  subroutine subtest_fson_test1(test, json_data)

    use fson_value_m

    class(unit_test_type), intent(in out) :: test
    type(fson_value), pointer :: json_data
    ! Locals:
    integer :: age
    real :: testReal, testExp, testNegExp, testLongReal
    double precision :: testDouble, testExpDouble

    call fson_get(json_data, "age", age)
    call fson_get(json_data, "testReal", testReal)
    call fson_get(json_data, "testLongReal", testLongReal)
    call fson_get(json_data, "testDouble", testDouble)
    call fson_get(json_data, "testExp", testExp)
    call fson_get(json_data, "testExpDouble", testExpDouble)
    call fson_get(json_data, "testNegExp", testNegExp)

    call test%assert(25, age, "integer")
    call test%assert(8.1235, testReal, "real")
    call test%assert(8.1234567890123456789, testLongReal, "longReal")
    call test%assert(testReal, 8.1235, "real")
    call test%assert(testDouble, 8.1234567890123456789d0, "double")
    call test%assert(testExpDouble, 1.234567890123456790d300, "double exp")
    call test%assert(1.23E12, testExp, "real exp")
    call test%assert(1.23e-12, testNegExp)

    call fson_destroy(json_data)

  end subroutine subtest_fson_test1

!------------------------------------------------------------------------

  subroutine test_fson_parse_string(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    type(fson_value), pointer :: json_data
    character(1024) :: inputString

    inputString = read_file(trim(adjustl(data_path)) // "test1.json")

    json_data => fson_parse(str=inputString)
    call subtest_fson_test1(test, json_data)

  end subroutine test_fson_parse_string

!------------------------------------------------------------------------

  subroutine test_fson_parse_file(test)

    class(unit_test_type), intent(in out) :: test
    ! Locals:
    type(fson_value), pointer :: json_data

    json_data => fson_parse(trim(adjustl(data_path)) // "test1.json")
    call subtest_fson_test1(test, json_data)

  end subroutine test_fson_parse_file

!------------------------------------------------------------------------

end module fson_test
