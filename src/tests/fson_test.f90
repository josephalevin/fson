module fson_test
  use fruit

  contains
  subroutine test_fson_test1
    use fson
    use fson_value_m
    type(fson_value), pointer :: json_data, array, item
    integer :: age
    REAL :: testReal, testExp, testNegExp
    character(len=1024) :: strval, strval2, inputString

    inputString = '{&
      &"age"        : 25,&
      &"testReal"   : 8.1235, &
      &"testExp"    : 1.23e12, &
      &}'

!    json_data => fson_parse(str=inputString)
    json_data => fson_parse("test1.json")
    call fson_get(json_data, "age", age)
    call fson_get(json_data, "testReal", testReal)
    call fson_get(json_data, "testExp", testExp)
!    call fson_get(json_data, "testNegExp", testNegExp)

    call assert_equals(25, age)
    call assert_equals(8.1235, testReal)
    call assert_equals(1.23E12, testExp)
!    call assert_equals(5.43E-21, testNegExp)

    call fson_destroy(json_data)

  end subroutine test_fson_test1
end module fson_test
