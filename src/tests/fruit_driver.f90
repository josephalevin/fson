program fruit_driver
  use fruit
  use fson_test
  call init_fruit
  call test_fson_parse_string
  call test_fson_parse_file
  call fruit_summary
end program fruit_driver
