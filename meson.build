# FSON Meson build script

project('FSON', ['fortran', 'c'], version: '1.0.0')

FRUIT  = dependency('FRUIT', version: '>=3.4.1', required: false)

fson_sources = [
  'src/fson_string_m.f90',
  'src/fson_value_m.f90',
  'src/fson_path_m.f90',
  'src/fson.f90']

FSON = shared_library('fson', fson_sources)

if FRUIT.found()

  python = find_program('python')
  test_driver_src = configure_file(
    output: 'fson_test_driver.f90',
    command: [python, join_paths(meson.source_root(),
                                 'fson_test_setup.py')])

  test_sources = [
    'src/tests/fson_test.f90',
    'src/tests/fson_test2.f90']

  unit_tests = executable('fson_test_driver',
             test_sources, test_driver_src,
             link_with: FSON,
             dependencies: FRUIT)

  json_datafiles = ['test1.json', 'test2.json', 'test3.json']
  foreach f: json_datafiles
    json_data = configure_file(
      input: join_paths(meson.source_root(), 'src', 'tests', f),
      output: f, copy: true)
  endforeach

  add_test_setup('FRUIT',
                 exe_wrapper: [python,
                               join_paths(meson.source_root(),
                                          'fson_run_tests.py')])
  test('unit_tests', unit_tests)

endif
