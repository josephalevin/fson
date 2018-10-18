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

  json_datafiles = ['test1.json', 'test2.json', 'test3.json']
  foreach f: json_datafiles
    json_data = configure_file(
      input: join_paths(meson.source_root(), 'src', 'tests', f),
      output: f, copy: true)
  endforeach

  python = find_program('python')

  test_names = ['fson_test', 'fson_test2']
  foreach test_name: test_names
    test_driver_src = configure_file(
      output: test_name + '_main.f90',
      command: [python,
                join_paths(meson.source_root(), 'fson_test_setup.py'),
                test_name])
    unit_test = executable(
      test_name, [join_paths(meson.source_root(),
                             'src', 'tests',
                             test_name + '.f90'),
                  test_driver_src],
      link_with: FSON,
      dependencies: FRUIT)
    test(test_name, unit_test)
  endforeach

endif
