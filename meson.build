# FSON Meson build script

project('FSON', ['fortran', 'c'], version: '1.0.0')

FRUIT  = dependency('FRUIT', version: '>=3.4.1', required: false)

fson_modules = ['fson_string_m.f90',
                'fson_value_m.f90',
                'fson_path_m.f90',
                'fson.f90']
fson_sources = []
foreach name: fson_modules
  fson_sources += join_paths('src', name)
endforeach

FSON = shared_library('fson', fson_sources, install: true)

FSON_dep = declare_dependency(link_with : FSON)

# install module files:
fson_objs = []
foreach name: fson_modules
  fson_objs += 'src_' + name + '.o'
endforeach
install_subdir(join_paths('build', 'fson@sha'),
               install_dir: 'include', strip_directory: true,
               exclude_files: [fson_objs, 'libfson.so.symbols'])

pkg = import('pkgconfig')
pkg.generate(FSON,
  name: 'FSON',
  description: 'Fortran 95 JSON parser library',
  filebase: 'FSON')

if FRUIT.found()

  # unit test data:
  json_datafiles = ['test1.json', 'test2.json', 'test3.json']
  foreach f: json_datafiles
    json_data = configure_file(
      input: join_paths(meson.current_source_dir(), 'src', 'tests', f),
      output: f, copy: true)
  endforeach

  # unit tests:
  python = find_program('python')
  test_names = ['fson_test', 'fson_test2']
  foreach test_name: test_names
    test_driver_src = configure_file(
      output: test_name + '_main.f90',
      command: [python,
                join_paths(meson.current_source_dir(), 'fson_test_setup.py'),
                meson.current_source_dir(), meson.current_build_dir(),
                test_name])
    unit_test = executable(
      test_name, [join_paths(meson.current_source_dir(),
                             'src', 'tests',
                             test_name + '.f90'),
                  test_driver_src],
      link_with: FSON,
      dependencies: FRUIT)
    test(test_name, unit_test)
  endforeach

endif
