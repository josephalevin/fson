# FSON Meson build script

project('FSON', ['fortran', 'c'], version: '1.0.0')

zofu  = dependency('zofu', version: '>=0.1.0', required: false)

fson_modules = ['fson_string_m.f90',
                'fson_value_m.f90',
                'fson_path_m.f90',
                'fson.f90']
fson_sources = []
foreach name: fson_modules
  fson_sources += join_paths('src', name)
endforeach

if meson.get_compiler('fortran').get_id() == 'gcc'
  f_args = ['-Wno-maybe-uninitialized']
else
  f_args = []
endif

fson = shared_library('fson', fson_sources, fortran_args: f_args, install: true)

fson_dep = declare_dependency(link_with : fson)

prefix = get_option('prefix')
libdir = get_option('libdir')
includedir = get_option('includedir')
if includedir != ''
  module_install_dir = join_paths(prefix, includedir)
  fson_objs = []
  foreach m: fson_modules
    fson_objs += ['src_' + m + '.o']
  endforeach
  # NB this is a temporary measure until Meson gets specific
  # functionality for installing Fortran modules:
  install_subdir(join_paths('build', 'fson@sha'),
                 install_dir: module_install_dir,
                 strip_directory: true,
                 exclude_files: fson_objs)
  pkg = import('pkgconfig')
  pkg.generate(fson,
               description: 'Fortran 95 JSON parser')
endif

if zofu.found()

  # unit test data:
  json_datafiles = ['test1.json', 'test2.json', 'test3.json']
  foreach f: json_datafiles
    json_data = configure_file(
      input: join_paths(meson.current_source_dir(), 'src', 'tests', f),
      output: f, copy: true)
  endforeach

  # unit tests:
  test_names = ['fson_test', 'fson_test2']
  foreach test_name: test_names
    test_src = join_paths(meson.current_source_dir(),
                        'src', 'tests', test_name + '_zofu.f90')
    driver_src_name = test_name + '_driver.f90'
    test_driver_src = configure_file(
      output: driver_src_name,
      command: ['zofu-driver', test_src, driver_src_name])
    unit_test = executable(
      test_name, [test_driver_src, test_src],
      link_with: fson,
      dependencies: zofu)
    test(test_name, unit_test)
  endforeach

endif
