# FSON Meson build script

project('FSON', ['fortran', 'c'], version: '1.0.3')

zofu  = dependency('zofu',  version: '>=0.1.0', fallback: ['zofu', 'zofu_dep'])

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
  if meson.version() < '0.55'
    mod_dir = 'fson@sha'
  else
    mod_dir = fson.full_path().split('/').get(-1) + '.p'
  endif
  mod_path = join_paths(meson.current_build_dir(), mod_dir)
  install_subdir(mod_path,
                 install_dir: module_install_dir,
                 strip_directory: true,
                 exclude_files: fson_objs)

  pkg = import('pkgconfig')
  pkg.generate(fson,
               description: 'Fortran 95 JSON parser')
endif

zofu_driver = find_program('zofu-driver', required: false)
if not zofu_driver.found()
  zofu_subproject = subproject('zofu')
  zofu_driver = zofu_subproject.get_variable('driver_exe')
endif

# unit tests:
test_names = ['fson_test', 'fson_test2']
test_env = environment()
test_env.set('FSON_TEST_DATA_PATH',
             join_paths(meson.current_source_dir(), 'src', 'tests', ''))

foreach test_name: test_names
  test_src = join_paths(meson.current_source_dir(),
                        'src', 'tests', test_name + '_zofu.f90')
  test_lib = static_library(test_name + '_lib', test_src,
                            link_with: fson,
                            dependencies: zofu)
  test_driver_src = custom_target(
    test_name + '_driver',
    input: test_src,
    output: test_name + '_driver.f90',
    command: [zofu_driver, '@INPUT@', '@OUTPUT@'])
  unit_test = executable(
    test_name, [test_driver_src, test_src],
    link_with: [fson, test_lib],
    dependencies: zofu)
  test(test_name, unit_test, env: test_env)
endforeach
