# FSON Meson build script

project('FSON', 'fortran', version: '1.0.0')

fson_sources = [
  'src/fson_string_m.f90',
  'src/fson_value_m.f90',
  'src/fson_path_m.f90',
  'src/fson.f90']

shared_library('fson', fson_sources)
