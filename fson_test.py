# Build and run FRUIT tests. Optional command line arguments are the test module
# names required in the test, otherwise all modules are used.

from FRUIT import *
from sys import argv
from glob import glob

driver_name = 'fson_test_driver'
test_dir = 'src/tests/'
output_dir = 'dist/tests/'

if len(argv) > 1: test_sources = [test_dir + arg + '.f90' for arg in argv[1:]]
else: test_sources = glob(test_dir + '*.f90')

driver_source = test_dir + driver_name + '.f90'
if driver_source in test_sources: test_sources.remove(driver_source)
build_command = 'make ' + output_dir + driver_name

suite = test_suite(test_sources)
suite.build_run(driver_source, build_command, output_dir = output_dir)
if suite.built: suite.summary()
else: print 'Failed to build/run tests'
