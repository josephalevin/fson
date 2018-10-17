# Writes driver program source for FRUIT unit tests.

import os
from FRUIT import *
from glob import glob

driver_name = 'fson_test_driver'
test_dir = 'src/tests/'
build_dir = 'build/'

orig_dir = os.getcwd()
os.chdir('..')

test_sources = glob(test_dir + '*.f90')

driver_source = build_dir + driver_name + '.f90'
if driver_source in test_sources: test_sources.remove(driver_source)

suite = test_suite(test_sources)
suite.write(driver_source)

os.chdir(orig_dir)
