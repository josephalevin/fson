# Writes driver program source for a FRUIT unit test.

import os
import sys
from FRUIT import *

test_dir = 'src/tests'
build_dir = 'build/'

test_name = sys.argv[1]
driver_source = os.path.join(build_dir, test_name + '_main.f90')
test_module = os.path.join(test_dir, test_name + '.f90')

orig_dir = os.getcwd()
os.chdir('..')

suite = test_suite([test_module])
suite.write(driver_source)

os.chdir(orig_dir)
