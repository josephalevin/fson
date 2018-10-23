# Writes driver program source for a FRUIT unit test.
# First two arguments are the paths to the project root directory and build directory.
# The third argument is the test name.

import os
import sys
from FRUIT import *

project_dir = sys.argv[1]
build_dir = sys.argv[2]
test_name = sys.argv[3]

driver_source = os.path.join(build_dir, test_name + '_main.f90')
test_module = os.path.join(project_dir, 'src', 'tests', test_name + '.f90')

orig_dir = os.getcwd()
os.chdir(project_dir)

suite = test_suite([test_module])
suite.write(driver_source)

os.chdir(orig_dir)
