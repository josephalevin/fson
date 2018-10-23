# Writes driver program source for a FRUIT unit test.
# First argument is the project root directory path, second argument is the test name.

import os
import sys
from FRUIT import *

project_dir = sys.argv[1]
test_name = sys.argv[2]

driver_source = os.path.join(project_dir, 'build', test_name + '_main.f90')
test_module = os.path.join(project_dir, 'src', 'tests', test_name + '.f90')

orig_dir = os.getcwd()
os.chdir(project_dir)

suite = test_suite([test_module])
suite.write(driver_source)

os.chdir(orig_dir)
