# Run FRUIT tests and print exit code - for running via Meson test

from __future__ import print_function
from FRUIT import *
import sys

driver = sys.argv[1]

suite = test_suite([])
success = suite.run([driver])

for line in suite.output_lines: print(line)
if success is False: sys.exit(1)
