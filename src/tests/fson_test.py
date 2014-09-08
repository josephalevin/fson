from FRUIT import *

test = 'fson_test'
test_modules = [test + '.f90', 'fson_test2.f90']
driver = test + '_driver.f90'
build_command = 'make ' + test + '_driver'

suite = test_suite(test_modules)
suite.build_run(driver, build_command)
suite.summary()
