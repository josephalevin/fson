SRC=src
DIST=dist
BUILD=build
TESTSDIR=tests
FRUITDIR=tests/fruit

F95=.f90
OBJ=.o
EXE=

LIBTARGET=$(DIST)/libfson.so
FRUITTARGET=$(DIST)/fruit-lib.so

FC = gfortran
FCFLAGS = -O2 #-g -fbounds-check
FMFLAGS = -J$(BUILD)
LDFLAGS=

AR = ar
ARFLAGS= r

# "make" builds all
all: lib examples

# List of example programs
EXAMPLES = basic example1 example2
examples: $(patsubst %, $(DIST)/examples/%$(EXE), $(EXAMPLES)) json

# Fruit module for testing
FRUIT = fruit_util fruit
FRUITOBJS = $(patsubst %, $(BUILD)/$(FRUITDIR)/%$(OBJ), $(FRUIT))

# List of test object
TESTSRC = fson_test
TESTOBJS = $(patsubst %, $(BUILD)/$(TESTSDIR)/%$(OBJ), $(TESTSRC))

# List of tests
TESTS = fruit_driver
tests: $(FRUITTARGET) $(LIBTARGET) $(TESTOBJS) $(patsubst %, $(DIST)/$(TESTSDIR)/%$(EXE), $(TESTS)) json

JSON = $(shell find src -name '*.json')
json: $(patsubst $(SRC)%, $(DIST)%, $(JSON))

FSON = fson_string_m fson_value_m fson_path_m fson
OBJECTS = $(patsubst %, $(BUILD)/%$(OBJ), $(FSON))

$(LIBTARGET) : $(OBJECTS)
	mkdir -p `dirname $@`
	$(AR) $(ARFLAGS) $@ $^

lib: $(LIBTARGET)

$(FRUITTARGET) : $(FRUITOBJS)
	mkdir -p `dirname $@`
	$(AR) $(ARFLAGS) $@ $^

libfruit: $(FRUITTARGET)

$(DIST)/%.json : $(SRC)/%.json
	mkdir -p $(@D)
	cp -f $< $@

# build test program
$(DIST)/$(TESTSDIR)/%$(EXE) : $(TESTOBJS) $(FRUITTARGET) $(BUILD)/$(TESTSDIR)/%$(OBJ) $(LIBTARGET)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) -I$(BUILD) -o $@ $^ $(LDFLAGS)

# build fruit library
$(BUILD)/$(FRUITDIR)/%$(OBJ): $(SRC)/$(FRUITDIR)/%$(F95)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) $(FMFLAGS) -c $< -o $@

# build test objects
$(BUILD)/$(TESTSDIR)/%$(OBJ): $(SRC)/$(TESTSDIR)/%$(F95)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) $(FMFLAGS) -c $< -o $@

# build fson objects
$(BUILD)/%$(OBJ): $(SRC)/%$(F95)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) $(FMFLAGS) -c $< -o $@

$(DIST)/%$(EXE) : $(BUILD)/%$(OBJ) $(OBJECTS)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -rf $(BUILD)
	rm -rf *.mod

clobber: clean
	rm -rf $(DIST)
