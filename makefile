SRC=src
DIST=dist
BUILD=build
TESTSDIR=tests
FUITDIR=tests/fruit

F95=.f90
OBJ=.o
EXE=

LIBTARGET=$(DIST)/fson-lib.so
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

# List of tests
TESTS = fruit_driver
tests: $(TESTOBJS) $(patsubst %, $(DIST)/$(TESTSDIR)/%$(EXE), $(TESTS)) testjson

# List of test object
TESTSRC = fson_test
TESTOBJS = $(patsubst %, $(BUILD)/$(TESTSDIR)/%$(OBJ), $(TESTSRC))

# Fruit module for testing
FRUIT = fruit_util fruit
FRUITOBJS = $(patsubst %, $(BUILD)/$(FUITDIR)/%$(OBJ), $(FRUIT))

JSON = $(shell find src -name '*.json')
json: $(patsubst $(SRC)%, $(DIST)%, $(JSON))

TESTJSON = $(shell find src -name '*.json')
testjson: $(patsubst $(SRC)%, $(DIST)%, $(JSON))

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
	cp -f $< $@

$(DIST)/%$(EXE) : $(BUILD)/%$(OBJ) $(OBJECTS)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

# build test program
$(DIST)/$(TESTSDIR)/%$(EXE) : $(TESTOBJS) $(FRUITTARGET) $(BUILD)/$(TESTSDIR)/%$(OBJ) $(LIBTARGET)  
	@echo $@
	@echo $^
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) -I$(BUILD) -o $@ $^ $(LDFLAGS)

# build fson objects
$(BUILD)/%$(OBJ): $(SRC)/%$(F95)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) $(FMFLAGS) -c $< -o $@

# build fruit library
$(BUILD)/$(FRUITDIR)/%$(OBJ): $(SRC)/$(FRUITDIR)/%$(F95)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) $(FMFLAGS) -c $< -o $@

# build test objects
$(BUILD)/$(TESTSDIR)/%$(OBJ): $(SRC)/$(TESTSDIR)/%$(F95)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) $(FMFLAGS) -c $< -o $@

clean:
	rm -rf $(BUILD)

clobber: clean
	rm -rf $(DIST)
