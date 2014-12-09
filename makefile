SRC=src
DIST=dist
BUILD=build
TEST=tests
LIB_DIR=$(HOME)/lib
INCL_DIR=$(HOME)/include
TESTPROG=fson_test_driver

F95=.f90
OBJ=.o
SO=.so
MOD=.mod
EXE=

LIBTARGET=$(DIST)/libfson$(SO)

FC = gfortran
FCFLAGS = -O2
FMFLAGS = -J$(BUILD)
LDFLAGS=
FRUIT=-L$(LIB_DIR) -lfruit
FRUITINCLS=-I$(INCL_DIR)

AR = ar
ARFLAGS= r

# "make" builds all
all: lib examples

# List of example programs
EXAMPLES = basic example1 example2
examples: $(patsubst %, $(DIST)/examples/%$(EXE), $(EXAMPLES)) json

JSON = $(shell find src -name '*.json')
json: $(patsubst $(SRC)%, $(DIST)%, $(JSON))

FSON = fson_string_m fson_value_m fson_path_m fson
OBJECTS = $(patsubst %, $(BUILD)/%$(OBJ), $(FSON))

TESTS = fson_test fson_test2
TESTOBJS = $(patsubst %, $(BUILD)/$(TEST)/%$(OBJ), $(TESTS))

$(LIBTARGET) : $(OBJECTS)
	mkdir -p `dirname $@`
	$(AR) $(ARFLAGS) $@ $^

lib: $(LIBTARGET)

$(DIST)/%.json : $(SRC)/%.json
	mkdir -p $(@D)
	cp -f $< $@

# build test program
$(DIST)/$(TEST)/$(TESTPROG)$(EXE) : $(BUILD)/$(TEST)/$(TESTPROG)$(OBJ) $(TESTOBJS) $(OBJECTS)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) -I$(BUILD) $(FRUIT) -o $@ $^ $(LDFLAGS)

# test dependencies
$(BUILD)/$(TEST)/$(TESTPROG)$(OBJ): $(TESTOBJS)

# build test objects
$(BUILD)/$(TEST)/%$(OBJ): $(SRC)/$(TEST)/%$(F95)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) $(FMFLAGS) $(FRUITINCLS) -c $< -o $@

# build fson objects
$(BUILD)/%$(OBJ): $(SRC)/%$(F95)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) $(FMFLAGS) -c $< -o $@

$(DIST)/%$(EXE) : $(BUILD)/%$(OBJ) $(OBJECTS)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -rf $(BUILD)
	rm -rf *$(MOD)

clobber: clean
	rm -rf $(DIST)

install : 
	cp $(LIBTARGET) $(LIB_DIR)
	cp $(BUILD)/*$(MOD) $(INCL_DIR)
