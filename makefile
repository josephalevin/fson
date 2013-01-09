SRC=src
DIST=dist
BUILD=build

F95=.f95
OBJ=.o
EXE=

LIBTARGET=$(DIST)/fson-lib.so

FC = gfortran
FCFLAGS = -g -fbounds-check
FMFLAGS = -J$(BUILD)
LDFLAGS=

AR = ar
ARFLAGS= r

# "make" builds all
all: lib examples

# List of example programs
EXAMPLES = basic example1
examples: $(patsubst %, $(DIST)/examples/%$(EXE), $(EXAMPLES)) json

JSON = $(shell find src -name '*.json')
json: $(patsubst $(SRC)%, $(DIST)%, $(JSON))

FSON = fson_string_m fson_value_m fson_path_m fson
OBJECTS = $(patsubst %, $(BUILD)/%.o, $(FSON))

$(LIBTARGET) : $(OBJECTS)
	mkdir -p `dirname $@`
	$(AR) $(ARFLAGS) $@ $^

lib: $(LIBTARGET)

$(DIST)/%.json : $(SRC)/%.json
	cp -f $< $@

$(DIST)/%$(EXE) : $(BUILD)/%$(OBJ) $(OBJECTS)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

$(BUILD)/%$(OBJ): $(SRC)/%$(F95)
	mkdir -p `dirname $@`
	$(FC) $(FCFLAGS) $(FMFLAGS) -c $< -o $@

clean:
	rm -rf $(BUILD)

clobber: clean
	rm -rf $(DIST)
