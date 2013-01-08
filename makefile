F95=.f95
OBJ=.o
EXE=.exe

LIBTARGET=fson-lib.so

FC = gfortran
FCFLAGS = -g -fbounds-check
LDFLAGS=

# List of example programs
EXAMPLES = basic 
FSON = fson_string_m fson_value_m fson_path_m fson
OBJECTS = $(patsubst %, %.o, $(FSON))
$(LIBTARGET):$(OBJECTS)
 
# "make" builds all
all: examples

examples: $(patsubst %, examples/%$(EXE), $(EXAMPLES)) 

lib:$(LIBTARGET) 

%$(EXE): %$(OBJ)
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

#
%$(OBJ): %$(F95)
	$(FC) $(FCFLAGS) -c $< -o $@

