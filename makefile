#commands
print = @echo
mkdir = mkdir
rm    = rm

# directories
src = src
build = build
target = target
VPATH=$(src)

# extensions
obj=.o
f95=.f95

# compiler
fc=gfortran
fc_flags_gfortran=-g -Wall -Wimplicit-interface -fbounds-check -c
fm_flags_gfortran=-J $(build)
fo_flags_gfortran=-o

fc_flags=$(fc_flags_$(fc))
fm_flags=$(fm_flags_$(fc))
fo_flags=$(fo_flags_$(fc))

.DEFAULT :build;

build : fson; 


# fson dependencies
fson_string_m : $(build)/fson_string_m$(obj);
fson_value_m : fson_string_m $(build)/fson_value_m$(obj);
fson_path_m : fson_value_m $(build)/fson_path_m$(obj)
fson : fson_path_m $(build)/fson$(obj);

# compile a fortran 95 source file
$(build)/%$(obj) : $(src)/%$(f95)
	$(print) $@
	$(mkdir) -p $(dir $@)
	$(fc) $(fc_flags) $< $(fm_flags) $(fo_flags) $@

# remove build artifacts
clean :
	$(rm) -rf $(build) 

# remove build and target artifacts
clobber : clean
	$(rm) -rf $(target)
