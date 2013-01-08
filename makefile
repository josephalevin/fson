#commands
print = @echo
mkdir = mkdir
rm    = rm

# directories
src = src
build = build
target = target
examples=examples

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

# linker
ld=ld
ld_flags=


# fson dependencies
fson_string_m : $(build)/fson_string_m$(obj);
fson_value_m : fson_string_m $(build)/fson_value_m$(obj);
fson_path_m : fson_value_m $(build)/fson_path_m$(obj)
fson : fson_path_m $(build)/fson$(obj);
example1: fson $(build)/examples/example1$(obj)


# compile a fortran 95 source file
$(build)/%$(obj) : $(src)/%$(f95) | dir_$(dir $@)
	$(print) $@
	$(fc) $(fc_flags) $< $(fm_flags) $(fo_flags) $@

$(target)/example1 : example1 fson
	$(mkdir) -p $(dir $@)
	$(fc) $(fc_flags) $^ $(fm_flags) $(fo_flags) $@

# build directories
dir_% : %
	$(print) hello $@ 	 

# remove build artifacts
clean :
	$(rm) -rf $(build) 

# remove build and target artifacts
clobber : clean
	$(rm) -rf $(target)
