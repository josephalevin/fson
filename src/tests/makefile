LIBS = -L$(HOME)/lib -lfruit -lfson
INCLS = -I$(HOME)/include
FC = gfortran
FFLAGS = -O2
SOURCE = fson_test
TARGET = $(SOURCE)_driver

$(TARGET): $(SOURCE).o fson_test2.o

%: %.o
	$(FC) -o $@ $^ $(LIBS)

%.o: %.f90
	$(FC) $(FFLAGS) $(INCLS) -c $<

clean:
	rm *.o *.mod $(TARGET) $(TARGET).f90
