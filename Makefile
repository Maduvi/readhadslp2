# This will help you compile everything here.
FC      = gfortran
CPPFLAGS = -I/usr/lib64/gfortran/modules
LDFLAGS  = -L/usr/lib64 -lnetcdff
FFLAGS  = -g
COMPILE  = $(FC) $(FFLAGS) $(CPPFLAGS) -c
LINK     = $(FC) $(LDFLAGS) -o
OBJECT  = gridmod.o calmod.o readermod.o ncmod.o main.o

all: main

main: $(OBJECT)
	$(LINK) bin/main.exe $(OBJECT)

%.o: src/%.f90
	$(COMPILE) $<

clean-all: clean
	rm -f bin/*.exe

clean:
	rm -f *.o *.mod *.mod0
