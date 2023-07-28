SRC = $(wildcard prog_*.f90)

MODS = $(wildcard mod_*.f90)
MODS_OBJS = $(patsubst %.f90,%.o,$(MODS))

FC = gfortran
FCFLAGS = -fbacktrace
LPFLAGS = -llapack -L/usr/lib/x86_64-linux-gnu/lapack/

PROGRAM = dipolar

default : $(PROGRAM)

$(PROGRAM) : $(MODS_OBJS)
			$(FC) -o $@ $(SRC) $^ $(FCFLAGS) $(LPFLAGS)

$(MODS_OBJS) : %.o : %.f90
			$(FC) -c $< $(FCFLAGS)

mod_procedures.o : mod_precision.o

debug : 
	@echo "Program f90 file:  " $(SRC)
	@echo "Modules f90 files: " $(MODS)
	@echo "Objects o files:   " $(MODS_OBJS)
	@echo "Fortran compiler:  " $(FC)
	@echo "Compiler flags:    " $(FCFLAGS)
	@echo "LAPACK comp flags: " $(LPFLAGS)
	@echo "Executable file    " $(PROGRAM)

clean : 
	rm $(PROGRAM) $(MODS_OBJS) $(patsubst %.o,%.mod,$(MODS_OBJS))

.PHONY : default debug clean

