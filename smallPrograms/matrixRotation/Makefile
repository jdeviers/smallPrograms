PROG = $(wildcard prog_*.f90)

MODS = $(wildcard mod_*.f90)
MODS_OBJS = $(patsubst %.f90,%.o,$(MODS))

FC = gfortran
FCFLAGS = -fbacktrace

PROGRAM = rotmat

default : $(PROGRAM)

$(PROGRAM) : $(MODS_OBJS)
			$(FC) -o $@ $(PROG) $^

$(MODS_OBJS) : %.o : %.f90
			$(FC) -c $< $(FCFLAGS)

# Dependencies:
mod_procedures.o : mod_precision.o

clean :
			rm $(PROGRAM) $(MODS_OBJS) $(patsubst %.f90,%.mod,$(MODS))

debug :
			@echo $(PROG)
			@echo $(MODS)
			@echo $(MODS_OBJS)
			@echo $(FC)
			@echo $(FCFLAGS)
			@echo $(PROGRAM)

.PHONY : default clean
