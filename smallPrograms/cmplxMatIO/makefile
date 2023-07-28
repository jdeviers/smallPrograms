PROG = $(wildcard prog*.f90)
MODS = $(wildcard mod_*.f90)
OBJS = $(patsubst %.f90,%.o,$(MODS))

FC      = gfortran
FCFLAGS = -fbacktrace -Wall -Wextra

PRGM = test

#
default: $(PRGM)

$(PRGM): $(OBJS)
	$(FC) -o $@ $(PROG) $^ 	$(FCFLAGS)

$(OBJS): %.o: %.f90
	$(FC) -c $<							$(FCFLAGS)

mod_interfaces.o: mod_io.o mod_procedures.o
#
clean:
	rm $(PRGM) $(OBJS)

#
debug:
	@echo "$(PROG)"
	@echo "$(MODS)"
	@echo "$(OBJS)"

.PHONY = default clean debug
