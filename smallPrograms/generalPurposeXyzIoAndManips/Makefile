PROG = $(wildcard prog_*.f90)
MODS = $(wildcard mod_*.f90)
OBJS = $(patsubst %.f90,%.o,$(MODS))

FC = gfortran
FF = -O3 -fbacktrace -Wall -Wextra

EXEC = caller.x

default: $(EXEC)
	./$(EXEC)

$(EXEC) : $(OBJS)
	$(FC) $(FF) -o $@ $^ $(PROG)

$(OBJS) : %.o : %.f90
	$(FC) $(FF) -c $<
	
mod_interfaces.o : mod_readWrite.o mod_procedures_sbr.o mod_procedures_fct.o

debug:
	@echo "$(PROG)"
	@echo "$(MODS)"
	@echo "$(OBJS)"

clean:
	rm $(OBJS) $(EXEC)

.PHONY = default debug clean