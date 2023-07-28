SRCS = $(wildcard prog_*.f90)

#MODS = mod_precision.f90 mod_procedures.f90 mod_diag.f90
MODS = $(wildcard mod_*.f90)
MODS_OBJS = $(patsubst %.f90,%.o,$(MODS))

FC = gfortran
FCFLAGS = -fbacktrace
LPFLAGS = -llapack -L/usr/lib/x86_64-linux-gnu/lapack/

PROGRAM = diag_nxn


# run 'make' without parameter: execute the command associated with $(PROGRAM) (i.e line 21, which queries line 1, then 17, then 18, then 22)
default : $(PROGRAM)

# Compile all modules
$(MODS_OBJS) : %.o : %.f90
	$(FC) $(FCFLAGS) -c $<

# Link
$(PROGRAM) : $(SRCS) $(MODS_OBJS)
	$(FC) $(FCFLAGS) -o $@ $^ $(LPFLAGS)

# Check variables contain what they're meant to
debug:
	@echo "SRCS = $(SRCS)"
	@echo "OBJS = $(OBJS)"
	@echo "MODS = $(MODS)"
	@echo "MODS_OBJS = $(MODS_OBJS)"
	@echo "PROGRAM = $(PROGRAM)"

# Dependencies
mod_procedures.o : mod_precision.o
mod_diag.o : mod_precision.o mod_procedures.o

clean:
	rm -rf $(PROGRAM) $(MODS_OBJS) $(patsubst %.o,%.mod,$(MODS_OBJS))

.PHONY: debug default clean

# La logique générale:
# lancer 'make debug' fait tourner le bout de code juste sous 'debug'.
# Idem pour 'make clean'.
# 'make' sans argument lance 'default': il fait donc tourner $(PROGRAM), dont l'action est définie ligne 22.
# Ligne 22 dit que l'action associée à $(PROGRAM) dépend des actions associées à $(SRCS) puis $(MODS_OBJS).
# La première n'est qu'une liste, sans action associée. Le contenu de cette liste est ramené ligne 22, pour usage dans l'action associée à $(PROGRAM).
# La seconde est une liste (ligne 5) avec des dépendances et une action associée (lignes 18 et 19.)
# Le programme va donc ligne 18: % itère dans les éléments de la liste. le 3ème terme crée plus ou moins une liste temporaire, où les extensions sont remplacées par .f90.
# Le programme bouge ligne 19: c'est l'action associée à $(MODS_OBJS). $(FC) $(FCFLAGS) -c $< se traduit par gfortran -fbacktrace -c %.f90.
# $ renvoie à la dépendance, i.e le terme à droite de ":", ici à la liste de .f90. < itère parmi les éléments de %.f90 un par un, au de tout dumper en une string comme ferait ^.
# Ainsi, ici $(FC) $(FCFLAGS) -c $< fait tourner ces commandes:
#
# gfortran -fbacktrace -c mod_precision.f90
# gfortran -fbacktrace -c mod_procedures.f90
# gfortran -fbacktrace -c mod_diag.f90
#
# Attention, ces éléments ne sont pas nécessairement placés dans cet ordre dans la liste, mais on a spécifié lignes 34-35 quels modules dépendaient d'autres modules.
# 'make' réordonne, en interne, $(MODS_OBJS) afin de compiler les modules dans l'ordre.
#
# C'est bon pour $(MODS_OBJS). Le programme a éxécuté les actions associées aux dépendences de $(PROGRAM): on peut maintenant éxécuter l'action associée à $(PROGRAM). Le programme avance ligne 23.
# Si $< ou $^ réfère aux dépendances (élément-par-élement ou en bloc), $@ réfère à $(PROGRAM), i.e au côté gauche de ":".
# $(FC) $(FCFLAGS) -o $@ $^ $(LPFLAGS) se traduit donc par gfortran -fbacktrace -o diag_nxn prog_diag_NxN.f90 mod_procedures.o mod_precision.o mod_diag.o -llapack -L/usr/lib/x86_64-linux-gnu/lapack/.
