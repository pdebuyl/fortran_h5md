
FC=h5fc
FCFLAGS=-g -Wall
VPATH=..

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

all: create_simple_colloid

create_simple_colloid: h5md_module.o create_simple_colloid.o
	$(FC) $(FCFLAGS) $^ -o $@
