#!/bin/make -f
QLKNN_SRC:=$(abspath $(dir $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))))
#MAKEFILE_DIRECTORY := $(dir $(CURRENT_MAKEFILE))
#ROOT_DIR=..
#CURRENT_DIRECTORY:=$(shell pwd)
#mkfile_pathz=$(abspath $(lastword $(MAKEFILE_LIST)))
#QLKNN_SRC= $(dir $(mkfile_pathz))
#include $(QLKNN_SRC)/Makefile.inc
# Directory for math routines
QLKNN_LIBSRC?=$(QLKNN)/lib/src
#FUKUSHIMA_DIR?=$(QUALIKIZ_LIBSRC)/fukushima

#FUKUSHIMA?=-L$(FUKUSHIMA_DIR) -lfukushima

#QLKNN_LIBS=$(FUKUSHIMA)

QLKNN_FILES=qlknn_primitives.f90 qlknn_disk_io.f90 qlknn_types.f90
QLKNN_SRCS=$(QLKNN_FILES:%=$(QLKNN_SRC)/%)
QLKNN_OBJS=$(QLKNN_SRCS:%f90=%o)
QLKNN_MODS=$(QLKNN_SRCS:%f90=%mod)

all: qlknn_test

# Qualikiz objects
$(QLKNN_SRC)/qlknn_primitives.o: $(QLKNN_SRC)/qlknn_disk_io.mod $(QLKNN_SRC)/qlknn_types.mod
$(QLKNN_SRC)/qlknn_disk_io.o: $(QLKNN_SRC)/qlknn_types.mod

###############################################################################
qlknn_test: $(QLKNN_SRC)/qlknn_test.f90 $(QLKNN_OBJS)
	#$(FC_PREAMBLE) $(FC_WRAPPER) $(FFLAGS) $(MPI_FLAGS) $(OPENMP_FLAGS) $(QLKNN_OBJS) $(QLKNN_LIBS) qlknn_test.f90 -o $@
	$(FC) $(FFLAGS) $(OPENMP) $(EXTEND) $(IFLAGS) $(QLKNN_OBJS) $(QLKNN_LIBS) -module $(QLKNN_SRC) $< -o $@
	chmod +x $@

#$(QLKNN_OBJS) :%.o:%.f90
	#$(FC) $(OPENMP) $(F90FLAGS) -c $<
	#$(FC_PREAMBLE) $(FC_WRAPPER) $(FFLAGS) $(MPI_FLAGS) $(OPENMP_FLAGS) $(QUALIKIZ_LIBS) -c $<


%.mod %.o: %.f90
	@echo blurgh
$(QLKNN_MODS):%.mod:%.o
	@echo balarg

$(QLKNN_OBJS) :%.o:%.f90
	$(FC) -c $(FFLAGS) $(OPENMP) $(EXTEND) $(IFLAGS) $< -module $(QLKNN_SRC) -o $@

clean:
	rm -f $(QLKNN_SRC)/*.o $(QLKNN_SRC)/*.mod $(QLKNN_SRC)/*__genmod.f90


realclean: clean
	rm -f QuaLiKiz


dump_variables:
	@echo "FC_PREAMBLE=$(FC_PREAMBLE)"
	@echo "FC_WRAPPER=$(FC_WRAPPER)"
	@echo "FFLAGS=$(FFLAGS)"
	@echo "MPI_FLAGS=$(MPI_FLAGS)"
	@echo "OPENMP_FLAGS=$(OPENMP_FLAGS)"
	@echo
	@echo "QLKNN=$(QLKNN)"
	@echo "QLKNN_LIBS=$(QLKNN_LIBS)"
	@echo
	@echo "QLKNN_OBJS=$(QLKNN_SRCS)"
	@echo "QLKNN_OBJS=$(QLKNN_OBJS)"
	@echo "QLKNN_MODS=$(QLKNN_MODS)"
