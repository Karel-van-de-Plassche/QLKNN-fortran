#!/bin/make -f
QLKNN?=..
QLKNN_SRC?=$(QLKNN)/src
include $(QLKNN_SRC)/Makefile.inc
# Directory for math routines
QLKNN_LIBSRC?=$(QLKNN)/lib/src
#FUKUSHIMA_DIR?=$(QUALIKIZ_LIBSRC)/fukushima

#FUKUSHIMA?=-L$(FUKUSHIMA_DIR) -lfukushima

#QLKNN_LIBS=$(FUKUSHIMA)

QLKNN_SRCS=qlknn_primitives.f90 qlknn_disk_io.f90
QLKNN_OBJS=$(QLKNN_SRCS:%f90=%o)
QLKNN_MODS=$(QLKNN_SRCS:%f90=%mod)

# Qualikiz objects
qlknn_primitives.mod: qlknn_disk_io.mod

###############################################################################
qlknn_test: qlknn_test.f90 $(QLKNN_OBJS)
	$(FC_PREAMBLE) $(FC_WRAPPER) $(FFLAGS) $(MPI_FLAGS) $(OPENMP_FLAGS) $(QLKNN_OBJS) $(QLKNN_LIBS) qlknn_test.f90 -o $@

$(QLKNN_OBJS) :%.o:%.f90
	$(FC_PREAMBLE) $(FC_WRAPPER) $(FFLAGS) $(MPI_FLAGS) $(OPENMP_FLAGS) $(QUALIKIZ_LIBS) -c $<


$(QLKNN_MODS):%.mod:%.o


clean:
	rm -f *.o *.mod *__genmod.f90


distclean: clean
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
	@echo "QLKNN_OBJS=$(QLKNN_OBJS)"
	@echo "QLKNN_MODS=$(QLKNN_MODS)"
