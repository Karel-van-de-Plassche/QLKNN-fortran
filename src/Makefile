#!/bin/make -f
QLKNN_SRC?=$(abspath $(dir $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))))
#MAKEFILE_DIRECTORY := $(dir $(CURRENT_MAKEFILE))
#ROOT_DIR=..
#CURRENT_DIRECTORY:=$(shell pwd)
#mkfile_pathz=$(abspath $(lastword $(MAKEFILE_LIST)))
#QLKNN_SRC= $(dir $(mkfile_pathz))
ifndef JSRCPATH
include $(QLKNN_SRC)/Makefile.inc
else
	MODULE_PATH_OPTION=-module
	FFLAGS+=-Mpreprocess
endif
# Directory for math routines
QLKNN_LIBSRC?=$(QLKNNDIR)/lib/src
#FUKUSHIMA_DIR?=$(QUALIKIZ_LIBSRC)/fukushima

#FUKUSHIMA?=-L$(FUKUSHIMA_DIR) -lfukushima

#QLKNN_LIBS=$(FUKUSHIMA)
all: $(QLKNN_OBJS)

QLKNN_NET_FILES = \
	net_efeetg_gb.f90 \
	net_efeitg_gb_div_efiitg_gb.f90 \
	net_efetem_gb.f90 \
	net_efiitg_gb.f90 \
	net_efitem_gb_div_efetem_gb.f90 \
	net_pfeitg_gb_div_efiitg_gb.f90 \
	net_pfetem_gb_div_efetem_gb.f90 \
	net_dfeitg_gb_div_efiitg_gb.f90 \
	net_dfetem_gb_div_efetem_gb.f90 \
	net_vteitg_gb_div_efiitg_gb.f90 \
	net_vtetem_gb_div_efetem_gb.f90 \
	net_vceitg_gb_div_efiitg_gb.f90 \
	net_vcetem_gb_div_efetem_gb.f90 \
	net_dfiitg_gb_div_efiitg_gb.f90 \
	net_dfitem_gb_div_efetem_gb.f90 \
	net_vtiitg_gb_div_efiitg_gb.f90 \
	net_vtitem_gb_div_efetem_gb.f90 \
	net_vciitg_gb_div_efiitg_gb.f90 \
	net_vcitem_gb_div_efetem_gb.f90 \
	net_efeitg_gb_div_efeitg_gb_rot0.f90 \
	net_efetem_gb_div_efetem_gb_rot0.f90 \
	net_efiitg_gb_div_efiitg_gb_rot0.f90 \
	net_efitem_gb_div_efitem_gb_rot0.f90 \
	net_pfeitg_gb_div_pfeitg_gb_rot0.f90 \
	net_pfetem_gb_div_pfetem_gb_rot0.f90 \
	net_dfeitg_gb_div_dfeitg_gb_rot0.f90 \
	net_dfetem_gb_div_dfetem_gb_rot0.f90 \
	net_vteitg_gb_div_vteitg_gb_rot0.f90 \
	net_vtetem_gb_div_vtetem_gb_rot0.f90 \
	net_vceitg_gb_div_vceitg_gb_rot0.f90 \
	net_vcetem_gb_div_vcetem_gb_rot0.f90 \
	net_dfiitg_gb_div_dfiitg_gb_rot0.f90 \
	net_dfitem_gb_div_dfitem_gb_rot0.f90 \
	net_vtiitg_gb_div_vtiitg_gb_rot0.f90 \
	net_vtitem_gb_div_vtitem_gb_rot0.f90 \
	net_vciitg_gb_div_vciitg_gb_rot0.f90 \
	net_vcitem_gb_div_vcitem_gb_rot0.f90 \
	net_gam_leq_gb.f90
QLKNN_NET_SRCS=$(QLKNN_NET_FILES:%=$(QLKNN_SRC)/%)
QLKNN_NET_OBJS=$(QLKNN_NET_SRCS:%f90=%o)
QLKNN_NET_MODS=$(QLKNN_NET_SRCS:%f90=%mod)

QLKNN_FILES=qlknn_primitives.f90 qlknn_disk_io.f90 qlknn_types.f90
QLKNN_SRCS=$(QLKNN_FILES:%=$(QLKNN_SRC)/%)
QLKNN_OBJS=$(QLKNN_SRCS:%f90=%o)
QLKNN_MODS=$(QLKNN_SRCS:%f90=%mod)

# QLKNN objects
#$(QLKNN_SRC)/qlknn_all_nets.o: $(QLKNN_NET_MODS)
$(QLKNN_SRC)/qlknn_primitives.o: $(QLKNN_SRC)/qlknn_types.mod
$(QLKNN_SRC)/qlknn_disk_io.o: $(QLKNN_SRC)/qlknn_types.mod

###############################################################################
qlknn_test: $(QLKNN_SRC)/qlknn_test.f90 $(QLKNN_OBJS) $(QLKNN_MODS)
	$(FC_PREAMBLE) $(FC) $(FFLAGS) $(MPI) $(QLKNN_OBJS) $(QLKNN_LIBS) $(MODULE_PATH_OPTION) $(QLKNN_SRC) $(INTEL_MKL_LINK) $< -o $@
	chmod +x $@

$(QLKNN_MODS) $(QLKNN_NET_MODS):%.mod:%.o

$(QLKNN_OBJS) :%.o:%.f90
	$(FC_PREAMBLE) $(FC) -c $(FFLAGS) $< $(MODULE_PATH_OPTION) $(QLKNN_SRC) $(INTEL_MKL) -o $@

$(QLKNN_NET_OBJS) :%.o:%.f90
	$(FC_PREAMBLE) $(FC) -c $(FFLAGS) $< $(MODULE_PATH_OPTION) $(QLKNN_SRC) -o $@

clean:
	rm -f $(QLKNN_SRC)/*.o $(QLKNN_SRC)/*.mod $(QLKNN_SRC)/*__genmod.f90
	rm -f $(QLKNN_SRC)/qlknn_test

clean_nets:
	rm -f $(QLKNN_SRC)/net_*.o $(QLKNN_SRC)/net_*.mod $(QLKNN_SRC)/net_*__genmod.f90
	rm -f $(QLKNN_SRC)/net_*.f90


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
	@echo "QLKNN_NET_OBJS=$(QLKNN_NET_OBJS)"
	@echo "QLKNN_NET_MODS=$(QLKNN_NET_MODS)"
	@echo "QLKNN_SRCS=$(QLKNN_SRCS)"
	@echo "QLKNN_OBJS=$(QLKNN_OBJS)"
	@echo "QLKNN_MODS=$(QLKNN_MODS)"
