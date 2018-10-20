#!/bin/make -f
QLKNN_SRC:=$(abspath $(dir $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))))
#MAKEFILE_DIRECTORY := $(dir $(CURRENT_MAKEFILE))
#ROOT_DIR=..
#CURRENT_DIRECTORY:=$(shell pwd)
#mkfile_pathz=$(abspath $(lastword $(MAKEFILE_LIST)))
#QLKNN_SRC= $(dir $(mkfile_pathz))
ifndef JSRCPATH
	include $(QLKNN_SRC)/Makefile.inc
endif
# Directory for math routines
QLKNN_LIBSRC?=$(QLKNN)/lib/src
#FUKUSHIMA_DIR?=$(QUALIKIZ_LIBSRC)/fukushima

#FUKUSHIMA?=-L$(FUKUSHIMA_DIR) -lfukushima

#QLKNN_LIBS=$(FUKUSHIMA)
all: qlknn_test

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

# Qualikiz objects
$(QLKNN_SRC)/qlknn_primitives.o: $(QLKNN_SRC)/qlknn_disk_io.mod $(QLKNN_SRC)/qlknn_types.mod $(QLKNN_NET_MODS)
$(QLKNN_SRC)/qlknn_disk_io.o: $(QLKNN_SRC)/qlknn_types.mod

###############################################################################
qlknn_test: $(QLKNN_SRC)/qlknn_test.f90 $(QLKNN_OBJS)
	#$(FC_PREAMBLE) $(FC_WRAPPER) $(FFLAGS) $(MPI_FLAGS) $(OPENMP_FLAGS) $(QLKNN_OBJS) $(QLKNN_LIBS) qlknn_test.f90 -o $@
	$(FC) $(FFLAGS) $(OPENMP) $(EXTEND) $(IFLAGS) $(QLKNN_OBJS) $(QLKNN_LIBS) $(QLKNN_NET_OBJS) $(MODULE_PATH_OPTION) $(QLKNN_SRC) $< -o $@
	chmod +x $@

#$(QLKNN_OBJS) :%.o:%.f90
	#$(FC) $(OPENMP) $(F90FLAGS) -c $<
	#$(FC_PREAMBLE) $(FC_WRAPPER) $(FFLAGS) $(MPI_FLAGS) $(OPENMP_FLAGS) $(QUALIKIZ_LIBS) -c $<


%.mod %.o: %.f90
	@echo blurgh
$(QLKNN_MODS):%.mod:%.o
	@echo balarg
$(QLKNN_NET_MODS):%.mod:%.o
	@echo balargukkel

$(QLKNN_OBJS) :%.o:%.f90
	@echo Make QLK object
	$(FC) -c $(FFLAGS) $(OPENMP) $(EXTEND) $(IFLAGS) $< $(MODULE_PATH_OPTION) $(QLKNN_SRC) -o $@
$(QLKNN_NET_OBJS) :%.o:%.f90
	$(FC) -c $(FFLAGS) $(OPENMP) $(EXTEND) $(IFLAGS) $< $(MODULE_PATH_OPTION) $(QLKNN_SRC) -o $@
clean:
	rm -f $(QLKNN_SRC)/*.o $(QLKNN_SRC)/*.mod $(QLKNN_SRC)/*__genmod.f90
	rm qlknn_test


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
	@echo "QLKNN_OBJS=$(QLKNN_SRCS)"
	@echo "QLKNN_OBJS=$(QLKNN_OBJS)"
	@echo "QLKNN_MODS=$(QLKNN_MODS)"
