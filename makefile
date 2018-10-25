#Makefile for the JAC cluster QuaLiKiz TCI interface

## Set the environment from the top-level Makefile of JETTO
## --------------------------------------------------------
-include ../../include.mk

## Set the local environment for ITCequ
## ------------------------------------
# Overwrite JETTO pattern rules. Keep the tab after the rule!
%.mod %.o: %.f90
	

-include ../include.mk

include ./src/Makefile

FOBJ=$(QLKNN_OBJS)
PYTHON=python2
QLKNNDIR?=$(abspath .)
QLKNN_LIB=$(QLKNNDIR)/lib
QLKNN_NETWORK_DIR=$(QLKNN_LIB)/QLKNN-networks
QLKNN_TOOLS_DIR=$(QLKNNDIR)/tools

## set PHONY targets
## -----------------
.PHONY: all realclean clean $(MASTER_RULES) $(LIBNAME)

# Rules for libtransport.a (LIBNAME inherited from includes)
# Must build extra_libs first for .mod files
#/home/kplass/jetto/libs/linux/jetto-karel_mpi_64/libtransport.a: $(FOBJ)
#	@echo $(LIBNAME)

all: $(LIBNAME)
$(LIBNAME): $(QLKNN_OBJS)
	ar vr $(LIBNAME) $?

#$(LIBNAME) $(QLKNNDIR)/src/qlknn_primitives.mod: $(FOBJ) $(QLKNNDIR)/src/qlknn_primitives.f90
#	@echo $(LIBNAME)
#	@echo $?
#	@echo $(FOBJ)
#	ar vr $(LIBNAME) $?

networks $(QLKNN_NET_SRCS): $(QLKNN_NET_FILES:net_%.f90=$(abspath $(QLKNN_NETWORK_DIR))/%.json)
	cd $(abspath $(QLKNN_TOOLS_DIR)) && $(PYTHON) -c "from json_nn_to_namelist import convert_all; convert_all('$(abspath $(QLKNN_NETWORK_DIR))', target_dir='$(QLKNN_SRC)', target='source')"

network_namelists: $(QLKNN_NET_FILES:net_%.f90=$(abspath $(QLKNN_NETWORK_DIR))/%.json)
	cd $(abspath $(QLKNN_TOOLS_DIR)) && $(PYTHON) -c "from json_nn_to_namelist import convert_all; convert_all('$(abspath $(QLKNN_NETWORK_DIR))', target_dir='$(QLKNN_SRC)', target='namelist')"



test:
	@echo Building test
	$(MAKE) -C src all
## make 'all' option
## -----------------
#all:
#	($(MAKE) -C ../../ all) || exit $$?

## Set rules passed to top-level Makefile
## --------------------------------------
#$(MASTER_RULES):
#	@($(MAKE) -C ../../ $@) || exit $$?

dump_top_variables:
	@echo LIBNAME=$(LIBNAME)
	@echo PYTHON=$(PYTHON)
	@echo QLKNN_LIB=$(QLKNN_LIB)
	@echo FOBJ=$(FOBJ)
	@echo QLKNN_NET_SRCS=$(QLKNN_NET_SRCS)
	@echo QLKNN_NET_JSON=$(QLKNN_NET_FILES:net_%.f90=$(abspath $(QLKNN_NETWORK_DIR))/%.json)
