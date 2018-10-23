#Makefile for the JAC cluster QuaLiKiz TCI interface

## Set the environment from the top-level Makefile of JETTO
## --------------------------------------------------------
-include ../../include.mk

## Set the local environment for ITCequ
## ------------------------------------
%.mod %.o: %.f90
	@echo wat

-include ../include.mk

include ./src/Makefile


%.mod %.o: %.f90
	@echo wut

FOBJ=$(QLKNN_OBJS)
PYTHON3=python3

## set PHONY targets
## -----------------
.PHONY: all realclean clean $(MASTER_RULES) $(LIBNAME)

# Rules for libtransport.a (LIBNAME inherited from includes)
# Must build extra_libs first for .mod files
#/home/kplass/jetto/libs/linux/jetto-karel_mpi_64/libtransport.a: $(FOBJ)
#	@echo $(LIBNAME)

$(LIBNAME) all: $(QLKNN_OBJS) networks

#$(LIBNAME) $(QLKNNDIR)/src/qlknn_primitives.mod: $(FOBJ) $(QLKNNDIR)/src/qlknn_primitives.f90
#	@echo $(LIBNAME)
#	@echo $?
#	@echo $(FOBJ)
#	ar vr $(LIBNAME) $?

networks $(QLKNN_NET_SRCS):
	@echo $(PYTHON3)
	cd tools && $(PYTHON3) -c "from json_nn_to_namelist import convert_all; convert_all('../lib/QLKNN-networks', target_dir='../src/')"


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
	@echo FOBJ=$(FOBJ)
