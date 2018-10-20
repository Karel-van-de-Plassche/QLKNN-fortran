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

$(LIBNAME): $(QLKNN_OBJS)

#$(LIBNAME) $(QLKNNDIR)/src/qlknn_primitives.mod: $(FOBJ) $(QLKNNDIR)/src/qlknn_primitives.f90
#	@echo $(LIBNAME)
#	@echo $?
#	@echo $(FOBJ)
#	ar vr $(LIBNAME) $?

networks:
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

# Rule for libtransport.a
# -----------------------

# make 'realclean' option
# -----------------------
realclean:
	$(MAKE) -C src realclean

# make 'clean' option
# -------------------
clean:
	$(MAKE) -C src clean

dump_more:
	@echo LIBNAME=$(LIBNAME)
	@echo FOBJ=$(FOBJ)

#%.o %.mod: %.f90
#	@echo hello?
#	$(MAKE) -C $<
#
#%.o %.mod: %.f
#	$(MAKE) -C $<
