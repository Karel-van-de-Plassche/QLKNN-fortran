#Makefile for the JAC cluster QuaLiKiz TCI interface

## Set the environment from the top-level Makefile of JETTO
## --------------------------------------------------------
include ./src/Makefile

ifdef JSRCPATH
	include ../../include.mk
endif

## Set the local environment for ITCequ
## ------------------------------------
%.mod %.o: %.f90
	@echo wat

ifdef JSRCPATH
	include ../include.mk
endif

%.mod %.o: %.f90
	@echo wut

FOBJ=$(QLKNN_OBJS)

## set PHONY targets
## -----------------
.PHONY: all realclean clean $(MASTER_RULES) $(LIBNAME)

$(LIBNAME): $(FOBJ)
	@echo $(LIBNAME)
	@echo $?
	ar vr $(LIBNAME) $?
	cp src/*.mod .

networks:
	cd tools && python -c "from json_nn_to_namelist import convert_all; convert_all('../lib/QLKNN-networks', target_dir='../src/')"


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

#%.o %.mod: %.f90
#	@echo hello?
#	$(MAKE) -C $<
#
#%.o %.mod: %.f
#	$(MAKE) -C $<
