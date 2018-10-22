#!/bin/make -f
QUALIKIZ=/home/karel/working/QLKNN-fortran
MKLROOT=/home/karel/intel/mkl
# Underlying compiler
FC=gfortran
# Wrapper/command used to compile with Open MPI
FC_WRAPPER=mpifort
# String of text that will be put in from of the $(FC) compile option
FC_PREAMBLE=export OMPI_FC=$(FC) && 
DEBUG=-g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan

FFLAGS_GFORTRAN=$(DEBUG) -O3 -fdefault-double-8 -fdefault-real-8 -ffree-line-length-none -flto -ffast-math -fwhole-program
FFLAGS_IFORT=$(DEBUG) -O2 -real-size 64
FFLAGS=$(FFLAGS_GFORTRAN)
OPENMP_FLAGS=-fopenmp
MPI_FLAGS=-DMPI
MODULE_PATH_OPTION=-L
INTEL_MKL_LINK=-Wl,--start-group $(MKLROOT)/lib/intel64/libmkl_gf_ilp64.a $(MKLROOT)/lib/intel64/libmkl_sequential.a $(MKLROOT)/lib/intel64/libmkl_core.a -Wl,--end-group -lpthread -lm -ldl
INTEL_MKL=-I${MKLROOT}/include -I${MKLROOT}/include/intel64/ilp64 -m64 -fdefault-integer-8
