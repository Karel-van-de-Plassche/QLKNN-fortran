#!/bin/make -f
QUALIKIZ=/home/karel/working/QLKNN-fortran
# Underlying compiler
FC=gfortran
# Wrapper/command used to compile with Open MPI
FC_WRAPPER=mpifort
# String of text that will be put in from of the $(FC) compile option
FC_PREAMBLE=export OMPI_FC=$(FC) && 
DEBUG=
FFLAGS_GFORTRAN=$(DEBUG) -O3 -fdefault-double-8 -fdefault-real-8 -ffree-line-length-none -flto -ffast-math -fwhole-program
FFLAGS_IFORT=$(DEBUG) -O2 -real-size 64
FFLAGS=$(FFLAGS_GFORTRAN)
OPENMP_FLAGS=-fopenmp
MPI_FLAGS=-DMPI
MODULE_PATH_OPTION=-L
