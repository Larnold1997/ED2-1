#!/bin/csh
#$ -q *@@medvigy
#$ -pe mpi-48 96
#$ -N SITE

module load ompi/3.0.0-intel-18.0
module load hdf5/1.8.19-intel

mpirun -np 96 ./../build/ed_2.1-opt -f ED2IN_PV_site1_regional
