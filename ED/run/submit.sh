#!/bin/csh
#$ -pe smp 1
#$ -q long
#$ -N SITE

module load ompi
setenv LD_LIBRARY_PATH /afs/crc.nd.edu/x86_64_linux/h/hdf5/intel/15.0/ompi-1.10.2/build/lib:$LD_LIBRARY_PATH
setenv PATH /afs/crc.nd.edu/x86_64_linux/h/hdf5/intel/15.0/ompi-1.10.2/build/bin:$PATH
module load ompi
setenv PATH /afs/crc.nd.edu/user/a/atrierwe/ED2/atrierwe/Hydro_Nutr_ED2_cp7/build:$PATH

### -q long@@crc_d12chas
#$ -e error.dat 

./../build/ed_2.1-opt -f ED2IN_PV_site1_FALL2018
