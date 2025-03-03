#!/bin/ksh
#set -xv
######################################################################
#
host=`uname -n`
user=`whoami`
#
## - Define paths
srcdir=`pwd`
datadir=$srcdir/data
casename=`basename $srcdir`
#
## - Name of the executables
    exe1=ocean
    exe2=ice
#
############### User's section #######################################
#
## - Define architecture and coupler 
arch=mac  # training, belenos, nemo_lenovo, mac 
              # kraken, gfortran_openmpi_openmp_linux
	      # pgi_openmpi_openmp_linux, 
	      # pgi20.4_openmpi_openmp_linux (not work with 4.0)
	      # gnu1020_openmpi_openmp_linux (not work with 4.0)
#
# - Define number of processes to run each executable
    #nproc_exe1=4
    #nproc_exe2=4
    nproc_exe1=1
    nproc_exe2=1
#
############### End of user's section ################################
#
# - Define rundir
    rundir=${srcdir}/work_${casename}_${nproc_exe1}_${nproc_exe2}_oa
#
echo '*****************************************************************'
echo '*** '$casename' : '$run
echo ''
echo 'Rundir       :' $rundir
echo 'Architecture :' $arch
echo 'Host         : '$host
echo 'User         : '$user
echo ''
echo $exe1' runs on '$nproc_exe1 'processes'
echo $exe2' runs on '$nproc_exe2 'processes'
echo ''
######################################################################
### 1. Create rundir and copy everything needed
#
\rm -fr $rundir
mkdir -p $rundir
cp -f $datadir/*nc  $rundir/.
cp -f $srcdir/$exe1 $rundir/.
cp -f $srcdir/$exe2 $rundir/.
cp -f $datadir/namcouple_LAG $rundir/namcouple
cd $rundir
######################################################################
### 2. Model execution or batch submission
#
echo 'Executing the model using mpirun'
ulimit -s unlimited
mpirun -np $nproc_exe1 ./$exe1 : -np $nproc_exe2 ./$exe2
echo $casename 'is executed or submitted to queue.'
echo 'Results are found in rundir : '$rundir 
#
######################################################################
