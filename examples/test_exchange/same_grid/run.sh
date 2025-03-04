#!/bin/bash

mkdir -p work

srcdir=`pwd`
datadir=$srcdir/../common_data
casename=`basename $srcdir`

exe1=sender-apple
exe2=receiver

n1=4
n2=1

make || exit

rundir=$srcdir/work

rm -fr $rundir
mkdir -p $rundir

ln -sf $srcdir/$exe1 $rundir/.
ln -sf $srcdir/$exe2 $rundir/.

ln -sf $datadir/grids.nc $rundir/.
ln -sf $datadir/areas.nc $rundir/.
ln -sf $datadir/masks_nogt_scrip.nc $rundir/masks.nc

cp namcouple $rundir

cd $rundir

echo "launching mpiexec -n $n1 ./$exe1 : -n $n2 ./$exe2"
mpiexec -n $n1 ./$exe1 : -n $n2 ./$exe2
