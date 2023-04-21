#!/bin/bash

#-----------------------------------------------------------------------------
#
# Run the chgres_cube consistency tests on WCOSS2.
#
# Set WORK_DIR to a general working location outside the UFS_UTILS directory.
# The exact working directory (OUTDIR) will be WORK_DIR/reg_tests/chgres-cube. 
#
# Set the PROJECT_CODE and QUEUE as appropriate.
#
# Invoke the script with no arguments.  To check the queue, type:
# "qstat -u USERNAME".
#
# The run output will be stored in OUTDIR.  Log output will be placed
# in LOG_FILE??.  Once the suite has completed, a summary is placed
# in SUM_FILE.
#
# A test fails when its output does not match the baseline files as
# determined by the "nccmp" utility.  The baseline files are stored in
# HOMEreg.
#
#-----------------------------------------------------------------------------

set -x

this_dir=$PWD

export compiler="intel"
export CMAKE_OPTS="-DCMAKE_BUILD_TYPE=Debug"

cd ../..
./build_all.sh

source ./sorc/machine-setup.sh > /dev/null 2>&1
module use ./modulefiles
module load build.$target.$compiler
module list

cd $this_dir

export OUTDIR="${WORK_DIR:-/lfs/h2/emc/stmp/$LOGNAME}"
export OUTDIR="${OUTDIR}/reg-tests/chgres-cube"

PROJECT_CODE="${PROJECT_CODE:-GFS-DEV}"
QUEUE="${QUEUE:-dev}"

#-----------------------------------------------------------------------------
# Should not have to change anything below here.  HOMEufs is the root
# directory of your UFS_UTILS clone.  HOMEreg contains the input data
# and baseline data for each test.
#-----------------------------------------------------------------------------

export UPDATE_BASELINE="FALSE"
#export UPDATE_BASELINE="TRUE"

if [ "$UPDATE_BASELINE" = "TRUE" ]; then
  source ../get_hash.sh
fi

export HOMEufs=$this_dir/../..

export HOMEreg=/lfs/h2/emc/nems/noscrub/emc.nems/UFS_UTILS/reg_tests/chgres_cube

LOG_FILE=consistency.debug.log
SUM_FILE=summary.debug.log
rm -f $LOG_FILE* $SUM_FILE

export OMP_STACKSIZE=1024M

export NCCMP=/lfs/h2/emc/global/noscrub/George.Gayno/util/nccmp/nccmp-1.8.5.0/src/nccmp
#export NCCMP=${NCCMP:-nccmp}
rm -fr $OUTDIR

#-----------------------------------------------------------------------------
# Initialize regional C96 using FV3 gaussian nemsio files.
#-----------------------------------------------------------------------------

LOG_FILE=${LOG_FILE}01
export APRUN="mpiexec -n 6 -ppn 6 --cpu-bind core"
TEST1=$(qsub -V -o $LOG_FILE -e $LOG_FILE -q $QUEUE -A $PROJECT_CODE -l walltime=00:05:00 \
        -N c96.regional.debug -l select=1:ncpus=6:ompthreads=1:mem=35GB $PWD/c96.regional.sh)

#-----------------------------------------------------------------------------
# Create summary log.
#-----------------------------------------------------------------------------

qsub -V -o ${LOG_FILE} -e ${LOG_FILE} -q $QUEUE -A $PROJECT_CODE -l walltime=00:01:00 \
        -N chgres_summary.debug -l select=1:ncpus=1:mem=100MB \
        -W depend=afterok:$TEST1 << EOF
#!/bin/bash
cd ${this_dir}
grep -a '<<<' ${LOG_FILE}?? | grep -v echo > $SUM_FILE
EOF

exit 0
