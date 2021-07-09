#!/bin/sh
#SBATCH --time=200:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=24
#SBATCH --account=gompert-kp
#SBATCH --partition=gompert-kp
#SBATCH --job-name=gatk
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=zach.gompert@usu.edu

cd /uufs/chpc.utah.edu/common/home/u6000989/data/lycaeides/timeseries/Variants_2018/ 
perl runCombineGVCFs.pl combine[0-9]*sh

