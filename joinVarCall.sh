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

java -Xmx460g -jar ~/bin/GenomeAnalysisTK.jar -T GenotypeGVCFs -R /uufs/chpc.utah.edu/common/home/u6000989/data/lycaeides/dovetail_melissa_genome/download/HiC_HiCRise_GLtS4/melissa_blue_21Nov2017_GLtS4/mod_melissa_blue_21Nov2017_GLtS4.fasta -hets 0.001 -nt 20 --variant combinded_10.g.vcf --variant combinded_11.g.vcf --variant combinded_12.g.vcf --variant combinded_13.g.vcf --variant combinded_14.g.vcf --variant combinded_15.g.vcf --variant combinded_16.g.vcf --variant combinded_1.g.vcf --variant combinded_2.g.vcf --variant combinded_3.g.vcf --variant combinded_4.g.vcf --variant combinded_5.g.vcf --variant combinded_6.g.vcf --variant combinded_7.g.vcf --variant combinded_8.g.vcf --variant combinded_9.g.vcf -o lycTimeSeriesVariants.vcf
