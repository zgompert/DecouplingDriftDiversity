#!/usr/bin/perl
#
# control script to estimate theta with angsd
# autosomes with all inds.
# sex with male only
# use GL1 = samtools gl model and GL2 = GATK gl model

foreach $set (@ARGV){## give all bam file lists
	if($set =~ m/all/){
		$reg = "ChrAuto.filelist";
	}
	else{
		$reg = "ChrZ.filelist";
	}
	$set =~ m/^([a-zA-Z0-9\-_]+)\.fileslist/ or print "no match for $set\n";
	$base = $1;
	foreach  $gl (1..2){ ## gl models
		$o = "o_$base"."_GL$gl";
		system "~/source/angsd/angsd -bam $set -doSaf 1 -GL $gl -rf $reg -P 24 -minMapQ 30 -minQ 20 -anc ~/data/lycaeides/dovetail_melissa_genome/download/HiC_HiCRise_GLtS4/melissa_blue_21Nov2017_GLtS4/mod_melissa_blue_21Nov2017_GLtS4.fasta -out $o\n";
		system "~/source/angsd/misc/realSFS $o".".saf.idx -fold 1 -P 24 > $o".".sfs\n";
		system "~/source/angsd/misc/realSFS saf2theta $o".".saf.idx -outname $o -sfs $o".".sfs\n";
		system "~/source/angsd/misc/thetaStat do_stat $o".".thetas.idx\n";
	}
}


