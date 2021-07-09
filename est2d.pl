#!/usr/bin/perl
#
# control script to estimate 2d SFS and Fst with angsd
# autosomes with all inds.
# sex with male only
# use GL1 = samtools gl model and GL2 = GATK gl model

#foreach $set (@ARGV){## give all bam file lists
#	if($set =~ m/all/){
#		$reg = "ChrAuto.filelist";
#	}
#	else{
#		$reg = "ChrZ.filelist";
#	}
#	$set =~ m/^([a-zA-Z0-9\-_]+)\.fileslist/ or print "no match for $set\n";
#	$base = $1;
#	foreach  $gl (1..2){ ## gl models
#		$o = "o_2d_$base"."_GL$gl";
#		system "~/source/angsd/angsd -bam $set -doSaf 1 -GL $gl -rf $reg -P 60 -minMapQ 30 -minQ 20 -anc ~/data/lycaeides/dovetail_melissa_genome/download/HiC_HiCRise_GLtS4/melissa_blue_21Nov2017_GLtS4/mod_melissa_blue_21Nov2017_GLtS4.fasta -out $o\n";
#	}
#}

@pops = ("BCR","BNP","BTB","GNP","HNV","MRF","PSP","RNV","SKI","USL");
@sets = ("all","males");
foreach $gl (1..2){
foreach $set (@sets){
foreach $i (0..8){
	foreach $j (($i+1)..9){
		$f1 = "o_2d_$pops[$i]"."-17_$set"."_GL$gl".".saf.idx";
		$f2 = "o_2d_$pops[$j]"."-17_$set"."_GL$gl".".saf.idx";
		$o1 = "o_2d_$pops[$i]"."X$pops[$j]"."_$set"."_GL$gl".".ml";
		$fst = "o_2d_$pops[$i]"."X$pops[$j]"."_$set"."_GL$gl";
		system "~/source/angsd/misc/realSFS $f1 $f2 -fold 1 -P 60 > $o1\n";
		system "~/source/angsd/misc/realSFS fst index $f1 $f2 -sfs $o1 -fstout $fst\n";
		system "~/source/angsd/misc/realSFS fst stats $fst".".fst.idx > global_fst_$fst\n";
	}
}
}
}

