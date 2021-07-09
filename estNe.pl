#!/usr/bin/perl
#

#open(OUT, "> ne_nm_estiamtes.txt") or die "failed to write\n";
#open(OUT, "> ne_lm_estiamtes.txt") or die "failed to write\n";
#open(OUT, "> ne_hm_estiamtes.txt") or die "failed to write\n";
#open(OUT, "> ne2_nm_estiamtes.txt") or die "failed to write\n";
#open(OUT, "> ne2_lm_estiamtes.txt") or die "failed to write\n";
open(OUT, "> ne2_hm_estiamtes.txt") or die "failed to write\n";

foreach $g0 (@ARGV){
	$g1 = $g0;
	$g1 =~ s/g4/g6/ or die "failed sub here: $g0\n";
	#$g1 =~ s/g0/g1/ or die "failed sub here: $g0\n";
	$line =`wc -l $g0`;
	$line =~ m/^(\d+)/;
	$L = $1; ## number of loci
	system "varne -a $g0 -b $g1 -l $L -t 4 -n 173 -x 10 2> out\n";
	#system "varne -a $g0 -b $g1 -l $L -t 2 -n 173 -x 10 2> out\n";
	open(VAR, "out") or die "faeild to read out\n"; 
	while(<VAR>){
		if(m/^hat\S+\s+=\s+(\S+)/){
			$ne = $1;
			$g0 =~ m/in_([a-z]+)_pop_(\d+)g0_rep_(\d+)/;
			print OUT "$1 $2 $3 $ne\n";
		}
	}
	close(VAR);
}
