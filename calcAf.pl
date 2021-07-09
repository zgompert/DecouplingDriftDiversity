#!/usr/bin/perl
# allele freqs from SLiM vcf output

foreach $file (@ARGV){
	open(IN, $file) or die "failed to read the infile: $file\n";
	open(OUT, "> pest_$file") or die "failed to write: pest_$file\n";
	$pop = 1;
	$gen = 1;
	$pos = 0;
	while(<IN>){
		if(m/PASS/){ ## data line
			chomp;
			@line = split(/\s+/,$_);
			if($line[1] < $pos){ ## new pop
				$pop++;
				if($pop > 36){
					$pop = 1;
					$gen++;
				}
			}
			$pos = $line[1];
			$dat = $line[7];
			$dat =~ m/PO=(\d+)/;
			$po = $1;
			$dat =~ m/GO=(\d+)/;
			$go = $1;
			$dat =~ m/AC=(\d+)/;
			$af = $1/346;
			print OUT "$pos $po $go $pop $gen $af\n";
		}
	}
	close(IN);
	close(OUT);
}

