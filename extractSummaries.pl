#!/usr/bin/perl
#
# summarize PG files
# write files for thetaW thetaP and Tajima D
#

foreach $pg (@ARGV){
	$pg =~ m/^o_([A-Z]+)\-(\d+)/ or die "failed here: $pg\n";
	$pop = $1;
	$yr = $2;
	open(IN, $pg) or die;
	<IN>; # burn header
	while(<IN>){
		chomp;
		@line = split(/\s+/,$_);
		$chr = $line[1];
		$n = $line[13]; ## effective number of sites
		$tw{$pop}{$yr}{$chr} = $line[3]/$n;
		$tp{$pop}{$yr}{$chr} = $line[4]/$n;
		$TD{$pop}{$yr}{$chr} = $line[8]/$n;
		$lgs{$chr} = 1;
	}
close(IN);
}
	
## theta w
open(OUT,"> thetaW.txt") or die "failed to write\n";
foreach $pop (sort keys %tw){
	foreach $yr (sort keys %{$tw{$pop}}){
		print OUT "$pop $yr";
		foreach $chr (sort keys %lgs){
			if(defined $tw{$pop}{$yr}{$chr}){
				print OUT " $tw{$pop}{$yr}{$chr}";
			}
			else{
				print OUT " NA";
				print "missing $pop $yr $chr\n";
			}
		}
		print OUT "\n";
	}
}
close(OUT);

## theta p
open(OUT,"> thetaPi.txt") or die "failed to write\n";
foreach $pop (sort keys %tp){
	foreach $yr (sort keys %{$tp{$pop}}){
		print OUT "$pop $yr";
		foreach $chr (sort keys %lgs){
			if(defined $tp{$pop}{$yr}{$chr}){
				print OUT " $tp{$pop}{$yr}{$chr}";
			}
			else{
				print OUT " NA";
				print "missing $pop $yr $chr\n";
			}
		}
		print OUT "\n";
	}
}
close(OUT);
