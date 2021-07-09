#!/usr/bin/perl
#
# then subset of files from a given set


foreach $in13 (@ARGV){
	if ($in13 =~ m/full/){
		$L = 2914;
	}
	else{
		$L = 1248;
	}
	$in13 =~ m/in_Z_([A-Z]+)\-\d+_([A-Z]+_[a-z]+)/;
	$pop = $1;
	$set = $2;
	$in17 = $in13;
	$in17 =~ s/13/17/ or die "failed sub $in13\n";
	system "varne -a $in13 -b $in17 -l $L -t 4 -n 3000 -x 1000 > Ne_Z_"."$set"."_"."$pop"."_13-17.txt\n";
}
