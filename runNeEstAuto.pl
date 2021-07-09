#!/usr/bin/perl
#
# then subset of files from a given set


foreach $in13 (@ARGV){
	if ($in13 =~ m/full/){
		$L = 27119;
	}
	else{
		$L = 11638;
	}
	$in13 =~ m/in_auto_([A-Z]+)\-\d+_([A-Z]+_[a-z]+)/;
	$pop = $1;
	$set = $2;
	$in17 = $in13;
	$in17 =~ s/13/17/ or die "failed sub $in13\n";
	system "varne -a $in13 -b $in17 -l $L -t 4 -n 3000 -x 1000 > Ne_auto_"."$set"."_"."$pop"."_13-17.txt\n";
}
