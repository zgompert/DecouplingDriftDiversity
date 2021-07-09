#!/usr/bin/perl
#

$L = 5883;

foreach $in13 (@ARGV){
	$in13 =~ m/pruned_in_([A-Z]+)\-\d+_([A-Z]+_[a-z]+)/;
	$pop = $1;
	$set = $2;
	$in17 = $in13;
	$in17 =~ s/13/17/ or die "failed sub $in13\n";
	system "varne -a $in13 -b $in17 -l $L -t 4 -n 3000 -x 1000 > Ne_pruned"."$set"."_"."$pop"."_13-17.txt\n";
}
