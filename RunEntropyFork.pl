#!/usr/bin/perl
#
# run entropy jobs
#

use Parallel::ForkManager;
my $max = 20;
my $pm = Parallel::ForkManager->new($max);

my $odir = "/uufs/chpc.utah.edu/common/home/u6000989/projects/lycaeides_diversity/Entropy/";
my $base = "/uufs/chpc.utah.edu/common/home/u6000989/projects/lycaeides_diversity/Entropy/";

foreach $in (@ARGV){
	$in =~ m/series_([A-Z]+_[a-z]+)/;
	$dat = $1;
	foreach $k (2..3){
		foreach $ch (0..2){
		$pm->start and next; ## fork
		system "entropy -i $in -l 10000 -b 5000 -t 5 -k $k -Q 0 -s 50 -q $base"."ldak$k".".txt -o $odir"."out_$dat"."_k$k"."_ch$ch".".hdf5 -w 0 -m 1\n";
		$pm->finish;
		}
	}
}

$pm->wait_all_children;



