#!/usr/bin/perl
# for script for dadi IM model

use Parallel::ForkManager;
my $max = 20;
my $pm = Parallel::ForkManager->new($max);

@pops = ("BCR-17","BNP-17","BTB-17","GNP-17","HNV-17","MRF-17","PSP-17","RNV-17","SKI-17","USL-17");
@n = (98, 94, 102, 112, 98, 44, 98, 64, 106, 118);

for($i=0;$i<9;$i++){
	for($j=($i+1);$j<10;$j++){
		$pm->start and next;
		print "python3 im_lowm_dadi3.py $pops[$i] $pops[$j] $n[$i] $n[$j]\n";	
		system "python3 im_lowm_dadi3.py $pops[$i] $pops[$j] $n[$i] $n[$j]\n";	
		#print "python3 im_dadi.py $pops[$i] $pops[$j] $n[$i] $n[$j]\n";	
		#system "python3 im_dadi.py $pops[$i] $pops[$j] $n[$i] $n[$j]\n";	
       		$pm->finish;
	}
}
$pm->wait_all_children;


