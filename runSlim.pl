#!/usr/bin/perl
#
# wrapper script for slim
#
use Parallel::ForkManager;
#my $max = 2;
my $max = 5;
my $pm = Parallel::ForkManager->new($max);


foreach $rep (1..10){
	$ran = int(rand(10000));
	$pm->start and next; ## fork
	#system "slim -s $ran -d \"rep=$rep\" -t -m SLimModPopStr.txt > slim_out_gen_$rep"."\n";
	#system "slim -s $ran -d \"rep=$rep\" -t -m SLimModPopStrNoMig.txt > slim_out_nm_gen_$rep"."\n";
	system "slim -s $ran -d \"rep=$rep\" -t -m SLimModPopStrLowMig.txt > slim_out_lm_gen_$rep"."\n";
	$pm->finish;
}

$pm->wait_all_children;


