#!/usr/bin/perl
# fix scaffold names

use Parallel::ForkManager;
my $max = 10;
my $pm = Parallel::ForkManager->new($max);

open(IN,"jobs.txt") or die;
while(<IN>){
	push(@jobs,$_);
}

FILES:
foreach $job (@jobs){
        $pm->start and next FILES; ## fork
	print "JOB=$job\n\n";
        system "$job\n";
	$pm->finish;
}

$pm->wait_all_children;

