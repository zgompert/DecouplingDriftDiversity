#!/usr/bin/perl
#
# splits a genotype likelihood file by population and year
#
# usage splitPops.pl in.gl
#

my $in = shift (@ARGV);

open(IN, $in) or die "failed to open the infile\n";

## get no. loci, no. inds
$line = <IN>;
chomp($line);
$line =~ m/(\d+)\s+(\d+)/;
$nind = $1;
$nloc = $2;

## get ind. and pop. ids
$line = <IN>;
chomp($line);
@line = split (" ",$line);
foreach $ind (@line){
	$ind =~ m/^lyc\-([A-Z0-9]+\-\d+)/;
	$id = $1;
	print "$id\n";
	push (@id,$id);
	push (@{$popids{$id}},$ind);
	$ids{$id} = 1;
	if(defined $popn{$id}){
		$popn{$id}++;
	}
	else {
		$popn{$id} = 1;
	}
}

## open one file per population
foreach $id (sort keys %ids){
	$fh = "F"."$id";
	$out = "$id"."_$in";
	print "$out\n";
	open ($fh, "> $out") or die "Could not write $id\n";
	$files{$id} = $fh;
	print {$files{$id}} "$popn{$id} $nloc\n";
	$pids = join (" ",@{$popids{$id}});
	print {$files{$id}} "$pids\n";
	@ones = ();
	for($i=0;$i<$popn{$id}; $i++){
		push (@ones,1);
	}
	$ones = join (" ", @ones);
	print {$files{$id}} "$ones\n";
}

## read and write
while (<IN>){
	chomp;
	@line = split (" ",$_);
	$a = shift(@line); ## locus info
	foreach $id (sort keys %ids){
		print {$files{$id}} "$a";
	}
	for ($i=0; $i<$nind; $i++){
		$id = $id[$i];
		for ($j=0; $j<3; $j++){
			$a = shift(@line); 
			print {$files{$id}} " $a";
		}
	}	
	foreach $id (sort keys %ids){
		print {$files{$id}} "\n";
	}
			
}
close (IN);
