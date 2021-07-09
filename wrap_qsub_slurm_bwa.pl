#!/usr/bin/perl

### Version 1.0 -- 19 August 2012 -- Zach Gompert
### a perl script to submit many serial slurm jobs to the queue using the sbatch command.  

use warnings;
use strict;

### ------------------ JOB CONFIGURATION ----------------------- 
### You will probably need to modify some of these variables


my $walltime = '8:00:00'; ## Format: hh:mm:ss
# Specify the maximum wall clock time for a single job. The
# wall clock time should take possible queue waiting time into
# account.  Format: hhhh:mm:ss hours:minutes:seconds Be sure to
# specify a reasonable value here.  If the job does not finish by the
# time reached, the job is terminated.

my $nodes = '1'; # Request number of compute nodes per job.
my $ppn = '8'; # Number of tasks per node
my $accnt= 'gompert'; # PI account
my $partition = 'kingspeak'; # Specify compute cluster, options include
		# kingspeak, ember, kingspeak-freecylce, ember-freecycle

my $jobname = 'bwa';
#          Set the name of the job (up to 15 characters, 
#          no blank spaces, start with alphanumeric character)

my $email = 'zach.gompert@usu.edu'; ## e-mail this address if the job fails

## build array of jobs to be run individually (serially) by slurm
my $dir = '/uufs/chpc.utah.edu/common/home/u6000989/data/lycaeides/timeseries/Parsed_2018/';
#my $dir = '/uufs/chpc.utah.edu/common/home/u6000989/data/lycaeides/timeseries/Parsed/';
my @jobarray;
my $aln;
my $samse;
my $ind;
my $genome = "/uufs/chpc.utah.edu/common/home/u6000989/data/lycaeides/dovetail_melissa_genome/download/HiC_HiCRise_GLtS4/melissa_blue_21Nov2017_GLtS4/melissa_blue_21Nov2017_GLtS4.fasta";
my $cd = "cd $dir\n";

foreach my $file (@ARGV){
    if ($file =~ m/(^[A-Z0-9\-_]+)\.fastq/){
        $ind = $1;
    }
    else {
        die "Failed to match $file\n";
    }
    $aln ="bwa aln -n 4 -l 20 -k 2 -t 8 -q 10 -f aln"."$ind".".sai $genome $file\n";
    $samse ="bwa samse -n 1 -r \'\@RG\\tID:lyc-"."$ind\\tPL:ILLUMINA\\tLB:lyc-"."$ind\\tSM:lyc-"."$ind"."\' -f aln"."$ind".".sam $genome aln"."$ind".".sai $file\n";

    push (@jobarray, "$cd"."$aln"."$samse"."");
    print "$cd"."$aln"."$samse";

}

printf "Ready to submit %d jobs to slurm (y/n): ", scalar @jobarray;
my $response = <STDIN>;
chomp $response;
if($response eq 'n'){
    print "Exiting without any slurm submissions.\n";
    exit;
}
else{
    print "Proceeding with slurm submission\n";
}
## -----------------------------------------------------------------




###### ------------------ SLURM CONFIGURATION ---------------
my @directives = "#!/bin/sh -f\n";

push @directives, "#SBATCH --time=$walltime\n";
push @directives, "#SBATCH --nodes=$nodes\n";
push @directives, "#SBATCH --ntasks=$ppn\n";
push @directives, "#SBATCH --account=$accnt\n";
push @directives, "#SBATCH --partition=$partition\n";
push @directives, "#SBATCH --job-name=$jobname\n";
push @directives, "#SBATCH --mail-type=FAIL\n";
push @directives, "#SBATCH --mail-user=$email\n";


my $directives = join '', @directives;

##########################################
#                                        #
#   Output some useful job information.  #
#                                        #
##########################################

my @slurmjob = ();
push @slurmjob, '
    echo ------------------------------------------------------
    echo -n \'Job is running on node \'; cat $SLURM_JOB_NODELIST
    echo ------------------------------------------------------
    echo SLURM: job identifier is $SLURM_JOBID
    echo SLURM: job name is $SLURM_JOB_NAME
    echo ------------------------------------------------------

';

##############################################################
#                                                            #
#   The prolog script loads necessary modules                #
#                                                            #
##############################################################

push @slurmjob, "module load bwa\n";
## add other needed modules below


############################################################
#                                                          #
#    Execute the run.  Do not run in the background.       #
#                                                          #
############################################################

## gather everything up to this point, use join with ""
my $prolog = join '', @slurmjob;
@slurmjob = ();

## this is where we would put if this were not in a perl wrapper:
## program_executable < input_file > output_file

#####################################################################

push @slurmjob, '
early()
{
 echo \' \'
 echo \' ############ WARNING:  EARLY TERMINATION ############# \'
 echo \' \'
}
';

##################################################
#                                                #
#   Staging in, running the job, and staging out #
#   were specified above as functions.  Now      #
#   call these functions to perform the actual   #
#   file transfers and program execution.        #
#                                                #
##################################################

my $epilog = join '', @slurmjob;

###### use loop to submit whole @jobarray ########------------
foreach my $job (0..($#jobarray - 1)){
  runserialjob($job);
}
## final job
runserialjob($#jobarray);


#### -------------------------------------------------------------------
sub runserialjob{
    my $j = $_[0];
    my $slurmjob = '';
    $slurmjob .= $directives;
    $slurmjob .= $prolog;
    $slurmjob .= $jobarray[$j];
    $slurmjob .= $epilog;
    $slurmjob .= "exit\n";
    open QSUB, "> sub.sh" or die "Failed to write";
    printf QSUB "$slurmjob";
    close(QSUB) or die "Couldn't close QSUB";
    system "sbatch sub.sh\n";
}

