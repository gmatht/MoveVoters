#!/usr/bin/perl
my $t="";
while (<STDIN>) {
	#if ($_ =~ /TASK\s+([0-9]+)\s+([0-9]+)\s+([0-9]+)/ 
	if ($_ =~ /TASK\s+([0-9].*[0-9])/) {
		if ($t > "") {
			print "$t  > NO NCNE!!!\n";
		}
		$t=$1;
	}
	if ($t > "" && $_ =~ /(.*); Exists NE:(.*)/) {
		$a=$1;$b=$2;
		#$solution=$_;
		$a=~ s/\s+/ /g;
		$b=~ s/\s+/ /g;
		#print "$t\n";
		printf  "%s  %16s   %s\n", $t, $a, $b;
		#print "$t\t> $solution\n";
		$t="";
	}
}
#perl parse_ab.pl < ab.txt
