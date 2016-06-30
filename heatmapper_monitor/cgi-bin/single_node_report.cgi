#!/usr/bin/perl

# Report the load for Heatmapper on the current node.
#
# Computes a 'load' value for the given Heatmapper app on this node, based on its activity
# log, for the past 1, 5, and 15 minutes.
#
# Also outputs results of Unix uptime command, which includes load values for the whole
# system for the past 1, 5, and 15 minutes.
#
# David Arndt, Mar-May 2016

use CGI;
use Shell;
use strict;

my $activity_log_dir = '/apps/heatmapper_monitor/heatmapper_logs';
my $max_bytes_to_scan = 432000; # 7200*60
	# In a test clicking, hovering, and zooming around in Geomap (perhaps the app most prone
	# to generate log activity, I produced an additional ~ 7200 bytes of log activity in 1
	# minute. We need at least the last 15 minutes of log activity, but use 4X just to make
	# sure. We do not want to scan too much of the log file, for efficiency reasons.
my @legal_app_names = (
	"expression", "pairwise", "image", "geomap", "geocoordinate"
);

my $script_start_time = time;


print 'Access-Control-Allow-Origin: *' . "\n"; # List domains that are allowed to make JavaScript requests to this page.
print "Content-type: text/plain\n\n";

my $q = CGI->new;
my $app = $q->param('app'); # Name of Heatmapper app passed in URL
$app = lc($app);
if ($app eq '') {
	print "Please provide app name as parameter in URL.";
	die "No app name given";
} elsif ($app !~ /^[a-z]*$/i) { # Security precaution
	print "Illegal app name\n";
	die "Illegal app name: $app";
} else {
	my $match = 0;
	for (my $i = 0; $i < scalar(@legal_app_names); $i++) {
		if ($legal_app_names[$i] eq $app) {
			$match = 1;
			last;
		}
	}
	if ($match == 0) {
		die "Illegal app name: $app";
	}
}
# print "$app\n";

my $activity_log_file = "$activity_log_dir/${app}_activity.log";
open (IN, "<", $activity_log_file) or die "Could not open file: $!";

seek(IN, 0, 2); # jump to end of file
#print "EOF is at " . tell(IN) . "\n";
my $eof_loc = tell(IN);

if ($eof_loc > $max_bytes_to_scan) {
	seek(IN, -$max_bytes_to_scan, 2); # jump to $max_bytes_to_scan bytes before the end of the file.
	my $line = <IN>; # We probably landed in the middle of a line. Read the rest of it. We will ignore this line.
# 	print ("jump to $max_bytes_to_scan bytes before end of file\n");
# 	print ($line);
} else {
	seek(IN, 0, 0); # jump to beginning of file
# 	print ("start from very beginning\n");
}

my $current_time = time; # in seconds since start of 1970
my $events_1 = 0; # events in past 1 minute
my $events_5 = 0;
my $events_15 = 0;
my $activity = '';

while (<IN>) {
	if ($_ =~ /^(\d+)\.\d+\t.*?\t(.*)/) {
		my $logged_time = $1;
		$activity = $2;
		my $seconds_ago = $current_time - $logged_time;
		if ($seconds_ago < 60) {
			$events_1++;
			$events_5++;
			$events_15++;
		} elsif ($seconds_ago < 5*60) {
			$events_5++;
			$events_15++;
		} elsif ($seconds_ago < 15*60) {
			$events_15++;
		}
	}
}

close(IN);

# If the name of the event logged by the app starts with "begin", it means the app is
# currently busy processing something for the user. 
if ($activity =~ /^begin\b/) {
	print "currently executing: yes\n";
} else {
	print "currently executing: no\n";
}

# divide number of events by number of minutes to get the load
$events_5 /= 5;
$events_15 /= 15;

printf "app activity: %.2f, %.2f, %.2f\n", $events_1, $events_5, $events_15;
my $uptime = uptime();
if ($uptime =~ /(load average:.*)/) {
	print $1 . "\n";
}

print "The above shows app activity and overall machine load for the past 1, 5, and 15 minutes, respectively.\n";

my $script_end_time = time;
printf "This script completed in %d seconds.", ($script_end_time - $script_start_time);
