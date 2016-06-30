#!/usr/bin/perl

# Report the load for Heatmapper on all nodes, and return the node with the least current
# load for the given Heatmapper app (heatmap type).
#
# David Arndt, May 2016

use CGI;
use Shell;
use LWP::Simple;
use List::Util qw(min);
use strict;

my $heatmap_monitor_port = '8803';
my $node_list_file = '/apps/heatmapper_monitor/project/current/public/node_list.txt';
my @legal_app_names = (
	"expression", "pairwise", "image", "geomap", "geocoordinate"
);

# my $script_start_time = time;


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

my $output_str = '';

open (IN, "<", $node_list_file) or die "Could not open file: $!";

my $current_time = time; # in seconds since start of 1970

# Arrays in which each index corresponds to a node: 0 for first node, 1 for second, etc.
my @curr_exec = (); # Whether nodes are currently executing something for the given app (boolean).
my @app_act_1 = (); # App activities in last 1 minute.
my @app_act_5 = (); # App activities in last 5 minutes.
my @app_act_15 = (); # App activities in last 15 minutes.
my @load_1 = (); # Node loads in last 1 minute.
my @load_5 = (); # Node loads in last 5 minutes.
my @load_15 = (); # Node loads in last 15 minutes.

my $browser = LWP::UserAgent->new;
my @nodes = ();
my $node_idx = 0;
while (<IN>) { # iterate through list of nodes (domain names or IP addresses)
# 	print $_;
	
	if ($_ =~ /^\s*([a-zA-Z0-9\.]+)\s*$/) {
		my $domain = $_;
		chomp($domain);
		$nodes[$node_idx] = $domain;
		
		my $url = 'http://' . $domain . ':' . $heatmap_monitor_port. '/cgi-bin/single_node_report.cgi?app=' . $app;
		$output_str .= $url . "\n";
		
		my $response = $browser->get( $url );
		print "Can't get $url -- ", $response->status_line unless $response->is_success;
		
		if ($response->content =~ /^currently executing\: (\w+)\napp activity\: *(\d+\.\d+), *(\d+\.\d+), *(\d+\.\d+)\nload average\: *(\d+\.\d+), *(\d+\.\d+), *(\d+\.\d+)/) {
			if ($1 eq 'no') {
				$curr_exec[$node_idx] = 0;
			} else {
				$curr_exec[$node_idx] = 1;
			}
			
			$app_act_1[$node_idx] = $2;
			$app_act_5[$node_idx] = $3;
			$app_act_15[$node_idx] = $4;
			$load_1[$node_idx] = $5;
			$load_5[$node_idx] = $6;
			$load_15[$node_idx] = $7;
		}
		
  	$output_str .= $response->content;
  	$output_str .= "\n";
  	
  	$node_idx++;
	}
}
my $num_nodes = $node_idx;

close(IN);


# Find minimum values for the app activity and load metrics (using List::Util):
my $min_app_act_1 = min @app_act_1;
my $min_app_act_5 = min @app_act_5;
my $min_app_act_15 = min @app_act_15;
my $min_load_1 = min @load_1;
my $min_load_5 = min @load_5;
my $min_load_15 = min @load_15;


# Compare node metrics and decide which one the user should be given to use.
#
# 1) Most important: if a node is currently being used for the requested app, avoid it.
# 2) If a node has been used for the requested app recently, avoid it.
# 3) All else being equal, prefer nodes with the lowest recent overall system load.
#
# We can treat each of the 7 metrics as a binary decision point, resulting in 2^7 = 128
# rank levels. If a node does better than all others on a given metric (or ties for first
# place), it gets points for that metric based on its priority in the decision tree. So
# a perfect score will be 127, and the worst possible score is 0. A node that wins in a
# higher-priority metric can never be beaten by nodes that lose in that metric (e.g. a
# node that loses at curr_exec can earn no more than 63 points).
#
# Metric       Points if win
# ----------   -------------
# curr_exec               64
# app_act_1               32
# app_act_5               16
# app_act_15               8
# load_1                   4
# load_5                   2
# load_15                  1

my $score = 0;
my $best_score = -1;
my $best_node_idx = -1;
for ($node_idx = 0; $node_idx < $num_nodes; $node_idx++) {
	if ($curr_exec[$node_idx] == 0) {
		$score += 64;
	}
	
	if ($app_act_1[$node_idx] == $min_app_act_1) {
		$score += 32;
	}
	if ($app_act_5[$node_idx] == $min_app_act_5) {
		$score += 16;
	}
	if ($app_act_15[$node_idx] == $min_app_act_15) {
		$score += 8;
	}
	
	if ($load_1[$node_idx] == $min_load_1) {
		$score += 4;
	}
	if ($load_5[$node_idx] == $min_load_5) {
		$score += 2;
	}
	if ($load_15[$node_idx] == $min_load_15) {
		$score += 1;
	}
	
	if ($score > $best_score) {
		$best_score = $score;
		$best_node_idx = $node_idx;
	}
	
	$score = 0;
}

my $best_domain = $nodes[$best_node_idx];

print "best: $best_domain\n";
print "app: $app\n\n";
print $output_str;

# my $script_end_time = time;
# printf "This script completed in %d seconds.", ($script_end_time - $script_start_time);
