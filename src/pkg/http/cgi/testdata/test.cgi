#!/usr/bin/perl
# Copyright 2011 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.
#
# Test script run as a child process under cgi_test.go

use strict;
use CGI;
use Cwd;

my $q = CGI->new;
my $params = $q->Vars;

if ($params->{"loc"}) {
    print "Location: $params->{loc}\r\n\r\n";
    exit(0);
}

my $NL = "\r\n";
$NL = "\n" if $params->{mode} eq "NL";

my $p = sub {
  print "$_[0]$NL";
};

# With carriage returns
$p->("Content-Type: text/html");
$p->("X-Test-Header: X-Test-Value");
$p->("");

print "test=Hello CGI\n";

foreach my $k (sort keys %$params) {
  print "param-$k=$params->{$k}\n";
}

foreach my $k (sort keys %ENV) {
  my $clean_env = $ENV{$k};
  $clean_env =~ s/[\n\r]//g;
  print "env-$k=$clean_env\n";
}

# NOTE: don't call getcwd() for windows.
# msys return /c/go/src/... not C:\go\...
my $dir;
if ($^O eq 'MSWin32' || $^O eq 'msys') {
  my $cmd = $ENV{'COMSPEC'} || 'c:\\windows\\system32\\cmd.exe';
  $cmd =~ s!\\!/!g;
  $dir = `$cmd /c cd`;
  chomp $dir;
} else {
  $dir = getcwd();
}
print "cwd=$dir\n";
