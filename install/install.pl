#!/usr/bin/env perl

use strict;
use warnings;

#######################################################################
#                           Options/Globals                           #
#######################################################################

my $DEBUG = 0;
my $DOT_DIR = "$ENV{PWD}/dotfiles";
my $usage = 'Usage: script_files COMMAND ARGS..';

my $command = shift;

sub dot_file { join '/', ($DOT_DIR, shift); }

sub un_dot { shift =~ s/^$DOT_DIR\/?//r }

sub home { join '/', ($ENV{HOME}, shift); }

#######################################################################
#                                DEBUG                                #
#######################################################################

sub debug {
    my $text = shift;
    print "DEBUG: $text\n" if $DEBUG;
}

sub error {
    my ($text, %options) = @_;
    if ($options{critical} // 1) {
        die "ERROR: $text\n";
    }
    warn "ERROR: $text\n";
}

#######################################################################
#                              Commands                               #
#######################################################################

sub link_program {
    my ($source, $target) = @_;
    do {
        debug("Skipping @{[un_dot($source)]} (already linked)");
        return;
    } unless check_link($source, $target);
    debug("Linking $source to $target");
    system("ln -s $source $target");
}

sub link_contents {
    my ($search_dir, $target_dir) = @_;
    foreach my $file (glob "$search_dir/*") {
        chomp (my $basename = `basename $file`);
        link_program($file, "$target_dir/$basename");
    }
}

sub install_program {
    my $program = shift;
    unless (`which $program`) {
        debug("Installing: $program");
        system("apt-get install $program -y");
    }
}

if ($command eq 'link') {
    my $source = dot_file(shift);
    my $target = home(shift);
    link_program($source, $target);
} elsif ($command eq 'install') {
    install_program(shift);
} elsif ($command eq 'link_contents') {
    my $search_dir = dot_file(shift);
    my $target_dir = home(shift);
    link_contents($search_dir, $target_dir);
}

sub check_link {
    my ($source, $target) = @_;
    my $link = `readlink -n $target`;
    if (-e $target && $link ne $source) {
        error("$target exists but does not match $source, "
            . 'skipping', critical => 0);
        return 0;
    } elsif ($link eq $source) {
        return 0;
    } else {
        return 1;
    }
}
