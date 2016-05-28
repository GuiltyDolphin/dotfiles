#!/usr/bin/env perl

use strict;
use warnings;

#######################################################################
#                           Options/Globals                           #
#######################################################################

my $DEBUG = 0;
my $INFO  = 1;
my $DOT_DIR = "$ENV{PWD}/dotfiles";
my $usage = 'Usage: script_files COMMAND ARGS..';

sub dot_file { join '/', ($DOT_DIR, shift); }

sub un_dot { shift =~ s/^$DOT_DIR\/?//r }

sub home { join '/', ($ENV{HOME}, shift); }

#######################################################################
#                                DEBUG                                #
#######################################################################

sub info {
    my $text = shift;
    print "INFO: $text\n" if $INFO;
}

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
#                              Utilities                              #
#######################################################################

# Execute a subroutine in the given directory, returning to the original
# directory on completion.
sub with_directory {
    my ($directory, $sub, %options) = @_;
    my $make_dir = $options{make_dir} // 1;
    if (!-d $directory && $make_dir) {
        # Not using Perl's 'mkdir' as it doesn't work with nested directories.
        `mkdir -p $directory`;
    }
    chomp(my $curr = `pwd`);
    chdir $directory or error("failed to change directory to '$directory'") && return 0;
    my $ret = $sub->();
    chdir $curr;
    return $ret;
}

my $local_bin = "$ENV{HOME}/.local/bin";

sub link_script_local {
    my ($path, $name) = @_;
    link_program($path, "$local_bin/$name");
}

#######################################################################
#                              Commands                               #
#######################################################################

my %software_config = (
    firefox => {
        install => \&install_firefox,
    },
);

#############
#  FireFox  #
#############

my $firefox_dir_url = "https://ftp.mozilla.org/pub/firefox/nightly/latest-mozilla-release-l10n/";
sub get_latest_firefox_tar_url {
    my $install_s = "curl -s $firefox_dir_url | grep " . q{'firefox-\([0-9]\+\.\)\+en-GB\.linux-x86_64\.tar\.bz2' -o | sort | head -n 1};
    my $sub_url = `$install_s`;
    $sub_url =~ /^(.*)\.tar\.bz2$/;
    debug("firefox version found: $1");
    return "$firefox_dir_url$sub_url";
}

my $software_directory = "$ENV{HOME}/software";

sub install_firefox {
    debug('Installing: firefox');
    with_directory $software_directory => sub {
        my $ffurl = get_latest_firefox_tar_url();
        $ffurl =~ /^$firefox_dir_url(.*)$/;
        my $firefile = "$1";
        system("wget $ffurl");
        system("tar xjf $firefile");
        system("rm -r $firefile");
        link_script_local("$software_directory/firefox/firefox", 'firefox');
    }
}

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

sub is_installed {
    my $program = shift;
    return `which $program`;
}

sub install_program {
    my $program = shift;
    if (is_installed($program)) {
        debug("Skipping '$program' (already installed)");
        return;
    }
    info("installing '$program'");
    if (my $config = $software_config{$program}) {
        $config->{install}();
    } else {
        system("apt-get install $program -y");
    }
    $? or info("successfully installed '$program'");
}

while (my $command = shift) {
    if ($command eq '--debug') {
        $DEBUG = 1;
        next;
    } elsif ($command eq 'link') {
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
    last;
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
