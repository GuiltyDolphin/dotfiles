#!/usr/bin/env perl

use strict;
use warnings;

use Cwd qw(abs_path getcwd);

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
    my $curr = getcwd();
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

sub sequence {
    my @commands = @_;
    foreach my $command (@commands) {
        system($command) == 0 or return $?;
    }
    return 0;
}

#######################################################################
#                              Commands                               #
#######################################################################

my %software_config = (
    firefox => {
        install => \&firefox_install,
        update  => \&firefox_update,
        version => {
            current => \&firefox_version_current,
            latest  => \&firefox_version_latest,
        },
    },
);

sub get_config {
    my ($program, @accessors) = @_;
    my $config = $software_config{$program};
    foreach (@accessors) {
        $config = $config->{$_};
        last unless defined $config;
    }
    return $config;
}

sub run_config_or_default {
    my ($program, @accessors) = @_;
    if (my $custom = get_config($program, @accessors)) {
        return $custom->();
    }
    my $default = \&{'default_' . join '_', @accessors};
    return $default->($program);
}

sub default_version_current {
    my $program = shift;
    chomp (my $version = `apt version $program`);
    return $version;
}

sub default_version_latest {
    my $program = shift;
    my $info = `apt-cache show $program`;
    $info =~ /^Version: (.+)$/m;
    return $1;
}

sub is_up_to_date {
    my $program = shift;
    my $current = run_config_or_default($program, 'version', 'current');
    my $latest  = run_config_or_default($program, 'version', 'latest');
    debug("comparing version $current (current) to $latest (latest)");
    return !system("dpkg --compare-versions $current ge $latest");
}

#############
#  FireFox  #
#############

my $firefox_dir_url = "https://ftp.mozilla.org/pub/firefox/nightly/latest-mozilla-release-l10n/";

sub firefox_version_current {
    chomp (my $version = `firefox --version`);
    $version =~ s/^Mozilla Firefox //;
    return $version;
}

my %cache;

sub firefox_version_latest {
    my $install_s = "curl -s $firefox_dir_url | grep " . q{'firefox-\([0-9]\+\.\)\+en-GB\.linux-x86_64\.tar\.bz2' -o | sort | head -n 1};
    my $sub_url = `$install_s`;
    $cache{firefox}->{sub_url} = $sub_url;
    $sub_url =~ /^firefox-((?:[0-9]+\.?)+)\./;
    return $1;
}

sub firefox_update {
    system("rm $local_bin/firefox");
    firefox_install();
}

sub get_latest_firefox_tar_url {
    my $install_s = "curl -s $firefox_dir_url | grep " . q{'firefox-\([0-9]\+\.\)\+en-GB\.linux-x86_64\.tar\.bz2' -o | sort | head -n 1};
    my $sub_url = $cache{firefox}->{sub_url} // `$install_s`;
    return "$firefox_dir_url$sub_url";
}

my $software_directory = "$ENV{HOME}/software";

sub firefox_install {
    with_directory $software_directory => sub {
        my $ffurl = get_latest_firefox_tar_url();
        $ffurl =~ /^$firefox_dir_url(.*)$/;
        my $firefile = abs_path("$1");
        sequence(
            "wget $ffurl",
            "tar xjf $firefile",
            "rm $firefile*",
        ) and return $?;
        link_script_local(abs_path('firefox/firefox'), 'firefox');
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

sub default_install {
    my $program = shift;
    return system("apt-get install $program -y");
}

sub install_program {
    my $program = shift;
    if (is_installed($program)) {
        debug("Skipping '$program' (already installed)");
        return;
    }
    info("installing '$program'");
    my $ret = run_config_or_default($program, 'install');
    $ret or info("successfully installed '$program'");
}

sub default_update {
    my $program = shift;
    return system("apt-get install $program -y");
}

sub update_program {
    my $program = shift;
    info("updating '$program'");
    unless (is_installed($program)) {
        error("cannot update '$program' (not installed)");
        return;
    }
    if (is_up_to_date($program)) {
        info("skipping $program (up-to-date)");
        return;
    }
    my $ret = run_config_or_default($program, 'update');
    $ret or info("successfully updated '$program'");
}

my @tmp_argv;
while (my $command = shift) {
    if ($command eq '--debug') {
        $DEBUG = 1;
        next;
    }
    push @tmp_argv, $command;
}

@ARGV = @tmp_argv;

while (my $command = shift) {
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
    } elsif ($command eq 'update') {
        update_program(shift);
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
