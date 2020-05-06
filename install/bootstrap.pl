#!/usr/bin/env perl

# Bootstrap installer for GultyDolphin's dotfiles

use 5.010001;

use strict;
use warnings;

use Cwd qw(getcwd);
use File::Path qw(make_path);
use File::Temp 0.19;
use version 0.77;

#######################################################################
#                           Options/Globals                           #
#######################################################################

my $DEBUG = 0;
my $INFO  = 1;

my $HOME = $ENV{HOME};

sub home { defined $_[0] ? join '/', ($HOME, $_[0]) : $HOME }

my $PERL_INSTALL_DIR = home('.local');

# Routines should return $OK or $ERROR to indicate whether they passed
# or failed if they interact with the system.
my $OK = 0;
my $ERROR = !$OK;
sub success { $_[0] eq $OK }

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

sub with_temp_directory {
    my ($sub) = @_;
    my $dir = File::Temp->newdir();
    with_directory($dir, $sub);
}

# Execute a subroutine in the given directory, returning to the original
# directory on completion.
sub with_directory {
    my ($directory, $sub) = @_;
    my $curr = getcwd();
    debug("entering directory '$directory'");
    chdir $directory or error("failed to change directory to '$directory'") && return 0;
    my $ret = $sub->();
    debug("leaving directory '$directory'");
    chdir $curr;
    return $ret;
}

sub sequence {
    my @commands = @_;
    foreach my $command (@commands) {
        next if $command eq $OK;
        # error code, for when subs are used instead of strings
        return $? if $command =~ /^[1-9][0-9]{0,2}$/;
        system($command) == 0 or return $?;
    }
    return $OK;
}

# 1 if the return status of the command is 0, 0 otherwise
sub query_ok {
    my ($program, $args) = @_;
    `$program $args 2>/dev/null`;
    return ($? eq $OK) ? 1 : 0;
}

###############################
#### Package Configuration ####
###############################

my %software_config = (
    cpanm   => {
        install   => \&cpanm_install,
        installed => perl_module_installed('App::cpanminus'),
        update    => \&cpanm_update,
        version   => {
            current => perl_module_version_current('App::cpanminus'),
            latest  => perl_module_version_latest('MIYAGAWA', qr/App-cpanminus-(\d+\.\d+)\.meta/),
            compare => \&perl_module_version_compare
        }
    },
    perl_local_lib   => {
        install   => \&perl_local_lib_install,
        installed => perl_module_installed('local::lib'),
        update    => \&perl_local_lib_install,
        version   => {
            current => perl_module_version_current('local::lib'),
            latest  => \&perl_local_lib_version_latest,
            compare => \&perl_module_version_compare
        },
    },
);

sub get_config_generic {
    my ($config_h, @accessors) = @_;
    my $config = $config_h;
    foreach (@accessors) {
        $config = $config->{$_};
        last unless defined $config;
    }
    return $config;
}

sub get_config {
    return get_config_generic(\%software_config, @_);
}

# Args, either: ($program, acc1, acc2, ..., accN)
# or ([args], $program, acc1, acc2, ..., accN)
# $program is passed implicitly as first argument to a default sub.
sub run_config {
    my @accessors = @_;
    my @args;
    if (ref $accessors[0] eq 'ARRAY') {
        @args = @{shift @accessors};
    }
    if (my $custom = get_config(@accessors)) {
        return $custom->(@args);
    }
}

#####################################
# Generic Perl module configuration #
#####################################

# $package_re should match the 'numeric' version in $1
sub version_latest_from_directory_url_multi_package {
    my ($url, $package_re) = @_;
    my $dat = `curl -s $url`;
    my @versions;
    while ($dat =~ /$package_re/g) {
        push @versions, $1;
    }
    return (sort @versions)[$#versions];
}

sub perl_module_installed {
    my ($module) = @_;
    return sub {
        query_ok('perl', "-e 'require $module'");
    }
}

sub perl_module_version_current {
    my ($module) = @_;
    return sub {
        `perl -e 'require $module; print $module->VERSION'`
    }
}

sub perl_module_version_latest {
    my ($user, $package_re) = @_;
    my @letters = split('', $user);
    my $fl = $letters[0];
    my $fsl = join '', @letters[0..1];
    my $base_url = "https://cpan.metacpan.org/authors/id/$fl/$fsl/$user/";
    return sub {
      version_latest_from_directory_url_multi_package($base_url, $package_re);
    }
}

sub perl_module_version_compare {
    my ($current, $latest) = @_;
    return version->parse($current) <=> version->parse($latest);
}

################
#  local::lib  #
################

sub perl_local_lib_version_latest {
    perl_module_version_latest('HAARG', 'local-lib-(\d+\.\d+)\.meta')->()
}

sub perl_local_lib_install {
    my $base_url = 'https://cpan.metacpan.org/authors/id/H/HA/HAARG/';
    my $latest = perl_local_lib_version_latest();
    my $package = "local-lib-$latest";
    my $tar_file = "$package.tar.gz";
    my $local_lib_url = "$base_url/$tar_file";
    with_temp_directory sub {
        debug("Downloading local::lib from '$local_lib_url'...");
        sequence(
            "curl -fsSL $local_lib_url --output $tar_file",
            "tar -xvf $tar_file",
        );
        debug("Building local-lib...");
        with_directory $package => sub {
            sequence(
                "perl Makefile.PL --bootstrap=$PERL_INSTALL_DIR",
                "make test && make install",
            );
        }
    };
}

###############
#  Cpanminus  #
###############

sub cpanm_install {
    system('curl -L https://cpanmin.us | perl - App::cpanminus');
}

sub cpanm_update {
    system('cpanm --self-upgrade');
}

################################
#### Configuration commands ####
################################

sub is_installed {
    my $program = shift;
    return run_config($program, 'installed');
}

sub is_up_to_date {
    my $program = shift;
    my $current = eval { run_config($program, 'version', 'current') };
    info('could not retrieve current version number, assuming up-to-date')
        and return 1 unless defined $current;
    my $latest  = run_config($program, 'version', 'latest');
    info('could not retrieve latest version number, assuming up-to-date')
        and return 1 unless defined $latest;
    debug("comparing version $current (current) to $latest (latest)");
    my $comp = run_config([$current, $latest], $program, 'version', 'compare');
    return ($comp >= 0);
}

sub install_program {
    my $program = shift;
    if (is_installed($program)) {
        debug("'$program' already installed, updating...");
        update_program($program);
        return;
    }
    info("installing '$program'");
    my $ok = success(run_config($program, 'install'));
    $ok and info("successfully installed '$program'");
    error("error encountered while installing '$program'") unless $ok;
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
    my $ok = success(run_config($program, 'update'));
    $ok and info("successfully updated '$program'");
    error("error encountered while updating '$program'") unless $ok;
}

################################
#### Initialize CLI options ####
################################

while (my $command = shift) {
    if ($command eq '--debug') {
        $DEBUG = 1;
        next;
    }
    if ($command =~ /--perl-install-dir=(.+)/) {
        $PERL_INSTALL_DIR = $1;
        next;
    }
    error("Unknown or invalid option '$command'");
}

#######################
#### Bootstrapping ####
#######################

my @required_executables = qw(curl make perl tar);

sub check_required_executables {
    foreach my $exec (@required_executables) {
        debug("Checking for executable for '$exec'");
        my $path = `sh -c 'type -P $exec'`
            or error("Could not find an executable for '$exec', and I don't know how to install it. Would you do the honours?");
        chomp $path;
        debug("Found executable for '$exec' ($path)")
    }
    return $OK;
}

sub bootstrap {
    debug('Executing bootstrapper with the following options: '
              . "debug ($DEBUG), "
              . "perl installation directory ($PERL_INSTALL_DIR)");
    check_required_executables();
    install_program('perl_local_lib');
    install_program('cpanm');
    info("Installer bootstrapped!")
}

bootstrap()
