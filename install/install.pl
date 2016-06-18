#!/usr/bin/env perl

use strict;
use warnings;

use Cwd qw(abs_path getcwd);
use File::Temp qw(tempdir);

#######################################################################
#                           Options/Globals                           #
#######################################################################

my $DEBUG = 0;
my $INFO  = 1;
my $DOT_DIR = do { my $pwd = getcwd(); "$pwd/dotfiles" };
my $usage = 'Usage: script_files COMMAND ARGS..';

my $user_distro;

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

# Intended to be useful output data.
sub data {
    print "$_[0]\n";
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

sub with_temp_dir (&) {
    my $to_execute = shift;
    my $dir = tempdir(CLEANUP => 1);
    with_directory $dir => $to_execute;
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

sub q_version {
    my $program = shift;
    return sub {
        return `$program --version 2>/dev/null`;
    }
}

my %distro_map = (
    'arch'      => 'arch',
    'debian'    => 'debian',
    'generic'   => 'debian',
    'linuxmint' => 'debian',
);

my $default_distro = 'debian';

sub get_distribution {
    chomp (my $kernel_release = `uname -r`);
    my $distro;
    unless ($kernel_release) {
        error('unable to detect distribution, defaulting to debian system');
        $distro = 'debian';
    } else {
        $kernel_release =~ /^.+?(\w+)$/;
        $distro = $1;
    }
    unless ($distro = $distro_map{lc $distro}) {
        error("no custom configuration for $distro - defaulting to $default_distro");
        $distro = $distro_map{lc $distro} // $default_distro;
    }
    return $distro;
}

#######################################################################
#                     Distribution Configuration                      #
#######################################################################

my %distro_config = (
    arch => {
        install   => \&distro_arch_install,
        installed => \&distro_arch_installed,
    },
    debian => {
        install   => \&distro_debian_install,
        installed => \&distro_debian_installed,
        update    => \&distro_debian_update,
        version   => {
            compare => \&distro_debian_version_compare,
            current => \&distro_debian_version_current,
            latest  => \&distro_debian_version_latest,
        },
    },
);

##########
#  Arch  #
##########

sub distro_arch_install {
    my $program = shift;
    return system("pacman --noconfirm -S $program");
}

sub distro_arch_installed {
    my $program = shift;
    return system("pacman -Q $program &>/dev/null") == 0;
}

############
#  Debian  #
############

sub distro_debian_install {
    my $program = shift;
    return system("apt-get install $program -y");
}

sub distro_debian_installed {
    my $program = shift;
    my $status = `dpkg -s $program 2>/dev/null`;
    return 1 if $status =~ /^Status: install .+? installed$/m;
    return 0;
}

sub distro_debian_update {
    my $program = shift;
    return system("apt-get install $program -y");
}

sub distro_debian_version_current {
    my $program = shift;
    chomp (my $version = `apt version $program`);
    return $version;
}

sub distro_debian_version_latest {
    my $program = shift;
    my $info = `apt-cache show $program`;
    $info =~ /^Version: (.+)$/m;
    return $1;
}

sub distro_debian_version_compare {
    my (undef, $current, $latest) = @_;
    my $comp = 'dpkg --compare-versions';
    return -1 if !system("$comp $current lt $latest");
    return 1  if !system("$comp $current gt $latest");
    return 0  if !system("$comp $current eq $latest");
    return;
}

# $version_re should match the 'numeric' version in $1
sub version_latest_from_directory_url {
    my ($url, $version_re) = @_;
    my $dat = `curl -s $url`;
    my @versions;
    while ($dat =~ /$version_re/g) {
        push @versions, $1;
    }
    return (sort @versions)[$#versions];
}


#######################################################################
#                              Commands                               #
#######################################################################

my %software_config = (
    cpanm   => {
        install   => \&cpanm_install,
        installed => q_version('cpanm'),
    },
    emacs   => {
        install   => \&emacs_install,
        installed => q_version('emacs'),
        update    => \&emacs_update,
        version   => {
            current => \&emacs_version_current,
            latest  => \&emacs_version_latest,
        },
    },
    firefox => {
        install   => \&firefox_install,
        installed => q_version('firefox'),
        update    => \&firefox_update,
        version   => {
            current => \&firefox_version_current,
            latest  => \&firefox_version_latest,
        },
    },
    pip => {
        install   => \&pip_install,
        installed => q_version('pip'),
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

sub get_config_distro {
    return get_config_generic(\%distro_config, @_);
}

# Args, either: ($program, acc1, acc2, ..., accN)
# or ([args], $program, acc1, acc2, ..., accN)
# $program is passed implicitly as first argument to a default sub.
sub run_config_or_default {
    my @accessors = @_;
    my @args;
    if (ref $accessors[0] eq 'ARRAY') {
        @args = @{shift @accessors};
    }
    if (my $custom = get_config(@accessors)) {
        return $custom->(@args);
    }
    $user_distro //= get_distribution();
    my $default = get_config_distro($user_distro, @accessors[1..$#accessors]);
    unless ($default) {
        error('no default implementation of ' . join('_', @accessors[1..$#accessors]) . " for '$user_distro'");
        return 1;
    }
    return $default->($accessors[0], @args);
}

sub is_up_to_date {
    my $program = shift;
    my $current = run_config_or_default($program, 'version', 'current');
    my $latest  = run_config_or_default($program, 'version', 'latest');
    debug("comparing version $current (current) to $latest (latest)");
    my $comp = run_config_or_default([$current, $latest], $program, 'version', 'compare');
    return ($comp >= 0);
}

###############
#  Cpanminus  #
###############

sub cpanm_install {
    system('curl -L https://cpanmin.us | perl - App::cpanminus');
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

sub firefox_version_latest {
    return version_latest_from_directory_url(
        $firefox_dir_url,
        qr/firefox-((?:[0-9]+\.?)+)\.en-GB\.linux-x86_64\.tar\.bz2/o,
    );
}

sub firefox_update {
    system("rm $local_bin/firefox");
    firefox_install();
}

sub get_latest_firefox_tar_url {
    my $latest = firefox_version_latest();
    my $tar = "firefox-$latest.en-GB.linux-x86_64.tar.bz2";
    return "$firefox_dir_url$tar";
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

###########
#  Emacs  #
###########

my $emacs_dir_url = 'https://ftp.gnu.org/gnu/emacs/';

sub emacs_install {
    my $version = emacs_version_latest();
    my $emacs = "emacs-$version";
    my $tar = "$emacs.tar.gz";
    my $emacs_url = "$emacs_dir_url/$tar";
    with_directory $software_directory => sub {
        sequence(
            "wget $emacs_url",
            "tar -xvf $tar",
            "cd $emacs && " .
            "./configure --prefix=$software_directory/emacs --bindir=$local_bin " .
            '&& make && src/emacs -Q && make install',
            "rm $tar",
        );
    };
}

sub emacs_version_current {
    my $version = `emacs --version`;
    $version =~ /^GNU Emacs (.+)$/m;
    return $1;
}

sub emacs_version_latest {
    return version_latest_from_directory_url(
        $emacs_dir_url,
        qr/emacs-([-.\d]+)\.tar\.gz/o
    );
}

sub emacs_update {
    emacs_install();
}

# Pip

sub pip_install {
    with_temp_dir {
        sequence(
            'wget https://bootstrap.pypa.io/get-pip.py',
            'python2.7 get-pip.py --user',
            'python get-pip.py --user',
        );
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
    return run_config_or_default($program, 'installed');
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
    error("error encountered while installing '$program'") if $ret;
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
    error("error encountered while updating '$program'") if $ret;
}

sub version {
    my ($type, $program) = @_;
    debug("fetching $type version for '$program'");
    my $version = run_config_or_default($program, 'version', $type);
    data($version);
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
    } elsif ($command eq 'version') {
        version(shift, shift);
    } else {
        print "$usage\n" and die 1;
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
