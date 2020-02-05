#!/usr/bin/env perl

use strict;
use warnings;

use Cwd qw(abs_path getcwd);
use File::Basename;
use File::Path qw(make_path);
use File::Temp qw(tempdir);
use IPC::Cmd qw(run);

#######################################################################
#                           Options/Globals                           #
#######################################################################

my $DEBUG = 0;
my $INFO  = 1;
my $DOT_DIR = do { my $pwd = getcwd(); "$pwd/dotfiles" };
my $usage = 'Usage: script_files COMMAND ARGS..';

# Routines should return $OK or $ERROR to indicate whether they passed
# or failed if they interact with the system.
my $OK = 0;
my $ERROR = !$OK;
sub success { $_[0] eq $OK }

my $user_distro;

my $dotted_version_re = qr/(?:[0-9]+\.?)+/;

my $semver_simple_re = qr/(?:(?:[0-9]+\.){2}(?:[0-9]+))/;

sub dot_file { join '/', ($DOT_DIR, shift); }

sub un_dot { shift =~ s/^$DOT_DIR\/?//r }

my $HOME = $ENV{HOME};

sub home { defined $_[0] ? join '/', ($HOME, $_[0]) : $HOME }

my $software_directory = home('software');

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

sub is_error {
    my ($err) = @_;
    $err =~ /^[1-9][0-9]{0,2}$/ ? 1 : 0;
}

# Execute a subroutine in the given directory, returning to the original
# directory on completion.
sub with_directory {
    my ($directory, $sub, %options) = @_;
    my $make_dir = $options{make_dir} // 1;
    if (!-d $directory && $make_dir) {
        make_path($directory);
    }
    my $curr = getcwd();
    debug("entering directory '$directory'");
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

my $local_bin = home('.local/bin');

sub link_script_local {
    my ($path, $name) = @_;
    link_program($path, "$local_bin/$name");
}

sub sequence {
    my @commands = @_;
    foreach my $command (@commands) {
        next if $command eq $OK;
        return $? if is_error($command);
        system($command) == 0 or return $?;
    }
    return $OK;
}

sub q_version {
    my $program = shift;
    return sub {
        chomp (my $version = `$program --version 2>/dev/null`);
        return $version;
    }
}

sub q_which {
    my $program = shift;
    return sub {
        `which $program 2>/dev/null`;
        return !$?;
    }
}

sub query_noerr {
    my ($program, $args) = @_;
    return sub {
        `$program $args 2>/dev/null`;
    };
}

my %distro_map = (
    'arch'      => 'arch',
    'debian'    => 'debian',
    'generic'   => 'debian',
    'linuxmint' => 'debian',
);

my $default_distro = 'arch';

sub get_distribution {
    chomp (my $kernel_release = `uname -r`);
    my $distro;
    unless ($kernel_release) {
        error('unable to detect distribution, defaulting to debian system');
        $distro = 'debian';
    } else {
        $kernel_release =~ /^.+?([[:alpha:]]+).*?$/;
        $distro = $1;
    }
    unless ($distro = $distro_map{lc $distro}) {
        error("no custom configuration for $distro - defaulting to $default_distro");
        $distro = $distro_map{lc $distro} // $default_distro;
    }
    return $distro;
}

my @local_bins = (
    home('.local/bin'),
    home('bin'),
);

sub is_local_bin {
    my $bin_path = shift or return;
    my $re = qr/^(?:@{[join '|', map { quotemeta $_ } @local_bins]})/o;
    return $bin_path =~ $re;
}

sub get_bin_path {
    my $program = shift;
    chomp (my $bin = `which $program &2>/dev/null`);
    return $bin;
}

# Install to user installs with pip
sub install_via_pip {
    my ($program, %options) = @_;
    my $version = $options{pip_version} // '';
    my $command = "pip$version install --user ";
    $command .= '-I ' if $options{ignore_installed};
    $command .= $program;
    system($command);
}

#######################################################################
#                     Distribution Configuration                      #
#######################################################################

my %distro_config = (
    arch => {
        install   => \&distro_arch_install,
        installed => \&distro_arch_installed,
        version   => {
            compare => \&distro_arch_version_compare,
            current => \&distro_arch_version_current,
            latest  => \&distro_arch_version_latest,
        },
    },
    debian => {
        install   => \&distro_debian_install,
        install_deps => \&distro_debian_install_deps,
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

sub distro_arch_version_compare {
    my (undef, $current, $latest) = @_;
    my $comp = 'vercmp';
    my $cmp = `$comp $current $latest`;
    return -1 if $cmp < 0;
    return 1  if $cmp > 0;
    return 0;
}

# Parse information about an Arch package into a hash.
sub distro_arch_parse_info {
    my ($raw) = @_;
    my %info;
    my @fields = qw(Name Version Description Architecture URL);
    foreach my $field (@fields) {
        $raw =~ /^$field\s*: (.*)$/m;
        $info{lc $field} = $1;
    }
    return %info;
}

sub distro_arch_current_info {
    my ($package) = @_;
    unless (distro_arch_installed($package)) {
        error("$package is not installed with pacman");
    }
    my $raw = `pacman -Qi $package`;
    return distro_arch_parse_info($raw);
}

sub distro_arch_latest_info {
    my ($package) = @_;
    my $raw = `pacman -Si $package`;
    return distro_arch_parse_info($raw);
}

sub distro_arch_version_current {
    my ($package) = @_;
    my %info = distro_arch_current_info($package);
    return $info{version};
}

sub distro_arch_version_latest {
    my ($package) = @_;
    my %info = distro_arch_latest_info($package);
    return $info{version};
}

sub with_arch_config {
    my ($package) = @_;
    return (
        install   => sub { distro_arch_install($package) },
        installed => sub { distro_arch_installed($package) },
        version   => {
            current => sub { distro_arch_version_current($package) },
            latest  => sub { distro_arch_version_latest($package) },
        },
    );
}

##############
# Arch - AUR #
##############

my $aur_directory = "$software_directory/aur";

my $aur_git_url_base = 'https://aur.archlinux.org/cgit/aur.git';

sub parse_arch_pkgbuild {
    my ($pkgbuild_info) = @_;
    my $raw;
    with_temp_dir sub {
        open(my $fh, ">", "PKGBUILD");
        print $fh $pkgbuild_info;
        $raw = `makepkg --printsrcinfo`;
    };
    my %info_raw;
    while ($raw =~ /^\s*(\w+) = (.*)$/mg) {
        $info_raw{$1} = $2;
    }
    my %info = (
        name         => $info_raw{pkgname},
        version      => "$info_raw{pkgver}-$info_raw{pkgrel}",
        description  => $info_raw{pkgdesc},
        architecture => $info_raw{arch},
        url          => $info_raw{url},
    );
    return %info;
}

sub arch_aur_latest_info {
    my ($package) = @_;
    my $pkgbuild_data = `curl -fsSL $aur_git_url_base/plain/PKGBUILD?h=$package`;
    return parse_arch_pkgbuild($pkgbuild_data);
}

# Install a package from AUR
sub arch_aur_install {
    my ($package, %options) = @_;
    my $tar_file = "$package.tar.gz";
    my $aur_url = "$aur_git_url_base/snapshot/$tar_file";
    with_directory $aur_directory => sub {
        debug("Downloading latest snapshot from '$aur_url'...");
        sequence(
            "curl -fsSL $aur_url --output $tar_file",
        );
        sequence(
            "tar -xvf $tar_file",
        );
        debug("Building package...");
        with_directory $package => sub {
            sequence(
                "makepkg -si"
            );
        }
    }, (make_dir => 1);
}

sub arch_aur_version_latest {
    my ($package) = @_;
    my %info = arch_aur_latest_info($package);
    return $info{version};
}

sub arch_aur_update {
    my ($package) = @_;
    return arch_aur_install($package);
}

sub with_arch_aur_config {
    my ($package) = @_;
    return (
        install   => sub { arch_aur_install($package) },
        update    => sub { arch_aur_update($package) },
        version => {
            latest  => sub { arch_aur_version_latest($package) },
        },
    );
}

############
#  Debian  #
############

sub distro_debian_install {
    my $program = shift;
    return system("apt-get install $program -y");
}

sub distro_debian_install_deps {
    my ($program) = @_;
    return system("apt-get build-dep $program -y");
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

sub github_version_latest_from_tags {
    my ($repo, $version_re) = @_;
    return version_latest_from_directory_url(
        "https://github.com/$repo/tags", $version_re,
    );
}

#########
#  Gem  #
#########

sub with_gem_config {
    my ($package, %options) = @_;
    my $gem = 'gem' . ($options{version} // '1.9.1');
    my $version_re = qr/\((?<version>\d+\.\d+\.\d+)(?:, [^)]+)?\)/;
    my $install = sub {
        return system("$gem install --user-install $package");
    };
    return (
        install   => $install,
        installed => sub {
            return `$gem list --local` =~ /^$package\s/im;
        },
        version   => {
            current => sub {
                `$gem list --local $package` =~ /^$package $version_re$/im;
                return $+{version};
            },
            latest  => sub {
                `$gem list --remote $package` =~ /^$package $version_re$/im;
                return $+{version};
            },
        },
        update    => $install,
    );
}

#################
# Git Utilities #
#################

sub git_clone {
    my ($url) = @_;
    my $buff;
    debug("Cloning $url");
    unless (scalar run(
                command => "git clone $url",
                buffer  => \$buff,
            )) {
        my ($path) = $buff =~ /destination path '(.+?)' already exists/i;
        with_directory $path => sub {
            my ($remote_url) = `git remote show origin` =~ /Fetch URL: (.+?)\.git$/m;
            return $OK if ($remote_url eq $url =~ s/\.git$//r);
            return $ERROR;
        }
    }
}

###########
# Default #
###########

# Install the program with the default configuration
sub with_default_config {
    with_arch_config(@_);
}

#######################################################################
#                              Commands                               #
#######################################################################

my %software_config = (
    aspell => {
        with_default_config('aspell'),
    },
    'aspell-dict-en' => {
        with_default_config('aspell-en'),
    },
    apache_ant => {
        with_default_config('ant'),
    },
    cask => {
        install   => \&cask_install,
        installed => q_which('cask'),
        update    => \&cask_update,
        version   => {
            current => q_version('cask'),
            latest  => sub {
                github_version_latest_from_tags(
                    'cask/cask',
                    qr/v($semver_simple_re)/,
                );
            },
        },
    },
    cpanm   => {
        install   => \&cpanm_install,
        installed => q_version('cpanm'),
        update    => \&cpanm_update,
    },
    eclim => {
        install   => \&eclim_install,
        installed => query_noerr('eclimd', '-version'),
    },
    eclipse => {
        install   => \&eclipse_install,
        installed => \&eclipse_installed,
    },
    emacs   => {
        with_default_config('emacs'),
    },
    font_inconsolata => {
        with_default_config('ttf-inconsolata'),
    },
    gcc => {
        with_default_config('gcc'),
    },
    git => {
        with_default_config('git'),
    },
    glibc => {
        with_default_config('glibc'),
    },
    icecat => {
        with_arch_aur_config('icecat'),
    },
    icedtea_jdk => {
        with_default_config('icedtea:jdk'),
    },
    idris => {
        with_default_config('idris'),
    },
    libreoffice => {
        with_default_config('libreoffice-fresh'),
    },
    mercurial => {
        with_default_config('mercurial'),
    },
    mu => {
        with_arch_aur_config('mu'),
    },
    node => {
        with_default_config('nodejs'),
    },
    offlineimap => {
        with_default_config('offlineimap'),
    },
    owncloud_desktop => {
        with_default_config('owncloud-client'),
    },
    pip2 => {
        with_default_config('python2-pip'),
    },
    pip3 => {
        with_default_config('python-pip'),
    },
    recutils => {
        with_arch_aur_config('recutils'),
    },
    rofi => {
        with_default_config('rofi'),
    },
    ruby_stable => {
        with_default_config('ruby'),
    },
    sbcl => {
        with_default_config('sbcl'),
    },
    setxkbmap => {
        with_default_config('xorg-setxkbmap'),
    },
    'swi-prolog' => {
        install   => \&swi_prolog_install,
        installed => \&swi_prolog_installed,
    },
    texlive => {
        with_default_config('texlive-most'),
    },
    tmux => {
        with_default_config('tmux'),
    },
    tmuxinator => {
        with_gem_config('tmuxinator', version => '1.9.1'),
    },
    urxvt => {
        with_default_config('rxvt-unicode'),
    },
    vim => {
        with_default_config('vim'),
    },
    xinit => {
        with_default_config('xorg-xinit'),
    },
    xmobar => {
        with_default_config('xmobar'),
    },
    xmonad => {
        with_default_config('xmonad'),
    },
    xmonad_contrib => {
        with_default_config('xmonad-contrib'),
    },
    xorg_server => {
        with_default_config('xorg-server'),
    },
    xrdb => {
        with_default_config('xorg-xrdb'),
    },
    xscreensaver => {
        with_default_config('xscreensaver'),
    },
    xset => {
        with_default_config('xorg-xset'),
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
    my $current = eval { run_config_or_default($program, 'version', 'current') };
    info('could not retrieve current version number, assuming up-to-date')
        and return 1 unless defined $current;
    my $latest  = run_config_or_default($program, 'version', 'latest');
    info('could not retrieve latest version number, assuming up-to-date')
        and return 1 unless defined $latest;
    debug("comparing version $current (current) to $latest (latest)");
    my $comp = run_config_or_default([$current, $latest], $program, 'version', 'compare');
    return ($comp >= 0);
}

########
# Cask #
########

sub cask_install {
    with_directory home() => sub {
        sequence(
            'curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python',
        );
        link_script_local(abs_path('.cask/bin/cask'), 'cask');
    };
}

sub cask_update {
    system('cask update-cask');
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

#########
# Eclim #
#########

sub eclim_install {
    with_directory $software_directory => sub {
        sequence(
            git_clone('git://github.com/ervandew/eclim.git'),
            with_directory "$software_directory/eclim" => sub {
                sequence("ant clean deploy -Dvim.skip=true -Declipse.home=$software_directory/eclipse");
            },
        );
        link_script_local(abs_path("$software_directory/eclipse/eclimd"), 'eclimd');
        link_script_local(abs_path("$software_directory/eclipse/eclim"), 'eclim');
    };
}

###########
# Eclipse #
###########

my $eclipse_tar_file = 'eclipse-java-photon-R-linux-gtk-x86_64.tar.gz';
my $eclipse_dl_url = "http://www.mirrorservice.org/sites/download.eclipse.org/eclipseMirror/technology/epp/downloads/release/photon/R/$eclipse_tar_file";

sub eclipse_install {
    with_directory $software_directory => sub {
        sequence(
            "wget $eclipse_dl_url",
            "tar xf $eclipse_tar_file",
            "rm $eclipse_tar_file",
        ) and return $?;
        link_script_local(abs_path('eclipse/eclipse'), 'eclipse');
    }
}

sub eclipse_installed {
    is_local_bin(get_bin_path('eclipse'));
}

##############
# SWI Prolog #
##############

sub swi_prolog_install {
    with_directory $software_directory => sub {
        sequence(
            git_clone('https://github.com/SWI-prolog/swipl.git'),
            with_directory 'swipl' => sub {
                sequence(
                    'cp build.templ build',
                    './build',
                    );
            },
        );
    }
}

sub swi_prolog_installed {
    is_local_bin(get_bin_path('swipl'));
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
    make_path($target_dir);
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
        debug("'$program' already installed, updating...");
        update_program($program);
        return;
    }
    info("installing '$program'");
    my $ok = success(run_config_or_default($program, 'install'));
    $ok and info("successfully installed '$program'");
    error("error encountered while installing '$program'") unless $ok;
}

sub install_deps {
    my $program = shift;
    if (is_installed($program)) {
        debug("Skipping '$program' (already installed)");
        return;
    }
    info("installing dependencies for '$program'");
    my $ok = success(run_config_or_default($program, 'install_deps'));
    $ok and info("successfully installed dependencies for '$program'");
    error("error encountered while installing dependencies for '$program'") unless $ok;
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
    my $ok = success(run_config_or_default($program, 'update'));
    $ok and info("successfully updated '$program'");
    error("error encountered while updating '$program'") unless $ok;
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
    } elsif ($command eq 'install_deps') {
        install_deps(shift);
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
