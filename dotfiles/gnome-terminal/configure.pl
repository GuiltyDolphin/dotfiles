#!/usr/bin/env perl

use warnings;
use strict;

sub exec_system {
    my ($cmd) = @_;
    my $code = system($cmd);
    $code and die "command failed with exit code $code: $cmd";
}

#################################
# API to terminal configuration #
#################################

my $PROFILES_PATH = '/org/gnome/terminal/legacy/profiles:';

sub get_profiles_key {
    my ($key, %options) = @_;
    chomp (my $value = `dconf read $PROFILES_PATH/$key`);
    if (($options{type} // '') eq 'string') {
        $value =~ s/^'(.*)'$/$1/;
    }
    return $value;
}

sub get_profile_key {
    my ($profile, $key, %options) = @_;
    get_profiles_key(":$profile/$key", %options);
}

sub set_profiles_key {
    my ($key, $value, %options) = @_;
    if (($options{type} // '') eq 'string') {
        $value = "'$value'";
    }
    if (ref $value eq 'HASH') {
        my $type = $value->{type};
        $value = $value->{value};
        if ($type eq 'list') {
            $value = "[@{[join ', ', @$value]}]";
        }
    }
    exec_system(qq{dconf write $PROFILES_PATH/$key "$value"});
}

sub set_profile_key {
    my ($profile, $key, $value, %options) = @_;
    set_profiles_key(":$profile/$key", $value, %options);
}

# get a new UID suitable for identifying a profile
# e.g., a2bee7bb-1351-5e6a-b972-e924b5c878c3
sub get_new_uid {
    my @hex_chars = ('0'..'9', 'a'..'f');
    my $rand_hex = sub { $hex_chars[int(rand(@hex_chars))] };
    my @component_lengths = (8, 4, 4, 4, 12);
    my $uid = '';
    $uid = join '-', map {
        my $l = $_;
        my $comp = '';
        for (my $i = 0; $i < $l; $i++) {
            $comp .= $rand_hex->();
        }
        $comp;
    } @component_lengths;
    return $uid;
}

sub get_profiles {
    return `dconf list $PROFILES_PATH/` =~ /^:([0-9a-f\-]+)\/$/gm;
}

sub create_new_profile {
    my @existing = get_profiles();
    my $uid;
    do { $uid = get_new_uid(); } while (grep { $_ eq $uid } @existing);
    # update the profiles listing so the profile can be found
    set_profiles_key('list', qq{[@{[join ', ', map { "'$_'" } (@existing, $uid)]}]});
    return $uid;
}

sub find_profile_by_ident {
    my ($ident) = @_;
    foreach my $profile (get_profiles()) {
        my $profile_ident = get_profile_key($profile, 'visible-name', type => 'string');
        return $profile if defined $profile_ident && $profile_ident eq $ident;
    }
    return;
}

sub initialise_named_profile {
    my ($ident) = @_;
    my $profile = find_profile_by_ident($ident);
    if (not defined $profile) {
        $profile = create_new_profile();
        set_profile_key($profile, 'visible-name', $ident, type => 'string');
    }
    return $profile;
}

sub set_default_profile {
    my ($profile) = @_;
    set_profiles_key('default', $profile, type => 'string');
}

sub apply_settings {
    my ($profile, $settings) = @_;
    foreach my $k (keys %$settings) {
        set_profile_key($profile, $k, $settings->{$k});
    }
}

# in case things get messy
sub hard_reset {
    exec_system("dconf reset -f $PROFILES_PATH/");
}

#########
# THEME #
#########

sub rgb {
    my (@vals) = @_;
    return "'rgb(@{[join ',', @vals]})'";
}

my $solarized_palette = {
    type => 'list',
    value => [
        rgb(7,54,66),
        rgb(220,50,47),
        rgb(133,153,0),
        rgb(181,137,0),
        rgb(38,139,210),
        rgb(211,54,130),
        rgb(42,161,152),
        rgb(238,232,213),
        rgb(0,43,54),
        rgb(203,75,22),
        rgb(88,110,117),
        rgb(101,123,131),
        rgb(131,148,150),
        rgb(108,113,196),
        rgb(147,161,161),
        rgb(253,246,227)
    ],
};

my %theme_base = (
    'bold-color-same-as-fg'      => 'true',
    'bold-is-bright'             => 'false',
    'cursor-colors-set'          => 'false',
    'highlight-colors-set'       => 'false',
    'use-theme-colors'           => 'false',
    'use-theme-transparency'     => 'true',
    'use-transparent-background' => 'false'
);

my %solarized_light = (
    %theme_base,
    'background-color' => rgb(253,246,227),
    'foreground-color' => rgb(101,123,131),
    'palette' => $solarized_palette,
);

my %solarized_dark = (
    %theme_base,
    'background-color' => rgb(0,43,54),
    'foreground-color' => rgb(131,148,150),
    'palette' => $solarized_palette,
);

my @theme_map = (
    ['solarized-dark' => \%solarized_dark],
    ['solarized-light' => \%solarized_light],
);

my @supported_themes = map { $_->[0] } @theme_map;

sub get_theme {
    my ($theme_name) = @_;
    my ($theme) = map { $_->[1] } grep { $_->[0] eq $theme_name } @theme_map;
    return $theme;
}

################
# Main Program #
################

sub error {
    my ($message) = @_;
    print "$message\n" and exit 1;
}

my $USAGE = "USAGE: configure.pl [--theme=<@{[join '|', @supported_themes]}>]";

sub usage_and_exit {
    print "$USAGE\n" and exit 1;
}

sub process_cl {
    my ($opts_var, $cl_args) = @_;
    my @cl_args = @$cl_args;

    while (my $arg = shift @cl_args) {
        if ($arg eq '--theme') {
            my $theme = shift @cl_args or usage_and_exit();
            if (defined $theme && grep { $_ eq $theme } @supported_themes) {
                $opts_var->{theme} = $theme;
            } else {
                error(qq{Unknown theme: $theme\nSupported themes are:\n  @{[join "\n  ", @supported_themes]}});
            }
            next;
        } else {
            usage_and_exit;
        }
    }
}

my $GDI_PROFILE_IDENT='GDI_PROFILE';
my $GDI_DUMMY_PROFILE_IDENT='GDI_DUMMY_PROFILE';

sub main {
    my %cli_opts = (
        # default to dark theme
        theme => 'solarized-dark',
    );
    process_cl(\%cli_opts, \@ARGV);

    use IPC::Cmd qw(can_run);
    if (!can_run('dconf')) {
        error('dconf not found, cannot configure terminal.');
    }

    # set up a new profile with the Solarized colortheme
    my $profile = initialise_named_profile($GDI_PROFILE_IDENT);
    my $theme = get_theme($cli_opts{theme});
    set_default_profile($profile);
    apply_settings($profile, $theme);

    # needed to allow selecting correct profile (otherwise the profile doesn't
    # activate and no profiles can be selected)
    initialise_named_profile($GDI_DUMMY_PROFILE_IDENT);

    print "Profiles initialised. Please select the '$GDI_PROFILE_IDENT' profile in the terminal settings to use the profile for future sessions.\n";
}

main;
