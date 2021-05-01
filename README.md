# dotfiles

My configuration files for Linux systems.

Hosted on [GitHub](https://github.com/GuiltyDolphin/dotfiles).

## Setting up a new system

When setting up a new system, you'll want to make sure that
things such as locales are set up correctly.

### Setting up locale

First, add a line `en_GB.UTF-8 UTF-8` (or a locale of your
choice) to `/etc/locale.gen` if it isn't there already, then
run `locale-gen` as root. The locale should now be enabled,
which you can check with `locale -a`. Run `locale` to see if
your locale is set correctly, and if not, reload bash
configuration files, which should set this for you.

If you get stuck, you can find more information at
https://wiki.archlinux.org/index.php/Locale.

## Installing

### Overview

#### Setting up the installer

To set up the installer, you first need to make sure you have
Perl installed. To check if you have a Perl executable, simply
run `type -p perl` in a terminal. If you see a path displayed
(as in `/usr/bin/perl`), you can move on to the next step,
otherwise you'll need to grab a copy of Perl for your
distribution.

**NOTE: By default the bootstrapper tries to configure Perl in
`~/.local`, if you want to use a different directory, specify
the flag `--perl-install-dir` (e.g.,
`--perl-install-dir=~/.local`)**

Once you have Perl installed, you can proceed to run
`./install/bootstrap.pl` (or `perl ./install/bootstrap.pl`),
which will try and configure the installer, and will let you
know if it encounters any issues (e.g., missing dependencies
that the bootstrapper can't install). If something is failing
and you're not sure what, try running the bootstrap script
with the `--debug` flag.

If you see a message saying the bootstrapping has been
successful (e.g., "Installer bootstrapped!") at the end of the
output, then the installer has been configured, and is ready
to use!

##### Root

Much of the software will likely require elevated privileges in order
to install. Recommended is the `sudo` program with

```
Defaults>username !lecture
username ALL=(ALL) ALL
```

in the `/etc/sudoers` file.

#### Commands

For a complete installation, run the following command: `git
clone https://github.com/GuiltyDolphin/dotfiles && cd dotfiles
&& ./install/bootstrap.pl && make configure_all` (making sure
you have `perl` somewhere on your `PATH`).

If a full setup is not desired, individual packages can be configured
via Make, or the following commands
(which should be run from the `dotfiles` directory) can be used:

* `make configure_dev`   - sets up a basic development environment.
* `make configure_tools` - sets up some tools for miscellaneous tasks.
* `make configure_web`   - configures software for web use.

More recipes can be found in the `Makefile` file in the `dotfiles` directory.

##### Other Commands

* `make get_solarized_colors` to download the solarized color-scheme
for terminal.
* `make setup_ycm` will configure the `YouCompleteMe` plug-in for
Vim - this may take a while.

To install/link individual programs, you can use `install_prog` or
`link_prog` (where `prog` is the required program). Thus `link_tmux`
will only install and link `tmux` (and any dependencies).

Using `install_prog` recipes (e.g., `install_tmux`) will _only_
install the given package (and its dependencies), and will not
perform any additional configuration.

#### Are my files safe?

This will (hopefully) not overwrite any existing files when attempting
to perform a link - it will instead warn you that an existing file
exists and differs from the source (if you wish to link that file,
you will need to move (or remove) the existing file).

##### Configuration Files Used

The following files are placed/linked when configuring
(or linking) the given packages:

* `bash`       - will link `.bash/`, `.profile`, and `.bashrc`.
* `emacs`      - will link `.emacs.d/custom/` and `.emacs.d/init.el`.
* `ghci`       - will link `.ghci`.
* `git`        - will link `.gitconfig`.
* `irb`        - will link `.irbrc`.
* `tmux`       - will link `.tmux.config`.
* `tmuxinator` - will link `.tmuxinator`.
* `vim`        - will link `.vimrc` and `.vim/UltiSnips`.
* `vimperator` - will link `.vimperatorrc`.
* `vundle`     - will place `Vundle` in `.vim/bundle/Vundle.vim/`.

#### Caveats

Some of the software is installed via `apt-get install`, which will
require root privileges on most systems, this can be fixed by running
the appropriate `make` command with `sudo`; this is not recommended
however, and I am working on making as much of the software install-able
and configurable without requiring root privileges.
