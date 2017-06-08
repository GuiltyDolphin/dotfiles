# dotfiles

My configuration files for Linux systems.

Hosted on [GitHub](https://github.com/GuiltyDolphin/dotfiles).

## Installing

### Overview

#### Prerequisites

Running these commands requires that you have `make` (or something
that can run like `make`) installed on your system. Additionally,
`git` and `apt-get` may be used for installing software.

##### Root

Much of the software will likely require elevated privileges in order
to install. Recommended is the `sudo` program with

```
Defaults>username !lecture
username ALL=(ALL) ALL
```

in the `/etc/sudoers` file.

##### Guix

To use Guix (https://www.gnu.org/software/guix/) to install and manage
packages, follow these steps (install scripts are found under `install/`):

1. Run the `install-guix.sh` script as root to install Guix
2. Run `guix pull` to update Guix distribution
3. Run the `guix-application-setup.sh` script to initialize Guix for the
   current user.

#### Commands

For a relatively complete installation, run the following command:
`git clone https://github.com/GuiltyDolphin/dotfiles && cd dotfiles && make link`.

General commands (described in more detail later on) - these should
be run in the `dotfiles` directory (the one with the `Makefile`
present):

* `make link` - the same as `make link_medium`.
* `make link_minimal` - will install and setup a minimal set of
software.
* `make link_medium` - will install and setup most of the configured
software.
* `make link_full` - will install and setup all configured software
(may take a while).

In each case, there exists an `install` version
(e.g, `install_minimal`) which will only install the software without
configuring it.

##### Other Commands

* `make get_solarized_colors` to download the solarized color-scheme
for terminal.
* `make setup_ycm` will configure the `YouCompleteMe` plug-in for
Vim - this may take a while.

To install/link individual programs, you can use `install_prog` or
`link_prog` (where `prog` is the required program). Thus `link_tmux`
will only install and link `tmux` (and any dependencies).

#### Are my files safe?

This will (hopefully) not overwrite any existing files when attempting
to perform a link - it will instead warn you that an existing file
exists and differs from the source (if you wish to link that file,
you will need to move (or remove) the existing file).

### 'Minimal' Installation

The `link_minimal` command is for the minimal setup I would consider
being able to work in for any system.

These programs are linked and/or installed when the `link_minimal` or
`install_minimal` make recipes are used.

* `bash` - will link `.bash/`, `.profile`, and `.bashrc`.
* `git` - will link `.gitconfig`.
* `vim` - will link `.vimrc` and `.vim/UltiSnips`.

### 'Medium' Installation

The `link_medium` command is for a setup I would be comfortable with
on many systems, allowing for ease of development across languages.

In addition to those described in "'Minimal' Installation", the
following programs will be configured:

* `tmux` - will link `.tmux.config`.
* `vundle` - will place `Vundle` in `.vim/bundle/Vundle.vim/`.

### 'Full' Installation

Using `make link_full` will install and link all configured software
(`make install_full` will just install).

The 'Full' installation is only really for home systems where I want
to be able to mess around with lots of different pieces of software.

In addition to those described in "'Medium' Installation", the
following programs will be configured:

* `emacs` - will link `.emacs.d/custom/` and `.emacs.d/init.el`.
* `ghci` - will link `.ghci`.
* `tmuxinator` - will link `.tmuxinator`.
* `vimperator` - will link `.vimperatorrc`.
* `irb` - will link `.irbrc`.

#### Caveats

Some of the software is installed via `apt-get install`, which will
require root privileges on most systems, this can be fixed by running
the appropriate `make` command with `sudo` - e.g, `sudo make link`.
