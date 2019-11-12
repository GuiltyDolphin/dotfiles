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

#### Commands

For a complete installation, run the following command:
`git clone https://github.com/GuiltyDolphin/dotfiles && cd dotfiles && make configure_all`.

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
