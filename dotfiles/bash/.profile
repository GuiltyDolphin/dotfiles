# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set keyboard to own
#xkbcomp -I$HOME/Dropbox/linux/config2/.xkb ~/Dropbox/linux/config2/.xkb/keymap/custommap $DISPLAY
xkbcomp -I$HOME/Dropbox/linux/config2/.xkb ~/Dropbox/linux/config2/.xkb/keymap/emacs $DISPLAY 2> /dev/null
#setxkbmap custom -option ctrl:nocaps

export HISTIGNORE="&"
export EDITOR="vim"
export EMAIL="guiltydolphin@gmail.com"

dropbox start &>/dev/null

if [[ -d "$HOME/Dropbox/programming/commands" ]]; then
  PATH="$HOME/Dropbox/programming/commands:$PATH"
fi

if [ -d "$HOME/.cabal/bin" ] ; then
  PATH="$HOME/.cabal/bin:$PATH"
fi

