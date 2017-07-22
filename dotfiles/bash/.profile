# ~/.profile: executed by the command interpreter for login shells.
# Part of GuiltyDolphin's dotfiles
# https://www.github.com/GuiltyDolphin/config

# Default path
PATH=~/.local/bin # Prefer local versions
PATH=$PATH:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/sbin:/usr/bin
PATH=$PATH:/sbin:/bin:/usr/games:/usr/local/games

# Add TexLive to path if it exists
[[ -d "/usr/local/texlive/2015/bin/x86_64-linux" ]] \
  && PATH=/usr/local/texlive/2015/bin/x86_64-linux:$PATH

# Add perl5 bin to path if it exists
[[ -d "$HOME/perl5/bin" ]] \
  && PATH="$HOME/perl5/bin:$PATH"

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    [[ -f "$HOME/.bashrc" ]] \
      && source "$HOME/.bashrc"
fi

# set PATH so it includes user's private bin if it exists
[[ -d "$HOME/bin" ]] \
  && PATH="$HOME/bin:$PATH"


################################################
#               Keyboard Layout                #
#                                              #
#  Capslock->Ctrl                              #
#  Default layout: Programmer Dvorak           #
#  Secondary layout: US Qwerty                 #
#  Both shifts together toggles layout         #
#                                              #
################################################

if [ $(which setxkbmap) ]; then
  setxkbmap -layout "us,us" -variant "dvp," \
    -option "ctrl:nocaps,grp:shifts_toggle"
  if [ ! $? ]; then
    echo "Failed to set keyboard layout, good luck..."
  fi
fi


HISTIGNORE="&"
EDITOR="vim"

# Force Dropbox to start if it is installed
if [[ $(which dropbox) ]]; then
  dropbox start &>/dev/null
fi

# Add ruby gems to path
[[ -d "$HOME/.gem/ruby/1.9.1/bin" ]] \
  && PATH=$PATH:~/.gem/ruby/1.9.1/bin

[[ -d "$HOME/.gem/ruby/2.0.0/bin" ]] \
  && PATH=$PATH:~/.gem/ruby/2.0.0/bin

# Commands compiled from cabal
[[ -d "$HOME/.cabal/bin" ]] \
  && PATH="$HOME/.cabal/bin:$PATH"

LANG="en_GB.UTF-8"

##########
#  Perl  #
##########

PERL5LIB="$HOME/perl5/lib/perl5"
PERL_MB_OPT="--install_base \"$HOME/perl5\""
PERL_LOCAL_LIB_ROOT="$HOME/perl5"
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"

#########
# Idris #
#########

# Set this to empty so it can be set correctly in the Guix profile.
IDRIS_LIBRARY_PATH=

########
# Guix #
########

GUIX_PROFILE="$HOME/.guix-profile"
export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
GUIX_CERTS_DIR="$GUIX_PROFILE/etc/ssl/certs"
if [[ -d $GUIX_CERTS_DIR ]]; then
  GUIX_CERT_FILE="$GUIX_CERTS_DIR/ca-certificates.crt"
  if [[ -f $GUIX_CERT_FILE ]]; then
    export SSL_CERT_DIR="$GUIX_CERTS_DIR"
    export SSL_CERT_FILE="$GUIX_CERT_FILE"
  fi
fi

GUIX_PROFILE_FILE="$GUIX_PROFILE/etc/profile"
[[ -f "$GUIX_PROFILE_FILE" ]] \
  && source "$GUIX_PROFILE_FILE"

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
