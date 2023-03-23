# ~/.profile: executed by the command interpreter for login shells.
# Part of GuiltyDolphin's dotfiles
# https://www.github.com/GuiltyDolphin/config

# Default path
PATH=~/.local/bin/scripts # Custom scripts override everything else
PATH=$PATH:~/.local/bin # Prefer local versions
PATH=$PATH:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/sbin:/usr/bin
PATH=$PATH:/sbin:/bin:/usr/games:/usr/local/games

# Add TexLive to path if it exists
[[ -d "/usr/local/texlive/2015/bin/x86_64-linux" ]] \
  && PATH=/usr/local/texlive/2015/bin/x86_64-linux:$PATH

# set PATH so it includes user's private bin if it exists
[[ -d "$HOME/bin" ]] \
  && PATH="$HOME/bin:$PATH"

HISTIGNORE="&"
EDITOR="vim"

# Add ruby gems to path
[[ -d "$HOME/.gem/ruby/1.9.1/bin" ]] \
  && PATH=$PATH:~/.gem/ruby/1.9.1/bin

[[ -d "$HOME/.gem/ruby/2.0.0/bin" ]] \
  && PATH=$PATH:~/.gem/ruby/2.0.0/bin

# Snap bin
[[ -d "/var/lib/snapd/snap/bin" ]] \
  && PATH=$PATH:/var/lib/snapd/snap/bin

# Commands compiled from cabal
[[ -d "$HOME/.cabal/bin" ]] \
  && PATH="$HOME/.cabal/bin:$PATH"

LANG="en_GB.UTF-8"

##########
#  Perl  #
##########

LOCAL_PATH="$HOME/.local"

eval "$(perl -I$LOCAL_PATH/lib/perl5 -Mlocal::lib=$LOCAL_PATH)"

#########
# OCaml #
#########

test -r ~/.opam/opam-init/init.sh && . ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

export PATH=$PATH:$HOME/.opam/default/bin

##########
# nodenv #
##########

export PATH="$HOME/.nodenv/bin:$PATH"

##########################################
# Personal/private environment variables #
##########################################

# change this line to point to whichever file you
# keep personal environment variables in
PERSONAL_ENV_FILE="$HOME/.env-personal"

if [[ -r "$PERSONAL_ENV_FILE" ]]; then
    source "$PERSONAL_ENV_FILE"
else
    echo "Personal environment file '$PERSONAL_ENV_FILE' not found"
fi

###########
# Finally #
###########

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists, and we are interactive
    # [[ -f "$HOME/.bashrc" && $- == *i* ]]
    [[ -f "$HOME/.bashrc" ]] \
      && source "$HOME/.bashrc"
fi

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
