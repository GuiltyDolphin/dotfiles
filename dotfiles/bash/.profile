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

# Add perl5 bin to path if it exists
[[ -d "$HOME/perl5/bin" ]] \
  && PATH="$HOME/perl5/bin:$PATH"

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

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    [[ -f "$HOME/.bashrc" ]] \
      && source "$HOME/.bashrc"
fi

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

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
