#!/usr/bin/env bash
# Part of GuiltyDolphin's dotfiles
# Hosted at: https://www.github.com/GuiltyDolphin/config

# If the 'dircolors' file has been downloaded then use solarized colorscheme.
if [ $(which dircolors) ]; then
  if [ -e "~/.dircolors" ]; then
    eval "$(dircolors -b ~/.dircolors)"
  fi
fi

source "$HOME/.bash/git_aliases.sh"

# ;}
[[ $(which fortune) ]] && fortune

##############
#  TERMINAL  #
##############

# Stop C-s from freezing terminal keys
stty -ixon

############
#  PROMPT  #
############

statusBar() {
  exitcode=$?
  Grey='\[\e[1;32m\]'
  Green='\[\e[0;32m\]'
  Red='\[\e[0;31m\]'
  LightGrey='\[\e[01;34m\]'
  Reset='\[\e[00m\]'
  Checkmark='\342\234\223'
  FancyX='\342\234\227'

  PS1=""

  # If the user is root then display host in 'red'
  # otherwise display both the user and host in 'green'
  if [[ $EUID == 0 ]]; then
    PS1+="$Red\\h "
  else
    PS1+="$Grey\\u@\\h "
  fi

  # Add the user's working directory
  PS1+="$LightGrey\\w "

  if branch=$(git branch 2> /dev/null | grep ^\*); then
    PS1+="$Green(${branch:2}) "
  fi

  PS1+="$LightGrey"

  if [[ $exitcode -eq 0 ]]; then
    PS1+="$Checkmark "
  else
    PS1+="$FancyX "
  fi

  PS1+="\$$Reset "
}

PROMPT_COMMAND='statusBar'

# Fast access to .bashrc
alias 'edbash=vim ~/.bashrc'
alias 'sobash=source ~/.bashrc'

# Fix coloring in tmux
alias tmux='TERM=xterm-256color tmux'

[ -z "$TMUX" ] && export TERM=xterm-256color
# export TERM=xterm-256color      - May want this in general

############
#  PYTHON  #
############

alias py='python3 '
alias pyt='python3 setup.py test '
alias ipy='ipython3 '


##############
#  APTITUDE  #
##############

# If we can't use apt then use apt-cache
if [[ $(which apt) ]]; then
  alias ash='apt search'
else
  alias ash='apt-cache search'
fi

alias as='apt-get show '

# Use vi-keys when invoking the info command.
alias info="info --vi-keys"


####################
#  OTHER COMMANDS  #
####################

alias la='ls -a'
alias ll='ls -la'
alias rutest="rake test"
alias ap='apropos'

alias irb='irb2.0'
alias ruby='ruby2.0'

###########
#  EMACS  #
###########

alias emacs='emacs-24.5'



# Attempt to compile the specified latex file to pdf then view it
pdfmkv() {
  pdflatex $1 && okular ${1%.*}.pdf
}


#############
#  Options  #
#############

# Allow ** to expand recursively.
shopt -s globstar

# Use vi commands for terminal editing
set -o vi

################
#  DuckDuckGo  #
################

# added by duckpan installer
eval $(perl -I${HOME}/perl5/lib/perl5 -Mlocal::lib)

# Don't download metadata (it is slow)
export NO_METADATA_DOWNLOAD=1

