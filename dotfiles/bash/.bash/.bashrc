#!/usr/bin/env bash
# Part of GuiltyDolphin's dotfiles
# Hosted at: https://www.github.com/GuiltyDolphin/config

# If the 'dircolors' file has been downloaded then use solarized colorscheme.
if [ $(which dircolors) ]; then
  if [ -e "~/.dircolors" ]; then
    eval "$(dircolors -b ~/.dircolors)"
  fi
fi

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


# Use vi-keys when invoking the info command.
alias info="info --vi-keys"


####################
#  OTHER COMMANDS  #
####################

alias la='ls -a'
alias ll='ls -la'
alias rutest="rake test"
alias ap='apropos'
alias cman='connmanctl'
alias pdftotext='pdftotext -layout -nopgbrk -eol unix'


# Searching

# Grep then order on number of matches
grepcount() {
  grep -r "$1" -oc "${@:2}" | grep -v ':0$' | sort -gr -t: -k 2 | uniq
}

alias greptxt='grepcount **/*.txt -ie'
alias greptext='grep **/*.txt -ie'


# Audio

# Show live percentages for the microphone's recording input
test-microphone() {
  arecord -vvv -f dat /dev/null
}


alias irb='irb2.0'
alias ruby='ruby2.0'

# Attempt to compile the specified latex file to pdf then view it
pdfmkv() {
  pdflatex $1 && okular ${1%.*}.pdf
}

monitor-list-primary() {
    xrandr -q | grep primary | grep '^[^ ]\+' -o
}

monitor-list-secondary() {
    xrandr -q | grep '\<connected' | grep '^[^ ]\+' -o | grep -v $(monitor-list-primary)
}

# only for one extra monitor at the moment (2020-02-13)
monitor-show-secondary() {
  xrandr --output $(monitor-list-secondary) --right-of $(monitor-list-primary) --auto
}

# only for one extra monitor at the moment (2020-02-13)
monitor-hide-secondary() {
  xrandr --output $(monitor-list-secondary) --off
}


#############
#  Options  #
#############

# Support extended glob patterns
shopt -s extglob

# Allow ** to expand recursively.
shopt -s globstar

# Use Emacs commands for terminal editing
set -o emacs

#######
# Git #
#######

source "$HOME/.bash/git_aliases.sh"

###########
# History #
###########

# Use ISO8601 format for history dates
#
# A space is included at the end to provide some separation
# between the timestamp and the command
export HISTTIMEFORMAT='%FT%T%z '

# Autocompletion
if [ -f ~/.git-completion.bash ]; then
  source ~/.git-completion.bash
fi
