#!/usr/bin/env bash

#PATH=$PATH:$HOME/Dropbox/programming/commands

# Add local programs to the path
export PATH=$PATH:~/.local/bin

# Add ruby gems to path
export PATH=$PATH:~/.gem/ruby/1.9.1/bin

if [[ -x /usr/bin/dircolors ]]; then
    #test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    test -r ~/Dropbox/linux/other/.dircolors && eval "$(dircolors -b ~/Dropbox/linux/other/.dircolors)" || eval "$(dircolors -b)"
fi

# WILL WANT THIS SOON!

#if [[ -f "$HOME/.dir_colors" ]]; then
#  eval $(dircolors -b "$HOME/.dir_colors")
#fi

#if [[ "$TERM" != "screen-256color" ]]; then
#  # Need to have this detect the number of sessions already taken
#  # and fill the smallest available number
#  #if [[ $(tmux ls -F#S | grep ^session\d+$) ]]; then
#  #  session_name="session$(
#  #fi
#  #session_name="basic_session$(
#  tmux attach-session -t "$USER" || tmux new-session -s "$USER"
#fi

source $HOME'/.bash/git_aliases.sh'

# ;}
if [[ $(which fortune) ]]; then
  fortune
fi

############
#  PROMPT  #
############

status-bar() {
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
PROMPT_COMMAND='status-bar'

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
alias ipy='ipython3 '
alias pyt='python3 setup.py test '

# Open a file as well as its test file
pytd() {
  vim $1 -c ":vs tests/test_$1"
}

##############
#  APTITUDE  #
##############

alias ai='sudo apt-get install '

# If we can't use apt then use apt-cache
if [[ $(which apt) ]]; then
  alias ash='apt search'
else
  alias ash='apt-cache search'
fi

alias as='apt-get show '

# Use vi commands for terminal editing
set -o vi

alias info="info --vi-keys"


####################
#  OTHER COMMANDS  #
####################

alias la='ls -a'
alias ll='ls -la'
# alias numfiles="du -a | wc -l"
alias rutest="rake test"
alias ap='apropos'

alias irb='irb2.0'
alias ruby='ruby2.0'

filecount() {
  local exclude=""
  while [[ $# > 1 ]]; do
    case "$1" in
      -e|--exclude)
      local exclude="$2"
      shift
      ;;
      *)

      ;;
    esac
    shift
  done
  du -a --exclude=$exclude | wc -l
}


# Quickly find all files under a certain path that
# link to (or are) the given file.
# If one argument is given then it is assumed to be
# the file to search for.
# If two arguments are given then the first argument
# becomes the path to search and the second argument
# is the file to search for.
symlinks() {
  if [[ $# -eq 2 ]]; then
    local dirToSearch=$1
    local fileSearch=$2
  else
    local dirToSearch=$HOME
    local fileSearch=$1
  fi
  find -L $dirToSearch -samefile $fileSearch
}

###########
#  EMACS  #
###########

alias emacs='emacs-24.4'



# Attempt to compile the specified latex file to pdf then view it
pdfmkv() {
  pdflatex $1 && okular ${1%.*}.pdf
}


# Run the '--help' for a command
h() {
  if [[ $# -lt 1 ]]; then
    echo "No command specified"
  elif  [[ ! $(which $1) ]]; then
    echo "Command not found"
  else
    res="$1 --help 2>/dev/null"
    if [[ ! $? || ! $($res) ]]; then
      echo "No help found"
    else
      $res
    fi
  fi
}

# Other aliases

export LANG="en_GB.UTF-8"


############
#  SCHOOL  #
############

alias datasheet='okular ~/Dropbox/school/Year_12/Physics/AQA-PHYA4-5-INS-JUN12.PDF'
alias timetable="cat ~/Dropbox/school/Year_12/timetable.markdown"
