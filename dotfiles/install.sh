#!/usr/bin/env bash

dotdir="$(cd $(dirname {$BASH_SOURCE[0]}) && pwd)"

linkBase="$HOME"

# Bash files
bashdir="$dotdir/bash"

dBashrc="$bashdir/.bash/.bashrc"
dBashprofile="$bashdir/.profile"

bashFiles=( "bash" $dBashrc $dBashprofile )


# Git files

gitDir="$dotdir/git"

dGitConf="$gitdir/.gitconfig"

gitFiles=( "git" $dGitConf )


# Haskell files

haskellDir="$dotdir/haskell"

dGhciConf="$haskellDir/.ghci"

#haskellFiles=( $dGhciConf )

ghciFiles=( "ghci" $dGhciConf )

# Tmux files

tmuxDir="$dotdir/tmux"

dTmuxConf="$tmuxDir/.tmux.conf"

dTmuxinator="$tmuxDir/.tmuxinator"

tmuxFiles=( "tmux" $dTmuxConf )
tmuxinatorFiles=( "tmuxinator" $dTmuxinator )


# Vim files

vimDir="$dotdir/vim"

dVimrc="$vimdir/.vimrc"

vimFiles=( "vim" $dVimrc )


linkPath() {
  file=$1
  echo "$linkBase/$(basename $file)"
}

linkFiles() {
  reqProg=$1
  files=$2
  linkDir=$3
  if [[ ! $(which "$reqProg") ]]; then
    echo "$reqProg not found, skipping related dotfiles"
    exit 1
  else
    for file in ${files[@]}; do
      local linkP="$(linkPath $file)"
      if [[ -e "$linkP" ]]; then
        echo "Skipping file $linkP - File exists"
      else
        echo "Would link!"
        # ln -s "$file" "$linkP" && echo "Linked file: $(basename $file) to $linkP" || echo "Failed to link file: $(basename $file)"
      fi
    done
  fi
}

allFiles=( bashFiles ghciFiles gitFiles tmuxFiles tmuxinatorFiles vimFiles )

mainProg() {
  echo "First: ${allFiles[0]}"
  echo "$(${allFiles[0]})"
  echo "Linking dotfiles..."
  for dotProg in ${allFiles[@]}; do
    local prog="${dotProg[0]}"
    local files=${dotProg[@]:1}
    echo "Prog: $dotProg"
    echo "Files: $files"
    echo "Linking files for $prog"
    linkFiles $prog $files
  done
}

mainProg
