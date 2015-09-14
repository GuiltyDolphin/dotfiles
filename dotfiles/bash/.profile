# ~/.profile: executed by the command interpreter for login shells.
# Part of GuiltyDolphin's dotfiles (https://www.github.com/GuiltyDolphin/config)

# Default path
PATH=/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/sbin:/usr/bin
PATH=$PATH:/sbin:/bin:/usr/games:/usr/local/games

# Add TexLive to path if it exists
[[ -d "/user/local/texlive/2015/bin/x86_64-linux" ]] \
  && PATH=/usr/local/texlive/2015/bin/x86_64-linux:$PATH

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    [[ -f "$HOME/.bashrc" ]] \
      && source "$HOME/.bashrc"
fi

# set PATH so it includes user's private bin if it exists
[[ -d "$HOME/bin" ]] \
  && PATH="$HOME/bin:$PATH"

# Change this to false if you want to use a
# keyboard layout other than [programmer]dvorak
useCustomKeyboard=true

# Set keyboard layout
#if $useCustomKeyboard; then

# Custom keyboard layout is essentially programmer dvorak but
# <CAPS> becomes a control key, <LSGT> becomes escape, and <RALT>
# becomes left shift.
if [[ $(which xkbcomp) ]]; then
  xkbcomp -I$HOME/.keyboard/xkb ~/.keyboard/xkb/keymap/emacs $DISPLAY 2> /dev/null
else
  echo "Cannot set custom keymap, using programmer dvorak"
  if [[ $(which setxkbmap) ]]; then
    setxkbmap -layout us -variant dvp -option ctrl:nocaps
    if [[ ! $? ]]; then
      echo "Failed to set programmer dvorak, using regular dvorak"
      setxkbmap -layout us -variant dvorak -option ctrl:nocaps \
        || echo "Could not set keyboard layout"
    fi
  else echo "Cannot find 'setxkbmap', keyboard layout not set"
  fi
fi
#fi

setKeyUs() {
  xkbcomp -I$HOME/.keyboard/xkb ~/.keyboard/xkb/keymap/blender $DISPLAY 2> /dev/null
}
setKeyDv() {
  xkbcomp -I$HOME/.keyboard/xkb ~/.keyboard/xkb/keymap/emacs $DISPLAY 2> /dev/null
}



HISTIGNORE="&"
EDITOR="vim"

# Force Dropbox to start if it is installed
if [[ $(which dropbox) ]]; then
  dropbox start &>/dev/null
fi

# Add local programs to the path
PATH=$PATH:~/.local/bin

# Add ruby gems to path
[[ -d "$HOME/.gem/ruby/1.9.1/bin" ]] \
  && PATH=$PATH:~/.gem/ruby/1.9.1/bin

[[ -d "$HOME/.gem/ruby/2.0.0/bin" ]] \
  && PATH=$PATH:~/.gem/ruby/2.0.0/bin

# Commands compiled from cabal
[[ -d "$HOME/.cabal/bin" ]] \
  && PATH="$HOME/.cabal/bin:$PATH"

LANG="en_GB.UTF-8"
