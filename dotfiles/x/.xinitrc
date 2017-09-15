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

# Add truetype fonts installed with Guix to path (if available)
GUIX_TRUETYPE_PATH="$GUIX_PROFILE/share/fonts/truetype"
if [ -d $GUIX_TRUETYPE_PATH ]; then
  # First remove it from the path, so we don't get duplicates.
  xset -fp $GUIX_TRUETYPE_PATH 2>/dev/null
  xset +fp $GUIX_TRUETYPE_PATH
fi

# Key repeat delay: 500ms delay with 33Hz repeat rate.
xset r rate 500 33

# Ensure scripts in /etc/X11/xinit/xinitrc.d are sourced.
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

exec xmonad
