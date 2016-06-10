# Use programmer dvorak if available
if [ $(which setxkbmap) ]; then
  setxkbmap -layout "us,us" -variant "dvp," \
    -option "ctrl:nocaps,grp:shifts_toggle"
  if [ ! $? ]; then
    echo "Failed to set keyboard layout, good luck..."
  fi
fi

# Key repeat delay: 500ms delay with 33Hz repeat rate.
xset r rate 500 33

exec xmonad
