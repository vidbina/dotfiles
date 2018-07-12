# Set background color
#xsetroot -solid '#5ac8fa'
#xsetroot -solid '#03a9f4' # blue
hsetroot -solid '#ff9800' # yellow
#xsetroot -solid '#e91e62' # pink
setxkbmap -option -model dell -layout us #-variant intl -option lv3:caps_switch

trayer \
  --edge top \
  --align right \
  --width 5 \
  --height 24 \
  --SetDockType true \
  --SetPartialStrut true \
  --transparent true \
  --alpha 0 \
  --expand true \
  --monitor primary \
  --tint 0x21212121 \
  &

compton -b

nm-applet --sm-disable &

blueman-applet &

exec xmonad
# vim: syntax=sh
