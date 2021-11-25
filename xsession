# Set background color
hsetroot -solid '#ff9800'

setxkbmap -option -model dell -layout us #-variant intl -option lv3:caps_switch

trayer \
  --edge top \
  --align right \
  --width 5 \
  --widthtype pixel \
  --height 24 \
  --SetDockType true \
  --SetPartialStrut true \
  --transparent true \
  --alpha 0 \
  --expand true \
  --monitor primary \
  --tint 0x00000000 \
  &

compton -b

nm-applet --sm-disable &

blueman-applet &

#redshift-gtk &

exec xmonad
# vim: syntax=sh
