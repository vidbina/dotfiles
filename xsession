# Set background color
#xsetroot -solid '#5ac8fa'
#xsetroot -solid '#03a9f4' # blue
hsetroot -solid '#ff9800' # yellow
#xsetroot -solid '#e91e62' # pink
setxkbmap -option -model dell -layout us #-variant intl -option lv3:caps_switch
trayer --edge top --align right --SetDockType true --SetPartialStrut true --monitor primary \
 --expand true --width 5 --transparent true --tint 0x21212121 --alpha 0 --height 24 &
compton -b
nm-applet --sm-disable &
blueman-applet &
exec xmonad
