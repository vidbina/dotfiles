# Set background color
#xsetroot -solid '#5ac8fa'
#xsetroot -solid '#03a9f4' # blue
xsetroot -solid '#ff9800' # yellow
#xsetroot -solid '#e91e62' # pink
setxkbmap -option -model dell -layout us #-variant intl -option lv3:caps_switch
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 5 --transparent true --tint 0x212121 --height 24 &
nm-applet --sm-disable &
exec xmonad
