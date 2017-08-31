#function kb_EU=setxkbmap -option -model dell -layout us -variant intl -option lv3:caps_switch
echo "Use kb_EU and kb_US to switch keyboard formats"

function kb_EU {
  setxkbmap -option -model dell -layout us -variant intl -option lv3:caps_switch
}

function kb_US {
  setxkbmap -option -model dell -layout us
}
