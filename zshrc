# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=30000
SAVEHIST=30000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
#zstyle :compinstall filename '/home/vid/.zshrc'

#autoload -Uz compinit
#compinit
# End of lines added by compinstall
#

function colors {
  for x in 0 1 4 5 7 8; do for i in {30..37}; do for a in {40..47}; do echo -ne "\e[$x;$i;$a""m\\\e[$x;$i;$a""m\e[0;37;40m "; done; echo; done; done; echo "";
}

function cheat {
  # 0 - 7 black red green yellow blue magenta cyan white
  # 3COL = fg, 4COL = bg
  # 0 = normal, 1 = bold 4 = underlined, 5 = blinking, 7 = reverse
  MOD="\033[1;5;47;30m"
  BTN="\033[1;5;47;30m"
  BTX="\033[1;5;41;37m"
  RES="\033[0m"
  SPACE="" #"\t\t"
  echo "${BTX} Mod ${RES} ${MOD} Shift ${RES} ${MOD} Return ${RES} ${SPACE} Launch term"
  echo "${BTX} Mod ${RES} ${BTN} P ${RES} ${SPACE} Launch dmenu"
  echo "${BTX} Mod ${RES} ${MOD} Shift ${RES} ${BTN} P ${RES} ${SPACE} Launch gmrun"
  echo "${BTX} Mod ${RES} ${MOD} Shift ${RES} ${BTN} C ${RES} ${SPACE} Close the focused window"
  echo "${BTX} Mod ${RES} ${BTN} Space ${RES} ${SPACE} Rotate through the available layout algorithms"
  echo "${BTX} Mod ${RES} ${MOD} Shift ${RES} ${BTN} Space ${RES} Reset the layouts on the current workspace to default"
  echo "${BTX} Mod ${RES} N${RES} Resize viewed windows to correct size"
  echo "${BTX} Mod ${RES} ${MOD} shift ${RES} ${BTN} tab ${RES} ${SPACE} Move focus to the previous window"
  echo "${BTX} Mod ${RES} ${BTN} J ${RES} ${SPACE} Move focus to the next window"
  echo "${BTX} Mod ${RES} ${BTN} K ${RES} ${SPACE} Move focu to the previous window"
  echo "${BTX} Mod ${RES} ${BTN} M ${RES} ${SPACE} Move focus to the master window"
  echo "${BTX} Mod ${RES} ${BTN} return ${RES} ${SPACE} Swap the focused windows and the master window"
  echo "${BTX} Mod ${RES} ${MOD} shift ${RES} ${BTN} J ${RES} ${SPACE} Swap the focused window with the next window"
  echo "${BTX} Mod ${RES} ${MOD} shift ${RES} ${BTN} k ${RES} ${SPACE} Swap the focused window with the previous window"
  echo "${BTX} Mod ${RES} ${BTN} h ${RES} ${SPACE} Shrink the master area"
  echo "${BTX} Mod ${RES} ${BTN} l ${RES} ${SPACE} Expand "
  echo "${BTX} Mod ${RES} ${BTN} t ${RES} ${SPACE} Push window back into tiling"
  echo "${BTX} Mod ${RES} ${BTN} , ${RES} ${SPACE} Increment the number of windows in the master area"
  echo "${BTX} Mod ${RES} ${BTN} . ${RES} ${SPACE} Deincrement the number of windows in the master area"
  echo "${BTX} Mod ${RES} ${MOD} shift ${RES} ${MOD} q ${RES} ${SPACE} Quit xmonad"
  echo "${BTX} Mod ${RES} ${BTN} q ${RES} Restart xmonad"
  echo "${BTX} Mod ${RES} ${MOD} shift ${RES} ${BTN} slash ${RES} ${SPACE} Run xmessage with a summary of the default keybindings"
  echo "${BTX} Mod ${RES} ${BTN} 1..9 ${RES} ${SPACE} Switch to workspace N"
  echo "${BTX} Mod ${RES} ${MOD} shift ${RES} ${BTN} 1..9 ${RES} ${SPACE} Move client ot workspace N"
  echo "${BTX} Mod ${RES} ${BTN} {w,e,r} ${RES} ${SPACE} Move client to screen 1, 2 or 3"
  echo "${BTX} Mod ${RES} ${MOD} button1 ${RES} ${SPACE} Set the window to floating mode and move by draggin"
  echo "${BTX} Mod ${RES} ${MOD} button2 ${RES} ${SPACE} Raise the window to the top of the stack"
  echo "${BTX} Mod ${RES} ${MOD} button3 ${RES} ${SPACE} Set the window to floating mode and resizse by dragging"
#  echo "${BTX} Mod ${RES} ${RES}
#  echo "${BTX} Mod ${RES} ${RES}
#  echo "${BTX} Mod ${RES} ${RES}
}

plugins=(pass, history-substring-search)

source ~/.dockerfunc
source ~/docker.firefox-dev/func.sh
source ~/.tmh.zsh
source ~/.xrandr-aliases.sh

alias vi=nvim
alias vim=nvim
export EDITOR=nvim

GPG_TTY=$(tty)
export GPG_TTY

#export GTK_IM_MODULE=ibus
#export XMODIFIERS=@im=ibus
#export QT_IM_MODULE=ibus
#
