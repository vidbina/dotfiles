grep () {
  command grep --color=always "$@"
}

egrep () {
  command egrep --color=always "$@"
}

tree () {
  command tree -C "$@"
}

dict () {
  dict $@ | colorit
}
