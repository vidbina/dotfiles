# https://blog.callstack.io/supercharge-your-terminal-with-zsh-8b369d689770
#
zstyle ':completion:*' menu select # select completions with arrow keys
zstyle ':completion:*' group-name '' # group results by category
zstyle ':completion:::::' completer _expand _complete _ignored _approximate # enable approximate matches for completion
