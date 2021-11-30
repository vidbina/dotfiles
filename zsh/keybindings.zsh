# Note ^ or \C is Ctrl, \M is Alt
bindkey '^ ' autosuggest-accept
bindkey -M viins '\C-U' kill-whole-line # removes everything
bindkey -M viins '\C-P' history-incremental-pattern-search-backward
bindkey -M viins '\C-N' history-incremental-pattern-search-forward
