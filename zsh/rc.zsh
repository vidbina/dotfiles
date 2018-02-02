source ~/.zsh_defaults # loads aliases and exports from ~/.defaults

setopt histignorespace # keeps lines preceded with SPACE out of history
# From a security perspective this is a wise choice, since you do not want any
# users on your terminal from being able to potentially discover very sensitive
# information by checking out the previous commands.

source ~/.zsh_plugins
