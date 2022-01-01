# Rofi

Rofi can be configured manually by symlinking the
`~/.config/rofi` directory to the current directory or through
Home Manager which is the recommended approach for this
configuration.

Note that when using Home Manager, the
[config.rasi](/config.rasi) file in this repository is not
relevant as Home Manager will generate your config (to
`~/.config/rofi/config.rasi`, unless Rofi has been configured to
use another config path) based on attributes specified in the
[nix configuration (in default.nix)](/default.nix).

When you manually configure Rofi by populating `~/.config/rofi`
yourself, you may want to tweak the contents of
[config.rasi](/config.rasi) and symlink
`~/config/rofi/config.rasi` to it in order to get the setup that
you want.

> :warning: When making changes to this repo, remember to pay
> attention to maintain some semblance of parity between 1) the
> manual configuration assets (i.e.: config.rasi and themes, if
> relevant) and 2) the Nix code for Home Manager.

> :bulb: You can create a template config using `rofi
> -dump-config config.rasi > template-config.rasi` and use it to
> update config.rasi according to your needs.
