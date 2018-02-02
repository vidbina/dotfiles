# Zsh configuration

This is an attempt at a very minimal and lightweight zsh configuration. Please
(sym)link rc.zsh to ~/.zshrc and copy defaults.zsh to `~/.defaults` in order to
use this setup.

```bash
ln /path/to/this/rc.zsh ~/.zshrc
cp /path/to/this/defaults.zsh ~/.zsh_defaults
cp /path/to/this/plugins.zsh ~/.zsh_plugins
```

## Zsh Options

Use `setopt` to examine which options are set for your current Zsh session. Conversely `unsetopt` gives an overview of the options that are not set.

 - [Zsh Options Documentation]:http://zsh.sourceforge.net/Doc/Release/Options.html
