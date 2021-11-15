# [vidbina](https://github.com/vidbina)'s dotfiles

These dotfiles make my life slightly more convenient.
Not promising they'll do the same for yours though :wink:.

## Inventory on Dell XPS 13 (my reference machine)

### Overview of everything (left is symlink file, right is destination)

```
~/.Xmodmap -> ~/dotfiles/xmodmap
~/.coloritrc -> ~/dotfiles/colorit/coloritrc
~/.conkyrc -> ~/dotfiles/conky.conf
~/.emacs.d -> ~/dotfiles/emacs
~/.gitconfig -> ~/dotfiles/gitconfig
~/.gitignore -> ~/dotfiles/.gitignore
~/.lein -> ~/dotfiles/lein
~/.octaverc -> ~/dotfiles/octave/.octaverc
~/.tmux.conf -> ~/dotfiles/tmux.conf
~/.xsession -> ~/dotfiles/xsession
~/.config/asciinema -> ~/dotfiles/asciinema
~/.config/ghorg -> ~/dotfiles/ghorg
~/.config/redshift.conf -> ~/dotfiles/redshift.conf
~/.config/rofi -> ~/dotfiles/rofi
~/.config/starship.toml -> ~/dotfiles/starship.toml
~/.config/termite -> ~/dotfiles/termite
```

## .xsession

```bash
ln -s ${PATH_TO_DOTFILES}/xsession ${HOME}/.xsession
```

 - sets the background (I just set a background color, but use `feh` to set a wallpaper)
 - loads .Xmodmap to load custom keyboard bindings
 - start WM

## .Xresources.d

> :warning: managed with nix home-manager?

```bash
ln -s ${PATH_TO_DOTFILES}/Xresources.d ${HOME}/.Xresources.d
```

## .Xmodmap

```bash
ln -s ${PATH_TO_DOTFILES}/Xmodmap ${HOME}/.Xmodmap
```

 - disables caps lock
 - remaps tilde and grave to capslock+[shift]+z in an effort to minimise finger travel (the macbook has a narrower left shift and places the tilde/grave button between the left shift and the Z key)

## Tmux

```bash
ln -s ${PATH_TO_DOTFILE}/tmux.conf ${HOME}/.config/tmux.conf
```

 - sets up vi key bindings in tmux
 - remaps colors

## Neovim

```bash
ln -s ${PATH_TO_DOTFILE}/nvim ${HOME}/.config/nvim
```

 - set tabbing behavior (expand tabs to 2 spaces)
 - enable mouse in all modes
 - define Plug extensions

## Termite

```bash
ln -s ${PATH_TO_DOTFILE}/termite ${HOME}/.config/termite
```

## Starship

```bash
ln -s ${PATH_TO_DOTFILE}/starship.toml ${HOME}/.config/starship.toml
```

## Rofi

```bash
ln -s ${PATH_TO_DOTFILE}/rofi ${HOME}/.config/rofi
```

## Redshift

```bash
ln -s ${PATH_TO_DOTFILE}/redshift ${HOME}/.config/redshift
```

## Ghorg

```bash
ln -s ${PATH_TO_DOTFILE}/ghorg ${HOME}/.config/ghorg
```

## Asciinema

```bash
ln -s ${PATH_TO_DOTFILE}/asciinema ${HOME}/.config/asciinema
```

## TODO: Compton

```bash
ln -s ${PATH_TO_DOTFILE}/compton/compton.conf ${HOME}/.config/compton.conf
```

## Octave

```bash
ln -s ${PATH_TO_DOTFILE}/octave/.octaverc ${HOME}/.config/.octaverc
```

### TODO: Rename hidden file to more visible file

## Lein

Package manager and build tool for Clojure. The .lein dotfile lists convenience plugins for development.

```bash
ln -s ${PATH_TO_DOTFILE}/lein ${HOME}/.lein
```

## Colorit

https://linux.die.net/man/1/colorit

Colorit is a script for markup-ing text input which is used in my setup by dict.

```bash
ln -s ${PATH_TO_DOTFILE}/colorit/coloritrc ${HOME}/.coloritrc
```

## Conky

https://github.com/brndnmtthws/conky

Conky is a system monitoring tool which allows the presentation of system metrics in a GUI.

```bash
ln -s ${PATH_TO_DOTFILE}/conky.conf ${HOME}/.conkyrc
```

## Git

### Config

```bash
ln -s ${PATH_TO_DOTFILE}/gitconfig ${HOME}/gitconfig
```
### Ignore

```bash
ln -s ${PATH_TO_DOTFILE}/gitignore ${HOME}/.gitignore
```

## Emacs

```bash
ln -s ${PATH_TO_DOTFILE}/emacs ${HOME}/.emacs.d
```
