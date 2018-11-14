# [vidbina](https://github.com/vidbina)'s dotfiles

These dotfiles make my life slightly more convenient.
Not promising they'll do the same for yours though :wink:.

## .xsession


```bash
ln -s ${PATH_TO_DOTFILES}/xsession ${HOME}/.xsession
```

 - sets the background (I just set a background color, but use `feh` to set a wallpaper)
 - loads .Xmodmap to load custom keyboard bindings
 - start WM

## .Xresources.d

```
ln -s ${PATH_TO_DOTFILES}/Xresources.d ${HOME}/.Xresources.d
```

## .Xmodmap

```bash
ln -s ${PATH_TO_DOTFILES}/Xmodmap ${HOME}/.Xmodmap
```

 - disables caps lock
 - remaps tilde and grave to capslock+[shift]+z in an effort to minimise finger travel (the macbook has a narrower left shift and places the tilde/grave button between the left shift and the Z key)

## tmux.conf

 - sets up vi key bindings in tmux
 - remaps colors

## neovim

```bash
ln -s ${PATH_TO_DOTFILE}/nvim ${HOME}/.config/nvim
```

 - set tabbing behavior (expand tabs to 2 spaces)
 - enable mouse in all modes
 - define Plug extensions
