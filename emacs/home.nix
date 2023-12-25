{ config, pkgs, ... }:
{
  # home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink ./emacs;
  # TODO: Fix hack of hardcoded dotfiles path
  # NOTE: This repo must be checked out to ~/Code/vidbina/dotfiles
  # A hardcoded .emacs.d source is used because mkOutOfStoreSymlink ./emacs
  # does not seem to work on macOS.
  # See https://discourse.nixos.org/t/accessing-home-manager-config-in-flakes/19864/8
  # See https://github.com/nix-community/home-manager/issues/2085#issuecomment-861427318
  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/vidbina/dotfiles/emacs";
}