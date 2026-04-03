# Home-manager module that defines the dotfiles.path option.
# mkOutOfStoreSymlink needs a real filesystem path (not a /nix/store path),
# and flakes can't discover the checkout location at evaluation time.
# This option centralizes the assumed path and makes it overridable.
{ config, lib, ... }:

{
  options.dotfiles.path = lib.mkOption {
    type = lib.types.str;
    default = "${config.home.homeDirectory}/Code/vidbina/dotfiles";
    description = "Absolute path to the dotfiles repository checkout on disk.";
  };
}
