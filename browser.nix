# Tangled from README.org
{ config, pkgs, lib, options, ... }:

let
  # TODO: Find a cleaner implementation, like an pkgs overlay at shell.nix?!?
  nur = import <NUR> { inherit pkgs; };
in
{
  home.packages = with pkgs; [
  ];

  programs.chromium = {
    enable = true;

    extensions = [
      {
        # Metamask
        # https://chrome.google.com/webstore/detail/metamask/nkbihfbeogaeaoehlefnkodbefgpgknn
        id = "nkbihfbeogaeaoehlefnkodbefgpgknn";
      }
      {
        # Vimium
        # https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb
        id = "dbepggeogbaibhgnhhndojpepiihcmeb";
      }
      {
        # Darkreader
        # https://chrome.google.com/webstore/detail/dark-reader/eimadpbcbfnmbkopoojfekhnkhdbieeh
        id = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
      }
      {
        # wasavi
        # https://chrome.google.com/webstore/detail/wasavi/dgogifpkoilgiofhhhodbodcfgomelhe
        # see https://github.com/philc/vimium/issues/2564
        id = "dgogifpkoilgiofhhhodbodcfgomelhe";
      }
    ];
  };

  programs.firefox = {
    enable = true;

    # NOTE: Extensions need firefox.profiles to be defined
    extensions =
      # https://nur.nix-community.org/repos/rycee/
      with nur.repos.rycee.firefox-addons; [
        multi-account-containers # needed by tridactyl
        darkreader
        tridactyl
      ];
  };
}
