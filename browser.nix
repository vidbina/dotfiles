{ config, pkgs, lib, options, ... }:

{
  home.packages = with pkgs; [
    nyxt
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
    ];
  };
}
