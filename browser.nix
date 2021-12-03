{ config, pkgs, lib, options, ... }:

{
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
    ];
  };
}
