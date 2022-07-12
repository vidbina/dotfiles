# Tangled from README.org
{ config, lib, pkgs, ... }:

# TODO: Config mutt
let
  inherit (pkgs) stdenv;
  pathIfExists = (p: if (builtins.pathExists p) then [ p ] else [ ]);
in
{
  imports = [
    ./mail.nix
    ./vim.nix
    ./dev.nix
  ]
  ++ (pathIfExists ./personal.nix);

  home.file.".config/ranger".source = config.lib.file.mkOutOfStoreSymlink ./ranger;

  # TODO: Remove, likely not necessary
  home.file.".direnvrc".source = config.lib.file.mkOutOfStoreSymlink ./direnv/direnvrc;


  home.sessionPath = [
    (toString ./bin)
  ];

  manual = {
    # Use `home-manager-help`
    html.enable = true;

    # Use `man home-configuration.nix`
    manpages.enable = true;
  };

  nixpkgs.overlays = [
    (self: super: { })
  ];

  programs.bat = {
    enable = true;
    config = {
      theme = "base16";
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };

  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile (./. + "/tmux.conf");
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    #dotDir = ".config/zsh";

    initExtraBeforeCompInit = ''
      export EDITOR="emacsclient -c -a emacs"
      export VISUAL="emacsclient -c -a emacs"

      setopt histignorespace # keeps lines preceded with SPACE out of history

      zmodload -i zsh/complist
      source ${./zsh/zstyle.zsh}

      autoload -U promptinit && \
      promptinit && \
      prompt adam2 8bit yellow red blue

      # enable bash completion
      autoload -U +X bashcompinit && \
      bashcompinit
    '';
    initExtra = ''
      bindkey -v # use vim key bindings
      source ${./zsh/keybindings.zsh}

      source ${./zsh/functions.zsh}

      source ${pkgs.fzf}/share/fzf/completion.zsh
      source ${pkgs.fzf}/share/fzf/key-bindings.zsh
    '';
  };
}
