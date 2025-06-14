# Tangled from README.org
# Please modify by editing README.org and re-tangling to generate this nix file.
{ config, pkgs, ... }:

{
  # Tangling individual dev tools through nix-devtools noweb reference
  programs.git = {
    enable = true;
    userName = "David Asabina";
    userEmail = "vid@bina.me";
    lfs.enable = true;
    aliases = {
      wdiff = "diff --word-diff --word-diff-regex='\\w+'";
      glog = "log --oneline --graph --all --decorate";
    };
    extraConfig = {
      init = {
        defaultBranch = "main";
      };

      core = {
        editor = "nvim";
      };

      gpg = {
        program = "gpg2";
      };

      sendemail = {
        annotate = true;
        smtpServer = "msmtp";
        smtpServerOption = "-a vidbina";
      };

      color = {
        ui = true;
        diff = {
          meta = "yellow bold";
          frag = "magenta bold";
           old = "red";
           new = "green";
        };
        grep = {
          match = "yellow";
          filename = "blue";
          linenumber = "brightblack";
        };
        status = {
          added = "yellow";
          changed = "green";
          untracked = "brightblack";
        };
      };
    };
  };
  # Set global gitignore
  home.file = {
    ".config/git/ignore".source = config.lib.file.mkOutOfStoreSymlink ./git/ignore;
  };

  home.packages = [
    pkgs.gh
    pkgs.nushell
    alacritty
    pkgs.wezterm
    pkgs.claude-code
    pkgs.codex
    pkgs.ollama
    pkgs.xxd
    pkgs.hexyl
    pkgs.ghidra-bin
    pkgs.kakoune
    pkgs.graphviz
    pkgs.shellcheck
    pkgs.shfmt
    pkgs.asciinema
    pkgs.exercism
    pkgs.html-tidy
    pkgs.httpie
    pkgs.httplab
    pkgs.checkmake
    #pkgs.cmakeCurses
    pkgs.gnumake
    pkgs.gleam
    pkgs.nixd
    pkgs.nixfmt-rfc-style
    pkgs.nodePackages.typescript-language-server
    pkgs.tree-sitter
    pkgs.jq
    pkgs.yq
    pkgs.sqlite-interactive
    pkgs.redis
  ] ++ (if pkgs.stdenv.isLinux then [
    pkgs.glibc
    pkgs.gdb
    pkgs.evemu
  ] else [ ]);
}
