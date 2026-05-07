{ pkgs, ... }:

{
  languages.javascript = {
    enable = true;
    package = pkgs.nodejs_22;
  };

  languages.javascript.pnpm = {
    enable = true;
    install.enable = true;
  };
}
