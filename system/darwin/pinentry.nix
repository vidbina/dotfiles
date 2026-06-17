{ pkgs, lib, inputs, ... }: {
  environment.shellAliases = {
    pinentry = "pinentry-mac";
  };

  homebrew = {
    brews = [
      "pinentry-mac"
    ];
  };
}
