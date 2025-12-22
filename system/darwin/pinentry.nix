{ pkgs, lib, inputs, ... }: {
  environment.shellAliases = {
    pinentry = "pinentry-mac";
  };

  system.defaults.CustomUserPreferences = {
    "org.gpgtools.common" = {
      # Disable "Save in Keychain" in pinentry-mac
      # as documented in pinentry-touchid setup instructions
      "DisableKeychain" = true;
      "UseKeychain" = false;
    };
  };

  homebrew = {
    brews = [
      "pinentry-mac"
      "pinentry-touchid" # from tap: jorgelbg/tap
    ];
    taps = [
      "jorgelbg/tap"
    ];
  };
}
