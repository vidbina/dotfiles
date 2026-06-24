{ pkgs, lib, ... }: {
  system.activationScripts.postActivation.text = lib.mkAfter ''
    # Enforce default app handlers via duti
    # URL schemes (2 args: bundle-id scheme)
    if command -v duti &>/dev/null; then
      echo "Setting default app handlers via duti..."
      duti -s com.linear linear
    else
      echo "duti not found — skipping default app handler setup"
    fi
  '';
}
