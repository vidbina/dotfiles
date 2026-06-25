{ pkgs, lib, ... }: {
  system.activationScripts.postActivation.text = lib.mkAfter ''
    # Enforce default app handlers via duti
    # URL schemes (2 args: bundle-id scheme)
    DUTI="/opt/homebrew/bin/duti"
    if [ -x "$DUTI" ]; then
      echo "Setting default app handlers via duti..."
      "$DUTI" -s com.linear linear
    else
      echo "duti not found at $DUTI — skipping default app handler setup"
    fi
  '';
}
