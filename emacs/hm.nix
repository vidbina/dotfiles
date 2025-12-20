# Tangled from emacs/README.org
{ config, pkgs, ... }:

{
  home.file."Applications/Gnoo.app".source = pkgs.stdenv.mkDerivation {
    name = "Gnoo.app";
    buildInputs = [ pkgs.my-emacs ];

    unpackPhase = "true";

    installPhase = ''
      mkdir -p $out/Contents/MacOS
      mkdir -p $out/Contents/Resources

      # Copy Emacs icon
      cp ${pkgs.my-emacs}/share/icons/hicolor/scalable/apps/emacs.ico $out/Contents/Resources/ || true

      # Create Info.plist
      cat > $out/Contents/Info.plist <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
      <dict>
        <key>CFBundleExecutable</key><string>Gnoo</string>
        <key>CFBundleIconFile</key><string>emacs</string>
        <key>CFBundleName</key><string>Gnoo</string>
        <key>CFBundleDisplayName</key><string>Gnoo opens Emacs</string>
      </dict>
      </plist>
      EOF

      # Create launcher
      cat > $out/Contents/MacOS/Gnoo <<'EOF'
      #!/bin/sh
      if ${pkgs.my-emacs}/bin/emacsclient -e "(+ 1 1)" >/dev/null 2>&1; then
        exec ${pkgs.my-emacs}/bin/emacsclient -n -c "$@"
      else
        exec ${pkgs.my-emacs}/bin/emacs "$@"
      fi
      EOF
      chmod +x $out/Contents/MacOS/Gnoo
    '';
  };
}
