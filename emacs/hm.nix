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
      cp ${pkgs.my-emacs}/share/icons/hicolor/128x128/apps/emacs.png $out/Contents/Resources/ || true

      # Create Info.plist
      cat > $out/Contents/Info.plist <<EOF
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
      <dict>
        <key>CFBundleExecutable</key><string>Gnoo</string>
        <key>CFBundleIconFile</key><string>emacs</string>
        <key>CFBundleIdentifier</key><string>com.vidbina.gnoo</string>
        <key>CFBundleName</key><string>Gnoo</string>
        <key>CFBundleDisplayName</key><string>Gnoo (Emacs)</string>
        <key>CFBundlePackageType</key><string>APPL</string>
        <key>CFBundleVersion</key><string>1.0</string>
      </dict>
      </plist>
      EOF

      # Create launcher that calls the gnoo script
      cat > $out/Contents/MacOS/Gnoo <<'EOF'
      #!/bin/sh
      export EMACS_SOCKET_NAME="/tmp/my-emacs/socket/server"
      exec /run/current-system/sw/bin/gnoo "$@"
      EOF
      chmod +x $out/Contents/MacOS/Gnoo
    '';
  };
}
