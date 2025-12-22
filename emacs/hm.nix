# Tangled from emacs/README.org
{ config, pkgs, ... }:

{
  home.file."Applications/Gnoo.app".source = pkgs.runCommand "Gnoo.app" {} ''
    # Create AppleScript that handles file open events
    cat > script.applescript <<'APPLESCRIPT'
    property socketDir : "/tmp/my-emacs/socket"

    on makeGnooCmd(fileArgs)
      set exportCmd to "export EMACS_SOCKET_NAME=" & quoted form of (socketDir & "/server")
      set baseCmd to exportCmd & " /run/current-system/sw/bin/gnoo"
      if fileArgs is not "" then
        return baseCmd & " " & fileArgs
      else
        return baseCmd
      end if
    end makeGnooCmd

    on open fileList
      set filePaths to {}
      repeat with aFile in fileList
        set end of filePaths to POSIX path of aFile
      end repeat

      set fileArgs to ""
      repeat with aPath in filePaths
        set fileArgs to fileArgs & quoted form of aPath & " "
      end repeat

      do shell script makeGnooCmd(fileArgs) & "> /dev/null 2>&1 &"
    end open

    on run
      do shell script makeGnooCmd("") & " > /dev/null 2>&1 &"
    end run
    APPLESCRIPT

    # Compile AppleScript into app bundle using macOS system utility
    /usr/bin/osacompile -o "$out" script.applescript

    # Copy icons
    mkdir -p $out/Contents/Resources
    cp ${pkgs.my-emacs}/Applications/Emacs.app/Contents/Resources/Emacs.icns $out/Contents/Resources/ 2>/dev/null || true
    cp ${pkgs.my-emacs}/Applications/Emacs.app/Contents/Resources/document.icns $out/Contents/Resources/ 2>/dev/null || true

    # Update Info.plist to use our icon and metadata
    /usr/libexec/PlistBuddy -c "Set :CFBundleIconFile Emacs.icns" $out/Contents/Info.plist || true
    /usr/libexec/PlistBuddy -c "Set :CFBundleIdentifier me.vidbina.Gnoo" $out/Contents/Info.plist || true
    /usr/libexec/PlistBuddy -c "Set :CFBundleName Gnoo" $out/Contents/Info.plist || true
    /usr/libexec/PlistBuddy -c "Set :CFBundleDisplayName 'Gnoo (Emacs)'" $out/Contents/Info.plist || true
  '';
}
