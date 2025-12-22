# Tangled from emacs/README.org
{ config, pkgs, ... }:

{
  home.file."Applications/Gnoo.app".source = pkgs.runCommand "Gnoo.app" {} ''
    # Create AppleScript that handles file open events
    cat > script.applescript <<'APPLESCRIPT'
    property socketDir : "/tmp/my-emacs/socket"

    on makeGnooCmd(fileArgs)
      set envVar to "EMACS_SOCKET_NAME=" & quoted form of (socketDir & "/server")
      set baseCmd to envVar & " /run/current-system/sw/bin/gnoo"
      if fileArgs is not "" then
        return baseCmd & " " & fileArgs
      else
        return baseCmd
      end if
    end makeGnooCmd

    on open fileList
      set logFile to "/tmp/gnoo-applescript.log"
      set timestamp to do shell script "date '+%Y-%m-%d %H:%M:%S'"

      set filePaths to {}
      repeat with aFile in fileList
        set end of filePaths to POSIX path of aFile
      end repeat

      set fileArgs to ""
      repeat with aPath in filePaths
        set fileArgs to fileArgs & quoted form of aPath & " "
      end repeat

      set cmd to makeGnooCmd(fileArgs)

      do shell script "echo '[" & timestamp & "] on open called with files: " & fileArgs & "' >> " & logFile
      do shell script "echo '[" & timestamp & "] Command: " & cmd & "' >> " & logFile

      try
        do shell script cmd & " >> " & logFile & " 2>&1 &"
        do shell script "echo '[" & timestamp & "] Command executed successfully' >> " & logFile
      on error errMsg
        do shell script "echo '[" & timestamp & "] Error: " & errMsg & "' >> " & logFile
      end try
    end open

    on run
      set logFile to "/tmp/gnoo-applescript.log"
      set timestamp to do shell script "date '+%Y-%m-%d %H:%M:%S'"
      set cmd to makeGnooCmd("")

      do shell script "echo '[" & timestamp & "] on run called (no files)' >> " & logFile
      do shell script "echo '[" & timestamp & "] Command: " & cmd & "' >> " & logFile

      try
        do shell script cmd & " >> " & logFile & " 2>&1 &"
        do shell script "echo '[" & timestamp & "] Command executed successfully' >> " & logFile
      on error errMsg
        do shell script "echo '[" & timestamp & "] Error: " & errMsg & "' >> " & logFile
      end try
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

    # Set icon for document types (fixes icon in 1Password and other modals)
    /usr/libexec/PlistBuddy -c "Add :CFBundleDocumentTypes:0:CFBundleTypeIconFile string document.icns" $out/Contents/Info.plist 2>/dev/null || \
    /usr/libexec/PlistBuddy -c "Set :CFBundleDocumentTypes:0:CFBundleTypeIconFile document.icns" $out/Contents/Info.plist || true
  '';
}
