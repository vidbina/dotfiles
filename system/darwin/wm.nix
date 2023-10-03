{ pkgs, lib, ... }: {
  homebrew.casks = [
    # NOTE: The follow keybindings are useful to remember:
    # - Option + Shift + T = Toggle float for focused window
    # - Option + Shift + J = Shift focus counterclockwise
    # - Option + Shift + K = Shift focus clockwise
    # - Option + Shift + M = Shift focus to main window
    # - Option + Shift + D = Toggle between Fullscreen layout
    # - Option + Shift + H = Shrink main pane
    # - Option + Shift + L = Expand main pane
    # - Option + Shift + S = Toggle between Wide layout
    # - Option + Shift + Z = Reeval/recalc windows
    # - Option + Shift + W = Focus Screen 1
    # - Option + Shift + E = Focus Screen 2
    # - Option + Shift + R = Focus Screen 3
    # - Control + Option + Shift + Z = Relaunch Amethyst
    # - Control + Option + Shift + Number = Throw focused window to space matching number
    "amethyst"
  ];
}
