{ pkgs, lib, ... }: {
  homebrew.casks = [
    # NOTE: Mod1 is: Option + Shift
    # - Mod1 + Space = Cycle layout (use Mod2 to reverse order)
    # - Mod1 + Enter = Promote focused window to main window
    # - Mod1 + T = Toggle float for focused window
    # - Mod1 + J = Shift focus counterclockwise
    # - Mod1 + K = Shift focus clockwise
    # - Mod1 + M = Shift focus to main window
    # - Mod1 + D = Toggle between Fullscreen layout
    # - Mod1 + H = Shrink main pane
    # - Mod1 + L = Expand main pane
    # - Mod1 + S = Toggle between Wide layout
    # - Mod1 + Z = Reeval/recalc windows
    # - Mod1 + W = Focus Screen 1
    # - Mod1 + E = Focus Screen 2
    # - Mod1 + R = Focus Screen 3
    # NOTE: Mod2 is: Control + Option + Shift
    # - Mod2 + Z = Relaunch Amethyst
    # - Mod2 + Number = Throw focused window to space matching number

    # NOTE: Amethyst does support changing of window order, so use Mod1+Enter
    "amethyst"
  ];
}
