import XMonad
import XMonad.Hooks.ManageDocks

main = xmonad def
    { manageHook = manageDocks <+> manageHook def
    , layoutHook = avoidStruts  $  layoutHook def
    , modMask    = mod4Mask  -- Rebind Mod to the 'Windows' key
    }
