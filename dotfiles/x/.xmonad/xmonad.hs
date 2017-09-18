import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

main = xmonad =<< xmobar myConfig

myConfig = def
    { manageHook = manageDocks <+> manageHook def
    , layoutHook = avoidStruts  $  layoutHook def
    , terminal   = "urxvt"
    , modMask    = mod4Mask  -- Rebind Mod to the 'Windows' key
    }
