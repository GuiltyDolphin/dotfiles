import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

main = xmonad =<< xmobar myConfig

myConfig = def
    { layoutHook = avoidStruts  $  layoutHook def
    , manageHook = manageDocks <+> manageHook def
    , modMask    = mod4Mask  -- Rebind Mod to the 'Windows' key
    , terminal   = "urxvt"
    }
