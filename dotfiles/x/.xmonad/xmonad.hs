import qualified Data.Map as M

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
-- to fix freemind not displaying correctly (https://stackoverflow.com/questions/30742662/java-swing-gui-not-displaying-in-xmonad#30742663)
import XMonad.Hooks.SetWMName (setWMName)

main = xmonad =<< xmobar myConfig
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { modMask = mm }) = M.union myKeys' (keys def conf)
    where myKeys' = M.fromList $
            [ ((mm,               xK_p), spawn "rofi -show run -location 2 -width 100") ]

myConfig = def
    { keys       = myKeys
    , layoutHook = avoidStruts  $  layoutHook def
    , manageHook = manageDocks <+> manageHook def
    , modMask    = mod4Mask  -- Rebind Mod to the 'Windows' key
      -- to fix freemind not displaying correctly (https://stackoverflow.com/questions/30742662/java-swing-gui-not-displaying-in-xmonad#30742663)
    , startupHook = setWMName "LG3D"
    , terminal   = "urxvt"
    }
