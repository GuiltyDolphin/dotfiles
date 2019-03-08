import qualified Data.Map as M

import XMonad
import XMonad.Actions.CycleWS
    ( nextWS
    , prevWS
    , shiftToNext
    , shiftToPrev
    )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
-- to fix freemind not displaying correctly (https://stackoverflow.com/questions/30742662/java-swing-gui-not-displaying-in-xmonad#30742663)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.EZConfig (additionalKeys)

main :: IO ()
main = xmonad =<< xmobar myConfig

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["main", "terminal", "web", "other"]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { modMask = mm }) = M.union myKeys' (keys def conf)
    where myKeys' = M.fromList $
            [ ((mm .|. shiftMask, xK_l           ), nextWS)
            , ((mm .|. shiftMask, xK_h           ), prevWS)
            , ((mm,               xK_bracketright), shiftToNext >> nextWS)
            , ((mm,               xK_bracketleft ), shiftToPrev >> prevWS)
            , ((mm,               xK_p           ), spawn "rofi -show run -location 2 -width 100")
            ]

myLayout = tiled ||| Mirror tiled ||| Full -- ||| fullMaster ||| Mirror fullMaster
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

     fullMaster = Tall 2 delta 1

myConfig = def
    { keys       = myKeys
    , layoutHook = avoidStruts  $  myLayout
    , manageHook = manageDocks <+> manageHook def
    , modMask    = mod4Mask  -- Rebind Mod to the 'Windows' key
      -- to fix freemind not displaying correctly (https://stackoverflow.com/questions/30742662/java-swing-gui-not-displaying-in-xmonad#30742663)
    , startupHook = setWMName "LG3D"
    , terminal   = "urxvt"
    , workspaces = myWorkspaces
    } `additionalKeys`
      -- XF86AudioLowerVolume
      [ ((0, 0x1008ff11), spawn "amixer set Master 5%-")
      -- XF86AudioRaiseVolume
      , ((0, 0x1008ff13), spawn "amixer set Master 5%+")
      -- XF86AudioMute
      , ((0, 0x1008ff12), spawn "amixer set Master toggle")
      ]
