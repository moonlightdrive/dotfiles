import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig(additionalKeysP)

main = xmonad $ ewmh xfceConfig
     { terminal = "xfce4-terminal --hide-menubar"
     , modMask = mod4Mask
     , borderWidth = 1
     , normalBorderColor = "#222222"
     , focusedBorderColor = "#4ce6f7"
     , manageHook = myManageHook <+> manageHook xfceConfig
     } `additionalKeysP` myKeys

myManageHook = composeAll
             [ className =? "Xfce4-notifyd" --> doIgnore ]

myKeys =
       [ ("M-p", spawn "rofi -show run")
       , ("M-S-q", spawn "xfce4-session-logout")
       ]