import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Config.Xfce
import XMonad.Util.EZConfig(additionalKeysP)

main = xmonad $ ewmh xfceConfig
     { terminal = "xfce4-terminal"
     , modMask = mod4Mask
     , borderWidth = 1
     , normalBorderColor = "#222222"
     , focusedBorderColor = "#4ce6f7"
     } `additionalKeysP` myKeys

myKeys =
       [ ("M-p", spawn "rofi -show run")
       , ("M-S-q", spawn "xfce4-session-logout")
       ]