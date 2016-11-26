import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Renamed
import XMonad.Layout.BinarySpacePartition
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Hooks.ManageDocks(avoidStruts)

main = xmonad $ ewmh xfceConfig
     { terminal = "xfce4-terminal --hide-menubar"
     , modMask = mod4Mask
     , borderWidth = 1
     , normalBorderColor = "#222222"
     , focusedBorderColor = "#4ce6f7"
     , handleEventHook = fullscreenEventHook <+> handleEventHook xfceConfig
     , layoutHook = myLayoutHook
     , manageHook = myManageHook <+> manageHook xfceConfig
     } `additionalKeysP` myKeys

myLayoutHook = avoidStruts . smartBorders $ myLayouts

myLayouts = (renamed [Replace "Binary"] $ emptyBSP)
            ||| (renamed [Replace "Full"] $ Full)

myManageHook = composeAll
               [ className =? "Xfce4-notifyd" --> doIgnore ]

myKeys =
       [ ("M-p", spawn "rofi -show run")
       --	 , ("M-M1-<Left>",    sendMessage $ ExpandTowards L)
       , ("M-M1-h", sendMessage $ ExpandTowards L)
       --	 , ("M-M1-<Right>",   sendMessage $ ShrinkFrom L)
       , ("M-M1-k", sendMessage $ ExpandTowards U)
  --	 , ("M-M1-<Down>",    sendMessage $ ShrinkFrom U)
  --	 , ("M-M1-C-<Left>",  sendMessage $ ShrinkFrom R)
  --	 , ("M-M1-C-<Right>", sendMessage $ ExpandTowards R)
       , ("M-M1-l", sendMessage $ ExpandTowards R)
  --	 , ("M-M1-C-<Up>",    sendMessage $ ShrinkFrom D)
  --	 , ("M-M1-C-<Down>",  sendMessage $ ExpandTowards D)
         , ("M-M1-j", sendMessage $ ExpandTowards D)
	 , ("M-s", sendMessage $ Swap)
         , ("M-r", sendMessage $ Rotate)
       ]
