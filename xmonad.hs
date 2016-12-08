import XMonad
import XMonad.Config.Xfce
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Renamed
import XMonad.Layout.BinarySpacePartition
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Replace

main = do
  replace
  dbus <- D.connectSession
  getWellKnownName dbus
  xmonad $ ewmh xfceConfig
     { terminal = "xfce4-terminal --hide-menubar"
     , modMask = mod4Mask
     , borderWidth = 1
     , normalBorderColor = "#222222"
     , focusedBorderColor = "#4ce6f7"
     , logHook = dynamicLogWithPP (prettyPrinter dbus) <+> logHook xfceConfig
     , handleEventHook = fullscreenEventHook <+> handleEventHook xfceConfig
     , layoutHook = myLayoutHook
     , manageHook = myManageHook <+> manageHook xfceConfig
     } `additionalKeysP` myKeys

myLayoutHook = smartBorders $ myLayouts

myLayouts = desktopLayoutModifiers $ (renamed [Replace "Binary"] $ emptyBSP) ||| (renamed [Replace "Full"] $ Full)

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

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppLayout   = const ""
    , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal
