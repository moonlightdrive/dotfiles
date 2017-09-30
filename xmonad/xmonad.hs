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
     { borderWidth = 1
     , focusedBorderColor = "#4ce6f7"
     , focusFollowsMouse = False
     , handleEventHook = fullscreenEventHook <+> handleEventHook xfceConfig
     , layoutHook = myLayoutHook
     , logHook = dynamicLogWithPP (prettyPrinter dbus) <+> logHook xfceConfig
     , manageHook = myManageHook <+> manageHook xfceConfig
     , modMask = mod4Mask
     , normalBorderColor = "#222222"
     , terminal = "xfce4-terminal --hide-menubar"
     } `additionalKeysP` myKeys

myLayoutHook = smartBorders $ myLayouts

myLayouts = desktopLayoutModifiers $ (renamed [Replace "Binary"] $ emptyBSP) ||| (renamed [Replace "Full"] $ Full)

myManageHook = composeAll
               [ className =? "Xfce4-notifyd" --> doIgnore
               , title =? "Whisker Menu"      --> doFloat
               , className =? "Gimp"          --> doFloat
               ]

myKeys =
       [ ("M-p", spawn "rofi -show run")

       , ("M-M1-h", sendMessage $ ExpandTowards L)
       , ("M-M1-k", sendMessage $ ExpandTowards U)
       , ("M-M1-l", sendMessage $ ExpandTowards R)
       , ("M-M1-j", sendMessage $ ExpandTowards D)
       -- do i really need these
       , ("M-S-M1-h", sendMessage $ ShrinkFrom L)
       , ("M-S-M1-k", sendMessage $ ShrinkFrom U)
       , ("M-S-M1-l", sendMessage $ ShrinkFrom R)
       , ("M-S-M1-j", sendMessage $ ShrinkFrom D)

       , ("M-s", sendMessage $ Swap)
       , ("M-r", sendMessage $ Rotate)
       ]

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
--    , ppLayout   = const ""
    , ppOrder = \(ws:_:t:_) -> [ws, t]
    , ppSep      = " "
    , ppUrgent = fgColor "#d66951"
    }
  where fgColor fg =
          let left = "<span foreground=\"" ++ fg ++ "\">"
              right = "</span>"
          in wrap left right
  {- fgColor <- pango text attribute markup language -}

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
