-- http://stackoverflow.com/questions/9993966/xmonad-confirmation-when-restarting

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
-- import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO

-- BSP
import XMonad.Layout.Renamed
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.EqualSpacing

-- compositing
import XMonad.Hooks.FadeInactive

-- Fullscreen (see http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#Watch_fullscreen_flash_video)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Hooks.ManageHelpers

-- What I really want to do is get my colors from .Xresources,
-- or somehow tie them together.
-- Hmm....

-- XXX look at ask/get in core

accentColor = "#FF0044"



main :: IO ()
main = do
--    xmproc <- spawnPipe "/home/moonlight/.cabal/bin/xmobar /home/moonlight/.config/xmobarrc"
  xmonad =<< myStatusBar (defaultConfig
        { terminal = "termite"
        , modMask  =  mod4Mask
        , borderWidth = 1
	, normalBorderColor = "#222222"
	, focusedBorderColor = accentColor
	, workspaces = myWorkspaces
        , manageHook = myManageHook
	, layoutHook = myLayoutHook
        , logHook = myLogHook {- xmproc -}
        , focusFollowsMouse = False
        } `additionalKeysP` myKeys)

myStatusBar = statusBar barCmd myPP toggleBar
  where
    barCmd = "/home/moonlight/.cabal/bin/xmobar /home/moonlight/.config/xmobarrc"
    toggleBar XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
    myPP = defaultPP { ppCurrent = xmobarColor accentColor ""
                     , ppSep = " :: "
                               -- , ppOrder = \(wkspaces::layout::title:__) -> [ws,t] -- don't display layout
                     }
        
myManageHook = composeOne [ isFullscreen -?> doFullFloat
--                          , (className =? "Gimp") -?> doFloat
                          , (role =? "gimp-toolbox" <||> role =? "gimp-image-window") -?> (ask >>= doF . W.sink)
                          ]
  where role = stringProperty "WM_WINDOW_ROLE"

{-
myManageHook = composeAll [ manageDocks
                          , isFullscreen --> doFullFloat
                          , manageHook defaultConfig
                          ]
-}


myWorkspaces :: [String]
myWorkspaces = map show [1..9]
-- myWorkspaces = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι"]

myLayoutHook = smartBorders $ {- avoidStruts $ -} myLayouts

myLogHook :: X ()
myLogHook = fade
  where fade = fadeInactiveLogHook 1 -- 0.6

gaps = equalSpacing gap add mult min
       where	    
       -- spacing from one window to the next (or screen edge to windows)
       gap = 25
       -- number of pixels subtracted from border for each new window
       add = 5
       -- not implemented yet
       mult = 0
       -- absolute minimum border
       min = 1

myLayouts = (renamed [Replace "Binary"] $ {- gaps $ -} emptyBSP)
            ||| (renamed [Replace "Full"] $ Full)

data DmenuPosition = Bottom | Top
instance Show DmenuPosition where
  show Bottom = "-b"
  show _ = ""

data DmenuFlag = Prompt | NormB | NormF | SelectedB | SelectedF | Font
instance Show DmenuFlag where
  show Prompt = "-p"
  show NormB = "-nb"
  show NormF = "-nf"
  show SelectedB = "-sb"
  show SelectedF = "-sf"
  show Font = "-fn"

-- TODO improve. also, what did you gain out of this?
data DmenuOption = DmenuOption DmenuFlag String | DmenuPos DmenuPosition
instance Show (DmenuOption) where
  show (DmenuPos Bottom) = show Bottom
  show (DmenuOption flag val) = unwords [show flag, val]

selectedForeground, selectedBackground, normalForeground, normalBackground :: String
selectedForeground = normalForeground
selectedBackground = quote accentColor
normalForeground = quote "#eee"
normalBackground = quote "#222"

quote :: String -> String
quote s = concat ["'", s, "'"]

dmenu :: String
dmenu = "dmenu_run " ++ (unwords . map show)
        [ DmenuPos Bottom
        , DmenuOption Prompt "'>'"
        , DmenuOption NormB normalBackground
        , DmenuOption NormF normalForeground
        , DmenuOption SelectedB selectedBackground
        , DmenuOption SelectedF selectedForeground
        ]

myKeys = [ ("M-p", spawn dmenu)
         , ("M-[", sendMessage $ LessSpacing)
         , ("M-]", sendMessage $ MoreSpacing)
--	 , ("M-M1-<Left>",    sendMessage $ ExpandTowards L)
         , ("M-M1-h",    sendMessage $ ExpandTowards L)
--	 , ("M-M1-<Right>",   sendMessage $ ShrinkFrom L)
	 , ("M-M1-k",      sendMessage $ ExpandTowards U)
--	 , ("M-M1-<Down>",    sendMessage $ ShrinkFrom U)
--	 , ("M-M1-C-<Left>",  sendMessage $ ShrinkFrom R)
--	 , ("M-M1-C-<Right>", sendMessage $ ExpandTowards R)
         , ("M-M1-l",    sendMessage $ ExpandTowards R)
--	 , ("M-M1-C-<Up>",    sendMessage $ ShrinkFrom D)
--	 , ("M-M1-C-<Down>",  sendMessage $ ExpandTowards D)
         , ("M-M1-j",    sendMessage $ ExpandTowards D)
	 , ("M-s",            sendMessage $ Swap)
	 , ("M-r",         sendMessage $ Rotate)
	 , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 3")
	 , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 3")
	 , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
	 , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
	 , ("<XF86AudioMute>", spawn "amixer set Master toggle")
         , ("M-e", spawn "emacs")
--         , ("M-b", dynamicLogString myPP >>= \d-> spawn $ "(echo \"%{c}"++d++"\"; sleep 3) | lemonbar -g 1600x15+0+0 -d")
	 ]
