import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO

import XMonad.Layout.Renamed
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.EqualSpacing

-- Fullscreen (see http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#Watch_fullscreen_flash_video)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Hooks.ManageHelpers

main = do
    xmproc <- spawnPipe "/home/moonlight/.cabal/bin/xmobar /home/moonlight/.config/xmobarrc"
    xmonad $ defaultConfig
        { terminal = "urxvt"
        , modMask  = mod4Mask
--	, borderWidth = 0
	, normalBorderColor = "#090C19"
	, focusedBorderColor = "#1AAA13"
	, workspaces = myWorkspaces
        , manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
--        , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
	, layoutHook = myLayoutHook
        , logHook = dynamicLogWithPP defaultPP
                        { ppOutput = hPutStrLn xmproc
			, ppCurrent = xmobarColor "#1aaa13" ""
			, ppSep = " :: "
--			, ppOrder = \(wkspaces::layout::title:__) -> [ws,t] -- don't display layout
                        }
        }

myManageHook = composeAll [ isFullscreen --> doFullFloat]

myWorkspaces = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι"]

{- equalSpacing spaces can be adjusted on the fly, if you want to look into that
https://github.com/egasimus/xmonad-equalspacing
-}
myLayoutHook = smartBorders $ avoidStruts $ myLayouts

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

myLayouts = (renamed [Replace "Tall"] $ gaps $ tiled)
	  ||| (renamed [Replace "Wide"] $ gaps $ Mirror tiled)
	  ||| (renamed [Replace "Full"] $ Full)
	  ||| (renamed [Replace "Binary"] $ gaps $ emptyBSP)
	  where
	     -- default tiling algorithm partitions the screen into two panes
	     tiled = Tall nmaster delta ratio
	     -- The default number of windows in the master pane
             nmaster = 1
	     -- Default proportion of screen occupied by master pane
             ratio   = 1/2
     	     -- Percent of screen to increment by when resizing panes
	     delta   = 3/100

