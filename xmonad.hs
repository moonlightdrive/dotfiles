import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO

import XMonad.Layout.BinarySpacePartition

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

myWorkspaces = ["α", "β", "γ", "δ", "λ", "φ", "χ", "ψ", "ω"]

myLayoutHook = smartBorders $ avoidStruts $ myLayouts
myLayouts = emptyBSP