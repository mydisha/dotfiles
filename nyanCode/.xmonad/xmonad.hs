import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeys)
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified GHC.IO.Handle.Types as H

nyanLogHook h = do
	dynamicLogWithPP $ tryPP h

tryPP :: Handle -> PP
tryPP h = defaultPP
	{ ppOutput = hPutStrLn h
	, ppCurrent	    = wrap "^fg(#ffffff)" "^fg()"
	, ppVisible	    = wrap "^fg(#cacaca)" "^fg()"
	, ppHidden	    = wrap "^fg(#cacaca)" "^fg()"
	, ppHiddenNoWindows = wrap "^fg(#cacaca)" "^fg()"
	, ppWsSep	    = ""
	, ppSep		    = ""
    , ppLayout = wrap "^bg(#1166aa)^ca(1,xdotool key alt+space)" "^ca()^bg()" . pad . wrap space space .
    ( \t -> case t of
    "Spacing 4 Grid"       -> dir_icon ++ "grid.xbm)"
    "Spacing 4 Tall"       -> dir_icon ++ "sptall.xbm)"
    "Mirror Spacing 4 Tall"    -> dir_icon ++ "mptall.xbm)"
    "Full"              -> dir_icon ++ "full.xbm)"
    )
	, ppOrder  = \(ws:l:t:_) -> [l,ws]
	}

nyanWorkspace :: [String]
nyanWorkspace = clickable $ [ "^bg(#1177aa)"++ space ++"  Dji  "++ space ++"^bg()"
	, "^bg(#1188aa)"++ space ++" Sam"++ space ++"^bg()"
	, "^bg(#1199aa)"++ space ++" Soe  "++ space ++"^bg()"
	]
	where clickable l = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
		(i,ws) <- zip [1..] l,
		let n = i ]

nyanKeys = [ ((mod1Mask, xK_p), spawn dmenu)
	, ((mod1Mask, xK_q), spawn "killall dzen2; xmonad --recompile; xmonad --restart")]

nLayout = avoidStruts $ smartBorders (  sTall ||| Mirror sTall ||| sGrid ||| Full )
	where
	 sTall = gaps [(U,20), (D,20), (L,40), (R,40)] $ spacing 4 $ Tall 1 (3/100) (1/2)
	 sGrid = spacing 4 $ Grid

nyanDocks = composeAll
	[ className =? "Gimp" --> doFloat
	, className =? "mpv" --> doFullFloat
	, className =? "Viewnior" --> doFullFloat
	, className =? "feh" --> doFullFloat
	]

main = do
	workspacePanel <- spawnPipe wBar
	infoPanel <- spawnPipe iBar
	xmonad $ defaultConfig
	 { manageHook = nyanDocks <+> manageDocks <+> manageHook defaultConfig
	 , layoutHook = nLayout
	 , modMask = mod1Mask
	 , workspaces = nyanWorkspace
	 , terminal = "urxvt"
	 , focusedBorderColor = "#efefef"
	 , normalBorderColor = col2
	 , borderWidth = 3
	 , startupHook = spawnOnce "sh /home/diarifa/.xmonad/assets/bin/autoload.sh" <+> setWMName "LG3D"
	 , logHook = nyanLogHook workspacePanel
	 } `additionalKeys` nyanKeys

space = "    "
col1 = "#efefef"
col2 = "#277a6d"
col3 = "#1188aa"
screenWidth="1366"
wBar = "dzen2 -p -ta l -e 'button3=' -fn 'Droid Sans Fallback-8:bold' -fg '" ++ col1 ++ "' -bg '" ++ col2 ++ "' -h 24 -w `expr "++ screenWidth ++" / 2 - 100`"
iBar = "sh /home/diarifa/.xmonad/assets/bin/panel-info.sh "++ screenWidth
dir_icon = "^ca(1,xdotool key alt+space)^i(/home/diarifa/.xmonad/assets/xbm/"
dmenu = "dmenu_run -b -h 24 -l 10 -w 500 -x 433 -y 234 -nb '"++ col2 ++"' -sb '" ++ col3 ++ "'"
