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

hPath = "/home/diarifa/"
clr1 = "#efefef"
clr2 = "#2d2d2d"
clr3 = "#C12B4D"
sWidth  = "1366"
sHeight = "768"
makeSpace = wrap "    " "    "

myLogHook h = do
    dynamicLogWithPP $ myPP h

myLogHook2 h = do
    dynamicLogWithPP $ myPP2 h

myPP :: Handle -> PP
myPP p = defaultPP
    { ppOutput             = hPutStrLn p
    , ppSep                = ""
    , ppTitle              = titleWrapper . makeSpace . shorten 111 . ( \t -> if t == [] then "Diuxion-A Nickzers Desktop" else t )
    , ppLayout             = buttonLayout . makeSpace .
                            ( \t -> case t of
                            "Spacing 10 Grid"           -> dir_icon ++ "grid.xbm)  Grid"
                            "Spacing 10 Tall"           -> dir_icon ++ "sptall.xbm)  Tile"
                            "Mirror Spacing 10 Tall"    -> dir_icon ++ "mptall.xbm)  mTile"
                            "Mirror Spacing 20 Tall"    -> dir_icon ++ "mptall.xbm)  rTile"
                            "Full"                      -> dir_icon ++ "full.xbm)  Full"
                            )
    , ppOrder  = \(ws:l:t:_) -> [l,t]
    }
    where
        titleWrapper = wrap "^fg(#C12B4D)^i(.xmonad/assets/xbm/mr1.xbm)^fg()" ""
        buttonLayout = wrap "^ca(1,xdotool key super+space)^bg(#C12B4D)" "^bg()^ca()"
        dir_icon     = "^ca(1,xdotool key alt+space)^i(.xmonad/assets/xbm/"


myPP2 :: Handle -> PP
myPP2 h = defaultPP
    { ppOutput             = hPutStrLn h
    , ppCurrent            = wrapCurrent . makeSpace
    , ppVisible            = wrapAnother . makeSpace
    , ppHidden             = wrapAnother . makeSpace
    , ppHiddenNoWindows    = wrapAnother . makeSpace
    , ppWsSep              = "  "
    , ppOrder  = \(ws:l:t:_) -> [ws]
    }
    where
        wrapCurrent  = wrap "^fg(#ffffff)^bg(#C12B4D)" "^bg()^fg()"
        wrapAnother  = wrap "^fg(#cacaca)^bg(#424242)" "^bg()^fg()"

myWorkspace :: [String]
myWorkspace = makeOnclick $ [ " TERM ", " WEB ", " CODE ", " OTHER "]
        where makeOnclick l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" | (i,ws) <- zip [1..] l, let n = i ]

myKeybinds = [ ((mod4Mask              , xK_p), spawn dmenu_run)
             , ((mod4Mask              , xK_q), spawn xmonad_restart)
             , ((mod4Mask .|. shiftMask, xK_q), spawn powermenu)
             ]
             where
                dmenu_run = hPath++".xmonad/assets/bin/dmenu-run.sh '"++sWidth ++"' '"++sHeight ++"' '"++clr2++"' '"++clr3++"'"
                powermenu = hPath++".xmonad/assets/bin/powermenu.sh '"++sWidth ++"' '"++sHeight ++"' '"++clr2++"' '"++clr3++"'"
                xmonad_restart = hPath++".xmonad/assets/bin/restart.sh"

myLayout = avoidStruts $ smartBorders ( sTall ||| Mirror mTall ||| sGrid ||| Full ||| Mirror ricing )
        where
            sTall = gaps [(U,20), (D,20), (L,50), (R,50)] $ spacing 10 $ Tall 1 (3/100) (1/2)
            mTall = gaps [(L,20), (R,20), (U,50), (D,50)] $ spacing 10 $ Tall 1 (3/100) (1/2)
            ricing = gaps [(L,90), (R,90), (U,50), (D,810)] $ spacing 20 $ Tall 1 (3/100) (1/2)
            sGrid = spacing 10 $ Grid

myDocks = composeAll
        	[ className =? "Gimp" --> doFloat
            , className =? "mpv" --> doFullFloat
            , className =? "Viewnior" --> doFullFloat
            , className =? "feh" --> doFullFloat
            ]


main = do
    bgPanel <- spawnPipe bgBar
    layoutPanel <- spawnPipe lBar
    infoPanel <- spawnPipe iBar
    workspacePanel <- spawnPipe wBar
    xmonad $ defaultConfig
     { manageHook = myDocks <+> manageDocks <+> manageHook defaultConfig
     , layoutHook = myLayout
     , modMask = mod4Mask
     , workspaces = myWorkspace
     , terminal = "urxvt"
     , focusedBorderColor = "#C12B4D"
     , normalBorderColor = "#424242"
     , borderWidth = 3
     , startupHook = spawnOnce autoload <+> setWMName "LG3D"
     , logHook = myLogHook layoutPanel <+> myLogHook2 workspacePanel
     } `additionalKeys` myKeybinds
     where
        autoload = hPath++"/.xmonad/assets/bin/autoload.sh"
        dzenArgs = "-p -e 'button3=' -fn 'Droid Sans Fallback-8:bold'"
        bgBar    = "echo '^fg(#C12B4D)^p(;+20)^r(1366x5)' | dzen2 "++dzenArgs++" -ta c -fg '" ++ clr1 ++ "' -bg '#2d2d2d' -h 35 -w "++sWidth
        lBar     = "sleep 0.1;dzen2 "++dzenArgs++" -ta l -fg '"++clr1++"' -bg '" ++clr2++"' -h 25 -w `expr "++sWidth++" / 2` -y 5"
        iBar     = "sleep 0.1; conky -c ~/.xmonad/assets/conky/info.conkyrc | dzen2 "++dzenArgs++" -ta r -fg '"++clr1++"' -bg '" ++clr2++"' -h 25 -w `expr "++sWidth++" / 2` -x `expr "++sWidth++" / 2` -y 5"
        wBar     = "sleep 0.3;dzen2 "++dzenArgs++" -ta c -fg '"++clr1++"' -bg '" ++clr2++"' -h 20 -w 300 -x `expr "++sWidth++" / 2 - 150` -y 6"
