import XMonad hiding ( (|||) )
import Control.Monad
import Data.Ratio ((%))
import Foreign.C (CChar)
import System.Exit
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.ByteString as B

 
-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import XMonad.Actions.FloatKeys
import XMonad.Actions.OnScreen
import XMonad.Actions.Promote
import XMonad.Actions.SpawnOn
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.UpdateFocus
 
-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceByPos
import XMonad.Hooks.UrgencyHook hiding (Never)
 
-- Layouts
import XMonad.Layout.Accordion
--import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
--import XMonad.Layout.TwoPane
--import XMonad.Layout.Tabbed
import qualified XMonad.Layout.ToggleLayouts as Tog
 
-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
 
-- Utils
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP, additionalKeys)
 
myBrowser            = "vivaldi-snapshot"
myTerminal           = "urxvtc"
myShell              = "zsh"
myFocusFollowsMouse  :: Bool
myFocusFollowsMouse  = True
myBorderWidth        = 1
myModMask            = mod4Mask
myWorkspaces         :: [String]
myWorkspaces         = [ "W", "M", "E", "F", "S", "V", "P", "J", "T" , "VM" , "IM", "XII"] 
myNormalBorderColor  = "#151515"
myFocusedBorderColor = "#9df"
myFont               = "xft:SonyEricssonLogo:size=10:antialias=true:hinting=true"
scratchPad = scratchpadSpawnActionTerminal "urxvtc -name scratchpad"
 
-- Key bindings.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
      --((0,                      0x1008ff13),     spawn "/usr/bin/pulseaudio-ctl up")
      ((0,                      0x1008ff13),     spawn "amixer set Master 1dB+")
    --, ((0,                      0x1008ff11),     spawn "/usr/bin/pulseaudio-ctl down")
    , ((0,                      0x1008ff11),     spawn "amixer set Master 1dB-")
    --, ((0,                      0x1008ff12),     spawn "/usr/bin/pulseaudio-ctl mute")
    , ((0,                      0x1008ff12),     spawn "amixer set Master toggle")
    , ((modm,                         0x60),     spawn "mpc toggle")
    , ((0,                      0x1008ff14),     spawn "mpc toggle")
    , ((0,                      0x1008ff15),     spawn "mpc stop")
    , ((0,                      0x1008ff16),     spawn "mpc prev")
    , ((0,                      0x1008ff17),     spawn "mpc next")
    , ((0,                      0x1008ff30),     spawn "transgui")
    , ((0,                      0x1008ff18),     spawn "vivaldi-snapshot")
    , ((0,                      0x1008ff19),     spawn "/usr/bin/thunderbird")
    , ((0,                      0x1008ff5d),     spawn "pcmanfm")
    , ((0       .|. shiftMask,  0x1008ff5d),     spawn "gksu pcmanfm")
    , ((0,                      0x1008ff1d),     spawn "galculator")
    , ((0,                      0x1008ff2f),     spawn "slock")
    , ((0,                      0x1008ff81),     spawn "urxvtc -name ncmpcpp -e /usr/bin/ncmpcpp")
    , ((0,                      0x1008ff1b),     spawn "urxvtc -name htop -e /usr/bin/htop")
    , ((0,                      0x1008ff73),     spawn "compreboot")
    , ((0,                          0xff69),     spawn "compdown")
    , ((0,                          0xffc9),     scratchPad)                                                                                          --F12
    , ((modm,                       0xffc9),     spawn "urxvtc -name term")                                                                           --Meta+F12
    , ((0,                          0xff61),     spawn "scrot -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'")                                        --Print
    , ((mod1Mask,                   0xff61),     spawn "scrot -s -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'")                                     --Alt+Print
	--, ((mod1Mask,                     0x61),     spawn "xterm")                                                                                       --Alt+A
    --, ((mod1Mask,                     0x62),     spawn "baobab")                                                                                      --Alt+B
    --, ((mod1Mask,                     0x63),     spawn "chromium")                                                                                    --Alt+C
    --, ((mod1Mask,                     0x64),     spawn "dmenu_run -i -p 'Run:' -sb '#333' -nf '#999' -sf '#9df' -fn 'Terminus Re33:size=12'")         --Alt+D
    --, ((mod1Mask,                     0x65),     spawn "pulseaudio-equalizer-gtk")                                                                    --Alt+E
    , ((mod1Mask,                     0x66),     spawn "fox")                                                                                         --Alt+F
    , ((mod1Mask,                     0x67),     spawn "gimp")                                                                                        --Alt+G
    --, ((mod1Mask .|. shiftMask,       0x67),     spawn "python3 /home/haron/lib/gis-weather/gis-weather.py")                                          --Alt+Shift+G
    --, ((mod1Mask,                     0x68),     spawn "hexchat")                                                                                     --Alt+H
    --, ((mod1Mask,                     0x69),     spawn "iron")                                                                                        --Alt+I
    , ((mod1Mask,                     0x6c),     spawn "cat /home/haron/Documents/last.pass | cut -c 1-24 | xclip -selection clipboard")              --Alt+L
    , ((mod1Mask,                     0x6d),     spawn "urxvtc -name mc -e mc")                                                                       --Alt+M
    , ((mod1Mask,                     0x6f),     spawn "opera-developer")                                                                             --Alt+O
    , ((mod1Mask,                     0x70),     spawn "wine /home/haron/lib/Pro100-5.20-GIV/PRO100.exe")                                             --Alt+P
	, ((mod1Mask,                     0x72),     spawn "urxvtc -name ranger -e ranger")                                                               --Alt+R
    , ((mod1Mask .|. controlMask,     0x72),     spawn "urxvtc -name home -e ranger")                                                                 --Alt+Ctrl+R
    , ((mod1Mask,                     0x73),     spawn "subl3")                                                                                       --Alt+S
    , ((mod1Mask .|. shiftMask,       0x73),     spawn "gksu subl3")                                                                                  --Alt+Shift+S
    , ((mod1Mask,                     0x74),     spawn "telegram")                                                                                    --Alt+T
	, ((mod1Mask .|. shiftMask,       0x74),     spawn "tor-browser")                                                                                 --Alt+Shift+T
    , ((mod1Mask,                     0x76),     spawn "urxvtc -name vim -e vim")                                                                     --Alt+V
    , ((mod1Mask,                     0x77),     spawn "gksu pacmanxg")                                                                               --Alt+W
    , ((modm,                         0x71),     spawn "xmonad --recompile && xmonad --restart")                                                      --Win+Q
    , ((modm,                         0x70),     spawn "gmrun")                                                                                       --Wim+P
    , ((modm     .|. shiftMask,  xK_Return),     spawn $ XMonad.terminal conf)                                                                        --Win+Shift+Enter
 
    , ((mod1Mask,                   0xffbe),     manPrompt myXPConfig)                                                                                --Alt+F1
    , ((mod1Mask,                   0xffbf),     shellPrompt myXPConfig)                                                                              --Alt+F2
    , ((mod1Mask,                   0xffc0),     runOrRaisePrompt myXPConfig)                                                                         --Alt+F3
    , ((modm,                       xK_Tab),     windows W.focusDown)                                                                                 --Alt+Tab
    , ((mod1Mask,                   xK_Tab),     windows W.focusMaster)                                                                               --Win+Tab
    , ((modm,                         0x63),     kill)                                                                                                --Win+C
    , ((modm,                     xK_space),     sendMessage NextLayout)
    , ((modm     .|. shiftMask,   xK_space),     setLayout $ XMonad.layoutHook conf)
    , ((modm,                         0x6e),     refresh)                                                                                             --Win+N
    , ((modm,                    xK_Escape),     toggleWS' ["NSP"])                                                                                   --Win+Escape
    , ((modm,                         0x6a),     windows W.focusDown)                                                                                 --Win+J
    , ((modm,                         0x6b),     windows W.focusUp)                                                                                   --Win+K
    , ((modm,                         0x6d),     windows W.focusMaster)                                                                               --Win+M
    , ((modm,                    xK_Return),     windows W.swapMaster)                                                                                --Win+Enter
    , ((modm     .|. shiftMask,       0x6a),     windows W.swapDown)                                                                                  --Win+Shift+J
    , ((modm     .|. shiftMask,       0x6b),     windows W.swapUp)                                                                                    --Win+Shift+K
    , ((modm,                         0x68),     sendMessage Shrink)                                                                                  --Win+H
    , ((modm,                         0x6c),     sendMessage Expand)                                                                                  --Win+L
    , ((modm,                         0x74),     withFocused $ windows . W.sink)                                                                      --Win+T
    , ((modm,                     xK_comma),     sendMessage (IncMasterN 1))
    , ((modm,                    xK_period),     sendMessage (IncMasterN (-1)))
    , ((modm,                         0x62),     sendMessage ToggleStruts)                                                                            --Win+B
    , ((modm     .|. shiftMask,       0x71),     io (exitWith ExitSuccess))                                                                           --Win+Shift+Q
    ]
    ++
 
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
 
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
 
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
 
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]
 
-- Layouts:
myLayout =  avoidStruts
            $ Tog.toggleLayouts (noBorders Full) 
            $ smartBorders
            $ onWorkspace "W"  (Full  ||| tiled ||| GridRatio 1)
            $ onWorkspace "M"  (tiled ||| Full)
            $ onWorkspace "E"  (Full  ||| tiled ||| GridRatio 1) 
            $ onWorkspace "F"  (Full  ||| tiled ||| Grid)
            $ onWorkspace "S"  (Full  ||| tiled ||| Mirror tiled)
            $ onWorkspace "V"  (Full  ||| tiled)
            $ onWorkspace "P"  (Full  ||| tiled ||| Grid)
            $ onWorkspace "J"  (Full  ||| Grid)
            $ onWorkspace "T"  (Full  ||| Accordion ||| (Mirror $ Accordion))
            $ onWorkspace "VM" (Full  ||| Grid)
            $ onWorkspace "IM" (smartSpacing 2 $ withIM 0.17 (ClassName "psi") (GridRatio 1))
            $ onWorkspace "XII"(Grid  ||| tiled ||| Mirror tiled)
            $ tiled ||| Mirror tiled  ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 0.5
    delta   = 0.01
 
-- IMLayout
myIMLayout = withIM (1%7) psi Grid
    where
      psi   = And (ClassName "psi") (Role "main")
 
-- XP
myXPConfig = defaultXPConfig {
          font              = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
        , bgColor           = "#151515"
        , fgColor           = "#999"
        , bgHLight          = "#151515"
        , fgHLight          = "#9df"
        , promptBorderWidth = 0
        , position          = Top
        , height            = 18
        , alwaysHighlight   = True
        , historySize       = 100
    }
 
-- Window rules:
myManageHook = composeAll . concat $
    [ 
      [className =? c             --> doF (W.shift "W")            | c <- myWeb]
    , [className =? c             --> doF (W.shift "M")            | c <- myMail]
    , [className =? c             --> doF (W.shift "E")            | c <- myEdit]
    , [className =? c             --> doF (W.shift "F")            | c <- myFile]
    , [className =? c             --> doF (W.shift "S")            | c <- mySystem]
    , [className =? c             --> doF (W.shift "V")            | c <- myVideo]
    , [className =? c             --> doF (W.shift "P")            | c <- myPic]
    , [className =? c             --> doF (W.shift "J")            | c <- myWork]
    , [className =? c             --> doF (W.shift "T")            | c <- myTorrent]
    , [className =? c             --> doF (W.shift "VM")           | c <- myVM]
    , [className =? c             --> doF (W.shift "IM")           | c <- myIM]
    , [appName   =? c             --> doF (W.shift "XII")          | c <- myTerm]
 
    , [className =? c             --> doCenterFloat                | c <- myFloatC]
    , [appName   =? a             --> doCenterFloat                | a <- myFloatA]
    , [title     =? t             --> doCenterFloat                | t <- myFloatT]
 
    , [title     =? "LibreOffice" --> doF (W.shift "E")]
 
    , [currentWs =? "W"           --> insertPosition Below Newer]
 
    , [currentWs =? "M"           --> insertPosition Below Newer]

    , [currentWs =? "T"           --> insertPosition Below Newer]

    , [currentWs =? "E"           --> insertPosition Below Newer]
 
    , [currentWs =? "VM"          --> insertPosition Below Newer]
 
    , [currentWs =? "XII"         --> insertPosition Below Newer]

    , [resource  =? "stalonetray" --> doIgnore]
 
    , [isDialog                   --> doCenterFloat]

    , [isFullscreen               --> doFullFloat]

	, [stringProperty "WM_WINDOW_ROLE" =? "About"       --> doCenterFloat]

    , [transience']
 
    , [manageDocks]
 
    ]
    where
    myWeb     = ["Firefox","Chromium","Opera","Opera developer","Iron","Tor Browser","vivaldi-snapshot"]
    myMail    = ["Thunderbird"]
    myEdit    = ["Subl3","Et","Wps","Wpp","Acroread","FoxitReader"]
    myFile    = ["Pcmanfm"]
    mySystem  = ["pacmanxg","systemdx","GParted","Sysinfo","PkgBrowser","Systemadm","Tk","Zenmap","Xfce4-power-manager-settings"]
    myVideo   = ["mpv","Vlc","Sopcast-player.py","Cheese","PornTime","Deadbeef"]
    myPic     = ["Pinta","Gimp","Gimp-2.8","Inkscape"]
    myWork    = ["Wine"]
    myTorrent = ["Tixati","Transgui","Transmission-gtk","Transmission-remote-gtk"]
    myVM      = ["VirtualBox"]
    myIM      = ["Hexchat","psi","Psi","Viber","Telegram"]
    myTerm    = ["term","TMUX"]
    -- CenterFloat
    myFloatC  = ["Galculator","Shutter","Zenity","Nvidia-settings","Pulseaudio-equalizer.py","Gnome-alsamixer","Gsmartcontrol","Gis-weather.py"]
    myFloatA  = ["lxappearance","xarchiver","gmrun"]
    myFloatT  = ["Software Update"]
 
-- Event handling
myEventHook = fullscreenEventHook <+> docksEventHook
 
-- Status bars and logging.
myLogHook = dynamicLogString $ xmobarPP {
          ppCurrent         = xmobarColor "#9fdfff" ""
        , ppTitle           = xmobarColor "#999" "". shorten 60
        }
 
-- Startup hook
myStartupHook = return () <+> adjustEventInput <+> setWMName "LG3D"

mynameScratchpads = [ NS "ncmpcpp" "urxvtc -name ncmpcpp -e ncmpcpp" (appName =? "ncmpcpp") (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)
                , NS "htop" "urxvtc -name htop -e htop" (appName =? "htop") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                , NS "mc" "urxvtc -name mc -e mc" (appName =? "mc") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                , NS "gpick" "gpick" (appName =? "gpick") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                , NS "ranger" "urxvtc -name ranger -e ranger" (appName =? "ranger") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                , NS "equaliser" "urxvtc -name equaliser -e alsamixer -D equal" (appName =? "equaliser") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                , NS "pavucontrol" "pavucontrol" (appName =? "pavucontrol") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                , NS "feh" "feh" (className =? "feh") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                , NS "Mirage" "mirage" (className =? "Mirage") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                , NS "font-manager" "font-manager" (className =? "Font-manager") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                --, NS "Telegram" "telegram" (className =? "Telegram") (customFloating $ W.RationalRect 0.8 0.02 0.2 0.98)
				, NS "page-info" "page-info " (stringProperty "WM_WINDOW_ROLE" =? "page-info") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
				, NS "Organizer" "Organizer" (stringProperty "WM_WINDOW_ROLE" =? "Organizer") (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
				]
 
-- Scratchpad
--
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)

  where

    h = 0.333   -- terminal height
    w = 1       -- terminal width
    t = 1 - h   -- distance from top edge
    l = 1 - w   -- distance from left edge
 
main = do
    xmonad =<< xmobar defaults
encodeCChar :: B.ByteString -> [CChar]
encodeCChar = map fromIntegral . B.unpack
 
defaults = ewmh $ withUrgencyHookC  NoUrgencyHook urgencyConfig { suppressWhen = Focused } defaultConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = avoidStruts $ myLayout,
        manageHook         = manageHook defaultConfig <+> manageDocks <+> myManageHook <+> manageScratchPad <+> namedScratchpadManageHook mynameScratchpads <+> placeHook (smart (0.5,0.5)) <+> workspaceByPos ,
        handleEventHook    = myEventHook,
        logHook            = myLogHook >>= xmonadPropLog,
        startupHook        = myStartupHook
    }

