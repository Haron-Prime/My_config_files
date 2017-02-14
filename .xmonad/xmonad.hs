-- Base
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
import XMonad.Actions.CycleSelectedLayouts
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import XMonad.Actions.UpdateFocus
import XMonad.Actions.GridSelect
import XMonad.Actions.WorkspaceNames

-- Hooks
import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceByPos
import XMonad.Hooks.UrgencyHook hiding (Never)
import XMonad.Hooks.XPropManage

-- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.ResizableTile
-- import XMonad.Layout.TwoPane
-- import XMonad.Layout.Tabbed
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
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare

browserClass         = "Vivaldi-snapshot"
myBrowser            = "vivaldi-snapshot"
myTerminal           = "urxvtc"
terminalClass        = "URxvt"
myShell              = "zsh"
myFocusFollowsMouse  :: Bool
myFocusFollowsMouse  = True
myBorderWidth        = 1
myModMask            = mod4Mask
myWorkspaces         :: [String]
myWorkspaces         = [ "W", "M", "E", "F", "S", "V", "P", "J", "T" , "X" , "XI" , "XII"] 
myNormalBorderColor  = "#454545"
myFocusedBorderColor = "#9df"
myFont               = "xft:SonyEricssonLogo:size=10:antialias=true:hinting=true"
scratchPad = scratchpadSpawnActionTerminal "urxvtc -name scratchpad"

-- Key bindings.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
      ((0,                      0x1008ff13),     spawn "/usr/bin/pulseaudio-ctl up")
      --((0,                      0x1008ff13),     spawn "amixer set Master 5%+")
    , ((0,                      0x1008ff11),     spawn "/usr/bin/pulseaudio-ctl down")
    --, ((0,                      0x1008ff11),     spawn "amixer set Master 5%-")
    , ((0,                      0x1008ff12),     spawn "/usr/bin/pulseaudio-ctl mute")
    --, ((0,                      0x1008ff12),     spawn "amixer set Master toggle")
    , ((modm,                         0x60),     spawn "mpc toggle")
    , ((0,                      0x1008ff14),     spawn "mpc toggle")
    , ((0,                      0x1008ff15),     spawn "mpc stop")
    , ((0,                      0x1008ff16),     spawn "mpc prev")
    , ((0,                      0x1008ff17),     spawn "mpc next")
    , ((0,                      0x1008ff30),     spawn "if(pidof transgui >/dev/null); then kill $(pidof transgui); else transgui; fi")
    , ((0,                      0x1008ff18),     spawn "vivaldi-snapshot")
    , ((0,                      0x1008ff19),     spawn "if(pidof thunderbird >/dev/null); then kill $(pidof thunderbird); else thunderbird; fi")
    , ((0,                      0x1008ff5d),     spawn "pcmanfm")
    , ((0       .|. shiftMask,  0x1008ff5d),     spawn "gksu pcmanfm")
    , ((0,                      0x1008ff1d),     spawn "if(pidof galculator >/dev/null); then kill $(pidof galculator); else galculator; fi")
    , ((0,                      0x1008ff2f),     spawn "i3lock -i /home/haron/wall/starrynight.png")
    , ((0,                      0x1008ff81),     spawn "if(pidof ncmpcpp >/dev/null); then kill $(pidof ncmpcpp); else urxvtc -name ncmpcpp -e /usr/bin/ncmpcpp; fi")
    , ((0,                      0x1008ff1b),     spawn "if(pidof htop >/dev/null); then kill $(pidof htop); else urxvtc -name htop -e /usr/bin/htop; fi")
    , ((0,                      0x1008ff73),     spawn "compreboot")
    , ((0,                          0xff69),     spawn "compdown")
    , ((0,                          0xff67),     spawn "gmrun")
    , ((0,                          0xffc9),     scratchPad)                                                                                          --F12
    , ((0,                          0xff61),     spawn "scrot -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'")                                        --Print
    , ((mod1Mask,                   0xff61),     spawn "scrot -s -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'")                                     --Mod1+Print
    , ((mod1Mask,                     0x61),     spawn "shutter")                                                                                     --Mod1+A
    , ((mod1Mask,                     0x64),     spawn "deadbeef")                                                                                    --Mod1+D
    , ((mod1Mask,                     0x65),     spawn "pulseaudio-equalizer-gtk")                                                                    --Mod1+E
    , ((mod1Mask,                     0x66),     spawn "firefox")                                                                                     --Mod1+F
    , ((mod1Mask,                     0x67),     spawn "gimp")                                                                                        --Mod1+G
    , ((mod1Mask,                     0x68),     spawn "hexchat")                                                                                     --Mod1+H
    -- , ((mod1Mask,                     0x69),     spawn "2D-Place")                                                                                    --Mod1+I
    , ((mod1Mask,                     0x6c),     spawn "cat /home/haron/Documents/last.pass | cut -c 1-24 | xclip -selection clipboard")              --Mod1+L
    , ((mod1Mask,                     0x6d),     spawn "urxvtc -name mc -e mc")                                                                       --Mod1+M
    , ((mod1Mask,                     0x6f),     spawn "opera12")                                                                                     --Mod1+O
    -- , ((mod1Mask,                     0x70),     spawn "PRO100-5")                                                                                    --Mod1+P
    , ((mod1Mask,                     0x71),     spawn "if(pidof pavucontrol >/dev/null); then kill $(pidof pavucontrol); else pavucontrol; fi")      --Mod1+Q
    , ((mod1Mask,                     0x72),     spawn "urxvtc -name ranger -e ranger")                                                               --Mod1+R
    , ((mod1Mask,                     0x73),     spawn "subl3")                                                                                       --Mod1+S
    , ((mod1Mask .|. shiftMask,       0x73),     spawn "gksu subl3")                                                                                  --Mod1+Shift+S
    , ((mod1Mask,                     0x74),     spawn "tor-browser")                                                                                 --Mod1+T
    , ((mod1Mask,                     0x76),     spawn "urxvtc -name vim -e vim")                                                                     --Mod1+V
    -- , ((mod1Mask,                     0x77),     spawn "gksu pacmanxg")                                                                               --Mod1+W
    , ((modm,                         0x71),     spawn "xmonad --recompile && xmonad --restart && killall xmobar && xmobar")                          --Mod4+Q
    , ((modm,                         0x78),     spawn "killall xmobar && xmobar")                                                                    --Mod4+X
    -- , ((modm,                         0x70),     spawn "gmrun")                                                                                       --Mod4+P
    , ((modm     .|. shiftMask,  xK_Return),     spawn $ XMonad.terminal conf)                                                                        --Mod4+Shift+Enter

    , ((mod1Mask,                   0xffbe),     manPrompt myXPConfig)                                                                                --Mod1+F1
    , ((mod1Mask,                   0xffbf),     shellPrompt myXPConfig)                                                                              --Mod1+F2
    , ((mod1Mask,                   0xffc0),     runOrRaisePrompt myXPConfig)                                                                         --Mod1+F3
    -- , ((mod1Mask,                   0xffc1),     spawn "dmenu_run_history -i -p 'Run:' -sb '#333' -nf '#999' -sf '#9df' -fn 'Terminus Re33:size=12'") --Mod1+F4
    , ((modm,                      xK_Down),     windows W.focusDown)                                                                                 --Mod4+Down
    , ((modm,                        xK_Up),     windows W.focusUp)                                                                                   --Mod4+Up
    , ((modm,                       xK_Tab),     windows W.focusDown)                                                                                 --Mod4+Tab
    , ((mod1Mask,                   xK_Tab),     windows W.focusMaster)                                                                               --Mod1+Tab
    , ((mod1Mask,                xK_Escape),     rotOpposite)                                                                                         --Mod1+Escape
    , ((modm,                         0x78),     kill)                                                                                                --Mod4+X
    , ((modm,                     xK_space),     sendMessage NextLayout)                                                                              --Mod4+Space
    , ((modm     .|. shiftMask,   xK_space),     setLayout $ XMonad.layoutHook conf)                                                                  --Mod4+Shift+Space
    , ((modm,                         0x6e),     refresh)                                                                                             --Mod4+N
    , ((modm,                    xK_Escape),     toggleWS' ["NSP"])                                                                                   --Mod4+Escape
    , ((modm,                       0xff08),     toggleWS' ["NSP"])                                                                                   --Mod4+Backspace
    , ((modm,                     xK_Right),     nextWS)                                                                                              --Mod4+Right
    , ((modm,                      xK_Left),     prevWS)                                                                                              --Mod4+Left
    , ((modm     .|. shiftMask,   xK_Right),     shiftToNext)                                                                                         --Mod4+Shift+Right
    , ((modm     .|. shiftMask,    xK_Left),     shiftToPrev)                                                                                         --Mod4+Shift+Left
    , ((modm,                         0x6a),     windows W.focusDown)                                                                                 --Mod4+J
    , ((modm,                         0x6b),     windows W.focusUp)                                                                                   --Mod4+K
    , ((modm,                         0x6d),     windows W.focusMaster)                                                                               --Mod4+M
    , ((modm,                    xK_Return),     windows W.swapMaster)                                                                                --Mod4+Enter
    , ((modm     .|. shiftMask,       0x6a),     windows W.swapDown)                                                                                  --Mod4+Shift+J
    , ((modm     .|. shiftMask,       0x6b),     windows W.swapUp)                                                                                    --Mod4+Shift+K
    , ((modm,                         0x68),     sendMessage Shrink)                                                                                  --Mod4+H
    , ((modm,                         0x6c),     sendMessage Expand)                                                                                  --Mod4+L
    , ((modm,                         0x74),     withFocused $ windows . W.sink)                                                                      --Mod4+T
    , ((modm,                     xK_comma),     sendMessage (IncMasterN 1))                                                                          --Mod4+Comma
    , ((modm,                    xK_period),     sendMessage (IncMasterN (-1)))                                                                       --Mod4+Period
    , ((modm,                         0x62),     sendMessage ToggleStruts)                                                                            --Mod4+B
    , ((modm     .|. shiftMask,       0x71),     io (exitWith ExitSuccess))                                                                           --Mod4+Shift+Q
    , ((modm,                         0x73),     sendMessage MirrorShrink)                                                                            --Mod4+S
    , ((modm,                         0x7a),     sendMessage MirrorExpand)                                                                            --Mod4+Z
    , ((modm,                         0x76),     goToSelected  def { gs_cellheight = 30, gs_cellwidth = 155 })                                        --Mod4+V
    , ((modm,                         0x61),     spawnSelected def { gs_cellheight = 30, gs_cellwidth = 155 } [
                                                                                                                           "PRO100-5"
                                                                                                                          ,"2D-Place"
                                                                                                                          ,"wps"
                                                                                                                          ,"soffice"
                                                                                                                          ,"tixati"
                                                                                                                          ,"gsmartcontrol"
                                                                                                                          ,"systemdx"
                                                                                                                          ,"systemadm"
                                                                                                                          ,"lxappearance"
                                                                                                                          ,"telegram-desktop"
                                                                                                                          ,"TV"
                                                                                                                          ,"vlc"
                                                                                                                          ,"heroes3"
                                                                                                                          ])                          --Mod4+A

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
            $ onWorkspace "W"  (Full  ||| ResizableTall 1 (3/100) (1/2) [] ||| Mirror tiled ||| mouseResizableTile)
            $ onWorkspace "M"  (mouseResizableTile ||| Full)
            $ onWorkspace "E"  (ResizableTall 1 (3/100) (1/2) [] ||| mouseResizableTile ||| Mirror tiled ||| Full) 
            $ onWorkspace "F"  (ResizableTall 1 (3/100) (1/2) [] ||| mouseResizableTile ||| Mirror tiled ||| Full)
            $ onWorkspace "S"  (ResizableTall 1 (3/100) (1/2) [] ||| mouseResizableTile ||| Mirror tiled ||| Full)
            $ onWorkspace "V"  (Full  ||| mouseResizableTile)
            $ onWorkspace "P"  (ResizableTall 1 (3/100) (1/2) [] ||| mouseResizableTile ||| Mirror tiled ||| Full)
            $ onWorkspace "J"  (Full  ||| Grid)
            $ onWorkspace "T"  (Full  ||| mouseResizableTile)
            $ onWorkspace "X"  (ResizableTall 1 (3/100) (1/2) [] ||| mouseResizableTile ||| Mirror tiled ||| Full)
            $ onWorkspace "XI" (smartSpacing 2 $ withIM 0.17 (ClassName "psi") (GridRatio 1))
            $ onWorkspace "XII"(ResizableTall 1 (3/100) (1/2) [] ||| mouseResizableTile ||| Mirror tiled ||| Full)
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
myXPConfig = def {
          font              = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
        , bgColor           = "#151515"
        , fgColor           = "#959595"
        , bgHLight          = "#151515"
        , fgHLight          = "#9df"
        , promptBorderWidth = 0
        , position          = Top
        , height            = 20
        , alwaysHighlight   = True
        , historySize       = 100
    }

-- Windows rules:
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
    , [className =? c             --> doF (W.shift "X")            | c <- myVM]
    , [className =? c             --> doF (W.shift "XI")           | c <- myIM]
    , [appName   =? c             --> doF (W.shift "XII")          | c <- myTerm]

    , [className =? c             --> doCenterFloat                | c <- myFloatC]
    , [appName   =? a             --> doCenterFloat                | a <- myFloatA]
    , [title     =? t             --> doCenterFloat                | t <- myFloatT]
    , [role      =? r             --> doCenterFloat                | r <- myFloatR]

    -- , [title     =? "LibreOffice" --> doF (W.shift "E")]

    , [currentWs =? "W"           --> insertPosition Below Newer]

    , [currentWs =? "M"           --> insertPosition Below Newer]

    , [currentWs =? "E"           --> insertPosition Below Newer]

    , [currentWs =? "F"           --> insertPosition Below Newer]

    , [currentWs =? "T"           --> insertPosition Below Newer]

    , [currentWs =? "X"           --> insertPosition Below Newer]

    , [currentWs =? "XII"         --> insertPosition Below Newer]

    , [resource  =? "stalonetray" --> doIgnore]

    , [isDialog                   --> doCenterFloat]

    , [isFullscreen               --> doFullFloat]

    , [transience']

    , [manageDocks]

    ]
    where
    myWeb     = ["Firefox","Opera","Tor Browser","Vivaldi-snapshot"]
    myMail    = ["Thunderbird"]
    myEdit    = ["Subl3","Et","Wps","Wpp","Acroread","FoxitReader"]
    myFile    = ["Pcmanfm","Thunar"]
    mySystem  = ["pacmanxg","systemdx","GParted","Sysinfo","PkgBrowser","Systemadm","Tk","Zenmap","Xfce4-power-manager-settings"]
    myVideo   = ["mpv","Vlc","Sopcast-player.py","Cheese","Easytag"]
    myPic     = ["Gimp","Gimp-2.8","Inkscape"]
    myWork    = ["Wine"]
    myTorrent = ["Tixati","Transgui","Transmission-gtk","Transmission-remote-gtk"]
    myVM      = ["VirtualBox"]
    myIM      = ["Hexchat","psi","Psi","Viber","Telegram"]
    myTerm    = ["term","TMUX","termux"]

    -- CenterFloat

    myFloatC  = ["Galculator","Shutter","Zenity","Nvidia-settings","Pulseaudio-equalizer.py","Gnome-alsamixer","Gsmartcontrol","Gis-weather.py","feh"]
    myFloatA  = ["lxappearance","xarchiver","gmrun"]
    myFloatT  = ["Software Update"]
    myFloatR  = ["task_dialog","messages","pop-up","^conversation$","About"]

    role      = stringProperty "WM_WINDOW_ROLE"

-- Event handling
myEventHook = fullscreenEventHook <+> docksEventHook <+> focusOnMouseMove
 
-- Status bars and logging.
myLogHook = do
    currentWorkspaceOnTop
    dynamicLogString $ xmobarPP {
          ppCurrent         = xmobarColor "#9fdfff" ""
        -- , ppTitle           = xmobarColor "#959595" "" . shorten 38
        , ppTitle           = (\str -> "")
        -- , ppTitle           = xmobarColor "#959595" ""
        }

-- Startup hook
myStartupHook = return () <+> adjustEventInput <+> setWMName "LG3D"

mynameScratchpads = [ NS "ncmpcpp" "urxvtc -name ncmpcpp -e ncmpcpp" (appName =? "ncmpcpp") (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)
                , NS "htop" "urxvtc -name htop -e htop" (appName =? "htop") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                -- , NS "mc" "urxvtc -name mc -e mc" (appName =? "mc") (customFloating $ W.RationalRect 0.02 0.02 0.96 0.96)
                , NS "gpick" "gpick" (appName =? "gpick") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                -- , NS "ranger" "urxvtc -name ranger -e ranger" (appName =? "ranger") (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
                -- , NS "equaliser" "urxvtc -name equaliser -e alsamixer -D equal" (appName =? "equaliser") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                , NS "pavucontrol" "pavucontrol" (appName =? "pavucontrol") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                -- , NS "feh" "feh" (className =? "feh") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                , NS "Mirage" "mirage" (className =? "Mirage") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                , NS "font-manager" "font-manager" (className =? "Font-manager") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                -- , NS "Telegram" "telegram" (className =? "Telegram") (customFloating $ W.RationalRect 0.8 0.02 0.2 0.98)
                -- , NS "page-info" "page-info " (stringProperty "WM_WINDOW_ROLE" =? "page-info") (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
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
    xmonad =<< xmobar myConfig
encodeCChar :: B.ByteString -> [CChar]
encodeCChar = map fromIntegral . B.unpack

myConfig = ewmh $ withUrgencyHookC  NoUrgencyHook urgencyConfig { suppressWhen = Focused } def {
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
        manageHook         = manageHook def <+> manageDocks <+> myManageHook <+> manageScratchPad <+> namedScratchpadManageHook mynameScratchpads <+> placeHook (smart (0.5,0.5)) <+> workspaceByPos ,
        handleEventHook    = myEventHook,
        logHook            = myLogHook >>= xmonadPropLog,
        startupHook        = myStartupHook
    }
