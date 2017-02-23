-- Base
import XMonad hiding ( (|||) )
import Control.Monad (liftM2)
import Data.Ratio ((%))
import Foreign.C (CChar)
import System.Exit
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.ByteString as B
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import XMonad.Actions.UpdateFocus
import XMonad.Actions.WorkspaceNames
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

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
import qualified XMonad.Layout.ToggleLayouts as Tog

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh

-- Utils
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP, additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare

myBrowser            = "vivaldi-snapshot"
browserClass         = "Vivaldi-snapshot"
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

    --Applications management

      ((0,                      0x1008ff13),     spawn "/usr/bin/pulseaudio-ctl up")
    , ((0,                      0x1008ff11),     spawn "/usr/bin/pulseaudio-ctl down")
    , ((0,                      0x1008ff12),     spawn "/usr/bin/pulseaudio-ctl mute")
    , ((modm,                         0x60),     spawn "mpc toggle")
    , ((0,                      0x1008ff14),     spawn "mpc toggle")
    , ((0,                      0x1008ff15),     spawn "mpc stop")
    , ((0,                      0x1008ff16),     spawn "mpc prev")
    , ((0,                      0x1008ff17),     spawn "mpc next")
    , ((0,                      0x1008ff30),     spawn "if(pidof transgui >/dev/null); then kill $(pidof transgui); else transgui; fi")
    , ((0,                      0x1008ff18),     spawn "vivaldi-snapshot")
    , ((0,                      0x1008ff19),     spawn "thunderbird-45")
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
    , ((0,                          0xff61),     spawn "scrot -q 100 -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'")                                 --Print
    , ((0        .|. shiftMask,     0xff61),     spawn "scrot -u -q 100 -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'")                              --Shift+Print
    , ((mod1Mask,                   0xff61),     spawn "scrot -s -q 100 -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'")                              --Alt+Print
    , ((mod1Mask,                     0x61),     spawn "shutter")                                                                                     --Alt+A
    , ((mod1Mask,                     0x64),     spawn "deadbeef")                                                                                    --Alt+D
    , ((mod1Mask,                     0x65),     spawn "pulseaudio-equalizer-gtk")                                                                    --Alt+E
    , ((mod1Mask,                     0x66),     spawn "firefox")                                                                                     --Alt+F
    , ((mod1Mask,                     0x67),     spawn "gimp")                                                                                        --Alt+G
    , ((mod1Mask,                     0x68),     spawn "hexchat")                                                                                     --Alt+H
    , ((mod1Mask,                     0x6c),     spawn "cat /home/haron/Documents/last.pass | cut -c 1-24 | xclip -selection clipboard")              --Alt+L
    , ((mod1Mask,                     0x6d),     spawn "urxvtc -name mc -e mc")                                                                       --Alt+M
    , ((mod1Mask,                     0x6f),     spawn "opera12")                                                                                     --Alt+O
    , ((mod1Mask,                     0x71),     spawn "sleep 1 && xset dpms force off")                                                              --Alt+Q
    , ((mod1Mask,                     0x72),     spawn "urxvtc -name ranger -e ranger")                                                               --Alt+R
    , ((mod1Mask,                     0x73),     spawn "subl3")                                                                                       --Alt+S
    , ((mod1Mask .|. shiftMask,       0x73),     spawn "gksu subl3")                                                                                  --Alt+Shift+S
    , ((mod1Mask,                     0x74),     spawn "tor-browser")                                                                                 --Alt+T
    , ((mod1Mask,                     0x76),     spawn "urxvtc -name vim -e vim")                                                                     --Alt+V
    , ((modm,                         0x71),     spawn "xmonad --recompile && xmonad --restart && killall xmobar && xmobar")                          --Win+Q
    , ((modm,                         0x78),     spawn "killall xmobar && xmobar")                                                                    --Win+X
    , ((modm     .|. shiftMask,     0xff0d),     spawn $ XMonad.terminal conf)                                                                        --Win+Shift+Enter

    --Prompt management

    , ((mod1Mask,                   0xffbe),     manPrompt myXPConfig)                                                                                --Alt+F1
    , ((mod1Mask,                   0xffbf),     shellPrompt myXPConfig)                                                                              --Alt+F2
    , ((mod1Mask,                   0xffc0),     runOrRaisePrompt myXPConfig)                                                                         --Alt+F3
    , ((mod1Mask,                   0xffc1),     sshPrompt myXPConfig)                                                                                --Alt+F4

    --Windows and WS management

    , ((mod1Mask,                   0xff09),     windows W.focusMaster)                                                                               --Alt+Tab
    , ((modm,                       0xff09),     rotOpposite)                                                                                         --Win+Tab
    , ((modm,                         0x78),     kill)                                                                                                --Win+X
    , ((modm,                         0x20),     sendMessage NextLayout)                                                                              --Win+Space
    , ((modm     .|. shiftMask,       0x20),     setLayout $ XMonad.layoutHook conf)                                                                  --Win+Shift+Space
    , ((modm,                         0x6e),     refresh)                                                                                             --Win+N
    , ((modm,                       0xff1b),     toggleWS' ["NSP"])                                                                                   --Win+Escape
    , ((modm,                       0xff08),     toggleWS' ["NSP"])                                                                                   --Win+Backspace
    , ((modm,                       0xff53),     DO.moveTo Next HiddenNonEmptyWS)                                                                     --Win+Right
    , ((modm,                       0xff51),     DO.moveTo Prev HiddenNonEmptyWS)                                                                     --Win+Left
    , ((modm     .|. shiftMask,     0xff53),     shiftToNext)                                                                                         --Win+Shift+Right
    , ((modm     .|. shiftMask,     0xff51),     shiftToPrev)                                                                                         --Win+Shift+Left
    , ((modm,                         0x6a),     windows W.focusDown)                                                                                 --Win+J
    , ((modm,                       0xff54),     windows W.focusDown)                                                                                 --Win+Down
    , ((modm,                         0x6b),     windows W.focusUp)                                                                                   --Win+K
    , ((modm,                       0xff52),     windows W.focusUp)                                                                                   --Win+Up
    , ((modm,                         0x6d),     windows W.focusMaster)                                                                               --Win+M
    , ((modm,                       0xff0d),     windows W.swapMaster)                                                                                --Win+Enter
    , ((modm     .|. shiftMask,       0x6a),     windows W.swapDown)                                                                                  --Win+Shift+J
    , ((modm     .|. shiftMask,     0xff54),     windows W.swapDown)                                                                                  --Win+Shift+Down
    , ((modm     .|. shiftMask,       0x6b),     windows W.swapUp)                                                                                    --Win+Shift+K
    , ((modm     .|. shiftMask,     0xff52),     windows W.swapUp)                                                                                    --Win+Shift+K
    , ((modm,                         0x68),     sendMessage Shrink)                                                                                  --Win+H
    , ((modm,                         0x6c),     sendMessage Expand)                                                                                  --Win+L
    , ((modm,                         0x74),     withFocused $ windows . W.sink)                                                                      --Win+T
    , ((modm,                         0x2c),     sendMessage (IncMasterN 1))                                                                          --Win+Comma
    , ((modm,                         0x2e),     sendMessage (IncMasterN (-1)))                                                                       --Win+Period
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
myLayoutHook =  avoidStruts
            $ Tog.toggleLayouts (noBorders Full) 
            $ smartBorders
            $ onWorkspace "W"  (Full ||| mouseResizableTile ||| Mirror tiled)
            $ onWorkspace "M"  (mouseResizableTile ||| Mirror tiled ||| Full)
            $ onWorkspace "E"  (Mirror tiled ||| mouseResizableTile ||| Full) 
            $ onWorkspace "F"  (Mirror tiled ||| mouseResizableTile ||| Full)
            $ onWorkspace "S"  (Mirror tiled ||| mouseResizableTile ||| Full)
            $ onWorkspace "V"  (Full ||| mouseResizableTile)
            $ onWorkspace "P"  (Mirror tiled ||| mouseResizableTile ||| Full)
            $ onWorkspace "J"  (Full ||| Grid)
            $ onWorkspace "T"  (Full ||| mouseResizableTile)
            $ onWorkspace "X"  (Mirror tiled ||| mouseResizableTile ||| Full)
            $ onWorkspace "XI" (smartSpacing 2 $ withIM 0.17 (ClassName "psi") (GridRatio 1))
            $ onWorkspace "XII"(Mirror tiled ||| mouseResizableTile ||| Full)
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
myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ 
      [className =? c                --> doF (W.shift "W")   <+> viewShift ("W")   | c <- myWeb]
    , [className =? c                --> doF (W.shift "M")   <+> viewShift ("M")   | c <- myMail]
    , [className =? c                --> doF (W.shift "E")   <+> viewShift ("E")   | c <- myEdit]
    , [className =? c                --> doF (W.shift "F")   <+> viewShift ("F")   | c <- myFile]
    , [className =? c                --> doF (W.shift "S")   <+> viewShift ("S")   | c <- mySystem]
    , [className =? c                --> doF (W.shift "V")   <+> viewShift ("V")   | c <- myVideo]
    , [className =? c                --> doF (W.shift "P")   <+> viewShift ("P")   | c <- myPic]
    , [className =? c                --> doF (W.shift "J")   <+> viewShift ("J")   | c <- myWork]
    , [className =? c                --> doF (W.shift "T")   <+> viewShift ("T")   | c <- myTorrent]
    , [className =? c                --> doF (W.shift "X")   <+> viewShift ("X")   | c <- myVM]
    , [className =? c                --> doF (W.shift "XI")  <+> viewShift ("XI")  | c <- myIM]
    , [appName   =? c                --> doF (W.shift "XII") <+> viewShift ("XII") | c <- myTerm]

    , [className =? c                --> doCenterFloat                             | c <- myFloatC]
    , [appName   =? a                --> doCenterFloat                             | a <- myFloatA]
    , [title     =? t                --> doCenterFloat                             | t <- myFloatT]
    , [role      =? r                --> doCenterFloat                             | r <- myFloatR]

    , [currentWs =? "W"              --> insertPosition Below Newer]
    , [currentWs =? "M"              --> insertPosition Below Newer]
    , [currentWs =? "E"              --> insertPosition Below Newer]
    , [currentWs =? "F"              --> insertPosition Below Newer]
    , [currentWs =? "T"              --> insertPosition Below Newer]
    , [currentWs =? "X"              --> insertPosition Below Newer]
    , [currentWs =? "XII"            --> insertPosition Below Newer]

    , [resource  =? "stalonetray"    --> doIgnore]

    , [className =? "Gis-weather.py" --> doIgnore]

    , [isDialog                      --> doCenterFloat]

    , [isFullscreen                  --> doFullFloat]

    , [transience']

    , [manageDocks]

    ]
    where
    myWeb     = ["Firefox","Opera","Tor Browser","Vivaldi-snapshot"]
    myMail    = ["Thunderbird"]
    myEdit    = ["Subl3","Et","Wps","Wpp","Acroread","FoxitReader"]
    myFile    = ["Pcmanfm"]
    mySystem  = ["pacmanxg","GParted","Sysinfo","PkgBrowser","Systemadm","Tk","Zenmap"]
    myVideo   = ["mpv","Vlc","Sopcast-player.py","Cheese","Easytag"]
    myPic     = ["Gimp","Gimp-2.8","Inkscape"]
    myWork    = ["Wine"]
    myTorrent = ["Tixati","Transgui","Transmission-gtk","Transmission-remote-gtk"]
    myVM      = ["VirtualBox"]
    myIM      = ["Hexchat","psi","Psi","Viber","TelegramDesktop"]
    myTerm    = ["term","TMUX","termux"]

    -- CenterFloat

    myFloatC  = ["Galculator","Shutter","Zenity","Nvidia-settings","Pulseaudio-equalizer.py","Gnome-alsamixer","Gsmartcontrol","feh"]
    myFloatA  = ["lxappearance","xarchiver","gmrun","Update"]
    myFloatT  = ["Software Update"]
    myFloatR  = ["task_dialog","messages","pop-up","^conversation$","About"]

    role      = stringProperty "WM_WINDOW_ROLE"

    viewShift = doF . liftM2 (.) W.greedyView W.shift

-- Event handling
myEventHook = fullscreenEventHook <+> docksEventHook <+> focusOnMouseMove
 
-- Status bars and logging.
myLogHook = do
    currentWorkspaceOnTop
    dynamicLogString $ xmobarPP {
          ppCurrent         = xmobarColor "#9fdfff" ""
        , ppUrgent          = xmobarColor "#ff6500" ""
        , ppTitle           = (\str -> "")
        }

-- Startup hook
myStartupHook = return () <+> adjustEventInput <+> setWMName "LG3D" <+> onScr 1 W.greedyView "W"

mynameScratchpads = [ NS "ncmpcpp" "urxvtc -name ncmpcpp -e ncmpcpp" (appName =? "ncmpcpp") (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)
                , NS "htop" "urxvtc -name htop -e htop" (appName =? "htop") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                , NS "gpick" "gpick" (appName =? "gpick") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                , NS "pavucontrol" "pavucontrol" (appName =? "pavucontrol") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                , NS "Mirage" "mirage" (className =? "Mirage") (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                , NS "font-manager" "font-manager" (className =? "Font-manager") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                , NS "Organizer" "Organizer" (stringProperty "WM_WINDOW_ROLE" =? "Organizer") (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
                ]

onScr :: ScreenId -> (WorkspaceId -> WindowSet -> WindowSet) -> WorkspaceId -> X ()
onScr n f i = screenWorkspace n >>= \sn -> windows (f i . maybe id W.view sn)

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
        layoutHook         = avoidStruts $ myLayoutHook,
        manageHook         = manageHook def <+> manageDocks <+> myManageHook <+> manageScratchPad <+> namedScratchpadManageHook mynameScratchpads <+> placeHook (smart (0.5,0.5)) <+> workspaceByPos ,
        handleEventHook    = myEventHook,
        logHook            = myLogHook >>= xmonadPropLog,
        startupHook        = myStartupHook
    }
