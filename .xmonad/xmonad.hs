-- Author - Haron Prime
-- License - © 2017 WTFPL - http://www.wtfpl.net/

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

-- Actions
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdateFocus
import XMonad.Actions.WorkspaceNames
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceByPos
import XMonad.Hooks.XPropManage
import XMonad.Hooks.FloatNext (floatNextHook, toggleFloatNext, toggleFloatAllNew)
import XMonad.Hooks.UrgencyHook hiding (Never)
import XMonad.Hooks.CurrentWorkspaceOnTop

-- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.Minimize
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
import XMonad.Util.EZConfig(additionalKeysP, additionalKeys)

-- Variables
myBrowser            =  "vivaldi-snapshot"
browserClass         =  "Vivaldi-snapshot"
myTerminal           =  "urxvtc"
terminalClass        =  "URxvt"
myShell              =  "zsh"
myModMask            =  mod4Mask
myWorkspaces         =  [ "W", "M", "E", "F", "S", "V", "P", "J", "T" , "X" , "XI" , "XII"]
myBorderWidth        =  1
myNormalBorderColor  =  "#555555"
myFocusedBorderColor =  "#95d5f5"
myFont               =  "xft:SonyEricssonLogo:size=10:antialias=true:hinting=true"
myFocusFollowsMouse  =  True
myMRTL               =  mouseResizableTile{draggerType = FixedDragger 1 1}
myMMRTL              =  mouseResizableTile{isMirrored = True, draggerType = FixedDragger 1 1}
mySGRL               =  spacing 1 $ GridRatio (16/10)
role                 =  stringProperty "WM_WINDOW_ROLE"
encodeCChar          =  map fromIntegral . B.unpack
onScr n f i          =  screenWorkspace n >>= \sn -> windows (f i . maybe id W.view sn)
scratchPad           =  scratchpadSpawnActionTerminal "urxvtc -name scratchpad"

-- Key bindings.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [

    --Applications management
      ((0,                         0x1008ff13),     spawn "/usr/bin/pulseaudio-ctl up 5")
    , ((0,                         0x1008ff11),     spawn "/usr/bin/pulseaudio-ctl down 5")
    , ((0,                         0x1008ff12),     spawn "/usr/bin/pulseaudio-ctl mute")
    , ((mod1Mask,                        0x60),     spawn "mpc toggle")
    , ((0,                         0x1008ff14),     spawn "mpc toggle")
    , ((0,                         0x1008ff15),     spawn "mpc stop")
    , ((0,                         0x1008ff16),     spawn "mpc prev")
    , ((0,                         0x1008ff17),     spawn "mpc next")
    , ((0,                         0x1008ff30),     spawn "if (pidof transgui >/dev/null); then kill $(pidof transgui); else transgui; fi")
    , ((0,                         0x1008ff18),     spawn "vivaldi-snapshot")
    , ((0,                         0x1008ff19),     spawn "thunderbird")
    , ((0,                         0x1008ff5d),     spawn "pcmanfm")
    , ((0        .|. shiftMask,    0x1008ff5d),     spawn "gksu pcmanfm")
    , ((0,                         0x1008ff1d),     spawn "if (pidof galculator >/dev/null); then kill $(pidof galculator); else galculator; fi")
    , ((0,                         0x1008ff2f),     spawn "i3lock -i /home/haron/wall/starrynight.png")
    , ((0,                         0x1008ff81),     spawn "XMncmpcpp")
    , ((0,                         0x1008ff1b),     spawn "XMHtop")
    , ((0,                         0x1008ff73),     spawn "compreboot")
    , ((0,                             0xff69),     spawn "compdown")
    , ((0,                             0xff67),     spawn "gmrun")
    , ((0,                             0xffc9),     scratchPad)                                                                                          --F12
    , ((0,                             0xff61),     spawn "scrot -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'")                                        --Print
    , ((0        .|. shiftMask,        0xff61),     spawn "scrot -u -q 100 -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'")                              --Shift+Print
    , ((mod1Mask,                      0xff61),     spawn "scrot -s -q 100 -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'")                              --Alt+Print
    , ((mod1Mask,                        0x61),     spawn "shutter")                                                                                     --Alt+A
    , ((mod1Mask,                        0x64),     spawn "deadbeef")                                                                                    --Alt+D
    , ((mod1Mask,                        0x65),     spawn "pulseaudio-equalizer-gtk")                                                                    --Alt+E
    , ((mod1Mask,                        0x66),     spawn "firefox")                                                                                     --Alt+F
    , ((mod1Mask,                        0x67),     spawn "gimp")                                                                                        --Alt+G
    , ((mod1Mask,                        0x68),     spawn "hexchat")                                                                                     --Alt+H
    , ((mod1Mask,                        0x6c),     spawn "cat /home/haron/Documents/last.pass | cut -c 1-24 | xclip -selection clipboard")              --Alt+L
    , ((mod1Mask,                        0x6d),     spawn "urxvtc -name mc -e mc")                                                                       --Alt+M
    , ((mod1Mask,                        0x6e),     spawn "XMNotes")                                                                                     --Alt+N
    , ((mod1Mask .|. controlMask,        0x6e),     spawn "urxvtc -name Notes -cd ~/MyNotes -e vim -c NERDTree")                                         --Alt+Ctrl+N
    , ((mod1Mask,                        0x6f),     spawn "opera12")                                                                                     --Alt+O
    , ((mod1Mask,                        0x72),     spawn "urxvtc -name ranger -e ranger")                                                               --Alt+R
    , ((mod1Mask,                        0x73),     spawn "subl3")                                                                                       --Alt+S
    , ((mod1Mask .|. shiftMask,          0x73),     spawn "gksu subl3")                                                                                  --Alt+Shift+S
    , ((mod1Mask,                        0x74),     spawn "tor-browser")                                                                                 --Alt+T
    , ((mod1Mask,                        0x76),     spawn "urxvtc -name vim -e vim")                                                                     --Alt+V
    , ((modm,                            0x71),     spawn "xmonad --recompile && xmonad --restart && killall xmobar && xmobar")                          --Mod4+Q
    , ((modm     .|. shiftMask,        0xff0d),     spawn $ XMonad.terminal conf)                                                                        --Mod4+Shift+Enter

    --Menu
    , ((modm,                           0xff67),    spawn "menu")                                                                                        --Mod4+Menu

    --Prompt management
    , ((mod1Mask,                      0xffbe),     manPrompt myXPConfig)                                                                                --Alt+F1
    , ((mod1Mask,                      0xffbf),     runOrRaisePrompt myXPConfig)                                                                         --Alt+F2
    , ((mod1Mask,                      0xffc0),     shellPrompt myXPConfig)                                                                              --Alt+F3
    , ((mod1Mask,                      0xffc1),     sshPrompt myXPConfig)                                                                                --Alt+F4

    --WS management
    , ((modm,                            0x20),     sendMessage NextLayout)                                                                              --Mod4+Space
    , ((modm     .|. shiftMask,          0x20),     setLayout $ XMonad.layoutHook conf)                                                                  --Mod4+Shift+Space
    , ((modm,                            0x6e),     refresh)                                                                                             --Mod4+N
    , ((modm,                          0xff1b),     toggleWS' ["NSP"])                                                                                   --Mod4+Escape
    , ((modm,                          0xff08),     toggleWS' ["NSP"])                                                                                   --Mod4+Backspace
    , ((modm,                            0x62),     sendMessage ToggleStruts)                                                                            --Mod4+B
    , ((modm,                            0x78),     kill)                                                                                                --Mod4+X
    , ((modm     .|. shiftMask,          0x71),     io (exitWith ExitSuccess))                                                                           --Mod4+Shift+Q

    --Windows management
    , ((modm,                            0x60),     rotOpposite)                                                                                         --Mod4+grave
    , ((modm,                          0xff09),     cycleRecentWindows [0xffeb] 0xff09 0x77)                                                             --Mod4+Tab
    , ((modm,                            0x6a),     windows W.focusDown)                                                                                 --Mod4+J
    , ((modm,                          0xff54),     windows W.focusDown)                                                                                 --Mod4+Down
    , ((modm,                            0x6b),     windows W.focusUp)                                                                                   --Mod4+K
    , ((modm,                          0xff52),     windows W.focusUp)                                                                                   --Mod4+Up
    , ((modm,                            0x6d),     windows W.focusMaster)                                                                               --Mod4+M
    , ((modm,                          0xff0d),     windows W.swapMaster)                                                                                --Mod4+Enter
    , ((modm     .|. shiftMask,          0x6a),     windows W.swapDown)                                                                                  --Mod4+Shift+J
    , ((modm     .|. shiftMask,        0xff54),     windows W.swapDown)                                                                                  --Mod4+Shift+Down
    , ((modm     .|. shiftMask,          0x6b),     windows W.swapUp)                                                                                    --Mod4+Shift+K
    , ((modm     .|. shiftMask,        0xff52),     windows W.swapUp)                                                                                    --Mod4+Shift+Up
    , ((modm,                          0xff53),     DO.moveTo Next HiddenNonEmptyWS)                                                                     --Mod4+Right
    , ((modm,                          0xff51),     DO.moveTo Prev HiddenNonEmptyWS)                                                                     --Mod4+Left
    , ((modm     .|. shiftMask,        0xff53),     shiftToNext)                                                                                         --Mod4+Shift+Right
    , ((modm     .|. shiftMask,        0xff51),     shiftToPrev)                                                                                         --Mod4+Shift+Left
    , ((modm,                            0x7a),     withFocused minimizeWindow)                                                                          --Mod4+Z
    , ((modm,                            0x61),     sendMessage RestoreNextMinimizedWin)                                                                 --Mod4+A
    , ((modm,                            0x68),     sendMessage Shrink)                                                                                  --Mod4+H
    , ((modm,                            0x6c),     sendMessage Expand)                                                                                  --Mod4+L
    , ((modm,                            0x74),     withFocused $ windows . W.sink)                                                                      --Mod4+T
    , ((modm,                            0x2c),     sendMessage (IncMasterN 1))                                                                          --Mod4+Comma
    , ((modm,                            0x2e),     sendMessage (IncMasterN (-1)))                                                                       --Mod4+Period
    -- , ((mod1Mask,                      0xff09),     windows W.focusMaster)                                                                               --Alt+Tab
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
                $ minimize
                $ Tog.toggleLayouts (noBorders Full) 
                $ smartBorders
                $ onWorkspace  "W"    (Full     ||| myMRTL  ||| myMMRTL)
                $ onWorkspace  "M"    (myMRTL   ||| myMMRTL ||| Full)
                $ onWorkspace  "E"    (myMMRTL  ||| myMRTL  ||| Full) 
                $ onWorkspace  "F"    (myMMRTL  ||| myMRTL  ||| Full)
                $ onWorkspace  "S"    (myMMRTL  ||| myMRTL  ||| Full)
                $ onWorkspace  "V"    (Full     ||| myMRTL)
                $ onWorkspace  "P"    (myMMRTL  ||| myMRTL  ||| Full)
                $ onWorkspace  "J"    (mySGRL   ||| myMRTL)
                $ onWorkspace  "T"    (Full     ||| myMRTL  ||| myMMRTL)
                $ onWorkspace  "X"    (mySGRL   ||| myMRTL)
                $ onWorkspace  "XI"   (mySGRL   ||| myMRTL)
                $ onWorkspace  "XII"  (mySGRL   ||| myMRTL)
                $ tiled ||| Mirror tiled  ||| Full
                where
                  tiled   = spacing 1 $ Tall nmaster delta ratio
                  nmaster = 1
                  ratio   = 0.5
                  delta   = 0.01


-- Prompts
myXPConfig = def {
                   font              = "xft:Terminus Re33:size=12:antialias=true:hinting=true"
                 , bgColor           = "#151515"
                 , fgColor           = "#959595"
                 , bgHLight          = "#151515"
                 , fgHLight          = "#95d5f5"
                 , promptBorderWidth = 0
                 , position          = Top
                 , height            = 20
                 , alwaysHighlight   = True
                 , historySize       = 100
                 }

-- Windows rules:
myManageHook = composeAll . concat $
    [ 
      [className =? c                --> doF (W.shift "W")   <+> viewShift ("W")   | c <- myWeb]
    , [className =? c                --> doF (W.shift "M")                         | c <- myMail]
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

    , [className =? "Gis-weather.py" --> doIgnore]
    , [resource  =? "stalonetray"    --> doIgnore]

    , [isDialog                      --> doCenterFloat]

    , [isFullscreen                  --> doFullFloat]

    , [transience']

    , [manageDocks]

    ]
    where
    myWeb     = ["Firefox","Opera","Tor Browser","Vivaldi-snapshot"]
    myMail    = ["Thunderbird"]
    myEdit    = ["Subl3","Meld","Et","Wps","Wpp","FoxitReader"]
    myFile    = ["Pcmanfm"]
    mySystem  = ["pacmanxg","GParted","Sysinfo","Tk","Systemadm","Zenmap"]
    myVideo   = ["mpv","Vlc","Sopcast-player.py","Easytag"]
    myPic     = ["Gimp","Gimp-2.8","Inkscape"]
    myWork    = ["Wine"]
    myTorrent = ["Tixati","Transgui","Transmission-gtk","Transmission-remote-gtk"]
    myVM      = ["VirtualBox"]
    myIM      = ["Hexchat","psi","Psi","Viber","TelegramDesktop"]
    myTerm    = []

    -- CenterFloat
    myFloatC  = ["Xmessage","Gxmessage","XClock","Galculator","Shutter","Zenity","Nvidia-settings","Pulseaudio-equalizer.py","Gnome-alsamixer","Gsmartcontrol","feh","Gconf-editor","Dconf-editor"]
    myFloatA  = ["lxappearance","xarchiver","gmrun","Update"]
    myFloatT  = ["Software Update"]
    myFloatR  = ["task_dialog","messages","pop-up","^conversation$","About"]

    viewShift = doF . liftM2 (.) W.greedyView W.shift

-- Event handling
myEventHook = minimizeEventHook <+> handleEventHook def <+> fullscreenEventHook <+> docksEventHook <+> focusOnMouseMove <+> ewmhDesktopsEventHook
 
-- Status bars and logging.
myLogHook = do
            currentWorkspaceOnTop
            dynamicLogString $ xmobarPP {
                                          ppCurrent         = xmobarColor "#9fdfff" "" . pad
                                        , ppUrgent          = xmobarColor "#ff6500" "" . pad . wrap "<" ">"
                                        , ppTitle           = (\str -> "")
                                        , ppLayout          = (\str -> "")
                                        }

-- nameScratchpad
mynameScratchpads = [ NS "ncmpcpp"      "urxvtc -name ncmpcpp -e ncmpcpp"                            (appName    =? "ncmpcpp")      (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)
                    , NS "htop"         "urxvtc -name htop -e htop"                                  (appName    =? "htop")         (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                    , NS "gpick"        "gpick"                                                      (appName    =? "gpick")        (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                    , NS "pavucontrol"  "pavucontrol"                                                (appName    =? "pavucontrol")  (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                    , NS "update"       "urxvtc -name update -e yaourt -Syua"                        (appName    =? "update")       (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)
                    , NS "Notes"        "urxvtc -name Notes -cd ~/MyNotes -e vim -c :NERDTreeToggle" (appName    =? "Notes")        (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)

                    , NS "Mirage"       "mirage"                                                     (className  =? "Mirage")       (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
                    , NS "font-manager" "font-manager"                                               (className  =? "Font-manager") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
                    , NS "Gsimplecal"   "Gsimplecal"                                                 (className  =? "Gsimplecal")   (customFloating $ W.RationalRect 0.43 0.4 0.14 0.2)

                    , NS "Organizer"    "Organizer"                                                  (role       =? "Organizer")    (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
                    , NS "Msgcompose"   "Msgcompose"                                                 (role       =? "Msgcompose")   (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
                    , NS "addressbook"  "addressbook"                                                (role       =? "addressbook")  (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
                    ]

-- Scratchpad                       
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.333   -- terminal height
    w = 1       -- terminal width
    t = 1 - h   -- distance from top edge
    l = 1 - w   -- distance from left edge

-- StartupHook
myStartupHook        =  return () <+> adjustEventInput <+> setWMName "LG3D" <+> onScr 1 W.greedyView "W"

main = do
       xmonad =<< xmobar myConfig

myConfig = ewmh $ withUrgencyHookC NoUrgencyHook urgencyConfig def {
                       terminal           = myTerminal
                      ,focusFollowsMouse  = myFocusFollowsMouse
                      ,borderWidth        = myBorderWidth
                      ,modMask            = myModMask
                      ,workspaces         = myWorkspaces
                      ,normalBorderColor  = myNormalBorderColor
                      ,focusedBorderColor = myFocusedBorderColor
                      ,keys               = myKeys
                      ,mouseBindings      = myMouseBindings
                      ,layoutHook         = myLayoutHook
                      ,manageHook         = floatNextHook <+> manageHook def <+> myManageHook <+> manageScratchPad <+> namedScratchpadManageHook mynameScratchpads <+> placeHook (smart (0.5,0.5)) <+> workspaceByPos
                      ,handleEventHook    = myEventHook
                      ,logHook            = myLogHook >>= xmonadPropLog
                      ,startupHook        = myStartupHook 
                      }