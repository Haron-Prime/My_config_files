-- Author - Haron Prime
-- License © 2017 WTFPL http://www.wtfpl.net/

-- Base
import XMonad hiding ( (|||) )
import Control.Monad (liftM2, filterM)
import Data.List
import Data.Ratio ((%))
import Foreign.C (CChar)
import Graphics.X11.Xlib
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import qualified Data.Map        as M
import qualified Data.ByteString as B
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdateFocus
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceByPos
import XMonad.Hooks.XPropManage
import XMonad.Hooks.UrgencyHook hiding (Never)

-- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.Master
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as Tog

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Ssh

-- Utils
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP, additionalKeys)

--- My options ---
-- Applications
myBrowser        =  spawn "vivaldi-snapshot"
browserClass     =  "Vivaldi-snapshot"
myTerminal       =  "urxvtc"
terminalClass    =  "URxvt"
myShell          =  "zsh"
myFM             =  "urxvtc -name ranger -e ranger"
myNotes          =  "urxvtc -name Notes -cd ~/MyNotes -e vim -c NERDTree"
myHtop           =  "urxvtc -name htop -e htop"
myPlayer         =  "urxvtc -name ncmpcpp -e ncmpcpp"
myEditor         =  spawn "urxvtc -name vim -e vim"
myFullScrot      =  spawn "scrot -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'"
myAreaScrot      =  spawn "scrot -u -q 100 -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'"
myWindowScrot    =  spawn "scrot -s -q 100 -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'"
myAppMenu        =  spawn "mygtkmenu .menurc"
myPlaceMenu      =  spawn "mygtkmenu .placerc"
mySreenLock      =  spawn "i3lock -i /home/haron/wall/starrynight.png"
myQST            =  scratchpadSpawnActionTerminal myTerminal
-- Decorations
myBorderWidth    =  1
myHLColor        =  "#95d5f5"
myUrgColor       =  "#ff6500"
myBgColor        =  "#151515"
myFgColor        =  "#959595"
myFont           =  "xft:SonyEricssonLogo:size=10:antialias=true:hinting=true"
myMonospaceFont  =  "xft:Terminus Re33 Nerd Bold:size=12:antialias=true:hinting=true"
-- Layouts combinations
myRTL1           =  windowNavigation (spacing 1 $ ResizableTall 1 (1/100) (1/2) [])
myRTL2           =  windowNavigation (spacing 1 $ ResizableTall 2 (1/100) (2/3) [])
myMRTL1          =  windowNavigation (spacing 1 $ Mirror (ResizableTall 1 (1/100) (2/3) []))
myMRTL2          =  windowNavigation (spacing 1 $ Mirror (ResizableTall 2 (1/100) (2/3) []))
myGL             =  windowNavigation (spacing 1 $ multimastered 2 (1/100) (1/3) $ GridRatio (16/10))
--
myWL             =  Full     |||  myRTL1   ||| myMRTL1
myML             =  Full     |||  myRTL1
myEL             =  myMRTL1  |||  Full     ||| myRTL2
myFL             =  Full     |||  myGL
mySL             =  myRTL1   |||  myMRTL1  ||| Full
myPL             =  Full     |||  myGL
myVL             =  Full     |||  myRTL2
myJL             =  Full     |||  myRTL2
myTL             =  Full     |||  myMRTL1
-- Modkey
modm             =  mod4Mask
altm             =  mod1Mask
ctrlm            =  controlMask
shftm            =  shiftMask
-- Actions
encodeCChar      =  map fromIntegral . B.unpack
onScr n f i      =  screenWorkspace n >>= \sn -> windows (f i . maybe id W.view sn)
viewShift        =  doF . liftM2 (.) W.greedyView W.shift
minWin           =  withFocused minimizeWindow <+> spawn "XMMWO"
restWin          =  sendMessage RestoreNextMinimizedWin <+> spawn "XMMWC"
-- Other
role             =  stringProperty "WM_WINDOW_ROLE"

-- Key bindings.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    --Applications management
      ((0,              0x1008ff13),  spawn "pactl set-sink-volume 0 +5%")      --XF86AudioRaiseVolume
    , ((0,              0x1008ff11),  spawn "pactl set-sink-volume 0 -5%")      --XF86AudioLowerVolume
    , ((0,              0x1008ff12),  spawn "pactl set-sink-mute 0 toggle")     --XF86AudioMute
    , ((0,              0x1008ff14),  spawn "XMMPCtoggle")                      --XF86Play
    , ((0,              0x1008ff15),  spawn "XMMPCstop")                        --XF86AudioStop
    , ((0,              0x1008ff16),  spawn "XMMPCprev")                        --XF86AudioPrev
    , ((0,              0x1008ff17),  spawn "XMMPCnext")                        --XF86AudioNext
    , ((0,              0x1008ff30),  spawn "subl3")                            --XF86Favorites
    , ((0,              0x1008ff18),  myBrowser)                                --XF86HomePage
    , ((0,              0x1008ff19),  spawn "thunderbird")                      --XF86Mail
    , ((0,              0x1008ff33),  spawn "pcmanfm")                          --XF86MyComputer
    , ((0,              0x1008ff5d),  spawn "pcmanfm")                          --XF86Explorer
    , ((0,              0x1008ff1d),  spawn "XMGalculator")                     --XF86Calculator
    , ((0,              0x1008ff1b),  namedScratchpadAction myNS "HTOP")        --XF86Search
    , ((0,              0x1008ff77),  namedScratchpadAction myNS "NCMPCPP")     --XF86Save
    , ((0,              0x1008ff46),  spawn "XMR")                              --XF86Launch6
    , ((0,              0x1008ff2f),  mySreenLock)                              --XF86Sleep
    , ((0,              0x1008ff56),  namedScratchpadAction myNS "OBLOGOUT")    --XF86Close
    , ((0,              0x1008ff73),  spawn "compreboot")                       --XF86Reload
    , ((0,                  0xff69),  spawn "compdown")                         --Cancel
    , ((0,                  0xff67),  spawn "gmrun")                            --Menu
    , ((0,                  0xffc9),  myQST)                                    --F12
    , ((0,                  0xff61),  myFullScrot)                              --Print
    , ((0    .|. shftm,     0xff61),  myAreaScrot)                              --Shift+Print
    , ((altm,               0xff61),  myWindowScrot)                            --Alt+Print
    , ((altm,                 0x63),  spawn "chromium")                         --Alt+C
    , ((altm,                 0x66),  spawn "firefox")                          --Alt+F
    , ((altm,                 0x67),  spawn "gitkraken")                        --Alt+G
    , ((altm .|. ctrlm,       0x67),  spawn "gimp")                             --Alt+Ctrl+G
    , ((altm,                 0x68),  spawn "hexchat")                          --Alt+H
    , ((altm,                 0x6c),  spawn "XMLPass")                          --Alt+L
    , ((altm,                 0x6d),  spawn "urxvtc -name mc -e mc")            --Alt+M
    , ((altm,                 0x6e),  spawn "XMNotes-w")                        --Alt+N
    , ((altm .|. ctrlm,       0x6e),  namedScratchpadAction myNS "NOTES")       --Alt+Ctrl+N
    , ((altm,                 0x6f),  spawn "opera-developer")                  --Alt+O
    , ((altm,                 0x71),  namedScratchpadAction myNS "OBLOGOUT")    --Alt+Q
    , ((altm,                 0x72),  namedScratchpadAction myNS "FM")          --Alt+R
    , ((altm,                 0x74),  spawn "XMTransgui")                       --Alt+T
    , ((altm,                 0x76),  myEditor)                                 --Alt+V
    , ((altm,                 0x79),  spawn "XMYaourt")                         --Alt+Y
    , ((modm .|. shftm,     0xff0d),  spawn $ XMonad.terminal conf)             --Mod4+Shift+Return

    --Recompile & restart
    , ((modm,                 0x63),  spawn "XMR")                              --Mod4+C
    , ((modm,                 0x71),  spawn "XMRR")                             --Mod4+Q

    --Menu
    , ((altm,                 0x61),  myAppMenu)                                --Alt+A
    , ((altm,                 0x62),  myPlaceMenu)                              --Alt+B

    --Prompt management
    , ((altm,               0xffbe),  manPrompt myPromptConfig)                 --Alt+F1
    , ((altm,               0xffbf),  runOrRaisePrompt myPromptConfig)          --Alt+F2
    , ((altm,               0xffc0),  sshPrompt myPromptConfig)                 --Alt+F3

    --WS management
    , ((altm,               0xff09),  nextWS)                                   --Alt+Tab
    , ((altm .|. ctrlm,     0xff09),  prevWS)                                   --Alt+Ctrl+Tab
    , ((altm .|. ctrlm,     0xff53),  DO.moveTo Next HiddenNonEmptyWS)          --Alt+Ctrl+Right
    , ((altm .|. ctrlm,     0xff51),  DO.moveTo Prev HiddenNonEmptyWS)          --Alt+Ctrl+Left
    , ((modm,               0xff1b),  toggleWS' ["NSP"])                        --Mod4+Escape
    , ((modm,               0xff08),  toggleWS' ["NSP"])                        --Mod4+Backspace
    , ((modm,                 0x20),  sendMessage NextLayout)                   --Mod4+Space
    , ((modm .|. shftm,       0x20),  setLayout $ XMonad.layoutHook conf)       --Mod4+Shift+Space
    , ((modm,                 0x6e),  refresh)                                  --Mod4+N
    , ((modm,                 0x62),  sendMessage ToggleStruts)                 --Mod4+B
    , ((modm,                 0x68),  sendMessage Shrink)                       --Mod4+H
    , ((modm .|. shftm,       0x68),  sendMessage MirrorShrink)                 --Mod4+Shift+H
    , ((modm,                 0x6c),  sendMessage Expand)                       --Mod4+L
    , ((modm .|. shftm,       0x6c),  sendMessage MirrorExpand)                 --Mod4+Shift+L
    , ((modm,                 0x74),  withFocused $ windows . W.sink)           --Mod4+T
    , ((modm,                 0x2c),  sendMessage (IncMasterN 1))               --Mod4+Comma
    , ((modm,                 0x2e),  sendMessage (IncMasterN (-1)))            --Mod4+Period
    , ((modm .|. shftm,       0x71),  io (exitWith ExitSuccess))                --Mod4+Shift+Q

    --Windows management
    , ((modm,                 0x60),  rotOpposite)                              --Mod4+grave
    , ((modm,               0xff09),  cycleRecentWindows [0xffeb] 0xff09 0x77)  --Mod4+Tab
    , ((modm,               0xff53),  sendMessage $ Go R)                       --Mod4+Right
    , ((modm,               0xff51),  sendMessage $ Go L)                       --Mod4+Left
    , ((modm,               0xff52),  sendMessage $ Go U)                       --Mod4+Up
    , ((modm,               0xff54),  sendMessage $ Go D)                       --Mod4+Down
    , ((modm .|. shftm,     0xff53),  sendMessage $ Swap R)                     --Mod4+Shift+Right
    , ((modm .|. shftm,     0xff51),  sendMessage $ Swap L)                     --Mod4+Shift+Left
    , ((modm .|. shftm,     0xff52),  sendMessage $ Swap U)                     --Mod4+Shift+Up
    , ((modm .|. shftm,     0xff54),  sendMessage $ Swap D)                     --Mod4+Shift+Down
    , ((modm .|. ctrlm,     0xff53),  shiftToNext)                              --Mod4+Ctrl+Right
    , ((modm .|. ctrlm,     0xff51),  shiftToPrev)                              --Mod4+Ctrl+Left
    , ((modm,                 0x6a),  windows W.focusDown)                      --Mod4+J
    , ((modm,                 0x6b),  windows W.focusUp)                        --Mod4+K
    , ((modm,                 0x6d),  windows W.focusMaster)                    --Mod4+M
    , ((modm,               0xff0d),  windows W.swapMaster)                     --Mod4+Return
    , ((modm .|. shftm,       0x6a),  windows W.swapDown)                       --Mod4+Shift+J
    , ((modm .|. shftm,       0x6b),  windows W.swapUp)                         --Mod4+Shift+K
    , ((modm,                 0x7a),  minWin)                                   --Mod4+Z
    , ((modm,                 0x61),  restWin)                                  --Mod4+A
    , ((modm,                 0x78),  kill)                                     --Mod4+X

    --XMobar management
    , ((altm,                 0x30),  spawn "XMUptimeState")                    --Alt+0
    , ((altm .|. ctrlm,       0x30),  spawn "XMinxi")                           --Alt+Ctrl+0
    , ((altm .|. shftm,       0x30),  spawn "XMScreenfetch")                    --Alt+Shift+0
    , ((altm,                 0x31),  spawn "XMNetState")                       --Alt+1
    , ((altm .|. ctrlm,       0x31),  spawn "XMVnstat-h & XMVnstat")            --Alt+Ctrl+1
    , ((altm,                 0x32),  spawn "XMTrafState")                      --Alt+2
    , ((altm .|. ctrlm,       0x32),  spawn "XMVnstat-d")                       --Alt+Ctrl+2
    , ((altm,                 0x33),  spawn "XMCPUState")                       --Alt+3
    , ((altm .|. ctrlm,       0x33),  spawn "XMTop-cpu")                        --Alt+Ctrl+3
    , ((altm,                 0x34),  spawn "XMTempState")                      --Alt+4
    , ((altm .|. ctrlm,       0x34),  spawn "XMSensors")                        --Alt+Ctrl+4
    , ((altm,                 0x35),  spawn "XMMemState")                       --Alt+5
    , ((altm .|. ctrlm,       0x35),  spawn "XMTop-mem")                        --Alt+Ctrl+5
    , ((altm .|. shftm,       0x35),  spawn "XMdf-h")                           --Alt+Shift+5
    , ((altm,                 0x36),  spawn "XMVolState")                       --Alt+6
    , ((altm,                 0x37),  spawn "XMDateState")                      --Alt+7
    , ((altm,                 0x60),  spawn "XMStateAll")                       --Alt+grave
    , ((altm,               0xff1b),  spawn "XMStateKill")                      --Alt+Escape
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
myMB (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

-- Clickable workspaces
xmobarEscape = concatMap doubleLts
    where doubleLts '<' = "<<"
          doubleLts x   = [x]

myWS = clickable . (map xmobarEscape) $ [ "W", "M", "E", "F", "S", "V", "P", "J", "T" , "X" , "XI" , "XII"]
    where clickable l = [ "<fn=4><action=`xdotool key 0xffeb+" ++ show (n) ++ "` button=1>" ++ ws ++ "</action></fn>" |
                        (i,ws) <- zip ["0x31", "0x32", "0x33", "0x34", "0x35", "0x36", "0x37", "0x38", "0x39", "0x30", "0x2d", "0x3d"] l,
                        let n = i 
                        ]

-- Layouts
myLH =  avoidStruts
        $ minimize
        $ Tog.toggleLayouts (noBorders Full) 
        $ smartBorders
        $ onWorkspace (myWS !! 0) myWL
        $ onWorkspace (myWS !! 1) myML
        $ onWorkspace (myWS !! 2) myEL
        $ onWorkspace (myWS !! 3) myFL
        $ onWorkspace (myWS !! 4) mySL
        $ onWorkspace (myWS !! 5) myVL
        $ onWorkspace (myWS !! 6) myPL
        $ onWorkspace (myWS !! 7) myJL
        $ onWorkspace (myWS !! 8) myTL
        $ myRTL1 ||| myMRTL1 ||| myGL ||| Full

-- Prompts
myPromptConfig = def {
                       font              = myMonospaceFont
                     , bgColor           = myBgColor
                     , fgColor           = myFgColor
                     , bgHLight          = myBgColor
                     , fgHLight          = myHLColor
                     , promptBorderWidth = 0
                     , position          = Top
                     , height            = 20
                     , alwaysHighlight   = True
                     , historySize       = 100
                     }

-- ManageHook
myMH = manageHook def <+> 
       myWindowsRules <+> 
       manageScratchPad <+> 
       namedScratchpadManageHook myNS <+> 
       placeHook (smart (0.5,0.5)) <+> 
       workspaceByPos

-- Windows rules
myWindowsRules = composeAll . concat $
    [ 
      [className =? c                 --> doShift (myWS !! 0)   <+> viewShift (myWS !! 0)   | c <- myW]
    , [className =? c                 --> doShift (myWS !! 1)                               | c <- myM]
    , [className =? c                 --> doShift (myWS !! 2)   <+> viewShift (myWS !! 2)   | c <- myE]
    , [className =? c                 --> doShift (myWS !! 3)   <+> viewShift (myWS !! 3)   | c <- myF]
    , [className =? c                 --> doShift (myWS !! 4)   <+> viewShift (myWS !! 4)   | c <- myS]
    , [className =? c                 --> doShift (myWS !! 5)   <+> viewShift (myWS !! 5)   | c <- myV]
    , [className =? c                 --> doShift (myWS !! 6)   <+> viewShift (myWS !! 6)   | c <- myP]
    , [className =? c                 --> doShift (myWS !! 7)   <+> viewShift (myWS !! 7)   | c <- myJ]
    , [className =? c                 --> doShift (myWS !! 8)   <+> viewShift (myWS !! 8)   | c <- myT]
    , [className =? c                 --> doShift (myWS !! 9)                               | c <- myX]
    , [className =? c                 --> doShift (myWS !! 10)  <+> viewShift (myWS !! 10)  | c <- myXI]
    , [className =? c                 --> doShift (myWS !! 11)  <+> viewShift (myWS !! 11)  | c <- myXII]

    , [className =? c                 --> doCenterFloat                                     | c <- myFC]
    , [appName   =? a                 --> doCenterFloat                                     | a <- myFA]
    , [title     =? t                 --> doCenterFloat                                     | t <- myFT]
    , [role      =? r                 --> doCenterFloat                                     | r <- myFR]

    , [currentWs =? (myWS !! 0)       --> insertPosition Below Newer]
    , [currentWs =? (myWS !! 1)       --> insertPosition Below Newer]
    , [currentWs =? (myWS !! 8)       --> insertPosition Below Newer]

    , [currentWs =? (myWS !! 2)       --> insertPosition End Newer]
    , [currentWs =? (myWS !! 9)       --> insertPosition End Newer]
    , [currentWs =? (myWS !! 11)      --> insertPosition End Newer]

    , [className =? "Gis-weather.py"  --> doIgnore]
    , [resource  =? "stalonetray"     --> doIgnore]

    , [isDialog                       --> doCenterFloat]

    , [isFullscreen                   --> doFullFloat]

    , [transience']

    , [manageDocks]

    ]
    where
-- Application groups attached to workspaces
        myW   = [
                  "Firefox"
                , "Chromium"
                , "Icecat"
                , "Opera"
                , "Opera developer"
                , "Tor Browser"
                , "Vivaldi-snapshot"
                ]
        myM   = [
                  "Thunderbird"
                ]
        myE   = [
                  "Atom"
                , "Cherrytree"
                , "Et"
                , "FoxitReader"
                , "Meld"
                , "Subl3"
                , "Wps"
                , "Wpp"
                , "Zim"
                ]
        myF   = [
                  "Pcmanfm"
                ]
        myS   = [
                  "GParted"
                , "pacmanxg"
                , "Sysinfo"
                , "Systemadm"
                , "Tk"
                , "Zenmap"
                ]
        myV   = [
                  "Easytag"
                , "mpv"
                , "Sopcast-player.py"
                , "Vlc"
                ]
        myP   = [
                  "Gimp"
                , "Gimp-2.8"
                , "Inkscape"
                ]
        myJ   = [
                  "Wine"
                ]
        myT   = [
                  "Tixati"
                , "Transgui"
                , "Transmission-gtk"
                , "Transmission-remote-gtk"
                ]
        myX   = [
                  "Gitg"
                , "Gitk"
                , "SWT"
                ]
        myXI  = [
                  "Hexchat"
                , "psi"
                , "Psi"
                , "TelegramDesktop"
                , "ViberPC"
                ]
        myXII = [
                  "GitKraken"
                ]
-- Application groups in floating windows (CenterFloat)
        myFC  = [
                  "Dconf-editor"
                , "feh"
                , "Font-manager"
                , "Galculator"
                , "Gconf-editor"
                , "Gksu-properties"
                , "Gnome-alsamixer"
                , "Gsmartcontrol"
                , "Gxmessage"
                , "Pulseaudio-equalizer.py"
                , "XClock"
                , "Xmessage"
                , "Zenity"
                ]
        myFA  = [
                  "gmrun"
                , "lxappearance"
                , "Update"
                , "xarchiver"
                ]
        myFT  = [
                  "Software Update"
                ]
        myFR  = [
                  "About"
                , "messages"
                , "pop-up"
                , "task_dialog"
                , "^conversation$"
                ]

-- NamedScratchpad
myNS = [
          NS "NCMPCPP"      myPlayer       (appName    =? "ncmpcpp")      (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)
        , NS "HTOP"         myHtop         (appName    =? "htop")         (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8)
        , NS "GPICK"        "gpick"        (appName    =? "gpick")        (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
        , NS "PAVUCONTROL"  "pavucontrol"  (appName    =? "pavucontrol")  (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
        , NS "XMUPDATE"     "XMUpdate"     (appName    =? "update")       (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)
        , NS "NOTES"        myNotes        (appName    =? "Notes")        (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
        , NS "FM"           myFM           (appName    =? "ranger")       (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)

        , NS "MIRAGE"       "mirage"       (className  =? "Mirage")       (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
        , NS "GSIMPLECAL"   "Gsimplecal"   (className  =? "Gsimplecal")   (customFloating $ W.RationalRect 0.43 0.4 0.14 0.2)
        , NS "OBLOGOUT"     "oblogout"     (className  =? "Oblogout")     (customFloating $ W.RationalRect 0.32 0.4 0.36 0.2)
        , NS "DEADBEEF"     "deadbeef"     (className  =? "Deadbeef")     (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)

        , NS "ORGANIZER"    "Organizer"    (role       =? "Organizer")    (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
        , NS "MSGCOMPOSE"   "Msgcompose"   (role       =? "Msgcompose")   (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
        , NS "ADDRESSBOOK"  "addressbook"  (role       =? "addressbook")  (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
        , NS "FILTERLIST"   "filterlist"   (role       =? "filterlist")   (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
        ]

-- Scratchpad (myQST)
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
    where
        h = 0.333   -- terminal height
        w = 1       -- terminal width
        t = 1 - h   -- distance from top edge
        l = 1 - w   -- distance from left edge

-- Event handling
myEH = handleEventHook def <+> 
       fullscreenEventHook <+> 
       docksEventHook <+> 
       ewmhDesktopsEventHook <+> 
       focusOnMouseMove

-- StartupHook
mySH = return () <+> 
       adjustEventInput <+> 
       setWMName "LG3D" <+> 
       onScr 1 W.greedyView (myWS !! 0) <+> 
       spawn "XMStart" 

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh $ withUrgencyHookC NoUrgencyHook urgencyConfig def 
        {
          terminal           = myTerminal
        , focusFollowsMouse  = True
        , borderWidth        = myBorderWidth
        , modMask            = modm
        , workspaces         = myWS
        , normalBorderColor  = myBgColor
        , focusedBorderColor = myHLColor
        , keys               = myKeys
        , mouseBindings      = myMB
        , startupHook        = mySH
        , layoutHook         = myLH
        , manageHook         = myMH
        , handleEventHook    = myEH
        , logHook            = dynamicLogWithPP $ def {
                                                        ppOutput  = System.IO.hPutStrLn xmproc
                                                      , ppCurrent = xmobarColor myHLColor ""
                                                      , ppUrgent  = xmobarColor myUrgColor ""
                                                      , ppOrder   = \(ws:l:t:_) -> [ws]
                                                      }
        }
