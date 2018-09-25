-- XMonad config
-- Author - Haron Prime
-- License WTFPL http://www.wtfpl.net/

-- List of all custom scripts used and their description is in the file https://github.com/Haron-Prime/My_config_files/blob/master/.local/bin/README.md


--- MODULES INCLUDED ---
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
import XMonad.Actions.Minimize
import XMonad.Actions.UpdateFocus
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

--- CUSTOM OPTIONS ---
-- Applications
myBrowser        =  spawn "vivaldi-snapshot"
myTerm           =  "urxvtc"
myFM             =  "urxvtc -name ranger -e ranger"
myNotes          =  "urxvtc -name Notes -cd ~/MyNotes -e vim -c NERDTree"
myHtop           =  "urxvtc -name htop -e htop"
myPlayer         =  "urxvtc -name ncmpcpp -e ncmpcpp"
myEditor         =  spawn "urxvtc -name vim -e vim"
myFScrot         =  spawn "scrot -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'"
myWScrot         =  spawn "scrot -u -q 100 -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'"
myAScrot         =  spawn "scrot -s -q 100 -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'"
myAppMenu        =  spawn "mygtkmenu .menurc"
myPlaceMenu      =  spawn "mygtkmenu .placerc"
mySreenLock      =  spawn "i3lock -i /home/haron/wall/flame+water.png"
myQST            =  scratchpadSpawnActionTerminal myTerm
-- Decorations
myHlColor        =  "#95d5f5"
myUrgColor       =  "#ff6500"
myBgColor        =  "#151515"
myFgColor        =  "#959595"
myRFont          =  "xft:SonyEricssonLogo:size=10:antialias=true:hinting=true"
myMFont          =  "xft:Terminus Re33 Nerd Bold:size=12:antialias=true:hinting=true"
-- Basic combinations of layouts
myRT1            =  windowNavigation (spacing 1 $ ResizableTall 1 (1/100) (1/2) [])
myRT2            =  windowNavigation (spacing 1 $ ResizableTall 2 (1/100) (2/3) [])
myMRT            =  windowNavigation (spacing 1 $ Mirror (ResizableTall 1 (1/100) (2/3) []))
myMGR            =  windowNavigation (spacing 1 $ multimastered 2 (1/100) (1/3) $ GridRatio (16/10))
-- Combinations of layouts for various workspaces
myWws            =  Full   |||  myRT1  |||  myMRT   -- for WS1 (W)
myMws            =  Full   |||  myRT1               -- for WS2 (M)
myEws            =  myMRT  |||  Full   |||  myRT2   -- for WS3 (E)
myFws            =  Full   |||  myMGR               -- for WS4 (F)
mySws            =  myRT1  |||  myMRT  |||  Full    -- for WS5 (S)
myPws            =  Full   |||  myMGR               -- for WS6 (P)
myVws            =  Full   |||  myRT2               -- for WS7 (V)
myJws            =  Full   |||  myRT2               -- for WS8 (J)
myTws            =  Full   |||  myMRT               -- for WS9 (T)
-- Modkey
altm             =  mod1Mask
ctrlm            =  controlMask
modm             =  mod4Mask
shftm            =  shiftMask
-- Actions
encodeCChar      =  map fromIntegral . B.unpack
onScr n f i      =  screenWorkspace n >>= \sn -> windows (f i . maybe id W.view sn)
viewShift        =  doF . liftM2 (.) W.greedyView W.shift
minWin           =  withFocused minimizeWindow <+> spawn "XMMWO"
-- restWin          =  sendMessage RestoreNextMinimizedWin <+> spawn "XMMWC"
restWin          =  withLastMinimized maximizeWindowAndFocus <+> spawn "XMMWC"
cycleWin         =  cycleRecentWindows [0xffeb] 0xff09 0x77
nSA              =  namedScratchpadAction myNS
-- Other options
role             =  stringProperty "WM_WINDOW_ROLE"
wname            =  stringProperty "WM_NAME"

--- CONFIG ---
-- Key bindings.
myHK conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    --Applications management
      ((0,              0x1008ff13),  spawn "pulseaudio-ctl up")           -- XF86AudioRaiseVolume
    , ((0,              0x1008ff11),  spawn "pulseaudio-ctl down")         -- XF86AudioLowerVolume
    , ((0,              0x1008ff12),  spawn "XMMute")                      -- XF86AudioMute
    , ((0,              0x1008ff14),  spawn "XMMPCtoggle")                 -- XF86Play
    , ((0,              0x1008ff15),  spawn "XMMPCstop")                   -- XF86AudioStop
    , ((0,              0x1008ff16),  spawn "XMMPCprev")                   -- XF86AudioPrev
    , ((0,              0x1008ff17),  spawn "XMMPCnext")                   -- XF86AudioNext
    , ((0,              0x1008ff30),  spawn "subl3")                       -- XF86Favorites
    , ((0,              0x1008ff18),  myBrowser)                           -- XF86HomePage
    , ((0,              0x1008ff19),  spawn "thunderbird")                 -- XF86Mail
    , ((0,              0x1008ff33),  spawn "pcmanfm")                     -- XF86MyComputer
    , ((0,              0x1008ff5d),  spawn "pcmanfm")                     -- XF86Explorer
    , ((0,              0x1008ff1d),  spawn "XMGalculator")                -- XF86Calculator
    , ((0,              0x1008ff1b),  nSA "HTOP")                          -- XF86Search
    , ((0,              0x1008ff81),  nSA "NCMPCPP")                       -- XF86Tools
    , ((0,              0x1008ff46),  spawn "XMR")                         -- XF86Launch6
    , ((0,              0x1008ff73),  spawn "XMUpdateNew")                 -- XF86Reload
    , ((0,              0x1008ff2f),  mySreenLock)                         -- XF86Sleep
    , ((0,                  0xff69),  nSA "ExitMenu")                      -- Cancel
    , ((0,                  0xff67),  spawn "gmrun")                       -- Menu
    , ((0,                  0xffc9),  myQST)                               -- F12
    , ((0,                  0xff61),  myFScrot)                            -- Print
    , ((0    .|. shftm,     0xff61),  myWScrot)                            -- Shift+Print
    , ((altm,               0xff61),  myAScrot)                            -- Alt+Print
    , ((altm,                 0x63),  spawn "chromium")                    -- Alt+C
    , ((altm,                 0x64),  nSA "DEADBEEF")                      -- Alt+D
    , ((altm,                 0x66),  spawn "firefox")                     -- Alt+F
    -- , ((altm,                 0x67),  spawn "gitkraken")                   -- Alt+G
    -- , ((altm .|. ctrlm,       0x67),  spawn "gimp")                        -- Alt+Ctrl+G
    , ((altm,                 0x68),  spawn "hexchat")                     -- Alt+H
    , ((altm,                 0x6c),  spawn "XMLPass")                     -- Alt+L
    , ((altm,                 0x6d),  spawn "urxvtc -name mc -e mc")       -- Alt+M
    , ((altm,                 0x6f),  spawn "opera-developer")             -- Alt+O
    , ((altm,                 0x72),  nSA "FM")                            -- Alt+R
    , ((altm,                 0x74),  spawn "XMTransgui")                  -- Alt+T
    , ((altm,                 0x75),  spawn "XMYAY")                       -- Alt+U
    , ((altm,                 0x76),  myEditor)                            -- Alt+V
    -- , ((altm,                 0x77),  spawn "VoteCoin Wallet")             -- Alt+W
    -- , ((altm,                 0x79),  spawn "XMAurman")                    -- Alt+Y
    -- Terminals
    , ((modm .|. ctrlm,     0xff0d),  spawn "st")                          -- Mod4+Ctrl+Return
    , ((modm .|. altm,      0xff0d),  spawn "urxvtc")                      -- Mod4+Alt+Return
    , ((modm .|. shftm,     0xff0d),  spawn $ XMonad.terminal conf)        -- Mod4+Shift+Return

    -- Recompile || restart || terminate XMonad
    , ((modm,                 0x63),  spawn "XMR")                         -- Mod4+C
    , ((modm,                 0x71),  spawn "XMRR")                        -- Mod4+Q
    , ((modm .|. shftm,       0x71),  io (exitWith ExitSuccess))           -- Mod4+Shift+Q

    -- Menu
    , ((altm,                 0x61),  myAppMenu)                           -- Alt+A
    , ((altm,                 0x62),  myPlaceMenu)                         -- Alt+B

    -- Prompts management
    , ((altm,               0xffbe),  manPrompt myPC)                      -- Alt+F1
    , ((altm,               0xffbf),  runOrRaisePrompt myPC)               -- Alt+F2
    , ((altm,               0xffc0),  sshPrompt myPC)                      -- Alt+F3

    -- WS management
    , ((altm,               0xff09),  nextWS)                              -- Alt+Tab
    , ((altm .|. ctrlm,     0xff09),  prevWS)                              -- Alt+Ctrl+Tab
    , ((altm .|. ctrlm,     0xff53),  DO.moveTo Next HiddenNonEmptyWS)     -- Alt+Ctrl+Right
    , ((altm .|. ctrlm,     0xff51),  DO.moveTo Prev HiddenNonEmptyWS)     -- Alt+Ctrl+Left
    , ((modm,               0xff1b),  toggleWS' ["NSP"])                   -- Mod4+Escape
    , ((modm,               0xff08),  toggleWS' ["NSP"])                   -- Mod4+Backspace
    , ((modm,                 0x20),  sendMessage NextLayout)              -- Mod4+Space
    , ((modm .|. shftm,       0x20),  setLayout $ XMonad.layoutHook conf)  -- Mod4+Shift+Space
    , ((modm,                 0x6e),  refresh)                             -- Mod4+N
    , ((modm,                 0x62),  sendMessage ToggleStruts)            -- Mod4+B
    , ((modm,                 0x68),  sendMessage Shrink)                  -- Mod4+H
    , ((modm .|. shftm,       0x68),  sendMessage MirrorShrink)            -- Mod4+Shift+H
    , ((modm,                 0x6c),  sendMessage Expand)                  -- Mod4+L
    , ((modm .|. shftm,       0x6c),  sendMessage MirrorExpand)            -- Mod4+Shift+L
    , ((modm,                 0x74),  withFocused $ windows . W.sink)      -- Mod4+T
    , ((modm,                 0x2c),  sendMessage (IncMasterN 1))          -- Mod4+Comma
    , ((modm,                 0x2e),  sendMessage (IncMasterN (-1)))       -- Mod4+Period

    -- Windows management
    , ((modm,                 0x60),  rotOpposite)                         -- Mod4+grave
    , ((modm,               0xff09),  cycleWin)                            -- Mod4+Tab
    , ((modm,               0xff53),  sendMessage $ Go R)                  -- Mod4+Right
    , ((modm,               0xff51),  sendMessage $ Go L)                  -- Mod4+Left
    , ((modm,               0xff52),  sendMessage $ Go U)                  -- Mod4+Up
    , ((modm,               0xff54),  sendMessage $ Go D)                  -- Mod4+Down
    , ((modm .|. shftm,     0xff53),  sendMessage $ Swap R)                -- Mod4+Shift+Right
    , ((modm .|. shftm,     0xff51),  sendMessage $ Swap L)                -- Mod4+Shift+Left
    , ((modm .|. shftm,     0xff52),  sendMessage $ Swap U)                -- Mod4+Shift+Up
    , ((modm .|. shftm,     0xff54),  sendMessage $ Swap D)                -- Mod4+Shift+Down
    , ((modm .|. ctrlm,     0xff53),  shiftToNext)                         -- Mod4+Ctrl+Right
    , ((modm .|. ctrlm,     0xff51),  shiftToPrev)                         -- Mod4+Ctrl+Left
    , ((modm,                 0x6a),  windows W.focusDown)                 -- Mod4+J
    , ((modm,                 0x6b),  windows W.focusUp)                   -- Mod4+K
    , ((modm,                 0x6d),  windows W.focusMaster)               -- Mod4+M
    , ((modm,               0xff0d),  windows W.swapMaster)                -- Mod4+Return
    , ((modm .|. shftm,       0x6a),  windows W.swapDown)                  -- Mod4+Shift+J
    , ((modm .|. shftm,       0x6b),  windows W.swapUp)                    -- Mod4+Shift+K
    , ((modm,                 0x7a),  minWin)                              -- Mod4+Z
    , ((modm .|. shftm,       0x7a),  restWin)                             -- Mod4+Shift+Z
    , ((modm,                 0x78),  kill)                                -- Mod4+X

    -- XMobar action management
    , ((altm,                 0x30),  spawn "XMUptimeToggle")              -- Alt+0
    , ((altm,                 0x31),  spawn "XMNetToggle")                 -- Alt+1
    , ((altm .|. ctrlm,       0x31),  spawn "XMVnstat+h")                  -- Alt+Ctrl+1
    , ((altm,                 0x32),  spawn "XMTrafToggle")                -- Alt+2
    , ((altm .|. ctrlm,       0x32),  spawn "XMVnstat-d")                  -- Alt+Ctrl+2
    , ((altm,                 0x33),  spawn "XMCPUToggle")                 -- Alt+3
    , ((altm .|. ctrlm,       0x33),  spawn "XMTop-cpu")                   -- Alt+Ctrl+3
    , ((altm,                 0x34),  spawn "XMTempToggle")                -- Alt+4
    , ((altm .|. ctrlm,       0x34),  spawn "XMSensors")                   -- Alt+Ctrl+4
    , ((altm,                 0x35),  spawn "XMMemToggle")                 -- Alt+5
    , ((altm .|. ctrlm,       0x35),  spawn "XMTop-mem")                   -- Alt+Ctrl+5
    , ((altm .|. shftm,       0x35),  spawn "XMdf-h")                      -- Alt+Shift+5
    , ((altm,                 0x36),  spawn "XMVolToggle")                 -- Alt+6
    , ((altm,                 0x37),  spawn "XMDateToggle")                -- Alt+7
    , ((altm,                 0x39),  spawn "XMGWeather")                  -- Alt+9
    , ((altm,                 0x60),  spawn "XMToggleAll")                 -- Alt+grave
    , ((altm,               0xff1b),  spawn "XMCleanAll")                  -- Alt+Escape
    ]

    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shftm)]]

    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shftm)]]

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
    where clickable l = [ "<action=`xdotool key 0xffeb+" ++ show (n) ++ "` button=1><fn=4>" ++ ws ++ "</fn></action>" |
                        (i,ws) <- zip ["0x31", "0x32", "0x33", "0x34", "0x35", "0x36", "0x37", "0x38", "0x39", "0x30", "0x2d", "0x3d"] l,
                        let n = i 
                        ]

-- Layouts Hook
myLH =  avoidStruts
        $ minimize
        $ Tog.toggleLayouts (noBorders Full) 
        $ smartBorders
        $ onWorkspace (myWS !! 0) myWws
        $ onWorkspace (myWS !! 1) myMws
        $ onWorkspace (myWS !! 2) myEws
        $ onWorkspace (myWS !! 3) myFws
        $ onWorkspace (myWS !! 4) mySws
        $ onWorkspace (myWS !! 5) myVws
        $ onWorkspace (myWS !! 6) myPws
        $ onWorkspace (myWS !! 7) myJws
        $ onWorkspace (myWS !! 8) myTws
        $ myRT1 ||| myMRT ||| myMGR ||| Full
-- Window Management Rules
myWR = composeAll . concat $
    [ 
      [className =? c                 --> doShift (myWS !! 0)   <+> viewShift (myWS !! 0)    | c <- myW]
    , [className =? c                 --> doShift (myWS !! 1)                                | c <- myM]
    , [className =? c                 --> doShift (myWS !! 2)   <+> viewShift (myWS !! 2)    | c <- myE]
    , [className =? c                 --> doShift (myWS !! 3)   <+> viewShift (myWS !! 3)    | c <- myF]
    , [className =? c                 --> doShift (myWS !! 4)   <+> viewShift (myWS !! 4)    | c <- myS]
    , [className =? c                 --> doShift (myWS !! 5)   <+> viewShift (myWS !! 5)    | c <- myV]
    , [className =? c                 --> doShift (myWS !! 6)   <+> viewShift (myWS !! 6)    | c <- myP]
    , [className =? c                 --> doShift (myWS !! 7)   <+> viewShift (myWS !! 7)    | c <- myJ]
    , [className =? c                 --> doShift (myWS !! 8)   <+> viewShift (myWS !! 8)    | c <- myT]
    , [className =? c                 --> doShift (myWS !! 9)   <+> viewShift (myWS !! 9)    | c <- myX]
    , [className =? c                 --> doShift (myWS !! 10)  <+> viewShift (myWS !! 10)   | c <- myXI]
    , [className =? c                 --> doShift (myWS !! 11)  <+> viewShift (myWS !! 11)   | c <- myXII]

    , [className =? c                 --> doCenterFloat                                      | c <- myFC]
    , [appName   =? a                 --> doCenterFloat                                      | a <- myFA]
    , [title     =? t                 --> doCenterFloat                                      | t <- myFT]
    , [role      =? r                 --> doCenterFloat                                      | r <- myFR]
    , [wname     =? n                 --> doCenterFloat                                      | n <- myFN]

    , [role      =? r                 --> (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)  | r <- myCF]

    , [currentWs =? w                 --> insertPosition Below Newer                         | w <- myPBN]
    , [currentWs =? w                 --> insertPosition End Newer                           | w <- myPEN]

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
                , "Nightly"
                , "Opera"
                , "Opera developer"
                , "Tor Browser"
                , "Vivaldi-snapshot"
                , "Vivaldi-stable"
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
                , "PkgBrowser"
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
                , "Auryo"
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
                , "GitKraken"
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
                  "VoteCoin Wallet"
                ]
        -- Application groups in center floating windows
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
        myFN  = [
                  "About Nightly"
                ]
        -- Custom floating windows
        myCF  = [
                  "Organizer"
                , "Msgcompose"
                , "addressbook"
                , "filterlist"
                ]
        -- The position below for newer windows to the specified workspaces
        myPBN = [
                  (myWS !! 0)
                -- , (myWS !! 1)
                , (myWS !! 8)
                ]
        -- The position end for newer windows to the specified workspaces
        myPEN = [
                  (myWS !! 2)
                , (myWS !! 9)
                , (myWS !! 10)
                , (myWS !! 11)
                ]

-- Prompts config
myPC = def {
             font              = myMFont   
           , bgColor           = myBgColor
           , fgColor           = myFgColor
           , bgHLight          = myBgColor
           , fgHLight          = myHlColor
           , promptBorderWidth = 0
           , position          = Top
           , height            = 20
           , alwaysHighlight   = True
           , historySize       = 100
           }

-- Named Scratchpad
myNS = [
         NS "MIRAGE"       "mirage"       (className  =? "Mirage")       (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
       , NS "GSIMPLECAL"   "gsimplecal"   (className  =? "Gsimplecal")   (customFloating $ W.RationalRect 0.43 0.4 0.14 0.2)
       , NS "ExitMenu"     "oblogout"     (className  =? "Oblogout")     (customFloating $ W.RationalRect 0.32 0.4 0.36 0.2)
       , NS "DEADBEEF"     "deadbeef"     (className  =? "Deadbeef")     (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)

       , NS "NCMPCPP"      myPlayer       (appName    =? "ncmpcpp")      (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)
       , NS "HTOP"         myHtop         (appName    =? "htop")         (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8)
       , NS "GPICK"        "gpick"        (appName    =? "gpick")        (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
       , NS "PAVUCONTROL"  "pavucontrol"  (appName    =? "pavucontrol")  (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
       , NS "XMUPDATE"     "XMUpdateNew"  (appName    =? "update")       (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)
       , NS "NOTES"        myNotes        (appName    =? "Notes")        (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
       , NS "FM"           myFM           (appName    =? "ranger")       (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)
       ]

-- Scratchpad (myQST)
mySP = scratchpadManageHook (W.RationalRect l t w h)
    where
        h = 0.333       -- terminal height
        w = 1           -- terminal width
        t = 1 - h       -- distance from top edge
        l = (1 - w)/2   -- distance from left edge

-- Manage Hook
myMH = manageHook def <+> 
       myWR <+> 
       mySP <+> 
       namedScratchpadManageHook myNS <+> 
       placeHook (smart (0.5,0.5)) <+> 
       workspaceByPos

-- Event handling
myEH = handleEventHook def <+> 
       fullscreenEventHook <+> 
       docksEventHook <+> 
       ewmhDesktopsEventHook <+> 
       minimizeEventHook <+>
       focusOnMouseMove

-- Startup Hook
mySH = return () <+> 
       adjustEventInput <+> 
       setWMName "LG3D" <+> 
       -- onScr 1 W.greedyView (myWS !! 0) <+> 
       spawn "XMStart" 

main = do
    xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobar.hs"
    xmonad $ ewmh $ withUrgencyHookC NoUrgencyHook urgencyConfig def 
        {
          terminal           = myTerm
        , borderWidth        = 1
        , normalBorderColor  = myBgColor
        , focusedBorderColor = myHlColor
        , modMask            = modm
        , focusFollowsMouse  = True
        , workspaces         = myWS
        , keys               = myHK
        , mouseBindings      = myMB
        , startupHook        = mySH
        , layoutHook         = myLH
        , manageHook         = myMH
        , handleEventHook    = myEH
        , logHook            = dynamicLogWithPP $ def {
                                                        ppOutput  = System.IO.hPutStrLn xmproc
                                                      , ppCurrent = xmobarColor myHlColor ""
                                                      , ppUrgent  = xmobarColor myUrgColor ""
                                                      , ppOrder   = \(ws:l:t:_) -> [ws]
                                                      }
        }
