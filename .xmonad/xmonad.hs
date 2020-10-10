{- LANGUAGE OverloadedStrings -}
import Control.Arrow ( first )
import Control.Concurrent (threadDelay)
import Data.List ( isInfixOf , intercalate)
import Data.Text (splitOn, unpack, pack, replace)
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.Posix.Unistd

import XMonad
import XMonad.Actions.CycleWS ( toggleWS' , moveTo , findWorkspace , shiftTo , WSType ( HiddenNonEmptyWS , EmptyWS))
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.Submap ( submap )
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.ManageDocks
import XMonad.Layout.CenteredMaster ( centerMaster )
import XMonad.Layout.NoBorders ( noBorders, smartBorders )
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPanePersistent
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Input
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Util.Paste
import XMonad.Util.Run ( spawnPipe , hPutStrLn, safeSpawn )
import XMonad.Util.SpawnOnce ( spawnOnce )
import XMonad.Util.WorkspaceCompare ( getSortByIndex )
                 
import qualified XMonad.StackSet               as W
import qualified Data.Map                      as M
import qualified XMonad.Actions.Search         as S
                 
main = do
  xmobarPipe <- spawnPipe "xmobar -x 0 /home/davison/.config/xmobar/xmobarrc"
  xmonad . docks . ewmh . myConfig $ xmobarPipe

-- KEYBINDING
--
--    alt      numlock     hyper      win
-- mod1Mask | mod2Mask | mod3Mask | mod4Mask
myModMask :: KeyMask
myModMask = mod3Mask
                 
data SimpleKeybind = K CDMask KeySym (X())
data SimpleMousebind = M CDMask Button (Window -> X())

data CDMask = Hyper | Win | Alt | Shift | HyperShift | WinShift | AltShift | HyperCtrl | None

fromKeybind :: SimpleKeybind -> ((KeyMask, KeySym), X ())
fromKeybind (K None       k command) = ((0, k), command)
fromKeybind (K Hyper      k command) = ((mod3Mask, k), command)
fromKeybind (K Win        k command) = ((mod4Mask, k), command)
fromKeybind (K Alt        k command) = ((mod1Mask, k), command)
fromKeybind (K Shift      k command) = ((shiftMask, k), command)
fromKeybind (K HyperShift k command) = ((mod3Mask .|. shiftMask, k), command)
fromKeybind (K HyperCtrl  k command) = ((mod3Mask .|. controlMask, k), command)
fromKeybind (K WinShift   k command) = ((mod4Mask .|. shiftMask, k), command)
fromKeybind (K AltShift   k command) = ((mod1Mask .|. shiftMask, k), command)

fromMousebind :: SimpleMousebind -> ((KeyMask, Button), Window -> X ())
fromMousebind (M None       b command) = ((0, b), command)
fromMousebind (M Hyper      b command) = ((mod3Mask, b), command)
fromMousebind (M Win        b command) = ((mod4Mask, b), command)
fromMousebind (M Alt        b command) = ((mod1Mask, b), command)
fromMousebind (M Shift      b command) = ((shiftMask, b), command)
fromMousebind (M HyperShift b command) = ((mod3Mask .|. shiftMask, b), command)
fromMousebind (M WinShift   b command) = ((mod4Mask .|. shiftMask, b), command)
fromMousebind (M AltShift   b command) = ((mod1Mask .|. shiftMask, b), command)

submapFromKeybind = submap . keysFromSimpleKeybinds
keysFromSimpleKeybinds = M.fromList . (map fromKeybind) 
keysFromSimpleMousebinds = M.fromList . (map fromMousebind)

myMouseBindings _conf = keysFromSimpleMousebinds
  [ M Hyper      button1 (\w -> focus w >> mouseMoveWindow w) -- Left = move
  , M Hyper      button2 (\w -> focus w >> windows W.swapMaster) -- Middle = make master
  , M Hyper      button3 (\w -> focus w >> mouseResizeWindow w) -- Right = resize
    -- Mouse wheel is sensitive, so use  smaller volume increments
  , M Hyper      button4 (\w -> (doVolume "up")) -- Scroll up = vol up
  , M Hyper      button5 (\w -> (doVolume "down")) -- Scroll down = vol down
  , M HyperShift button4 (\w -> gotoNonEmptyWS Prev) -- Scroll up = vol up
  , M HyperShift button5 (\w -> gotoNonEmptyWS Next) -- Scroll down = vol down
  ]

-- need '$' to apply func to all '++' concatenated lists
myKeys conf = keysFromSimpleKeybinds $  
  [ K HyperShift xK_q            (io (exitWith ExitSuccess))      -- QUIT
  , K HyperShift xK_c            (kill)                           -- close the focused window
  , K Hyper      xK_q            (restart "xmonad" True)
  --- WINDOW FOCUS 
  , K Hyper      xK_bracketleft  (gotoNonEmptyWS Prev)            -- find prev empty workspace
  , K Hyper      xK_bracketright (gotoNonEmptyWS Next)            -- find next empty workspace
  , K HyperShift xK_bracketleft  (tagToEmptyWorkspace)         -- move win to prev empty workspace
  , K HyperShift xK_bracketright (shiftToEmptyWS Next)         -- move win to next empty workspace
  , K Hyper      xK_j            (windows W.focusDown)            -- focus window up stack
  , K Hyper      xK_k            (windows W.focusUp)              -- focus window down stack
  , K Hyper      xK_m            (windows W.focusMaster)          -- focus master window
  , K HyperShift xK_j            (windows W.swapDown)             -- move window up stack
  , K HyperShift xK_k            (windows W.swapUp)               -- move window down stack
  , K HyperShift xK_m            (windows W.swapMaster)           -- make window stack master
  --- LAYOUT - MASTER
  , K Hyper      xK_h            (sendMessage Shrink)             -- smaller master window
  , K Hyper      xK_l            (sendMessage Expand)             -- larger master window
  , K HyperShift xK_h            (sendMessage MirrorShrink)       -- smaller slave window
  , K HyperShift xK_l            (sendMessage MirrorExpand)       -- larger slave window
  , K Hyper      xK_comma        (sendMessage (IncMasterN 1))     -- increase num master windows
  , K Hyper      xK_period       (sendMessage (IncMasterN (-1)))  -- decrease num master windows
  --- LAYOUT   
  , K Hyper      xK_space        (sendMessage NextLayout)         -- Use next configured layout
  , K HyperShift xK_space        (setLayout $ XMonad.layoutHook conf)             -- reset to default layout
  , K Hyper      xK_f            (fullscreenNoBar)                -- toggle fullscreen on focused window
  , K HyperShift xK_f            (toggleBar)                      -- toggle fullscreen on focused window
  , K Hyper      xK_t            (withFocused $ windows . W.sink) -- make float tiled again
  --- LAUNCHERS
  , K Hyper      xK_g            (windowPromptGoto myXPConfig)
  , K HyperShift xK_g            (bringMenuConfig myBringConfig)
  , K Hyper      xK_r            (spawn $ myTerminal ++ " -e ranger")
  , K Hyper      xK_Return       (spawn myTerminal)
  , K Alt        xK_Return       (spawn myTerminal)
  , K Win        xK_Return       (spawn myTerminal)
  , K Hyper      xK_p            (spawn $ myJ4Command)
  , K Hyper      xK_w            (spawn "$HOME/.bin/dmenu_win_switcher.sh")
  , K Hyper      xK_b            (spawn "$HOME/.bin/dmenu_bookmark_groups.sh")
  , K HyperShift xK_b            (spawn "$HOME/.bin/dmenu_bookmarks.sh")
  , K Hyper      xK_F5           (runOrRaise "spotify" (className =? "Spotify"))
  , K Hyper      xK_F6           (cycleFirefox)
  , K HyperShift xK_d            (spawn $ "notify-send -- " ++ myDmenuConfig)
  --- firefox / youtube doesn't work well with instant xdotool, so add 100ms delay after switching window
  , K Hyper      xK_F7           (raiseNext (title =?? "ASMR") <> delay 100000 <> spawn "xdotool key N")
  , K Hyper      xK_F11          (spawn "$HOME/.bin/dmenu_asmr.py")
  , K Hyper      xK_F12          (S.promptSearch myXPConfig S.duckduckgo)
  --- LAUNCHERS EMACS
  , K Hyper      xK_F1           (onMainMonitor <> raiseEmacsAndRun "(org-capture)")
  , K Hyper      xK_F2           (onMainMonitor <> raiseEmacsAndRun "(org-agenda)")
  , K Hyper      xK_F3           (onMainMonitor <> raiseEmacsAndRun "(org-agenda nil \"c1\")")
  , K Hyper      xK_F4           (onMainMonitor <> raiseEmacsAndRun "(org-agenda nil \"Rw\")")
  -- Keybinds for specific captures - note, note entry, todo, and work todo
  , K Hyper      xK_c            (submapFromKeybind [ K None  xK_n (onMainMonitor <> orgCapture "nn")
                                                    , K Shift xK_n (onMainMonitor <> orgCapture "nN")
                                                    , K None  xK_t (onMainMonitor <> orgCapture "tt")
                                                    , K None  xK_w (onMainMonitor <> orgCapture "tw")])
  --- AUDIO / MUSIC
  , K Hyper      xK_Home                  (doVolume "up")
  , K Hyper      xK_End                   (doVolume "down")
  , K Hyper      xK_Delete                (doSpotify "prev")
  , K Hyper      xK_Next                  (doSpotify "next")
  , K Hyper      xK_Prior                 (doSpotify "play-pause")
  , K None       xF86XK_AudioRaiseVolume  (doVolume "up")
  , K None       xF86XK_AudioLowerVolume  (doVolume "down")
  , K None       xF86XK_AudioMute         (doVolume "mute")
  , K None       xF86XK_AudioPrev         (doSpotify "prev")
  , K None       xF86XK_AudioNext         (doSpotify "next")
  , K None       xF86XK_AudioPlay         (doSpotify "play-pause")
  , K HyperShift xK_Home                  (doMic "up")
  , K HyperShift xK_End                   (doMic "down")
                                                                           -- Brightness
  , K None       xF86XK_KbdBrightnessUp   (spawn "$HOME/.bin/bright.sh up")
  , K None       xF86XK_MonBrightnessUp   (spawn "$HOME/.bin/bright.sh up")
  , K None       xF86XK_KbdBrightnessDown (spawn "$HOME/.bin/bright.sh down")
  , K None       xF86XK_MonBrightnessDown (spawn "$HOME/.bin/bright.sh down")
                                                                           -- KEYCHORD - H-d {e,a} -- open ebooks or literature
  , K Hyper      xK_d (submapFromKeybind [ K None xK_e (spawn "$HOME/.bin/dmenu_ebooks.sh")
                                         , K None xK_a (spawn "$HOME/.bin/dmenu_articles.sh")])
  ]
                                                                           
  ++ [ K Hyper      key (pullWs ws)           | (key, ws) <- keyWsPairs ]     -- mod-[1..9]       :: view workspace on current monitor
  ++ [ K HyperShift key (moveWinToWs ws)      | (key, ws) <- keyWsPairs ]     -- mod-shift-[1..9] :: move window to workspace
  ++ [ K Win        key (viewWsOnMon ws 0)    | (key, ws) <- keyWsPairs ]     -- win-[1..9]       :: view workspace on main monitor
  ++ [ K Alt        key (viewWsOnMon ws 1)    | (key, ws) <- keyWsPairs ]     -- alt-[1..9]       :: view workspace on second monitor
                                                                           
  ++ [ K Hyper      key (focusMonitor sc)     | (key, sc) <- keyScreenPairs ] -- mod-{a,s}        :: view main or second monitor
  ++ [ K HyperShift key (moveWinToMonitor sc) | (key, sc) <- keyScreenPairs ] -- mod-{A,S}        :: move window to workspace on main or second monitor
 where
  keyWsPairs     = zip [xK_1 .. xK_9] myWorkspaces
  keyScreenPairs = zip [xK_a, xK_s] [0 ..]
  

doVolume :: String -> X()
doVolume cmd = spawn $ "$HOME/.bin/volume.sh --" ++ cmd

doMic :: String -> X()
doMic cmd = spawn $ "$HOME/.bin/micgain.sh --" ++ cmd

doSpotify :: String -> X()
doSpotify cmd = spawn $ "$HOME/.bin/spotify.sh " ++ cmd

cycleFirefox :: X()
cycleFirefox = raiseNextMaybe (return ()) (className =? "Firefox")

------------------------------------------------------------------------
-- Layouts:
lFull      = noBorders Full
lTall      = smartBorders $ ResizableTall 1 (3/100) (2/3) []
lTallEven  = smartBorders $ ResizableTall 1 (3/100) (1/2) []
lTiled     = toggleLayouts lFull lTall
lTiledEven = toggleLayouts lFull lTallEven
lTwoPane   = toggleLayouts lFull $ TwoPanePersistent Nothing (3/100) (1/2)
gaps       = spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True

defaultLayouts = avoidStruts . gaps $
  lTiled
  ||| Mirror lTiled
  ||| lTwoPane

myLayout = defaultLayouts

-- Manage Hook is for applying rules to specific windows
-- className matches xprop WM_CLASS[2]
-- title matches xprop WM_NAME
--
-- Matching (=?? =?! defined below)
--    exact =?    (e.g. 'needle' =? 'needle')
--    infix =??   (e.g. 'this has a needle somewhere' =?? 'needle')
--    fuzzy =?!   (e.g. 'n something EEDLE' =?! 'needle')   ! case insensitive
myManageHook = manageDocks <> composeAll
  [
    className =? "zoom"            --> doShift "3"
  , className =? "Anki"            --> doShift "4"
  , className =? "TeamViewer"      --> doShift "5"
  , className =? "Droidcam"        --> doShift "7"
  , className =? "Pulseeffects"    --> doShift "7"
  , className =? "Steam"           --> doShift "7"
  , className =? "Spotify"         --> doShift "8"
  , className =? "Bitwarden"       --> doFloat
  , className =? "Pavucontrol"     --> doFloat
  , className =? "Blueman-manager" --> doFloat
  , resource  =? "desktop_window"  --> doIgnore
  , resource  =? "kdesktop"        --> doIgnore
  , title     =? "scratchpad"      --> doFloat
  ]

------------------------------------------------------------------------
-- Startup hook
myStartupHook = do
  spawnOnce "redshift"
  spawnOnce "autorandr"
  spawnOnce "~/.fehbg"
  spawnOnce "compton &"
  spawnOnce "/opt/Mullvad VPN/mullvad-vpn"
  spawnOnce
    "trayer --edge top --align center --widthtype request --padding 0 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 255 --height 21 &"
  spawnOnce "dropbox start"

------------------------------------------------------------------------
-- Configure workspace names in xmobar
logWorkspacesOnXmobar pipe = dynamicLogWithPP xmobarPP
  { ppOutput  = hPutStrLn pipe
  , ppTitle   = \w -> xmobarColor cPink "" $ shorten 50 w
  , ppCurrent = \w -> xmobarColor cPink "" $ "[" ++ w ++ "]"
  , ppVisible = \w -> "(" ++ w ++ ")"
  , ppWsSep   = " "
  , ppSep     = " ::: "
  , ppLayout  = \_ -> ""
  }
  where surround s c = c ++ s ++ c

myConfig pipe = def { terminal           = myTerminal
                    , focusFollowsMouse  = True
                    , borderWidth        = 2
                    , modMask            = myModMask
                    , workspaces         = myWorkspaces
                    , normalBorderColor  = cGrey
                    , focusedBorderColor = cPurple
                    , keys               = myKeys
                    , mouseBindings      = myMouseBindings
                    , layoutHook         = myLayout
                    , manageHook         = myManageHook
                    , logHook            = logWorkspacesOnXmobar pipe
                    , startupHook        = myStartupHook
                    }

--------------------------------------------------------------------------------
-- OTHER GENERAL UTILITY STUFF
-- functions, and personal configuration

-- COLOURS & other style stuff
cPurple = "#8620e6"
cLightPurple = "#bf7efc"
cPink = "#f438ee"
cGrey = "#333333"
cLightGrey = "#dddddd"
cWhite = "#ffffff"

myFont = "mononoki"
myFontXFT size = "xft:" ++ myFont ++ ":pixelsize=" ++ show size ++ ":antialias=true:hinting=true"

myTerminal = "alacritty"
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myXPConfig :: XPConfig
myXPConfig = def { font                = myFontXFT 14
                 , bgColor             = cGrey
                 , fgColor             = cLightGrey
                 , borderColor         = cGrey
                 , bgHLight            = cPurple
                 , fgHLight            = cWhite
                 , promptBorderWidth   = 0
                 , position            = Top
                 --, height              = 21
                 , alwaysHighlight     = True
                 }
myXPConfigLG = myXPConfig { font = myFontXFT 18
                          , height = 21
                          }

myBringConfig :: WindowBringerConfig
myBringConfig = def {
  menuArgs = map (repl . unpack) $ splitOn (pack " ") (pack myDmenuConfig)
  }
  where repl c = case c of
          "'"       -> ""
          otherwise -> otherwise

-- Execution of programs
myJ4Command :: String
myJ4Command = "j4-dmenu-desktop --dmenu=\"dmenu " ++ myDmenuConfig ++ "\""

myDmenuConfig :: String
myDmenuConfig = "-l 10 -i -fn '" ++ font ++ "' -p 'App:' " ++ intercalate " " pairs
  where
    font = myFont ++ "-14"
    args = [("-sb", cPurple),                         -- background colour
            ("-nhb", cLightPurple), ("-nhf", cWhite), -- colour for non-highlighted lines
            ("-shb", cLightPurple), ("-shf", cWhite)] -- colour for highlighted lines
    pairs = [arg ++ " '" ++ colour ++ "'" | (arg, colour) <- args]
                 

fullscreenNoBar :: X()
fullscreenNoBar = toggleFullscreen <> toggleBar

toggleFullscreen :: X()
toggleFullscreen = sendMessage (Toggle "Full")

toggleBar :: X()
toggleBar = sendMessage ToggleStruts

getNonEmptyWs :: Direction1D -> X WorkspaceId
getNonEmptyWs dir = findWorkspace getSortByIndex dir HiddenNonEmptyWS 1

getEmptyWs :: Direction1D -> X WorkspaceId
getEmptyWs dir = findWorkspace getSortByIndex dir EmptyWS 1

focusMonitor :: ScreenId -> X()
focusMonitor n = flip whenJust viewWs =<< screenWorkspace n

moveWinToMonitor :: ScreenId -> X()
moveWinToMonitor n = flip whenJust moveWinToWs =<< screenWorkspace n

pullWs :: WorkspaceId -> X()
pullWs      = windows . W.greedyView

viewWsOnMon :: WorkspaceId -> ScreenId -> X()
viewWsOnMon ws mon = focusMonitor mon <> pullWs ws

moveWinToWs :: WorkspaceId -> X()
moveWinToWs = windows . W.shift

viewWs :: WorkspaceId -> X()
viewWs = windows . W.view

-- Move to Next or Prev non empty workspace
gotoNonEmptyWS :: Direction1D -> X ()
gotoNonEmptyWS dir = (getNonEmptyWs dir) >>= viewWs

shiftToEmptyWS :: Direction1D -> X ()
shiftToEmptyWS dir = moveWinToWs =<< getEmptyWs dir

-- EMACS utilities
runEmacs :: String -> X ()
runEmacs cmd = spawn $ "emacsclient -e '" ++ cmd ++ "'"

raiseEmacsAndRun :: String -> X ()
raiseEmacsAndRun cmd = runOrRaise "emacsclient -c" (className =? "Emacs") <> runEmacs cmd

onMainMonitor :: X()
onMainMonitor = focusMonitor 0

onSecondMonitor :: X()
onSecondMonitor = focusMonitor 1

orgCapture :: String -> X ()
orgCapture keys = raiseEmacsAndRun $ "(org-capture nil \"" ++ keys ++ "\")"
--- end emacs utilities


-- Substring version of XMonad's =?
-- e.g. (title =?? "asmr") will match ("this is an asmr video")
(=??) :: Query String -> String -> Query Bool
q =?? f = fmap (f `isInfixOf`) q

-- Fuzzy version of XMonad's =?
-- e.g. (title =?! "asmr") will match ("A S blahblah M R")
(=?!) :: Query String -> String -> Query Bool
q =?! f = fmap (fuzzyMatch f) q

delay :: Int -> X ()
delay us = io (threadDelay us)
