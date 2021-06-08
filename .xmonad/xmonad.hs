{- LANGUAGE OverloadedStrings -}
import Control.Arrow ( first )
import Control.Concurrent (threadDelay)
import Control.Monad (liftM2)
import Data.List ( isInfixOf , intercalate)
import Data.Text (splitOn, unpack, pack, replace)
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.Posix.Unistd

import XMonad
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
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
data CDMask = Hyper | Win | Alt | Shift
  | HyperShift | WinShift | AltShift
  | HyperCtrl | HyperAlt
  | HyperAltShift
  | None

toKeyMask :: CDMask -> KeyMask
toKeyMask None          = 0
toKeyMask Hyper         = mod3Mask                 
toKeyMask Win           = mod4Mask                 
toKeyMask Alt           = mod1Mask                 
toKeyMask Shift         = shiftMask                
toKeyMask HyperShift    = mod3Mask .|. shiftMask   
toKeyMask HyperCtrl     = mod3Mask .|. controlMask 
toKeyMask HyperAlt      = mod3Mask .|. mod1Mask
toKeyMask HyperAltShift = mod3Mask .|. mod1Mask .|. shiftMask
toKeyMask WinShift      = mod4Mask .|. shiftMask   
toKeyMask AltShift      = mod1Mask .|. shiftMask   

fromKeybind :: SimpleKeybind -> ((KeyMask, KeySym), X ())
fromKeybind (K mask k command) = ((toKeyMask mask, k), command)

fromMousebind :: SimpleMousebind -> ((KeyMask, Button), Window -> X ())
fromMousebind (M mask b command) = ((toKeyMask mask, b), command)

myMouseBindings _conf = M.fromList . (map fromMousebind) $
  [ M Hyper      button1 (\w -> focus w >> mouseMoveWindow w)     -- Left        = move
  , M Hyper      button2 (\w -> focus w >> windows W.swapMaster)  -- Middle      = make master
  , M Hyper      button3 (\w -> focus w >> mouseResizeWindow w)   -- Right       = resize
    --- Mouse wheel is sensitive, so use  smaller volume increments
  , M Hyper      button4 (\w -> (doVolume "up"))                  -- Scroll up   = vol up
  , M Hyper      button5 (\w -> (doVolume "down"))                -- Scroll down = vol down
  , M HyperShift button4 (\w -> gotoNonEmptyWS Prev)              -- Scroll up   = vol up
  , M HyperShift button5 (\w -> gotoNonEmptyWS Next)              -- Scroll down = vol down
  ]

-- need '$' to apply func to all '++' concatenated lists
makeKeybinds = M.fromList . (map fromKeybind)
makeSubmap = submap . M.fromList . (map fromKeybind)
myKeys conf = makeKeybinds $  
  [ K HyperShift xK_q            (io (exitWith ExitSuccess))      -- QUIT
  , K HyperShift xK_c            (kill)                           -- close the focused window
  , K Hyper      xK_q            (restart "xmonad" True)
  --- WINDOW FOCUS 
  , K Hyper      xK_bracketleft  (gotoNonEmptyWS Prev)            -- find prev empty workspace
  , K Hyper      xK_bracketright (gotoNonEmptyWS Next)            -- find next empty workspace
  , K HyperAlt   xK_bracketleft  (gotoEmptyWS Prev)               -- find prev empty workspace
  , K HyperAlt   xK_bracketright (gotoEmptyWS Next)               -- find next empty workspace
  , K HyperShift xK_bracketleft  (tagToEmptyWorkspace)            -- move win to prev empty workspace
  , K HyperShift xK_bracketright (shiftToEmptyWS Next)            -- move win to next empty workspace
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
  --- Github issues
  , K HyperAlt   xK_i            (spawn "firefox https://github.com/ChrisDavison/knowledge/issues/new")
  , K HyperAlt   xK_l            (spawn "firefox https://github.com/ChrisDavison/logbook/issues/new")
  --- LAYOUT   
  , K Hyper      xK_space        (sendMessage NextLayout)         -- Use next configured layout
  , K HyperShift xK_space        (setLayout $ XMonad.layoutHook conf)             -- reset to default layout
  , K Hyper      xK_f            (toggleFullscreen)               -- toggle fullscreen on focused window
  , K HyperShift xK_f            (toggleBar)                      -- toggle fullscreen on focused window
  , K HyperAlt   xK_f            (windows copyToAll)              -- toggle fullscreen on focused window
  , K HyperAltShift xK_f         (killAllOtherCopies)     -- toggle fullscreen on focused window
  , K Hyper      xK_t            (withFocused $ windows . W.sink) -- make float tiled again
  --- LAUNCHERS
  , K Hyper      xK_g            (windowPromptGoto myXPConfig)
  , K HyperShift xK_g            (bringMenuConfig myBringConfig)
  -- , K HyperShift xK_g            (bringMenuConfig def)
  , K Hyper      xK_r            appSpawnerSubmap
  , K Hyper      xK_Return       (spawn myTerminal)
  , K Alt        xK_Return       (spawn myTerminal)
  , K Win        xK_Return       (spawn myTerminal)
  , K Hyper      xK_p            (spawn myJ4Command)
  , K Hyper      xK_w            (spawn "$HOME/.bin/dmenu_win_switcher.sh")
  , K Hyper      xK_b            (spawn "$HOME/.bin/dmenu_bookmark_groups.sh")
  , K HyperShift xK_b            (spawn "$HOME/.bin/dmenu_bookmarks.sh")
  , K Hyper      xK_F5           (runOrRaise "spotify" (className =? "Spotify"))
  , K Hyper      xK_F6           (cycleFirefox)
  , K HyperShift xK_w            (spawn "nitrogen /media/nas/pictures/wallpapers")
  --- firefox / youtube doesn't work well with instant xdotool, so add 100ms delay after switching window
  , K Hyper      xK_F7           (raiseNext (title =?? "ASMR") <> delay 100000 <> spawn "xdotool key N")
  , K Hyper      xK_F11          (spawn "$HOME/.bin/dmenu_asmr.py")
  , K Hyper      xK_F12          (S.promptSearch myXPConfig S.duckduckgo)
  --- LAUNCHERS EMACS
  , K Hyper      xK_F1           (focusMonitor 0 <> raiseEmacsAndRun "(org-capture)") 
  , K Hyper      xK_F2           (focusMonitor 0 <> orgAgenda "c1") 
  , K Hyper      xK_F3           (focusMonitor 0 <> raiseEmacsAndRun "(org-agenda)") 
  -- Keybinds for specific captures - note, note entry, todo, and work todo
  , K Hyper      xK_c            (makeSubmap [ K None  xK_t (orgCapture "t")
                                             , K None  xK_n (orgCapture "j")
                                             , K Shift xK_n (orgCapture "J")
                                             ])
  , K HyperCtrl  xK_c            (cycleFirefoxNotMatching (title =?? "ASMR"))
  --- AUDIO / MUSIC
  , K Hyper      xK_Home                  (doVolume "up")
  , K Hyper      xK_End                   (doVolume "down")
  , K Hyper      xK_v                     (doVolume "up")
  , K HyperShift xK_v                     (doVolume "down")
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
  , K None       xF86XK_KbdBrightnessUp   (spawn $ brightness "up")
  , K None       xF86XK_MonBrightnessUp   (spawn $ brightness "up")
  , K None       xF86XK_KbdBrightnessDown (spawn $ brightness "down")
  , K None       xF86XK_MonBrightnessDown (spawn $ brightness "down")
                                                                           -- KEYCHORD - H-d {e,a} -- open ebooks or literature
  , K Hyper      xK_d                     (makeSubmap [
                        K None xK_e (spawn "$HOME/.bin/dmenu_ebooks.sh")
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
  appSpawnerSubmap = makeSubmap [
     K None xK_r (spawn $ myTerminal ++ " -e ranger")
   , K None xK_c (spawn $ myTerminal ++ " -e eva")
   , K None xK_f (spawn "firefox")
   , K None xK_l (spawn $ myTerminal ++ " -e $HOME/.bin/lyricsearch.sh")
   , K None xK_e (runOrRaise "emacsclient -c" (className =? "Emacs"))
   , K None xK_z (spawn "zoom")
   , K None xK_b (runOrRaise "bitwarden" (className =? "Bitwarden"))
   , K None xK_p (runOrRaise "pavucontrol" (className =? "Pavucontrol"))
   , K None xK_a (spawn "anki")]
  brightness action = "$HOME/.bin/laptop-brightness" ++ " " ++ action

doVolume :: String -> X()
doVolume cmd = spawn $ "$HOME/.bin/laptop-volume --" ++ cmd

doMic :: String -> X()
doMic cmd = spawn $ "$HOME/.bin/micgain.sh --" ++ cmd

doSpotify :: String -> X()
doSpotify cmd = spawn $ "$HOME/.bin/spotify.sh " ++ cmd

cycleFirefox :: X()
cycleFirefox = raiseNextMaybe (return ()) (className =? "Firefox")

cycleFirefoxMatching query = raiseNextMaybe f foxQuery
  where
    f = return ()
    fox = className =? "Firefox"
    foxQuery = liftM2 (\a -> \b -> a && b) fox query

cycleFirefoxNotMatching query = raiseNextMaybe f foxQuery
  where
    f = return ()
    fox = className =? "Firefox"
    foxQuery = liftM2 (\a -> \b -> a && (not b)) fox query

------------------------------------------------------------------------
-- Layouts:
lFull      = noBorders Full
lTall      = smartBorders $ ResizableTall 1 (3/100) (2/3) []
lTallEven  = smartBorders $ ResizableTall 1 (3/100) (1/2) []
lTiled     = toggleLayouts lFull lTall
lTiledEven = toggleLayouts lFull lTallEven
lTwoPane   = toggleLayouts lFull $ TwoPanePersistent Nothing (3/100) (1/2)
gaps       = spacingRaw True (Border gaps gaps gaps gaps) True (Border gaps gaps gaps gaps) True
  where gaps = 5

myLayout = avoidStruts . gaps $
  lTiledEven
  ||| Mirror lTiled
  ||| lTwoPane

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
    className =? "zoom"               --> doShift "3"
  , className =? "Anki"               --> doShift "4"
  , className =? "TeamViewer"         --> doShift "5"
  , className =? "Droidcam"           --> doShift "7"
  , className =? "Pulseeffects"       --> doShift "7"
  , className =? "Steam"              --> doShift "7"
  , className =? "Spotify"            --> doShift "8"
  , className =? "Bitwarden"          --> doFloat
  , className =? "Pavucontrol"        --> doFloat
  , className =? "Blueman-manager"    --> doFloat
  , resource  =? "desktop_window"     --> doIgnore
  , resource  =? "kdesktop"           --> doIgnore
  , title     =? "scratchpad"         --> doFloat
  , title     =? "Picture-in-Picture" --> doFloat
  ]

------------------------------------------------------------------------
-- Startup hook
myStartupHook = do
  -- spawnOnce "redshift"
  spawnOnce "autorandr"
  spawnOnce "~/.fehbg"
  spawnOnce "compton &"
  spawnOnce "/opt/Mullvad VPN/mullvad-vpn"
  spawnOnce "nvidia-settings --assign CurrentMetaMode=\"nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }\""
  spawnOnce "nm-applet"
  spawnOnce
    "trayer --edge top --align center --widthtype request --padding 0 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 255 --height 21 &"
  -- spawnOnce "dropbox start"

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
                    , borderWidth        = 3
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
  menuArgs = map (removeQuote . unpack) $ splitOn (pack " ") (pack myDmenuConfig)
  }

removeChars :: [Char] -> [Char] -> [Char]
removeChars chars str = [x | x <- str, not (x `elem` chars)]

removePunc :: [Char] -> [Char]
removePunc str = removeChars ",.?!-:;\"\'" str

removeQuote :: [Char] -> [Char]
removeQuote str = removeChars "\'" str

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

gotoEmptyWS :: Direction1D -> X()
gotoEmptyWS dir = (getEmptyWs dir) >>= viewWs

shiftToEmptyWS :: Direction1D -> X ()
shiftToEmptyWS dir = moveWinToWs =<< getEmptyWs dir

-- EMACS utilities
runEmacs :: String -> X ()
runEmacs cmd = spawn $ "emacsclient -e '" ++ cmd ++ "'"

raiseEmacsAndRun :: String -> X ()
raiseEmacsAndRun cmd = runOrRaise "emacsclient -c" (className =? "Emacs") <> runEmacs cmd

orgCapture :: String -> X ()
orgCapture keys = raiseEmacsAndRun $ "(org-capture nil \"" ++ keys ++ "\")"

orgAgenda :: String -> X ()
orgAgenda keys = raiseEmacsAndRun $ "(org-agenda nil \"" ++ keys ++ "\")"
--- end emacs utilities

delay :: Int -> X ()
delay us = io (threadDelay us)

-- Substring version of XMonad's =?
-- e.g. (title =?? "asmr") will match ("this is an asmr video")
(=??) :: Query String -> String -> Query Bool
q =?? f = fmap (f `isInfixOf`) q

-- Fuzzy version of XMonad's =?
-- e.g. (title =?! "asmr") will match ("A S blahblah M R")
(=?!) :: Query String -> String -> Query Bool
q =?! f = fmap (fuzzyMatch f) q

-- Basically query NOT infix match
(=/?) :: Query String -> String -> Query Bool
q =/? f = fmap (not . (f `isInfixOf`)) q
