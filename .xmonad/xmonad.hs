{- LANGUAGE OverloadedStrings -}
import Control.Arrow ( first )
import Control.Concurrent (threadDelay)
import Data.List ( isInfixOf , intercalate)
import Data.Text (splitOn, unpack, pack)
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
import XMonad.Layout.NoBorders ( noBorders )
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Spacing ( spacing )
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

data CDMask = Hyper | Win | Alt | HyperShift | WinShift | AltShift | None

fromKeybind :: SimpleKeybind -> ((KeyMask, KeySym), X ())
fromKeybind (K None       k command) = ((0, k), command)
fromKeybind (K Hyper      k command) = ((mod3Mask, k), command)
fromKeybind (K Win        k command) = ((mod4Mask, k), command)
fromKeybind (K Alt        k command) = ((mod1Mask, k), command)
fromKeybind (K HyperShift k command) = ((mod3Mask .|. shiftMask, k), command)
fromKeybind (K WinShift   k command) = ((mod4Mask .|. shiftMask, k), command)
fromKeybind (K AltShift   k command) = ((mod1Mask .|. shiftMask, k), command)

fromMousebind :: SimpleMousebind -> ((KeyMask, Button), Window -> X ())
fromMousebind (M None       b command) = ((0, b), command)
fromMousebind (M Hyper      b command) = ((mod3Mask, b), command)
fromMousebind (M Win        b command) = ((mod4Mask, b), command)
fromMousebind (M Alt        b command) = ((mod1Mask, b), command)
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
  , M Hyper      button4 (\w -> (spawn "volume.sh --up")) -- Scroll up = vol up
  , M Hyper      button5 (\w -> (spawn "volume.sh --down")) -- Scroll down = vol down
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
  , K HyperShift xK_f            (sendMessage ToggleStruts)       -- toggle fullscreen on focused window
  , K Hyper      xK_t            (withFocused $ windows . W.sink) -- make float tiled again
  --- LAUNCHERS
  , K Hyper      xK_g            (windowPromptGoto myXPConfig)
--   , K HyperShift xK_g            (bringMenu)--Config myBringConfig)
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
  , K Hyper      xK_F1           (raiseEmacsAndRun "(org-capture)")
  , K Hyper      xK_F2           (raiseEmacsAndRun "(org-agenda)")
  , K Hyper      xK_F3           (raiseEmacsAndRun "(org-agenda nil \"c1\")")
  , K Hyper      xK_F4           (raiseEmacsAndRun "(org-agenda nil \"cW\")")
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
  
  doSpotify cmd = spawn $ "$HOME/.bin/spotify.sh " ++ cmd
  doVolume cmd = spawn $ "$HOME/.bin/volume.sh --" ++ cmd
  cycleFirefox = raiseNextMaybe (return ()) (className =? "Firefox")

data Volume = VolUp | VolDown | VolToggleMute

------------------------------------------------------------------------
-- Layouts:
lFull      = noBorders Full
lTall      = ResizableTall 1 (3/100) (2/3) []
lTallEven  = ResizableTall 1 (3/100) (1/2) []
lTiled     = toggleLayouts lFull (gaps $ lTall)
lTiledEven = toggleLayouts lFull (gaps $ lTallEven)
lTwoPane   = toggleLayouts lFull $ gaps $ TwoPanePersistent Nothing (3/100) (1/2)
gaps       = spacing 10

defaultLayouts = avoidStruts $
  lTiled
  ||| Mirror lTiled
  ||| lTwoPane

myLayout = onWorkspace "8:spotify" lFull
  $ defaultLayouts 

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
  , ppTitle   = \w -> xmobarColor cPink "" . shorten 70 $ w
  , ppCurrent = \w -> xmobarColor cPink "" w
  , ppWsSep   = "  "
  , ppSep     = " ::: "
  , ppLayout  = \_ -> ""
  }

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

myFont      = "xft:Hack:pixelsize=14:antialias=true:hinting=true"
myFontXL    = "xft:Hack:pixelsize=18:antialias=true:hinting=true"
myFontSmall = "xft:Hack:pixelsize=12:antialias=true:hinting=true"

myTerminal = "alacritty"
myWorkspaces = ["1:emc", "2:web", "3:zUm", "4:anK", "5", "6", "7", "8:mzk", "9:vid"]

myXPConfig :: XPConfig
myXPConfig = def { font                = myFont
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
myXPConfigLG = myXPConfig { font = myFontXL
                          , height = 21
                          }

myBringConfig :: WindowBringerConfig
myBringConfig = def {
  menuArgs = map unpack $ splitOn (pack " ") (pack myDmenuConfig)
  }

-- Execution of programs
myJ4Command :: String
myJ4Command = "j4-dmenu-desktop --dmenu=\"dmenu " ++ myDmenuConfig ++ "\""

myDmenuConfig :: String
myDmenuConfig = "-l 10 -i -fn 'Hack-14' -p 'App:' " ++ intercalate " " pairs
  where
    args = [("-sb", cPurple),                         -- background colour
            ("-nhb", cLightPurple), ("-nhf", cWhite), -- colour for non-highlighted lines
            ("-shb", cLightPurple), ("-shf", cWhite)] -- colour for highlighted lines
    pairs = [arg ++ " '" ++ colour ++ "'" | (arg, colour) <- args]
                 

fullscreenNoBar :: X()
fullscreenNoBar = sendMessage (Toggle "Full") <> sendMessage ToggleStruts

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

raiseEmacsAndRun :: String -> X ()
raiseEmacsAndRun cmd = sequence_ [raise (className =? "Emacs"), runEmacs cmd]

runEmacs :: String -> X ()
runEmacs cmd = spawn $ "emacsclient -e '" ++ cmd ++ "'"

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
