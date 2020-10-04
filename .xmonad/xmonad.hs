import           Control.Arrow                  ( first )
import           Data.List                      ( isInfixOf
                                                , intercalate
                                                )
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit
import           System.Posix.Unistd

import           XMonad
import           XMonad.Actions.CycleWS         ( toggleWS'
                                                , moveTo
                                                , findWorkspace
                                                , shiftTo
                                                , WSType
                                                  ( HiddenNonEmptyWS
                                                  , EmptyWS
                                                  )
                                                )
import           XMonad.Actions.Submap          ( submap )
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.CenteredMaster   ( centerMaster )
import           XMonad.Layout.NoBorders        ( noBorders )
import           XMonad.Layout.Spacing          ( spacing )
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ToggleLayouts
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Workspace      -- (27) prompt for a workspace
import           XMonad.Util.Paste
import           XMonad.Util.Run                ( spawnPipe
                                                , hPutStrLn
                                                )
import           XMonad.Util.SpawnOnce          ( spawnOnce )
import           XMonad.Util.WorkspaceCompare   ( getSortByIndex )
import qualified XMonad.StackSet               as W
import qualified Data.Map                      as M
import qualified XMonad.Actions.Search         as S

main = do
  xmobarPipe <- spawnPipe "xmobar -x 0 /home/davison/.config/xmobar/xmobarrc"
  xmonad . docks . ewmh . myConfig $ xmobarPipe

-- KEYBINDING
-- 
-- mod1Mask == alt
-- mod2Mask == num lock
-- mod3Mask == hyper (i've rebound to caps, but have some problems)
-- mod4Mask == windows
myModMask :: KeyMask
myModMask = mod3Mask

-- AVAILABLE - F5..F12  (and shift-F1..F12)
--             `~ 0 S-0 -_ =+ Insert
--             tabTAB W eE R T yY uU iI oO \
--             D F gG H ;: '"
--             zZ xX c vV B nN mM <> /?
--             [_,S-][left, right, up, down]
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings _conf = M.fromList
  [ ( (myModMask, button1)
    , (\w -> focus w >> mouseMoveWindow w)
    ) -- Left = move
  , ( (myModMask, button2)
    , (\w -> focus w >> windows W.swapMaster)
    ) -- Middle = make master
  , ( (myModMask, button3)
    , (\w -> focus w >> mouseResizeWindow w)
    ) -- Right = resize
    -- Mouse wheel is sensitive, so use  smaller volume increments
  , ( (myModMask, button4)
    , (\w -> (spawn "amixer sset Master 3dB+"))
    ) -- Scroll up = vol up
  , ( (myModMask, button5)
    , (\w -> (spawn "amixer sset Master 3dB-"))
    ) -- Scroll down = vol down
  , ( (myModMask .|. shiftMask, button4)
    , \w -> gotoNonEmptyWS Prev
    ) -- Scroll up = vol up
  , ((myModMask .|. shiftMask, button5), \w -> gotoNonEmptyWS Next) -- Scroll down = vol down
  ]

keybind mask key command = ((mask, key), command)

data Keybind = K KeyMask KeySym (X())

data Mask = Hyper | Win | Alt | HyperShift | WinShift | AltShift

toMask :: Mask -> KeyMask
toMask Hyper      = mod3Mask
toMask HyperShift = mod3Mask .|. shiftMask
toMask Win        = mod4Mask
toMask WinShift   = mod4Mask .|. shiftMask
toMask Alt        = mod1Mask
toMask AltShift   = mod1Mask .|. shiftMask

fromKeybind (K mask key command) = ((mask, key), command)

myKeys conf =
  M.fromList
    $  map
         fromKeybind
         [ K (xH .|. xS) xK_q            (io (exitWith ExitSuccess)) -- QUIT
         , K (xH .|. xS) xK_c            kill  -- close the focused window
         , K (xH)        xK_q            (restart "xmonad" True)
                                                                                -- Changing window focus
         , K (xH)        xK_bracketleft  (gotoNonEmptyWS Prev)
         , K (xH)        xK_bracketright (gotoNonEmptyWS Next)
         , K (xH .|. xS) xK_bracketleft  (shiftToNonEmptyWS Prev)
         , K (xH .|. xS) xK_bracketright (shiftToNonEmptyWS Next)
         , K (xH)        xK_j            (windows W.focusDown) -- focus window up stack
         , K (xH)        xK_k            (windows W.focusUp) -- focus window down stack
         , K (xH)        xK_m            (windows W.focusMaster) -- focus master window
         , K (xH .|. xS) xK_j            (windows W.swapDown) -- move window up stack
         , K (xH .|. xS) xK_k            (windows W.swapUp) -- move window down stack
         , K (xH .|. xS) xK_m            (windows W.swapMaster) -- make window stack master
         , K (xH)        xK_h            (sendMessage Shrink) -- smaller master
         , K (xH)        xK_l            (sendMessage Expand) -- larger master
         , K (xH .|. xS) xK_h            (sendMessage MirrorShrink) -- smaller slave
         , K (xH .|. xS) xK_l            (sendMessage MirrorExpand) -- larger slavek
                                                                                -- Changing layout
         , K (xH)        xK_space        (sendMessage NextLayout)             -- Use next configured layout
         , K (xH .|. xS) xK_space (setLayout $ XMonad.layoutHook conf) -- reset to default layout
         , K (xH)        xK_f            (fullscreenNoBar)                        -- toggle fullscreen on focused window
         , K (xH .|. xS) xK_f            (sendMessage ToggleStruts)               -- toggle fullscreen on focused window
         , K (xH)        xK_t            (withFocused $ windows . W.sink)         -- make float tiled again
         , K (xH)        xK_comma        (sendMessage (IncMasterN 1))     -- increase num master windows
         , K (xH)        xK_period       (sendMessage (IncMasterN (-1)))     -- decrease num master windows
                                                                                -- Launchers
         , K (xH)        xK_Return       (spawn myTerminal)
         , K (xH)        xK_p            (spawn $ myJ4Command)
         , K (xH)        xK_w            (spawn "dmenu_win_switcher.sh")
         , K (xH)        xK_b            (spawn "dmenu_bookmark_groups.sh")
         , K (xH .|. xS) xK_b            (spawn "dmenu_bookmarks.sh")
         , K (xH) xK_r (spawn $ myTerminal ++ " -e ranger")
                                                                                -- Emacs / org mode
         , K (xH)        xK_F1           (raiseEmacsAndRun "(org-capture)")
         , K (xH)        xK_F2           (raiseEmacsAndRun "(org-agenda)")
         , K (xH) xK_F3 (raiseEmacsAndRun "(org-agenda nil \"c1\")")
         , K (xH) xK_F4 (raiseEmacsAndRun "(org-agenda nil \"cW\")")
                                                                                -- Other apps
         , K (xH) xK_F5 (runOrRaise "spotify" (className =? "Spotify"))
         , K (xH)
             xK_F6
             (raiseNextMaybe (spawn "firefox") (className =? "Firefox"))
         , K (xH) xK_F7                    (raiseNext (title =?? "ASMR"))
         , K (xH) xK_F11                   (spawn "dmenu_asmr.py")
         , K (xH) xK_F12 (S.promptSearch myXPConfig S.duckduckgo)
                                                                                -- Media keys
         , K (xH) xK_Home                  (spawn "amixer sset Master 6dB+")
         , K (xH) xK_End                   (spawn "amixer sset Master 6dB-")
         , K (xH) xK_Delete                (doSpotify "prev")
         , K (xH) xK_Next                  (doSpotify "next")
         , K (xH) xK_Prior                 (doSpotify "play-pause")
         , K (0)  xF86XK_AudioRaiseVolume  (spawn "amixer set Master 6dB+")
         , K (0)  xF86XK_AudioLowerVolume  (spawn "amixer set Master 6dB-")
         , K (0)  xF86XK_AudioPrev         (doSpotify "prev")
         , K (0)  xF86XK_AudioNext         (doSpotify "next")
         , K (0)  xF86XK_AudioPlay         (doSpotify "play-pause")
                                                                                -- Brightness
         , K (0)  xF86XK_KbdBrightnessUp   (spawn "bright.sh up")
         , K (0)  xF86XK_MonBrightnessUp   (spawn "bright.sh up")
         , K (0)  xF86XK_KbdBrightnessDown (spawn "bright.sh down")
         , K (0)  xF86XK_MonBrightnessDown (spawn "bright.sh down")
                                                                                -- KEYCHORD - H-d {e,a} -- open ebooks or literature
         , K
           (xH)
           xK_d
           ( submap
           . M.fromList
           $ [ (((0, xK_e), spawn "dmenu_ebooks.sh"))
             , (((0, xK_a), spawn "dmenu_articles.sh"))
             ]
           )
         ]
    -- mod-[1..9] view workspace on current monitor
    -- win-[1..9] view workspace on main monitor 
    -- alt-[1..9] view workspace on second monitor
    -- mod-shift-[1..9] move window to workspace 
    ++ map fromKeybind [ K (xH) key (pullWs ws) | (key, ws) <- keyWsPairs ]
    ++ map fromKeybind
           [ K (xW) key (pullWsToMonitor ws 0) | (key, ws) <- keyWsPairs ]
    ++ map fromKeybind
           [ K (xA) key (pullWsToMonitor ws 1) | (key, ws) <- keyWsPairs ]
    ++ map fromKeybind
           [ K (xH .|. xS) key (moveWinToWs ws) | (key, ws) <- keyWsPairs ]
    -- mod-{a,s} -- view main or second monitor
    -- mod-{A,S} -- move window to workspace on main or second monitor
    ++ map fromKeybind
           [ K (xH) key (focusMonitor sc) | (key, sc) <- keyScreenPairs ]
    ++ map
         fromKeybind
         [ K (xH .|. xS) key (moveWinToMonitor sc)
         | (key, sc) <- keyScreenPairs
         ]
 where
  keyWsPairs     = zip [xK_1 .. xK_9] myWorkspaces
  keyScreenPairs = zip [xK_a, xK_s] [0 ..]
  focusMonitor n = flip whenJust viewWs =<< screenWorkspace n
  moveWinToMonitor n = flip whenJust moveWinToWs =<< screenWorkspace n
  pullWs      = windows . W.greedyView
  viewWs      = windows . W.view
  moveWinToWs = windows . W.shift
  pullWsToMonitor ws mon = focusMonitor mon <> pullWs ws
  doSpotify cmd = spawn $ "$HOME/.bin/spotify.sh" ++ cmd
  cycleFirefox = raiseNextMaybe (return ()) (className =? "Firefox")
  xH           = mod3Mask
  xW           = mod4Mask
  xA           = mod1Mask
  xS           = shiftMask

------------------------------------------------------------------------
-- Layouts:
myLayout = twoThirdsMasterTiled ||| Mirror twoThirdsMasterTiled
 where
     -- Layouts
  cleanFull      = noBorders Full
  centerOverTile = toggleLayouts cleanFull $ centerMaster tiled
  twoThirdsMasterTiled =
    toggleLayouts cleanFull $ spacing 10 $ ResizableTall nmaster delta ratio2 []
  tiled =
    toggleLayouts cleanFull $ spacing 10 $ ResizableTall nmaster delta ratio []
  -- Config
  nmaster = 1 -- Number of windows in master pane
  ratio2  = 2 / 3 -- Use 2/3 of screen for master
  ratio   = 1 / 2 -- Use 1/2 of screen for master
  delta   = 3 / 100 -- Expand/shrink window by X%

-------------------------------------------------------------------------
-- Manage Hook is for applying rules to specific windows
-- className matches xprop WM_CLASS[2]
-- title matches xprop WM_NAME
--
-- Matching (=?? =?! defined below)
--    exact =?    (e.g. 'needle' =? 'needle')
--    infix =??   (e.g. 'this has a needle somewhere' =?? 'needle')
--    fuzzy =?!   (e.g. 'n something EEDLE' =?! 'needle')   ! case insensitive
myManageHook = composeAll
  [ className =? "Bitwarden" --> doFloat
  , className =? "Pavucontrol" --> doFloat
  , className =? "Droidcam" --> doShift "7"
  , className =? "TeamViewer" --> doShift "5"
  , className =? "zoom" --> doShift "4"
  , title =? "scratchpad" --> doFloat
  , className =? "Anki" --> doShift "3"
  , className =? "Spotify" --> doShift "8"
  , resource =? "desktop_window" --> doIgnore
  , resource =? "kdesktop" --> doIgnore
  ]

------------------------------------------------------------------------
-- Startup hook
myStartupHook = do
  spawnOnce "redshift"
  spawnOnce "~/.fehbg"
  spawnOnce "compton &"
  spawnOnce "/opt/Mullvad VPN/mullvad-vpn"
  spawnOnce
    "trayer --edge top --align center --widthtype request --padding 0 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --height 21 &"
  spawnOnce "dropbox start"

------------------------------------------------------------------------
-- Configure workspace names in xmobar
logWorkspacesOnXmobar pipe = dynamicLogWithPP xmobarPP
  { ppOutput  = hPutStrLn pipe
  , ppTitle   = \w -> xmobarColor cPink "" . shorten 70 $ w
  , ppCurrent = \w -> xmobarColor cPink "" w
  , ppWsSep   = "  "
  , ppSep     = " ::: "
  , ppLayout  = \c -> ""
  }

myConfig pipe = defaultConfig { terminal           = myTerminal
                              , focusFollowsMouse  = True
                              , borderWidth        = 3
                              , modMask            = myModMask
                              , workspaces         = myWorkspaces
                              , normalBorderColor  = cGrey
                              , focusedBorderColor = cPurple
                              , keys               = myKeys
                              , mouseBindings      = myMouseBindings
                              , layoutHook         = avoidStruts $ myLayout
                              , manageHook = manageDocks <+> myManageHook
                              , logHook            = logWorkspacesOnXmobar pipe
                              , startupHook        = myStartupHook
                              }

--------------------------------------------------------------------------------
-- OTHER GENERAL UTILITY STUFF
-- functions, and personal configuration

-- COLOURS & other style stuff
cPurple = "#8620e6"
cPink = "#f438ee"
cGrey = "#333333"
cLightGrey = "#dddddd"
cWhite = "#ffffff"

myFont = "xft:Hack:pixelsize=14:antialias=true:hinting=true"
myFontSmall = "xft:Hack:pixelsize=12:antialias=true:hinting=true"

myTerminal = "alacritty"
myWorkspaces =
  ["1:emacs", "2:web", "3", "4:anki", "5", "6", "7", "8:spotify", "9:video"]

myXPConfig :: XPConfig
myXPConfig = def { font                = myFont
                 , bgColor             = cGrey
                 , fgColor             = cLightGrey
                 , bgHLight            = cPurple
                 , fgHLight            = "#ffffff"
                 , borderColor         = cGrey
                 , promptBorderWidth   = 0
                 , position            = Top
                 , height              = 21
                 , historySize         = 256
                 , historyFilter       = id
                 , defaultText         = []
                 , autoComplete        = Nothing
                 , showCompletionOnTab = False
                 , searchPredicate     = fuzzyMatch
                 , alwaysHighlight     = True
                 , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
                 }

strSurround c str = c ++ str ++ c

-- Execution of programs
myJ4Command = "j4-dmenu-desktop --dmenu=\"" ++ dmenu ++ "\""
 where
  dmenu   = "dmenu -i -fn 'Hack-14' -p 'App:' " ++ colours
  colours = intercalate " " args
  args =
    [ "-sb"
    , cPurple'
    , "-nhb"
    , cPink'
    , "-nhf"
    , cWhite'
    , "-shb"
    , cPink'
    , "-shf"
    , cWhite'
    ]
  cPurple'    = "'" ++ cPurple ++ "'"
  cPink'      = "'" ++ cPink ++ "'"
  cGrey'      = "'" ++ cGrey ++ "'"
  cWhite'     = "'" ++ cWhite ++ "'"
  cLightGrey' = "'" ++ cLightGrey ++ "'"

fullscreenNoBar = sendMessage (Toggle "Full") <> sendMessage ToggleStruts

-- Move to Next or Prev non empty workspace
gotoNonEmptyWS :: Direction1D -> X ()
gotoNonEmptyWS dir = windows . W.view =<< ws
  where ws = findWorkspace getSortByIndex dir HiddenNonEmptyWS 1

shiftToNonEmptyWS :: Direction1D -> X ()
shiftToNonEmptyWS dir = windows . W.shift =<< ws
  where ws = findWorkspace getSortByIndex dir EmptyWS 1

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



