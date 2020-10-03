import Control.Arrow (first)
import Data.List (isInfixOf)
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.CycleWS (toggleWS', moveTo, findWorkspace, shiftTo, WSType( HiddenNonEmptyWS, EmptyWS ))
import XMonad.Actions.Submap (submap)
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Util.Paste
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.WorkspaceCompare(getSortByIndex)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Actions.Search as S

-- COLOURS & other style stuff
myColourPurple = "#8620e6"
myColourPink = "#f438ee"
myColourGrey = "#333333"
myColourLightGrey = "#dddddd"

myFont = "xft:Hack:pixelsize=14:antialias=true:hinting=true"
myFontSmall = "xft:Hack:pixelsize=12:antialias=true:hinting=true"

myTerminal = "alacritty"
myWorkspaces = map show [1..9]

myXPConfig :: XPConfig
myXPConfig = def
      { font                = myFont
      , bgColor             = myColourGrey
      , fgColor             = myColourLightGrey
      , bgHLight            = myColourPurple
      , fgHLight            = "#ffffff"
      , borderColor         = myColourGrey
      , promptBorderWidth   = 0
      , position            = Top
--    , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 21
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      --, autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , autoComplete        = Nothing
      , showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
      , searchPredicate     = fuzzyMatch
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }


-- Execution of programs
myDmenuConfig = "dmenu -i -fn 'Hack-14' -sb '#8620e6' -nhb '#8620e6' -nhf '#f438ee' -shb '#8620e6' -shf '#ffffff' -p 'App:'"
myJ4Command = "j4-dmenu-desktop --dmenu=\"" ++ myDmenuConfig ++ "\""

-- Move to Next or Prev non empty workspace
gotoNonEmptyWS :: Direction1D -> X()
gotoNonEmptyWS dir = windows . W.view =<< ws
  where
    ws = findWorkspace getSortByIndex dir HiddenNonEmptyWS 1

shiftToNonEmptyWS :: Direction1D -> X()
shiftToNonEmptyWS dir = windows . W.shift =<< ws
  where
    ws = findWorkspace getSortByIndex dir EmptyWS 1

raiseEmacsAndRun :: String -> X()
raiseEmacsAndRun cmd = sequence_ [raise (className =? "Emacs"), runEmacs cmd]

runEmacs :: String -> X()
runEmacs cmd = spawn $ "emacsclient -e '" ++ cmd ++ "'"

-- Substring version of XMonad's =?
-- e.g. (title =?? "asmr") will match ("this is an asmr video")
(=??) :: Query String -> String -> Query Bool
q =?? f = fmap (f `isInfixOf`) q

-- Fuzzy version of XMonad's =?
-- e.g. (title =?! "asmr") will match ("A S blahblah M R")
(=?!) :: Query String -> String -> Query Bool
q =?! f = fmap (fuzzyMatch f) q

-- KEYBINDING
-- 
-- mod1Mask == alt
-- mod2Mask == num lock
-- mod3Mask == hyper (i've rebound to caps, but have some problems)
-- mod4Mask == windows
myModMask :: KeyMask
myModMask = mod4Mask

-- AVAILABLE - F5..F12  (and shift-F1..F12)
--             `~ 0 S-0 -_ =+ Insert
--             tabTAB W eE R T yY uU iI oO \
--             D F gG H ;: '"
--             zZ xX c vV B nN mM <> /?
--             [_,S-][left, right, up, down]
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X())
myMouseBindings _conf = M.fromList [
      ((mod4Mask, button1), (\w -> focus w >> mouseMoveWindow w)) -- Left = move
    , ((mod4Mask, button2), (\w -> focus w >> windows W.swapMaster)) -- Middle = make master
    , ((mod4Mask, button3), (\w -> focus w >> mouseResizeWindow w)) -- Right = resize
    -- Mouse wheel is sensitive, so use  smaller volume increments
    , ((mod4Mask, button4), (\w -> (spawn "amixer sset Master 3dB+"))) -- Scroll up = vol up
    , ((mod4Mask, button5), (\w -> (spawn "amixer sset Master 3dB-")))] -- Scroll down = vol down

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X())
myKeys conf = M.fromList $ [
      ((mod4Mask .|. shiftMask, xK_q),       (io (exitWith ExitSuccess))) -- QUIT
    , ((mod4Mask .|. shiftMask, xK_c),       kill) -- close the focused window
    , ((mod4Mask,               xK_q),       restart "xmonad" True)
    -- Changing window focus
    , ((mod4Mask,               xK_bracketleft), gotoNonEmptyWS Prev) 
    , ((mod4Mask,               xK_bracketright), gotoNonEmptyWS Next)
    , ((mod4Mask .|. shiftMask, xK_bracketleft),   shiftToNonEmptyWS Prev) 
    , ((mod4Mask .|. shiftMask, xK_bracketright),  shiftToNonEmptyWS Next)
    , ((mod4Mask,               xK_j), windows W.focusDown) -- focus window up stack
    , ((mod4Mask,               xK_k), windows W.focusUp  ) -- focus window down stack
    , ((mod4Mask,               xK_m), windows W.focusMaster  ) -- focus master window
    , ((mod4Mask .|. shiftMask, xK_j), windows W.swapDown) -- move window up stack
    , ((mod4Mask .|. shiftMask, xK_k), windows W.swapUp  ) -- move window down stack
    , ((mod4Mask .|. shiftMask, xK_m), windows W.swapMaster) -- make window stack master
    , ((mod4Mask,               xK_h), sendMessage Shrink) -- fewer windows in master
    , ((mod4Mask,               xK_l), sendMessage Expand) -- more windows in master
    -- Changing layout
    , ((mod4Mask,               xK_space), sendMessage NextLayout) -- Use next configured layout
    , ((mod4Mask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf) -- reset to default layout
    , ((mod4Mask,               xK_f), fsNoBar) -- toggle fullscreen on focused window
    , ((mod4Mask .|. shiftMask, xK_f), sendMessage ToggleStruts) -- toggle fullscreen on focused window
    -- dumb function to turn my external monitor 'off'
    -- by just displaying a fullscreen black wallpaper
    , ((mod4Mask,               xK_o), (spawn "feh $HOME/Dropbox/pictures/wallpapers/justblack.png"
                                 <+> (flip whenJust (windows . W.view) =<< screenWorkspace 1)
                                 <+> fsNoBar)) -- toggle fullscreen on focused window
    , ((mod4Mask .|. shiftMask, xK_o), raiseAndDo (return ()) (title =?? "justblack.png") (\w -> kill))
    , ((mod4Mask,               xK_t), withFocused $ windows . W.sink) -- make float tiled again , withWin      xK_comma   (sendMessage (IncMasterN 1)) -- increase num master windows
    , ((mod4Mask,               xK_period), sendMessage (IncMasterN (-1))) -- decrease num master windows
    -- Launchers
    , ((mod4Mask,               xK_Return), spawn myTerminal)
    , ((mod4Mask,               xK_p), spawn myJ4Command)
    , ((mod4Mask,               xK_w), spawn "dmenu_win_switcher.sh")
    , ((mod4Mask,               xK_b), spawn "dmenu_bookmark_groups.sh")
    , ((mod4Mask .|. shiftMask, xK_b), spawn "dmenu_bookmarks.sh")
    , ((mod4Mask,               xK_r), spawn $ myTerminal ++ " -e ranger")
    , ((mod4Mask,               xK_x), spawn $ myTerminal ++ " -t scratchpad -e $HOME/.bin/get_xprop.sh")
    -- Emacs / org mode
    , ((mod4Mask,               xK_F1), raiseEmacsAndRun "(org-capture)")
    , ((mod4Mask,               xK_F2), raiseEmacsAndRun "(org-agenda)")
    , ((mod4Mask,               xK_F3), raiseEmacsAndRun "(org-agenda nil \"c1\")")
    , ((mod4Mask,               xK_F4), raiseEmacsAndRun "(org-agenda nil \"cW\")")
    -- Other apps
    , ((mod4Mask,               xK_F5), runOrRaise "spotify" (className =? "Spotify"))
    , ((mod4Mask,               xK_F6), raiseNextMaybe (spawn "firefox") (className =? "Firefox"))
    , ((mod4Mask,               xK_F7), raiseNext (title =?? "ASMR"))
    , ((mod4Mask,               xK_F11), spawn "dmenu_asmr.py")
    , ((mod4Mask,               xK_F12), S.promptSearch myXPConfig S.duckduckgo)
    -- Media keys
    , ((mod4Mask,               xK_Home), spawn "amixer sset Master 6dB+")
    , ((mod4Mask,               xK_End), spawn "amixer sset Master 6dB-")
    , ((mod4Mask,               xK_Delete), spawn "$HOME/.bin/spotify.sh prev")
    , ((mod4Mask,               xK_Next), spawn "$HOME/.bin/spotify.sh next")
    , ((mod4Mask,               xK_Prior), spawn "$HOME/.bin/spotify.sh play-pause")
    , ((0,                      xF86XK_AudioRaiseVolume), spawn "amixer set Master 6dB+")
    , ((0,                      xF86XK_AudioLowerVolume), spawn "amixer set Master 6dB-")
    , ((0,                      xF86XK_AudioPrev), spawn "$HOME/.bin/spotify.sh prev")
    , ((0,                      xF86XK_AudioNext), spawn "$HOME/.bin/spotify.sh next")
    , ((0,                      xF86XK_AudioPlay), spawn "$HOME/.bin/spotify.sh play-pause")
    -- Brightness
    , ((0,                      xF86XK_KbdBrightnessUp), spawn "bright.sh up")
    , ((0,                      xF86XK_MonBrightnessUp), spawn "bright.sh up")
    , ((0,                      xF86XK_KbdBrightnessDown), spawn "bright.sh down")
    , ((0,                      xF86XK_MonBrightnessDown), spawn "bright.sh down")
    -- KEYCHORD - H-d {e,a} -- open ebooks or literature
    , ((mod4Mask,               xK_d),  submap . M.fromList $ [
                           (((0, xK_e), spawn "dmenu_ebooks.sh"))
                         , (((0, xK_a), spawn "dmenu_articles.sh"))])
    ]

    -- -- mod-[1..9] view workspace on current monitor
    -- win-[1..9] view workspace on main monitor 
    -- alt-[1..9] view workspace on second monitor
    -- mod-shift-[1..9] move window to workspace 
    ++ [((mod3Mask, key), windows $ W.greedyView ws)                    
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]
    ++ [((mod4Mask, key), (sequence_ [
                             flip whenJust (windows . W.view) =<< screenWorkspace 0,
                             windows $ W.greedyView ws]))                
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]
    ++ [((mod1Mask, key), (sequence_ [
                             flip whenJust (windows . W.view) =<< screenWorkspace 1,
                             windows $ W.greedyView ws]))      
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]
    ++ [((mod4Mask .|. shiftMask, key), windows $ W.shift ws)                      
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]
    -- mod-{a,s} focus physical screen
    -- mod-shift-{a,s} move window physical screen
    ++ [((mod4Mask, key), flip whenJust (windows . W.view) =<< screenWorkspace sc)
        | (key, sc) <- zip [xK_a, xK_s] [0..]]
    -- This is equivalent to /shift focused window to whatever workspace is on monitor N/
    -- Do i really need this keybind? I have relatively set keybinds
    ++ [((mod4Mask .|. shiftMask, key), flip whenJust (windows . W.shift) =<< screenWorkspace sc)
        | (key, sc) <- zip [xK_a, xK_s] [0..]]
    where
      fsNoBar                    = sendMessage (Toggle "Full") <+> sendMessage ToggleStruts

------------------------------------------------------------------------
-- Layouts:
myLayout = twoThirdsMasterTiled ||| tiled
  where
     -- Layouts
     cleanFull = noBorders Full
     centerOverTile = toggleLayouts cleanFull $ centerMaster tiled
     twoThirdsMasterTiled = toggleLayouts cleanFull $ spacing 10 $ Tall nmaster delta ratio2
     tiled   = toggleLayouts cleanFull $ spacing 10 $ Tall nmaster delta ratio
     -- Config
     nmaster = 1 -- Number of windows in master pane
     ratio2 = 2/3 -- Use 2/3 of screen for master
     ratio   = 1/2 -- Use 1/2 of screen for master
     delta   = 3/100 -- Expand/shrink window by X%

------------------------------------------------------------------------
-- Window rules:
myManageHook = composeAll
    [ className =? "Bitwarden"      --> doFloat
    , className =? "Pavucontrol"    --> doFloat
    , className =? "Droidcam"       --> doShift "7"
    , className =? "TeamViewer"     --> doShift "5"
    , className =? "zoom"           --> doShift "4"
    , title =? "scratchpad"         --> doFloat
    , className =? "Anki"           --> doShift "3"
    , className =? "Spotify"        --> doShift "8"
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Startup hook
myStartupHook = do
    spawnOnce "redshift"
    spawnOnce "~/.fehbg"
    spawnOnce "compton &"
    spawnOnce "/opt/Mullvad VPN/mullvad-vpn"
    -- spawnOnce "trayer --edge top --align center --widthtype request --padding 0 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --tint 0x000000 --alpha 100 --height 21 &"
    spawnOnce "dropbox start"

------------------------------------------------------------------------
-- Configure workspace names in xmobar
logWorkspacesOnXmobar pipe = dynamicLogWithPP xmobarPP {
    ppOutput = hPutStrLn pipe
    , ppTitle = \w -> xmobarColor myColourPink "" . shorten 70 $ w
    , ppCurrent = \w -> xmobarColor myColourPink "" w
    , ppWsSep = "  "
    , ppLayout = \c -> ""
    }

myConfig pipe = defaultConfig {
    terminal           = myTerminal,
    focusFollowsMouse  = True,
    borderWidth        = 3,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myColourGrey,
    focusedBorderColor = myColourPurple,
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    layoutHook         = avoidStruts $ myLayout,
    manageHook         = manageDocks <+> myManageHook,
    logHook            = logWorkspacesOnXmobar pipe,
    startupHook        = myStartupHook
    } 

main = do
  xmobarPipe <- spawnPipe "xmobar -x 0 /home/davison/.config/xmobar/xmobarrc"
  xmonad . docks $ ewmh . myConfig $ xmobarPipe

