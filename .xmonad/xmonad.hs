import Control.Arrow (first)
import Data.List (isInfixOf)
import System.Exit

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
import XMonad.Layout.TwoPanePersistent
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
-- Key aliases, as a reminder for keybinds (mod2 is numlock)
keyAlt = mod1Mask
keyHyper = mod3Mask
keyWindows = mod4Mask
myModMask = keyHyper
-- AVAILABLE - F5..F12  (and shift-F1..F12)
--             `~ 0 S-0 -_ =+ Insert
--             tabTAB W eE R T yY uU iI oO \
--             D F gG H ;: '"
--             zZ xX c vV B nN mM <> /?
--             [_,S-][left, right, up, down]
--
myMouseBindings conf = M.fromList [
      withHyper button1 (\w -> focus w >> mouseMoveWindow w) -- Left = move
    , withHyper button2 (\w -> focus w >> windows W.swapMaster) -- Middle = make master
    , withHyper button3 (\w -> focus w >> mouseResizeWindow w) -- Right = resize
    -- Mouse wheel is sensitive, so use  smaller volume increments
    , withHyper button4 (\w -> spawn "amixer set Master 3dB+") -- Scroll up = vol up
    , withHyper button5 (\w -> spawn "amixer set Master 3dB-")] -- Scroll down = vol down
  where
    keybind keymask key command = ((keymask, key), command)
    withHyper key command = keybind mod3Mask key command

myKeys conf = M.fromList $ [
      withHyperShift xK_q       (io (exitWith ExitSuccess)) -- QUIT
    , withHyperShift xK_c       (kill) -- close the focused window
    , withHyper      xK_q       (restart "xmonad" True)
    -- Changing window focus
    , withHyper      xK_bracketleft   (gotoNonEmptyWS Prev) 
    , withHyper      xK_bracketright  (gotoNonEmptyWS Next)
    , withHyper      xK_j       (windows W.focusDown) -- focus window up stack
    , withHyper      xK_k       (windows W.focusUp  ) -- focus window down stack
    , withHyper      xK_m       (windows W.focusMaster  ) -- focus master window
    , withHyperShift xK_j       (windows W.swapDown) -- move window up stack
    , withHyperShift xK_k       (windows W.swapUp  ) -- move window down stack
    , withHyperShift xK_m       (windows W.swapMaster) -- make window stack master
    , withHyper      xK_h       (sendMessage Shrink) -- fewer windows in master
    , withHyper      xK_l       (sendMessage Expand) -- more windows in master
    -- Changing layout
    , withHyper      xK_space   (sendMessage NextLayout) -- Use next configured layout
    , withHyperShift xK_space   (setLayout $ XMonad.layoutHook conf) -- reset to default layout
    , withHyper      xK_f       (fsNoBar) -- toggle fullscreen on focused window
    , withHyperShift xK_f       (sendMessage ToggleStruts) -- toggle fullscreen on focused window
    -- dumb function to turn my external monitor 'off'
    -- by just displaying a fullscreen black wallpaper
    , withHyper      xK_o       (spawn "feh $HOME/Dropbox/pictures/wallpapers/justblack.png"
                                 <+> (flip whenJust (windows . W.view) =<< screenWorkspace 1)
                                 <+> fsNoBar) -- toggle fullscreen on focused window
    , withHyperShift xK_o       (raiseAndDo (return ()) (title =?? "justblack.png") (\w -> kill))
    , withHyper      xK_t       (withFocused $ windows . W.sink) -- make float tiled again , withHyper      xK_comma   (sendMessage (IncMasterN 1)) -- increase num master windows
    , withHyper      xK_period  (sendMessage (IncMasterN (-1))) -- decrease num master windows
    -- Launchers
    , withHyper      xK_Return  (spawn myTerminal)
    , withHyper      xK_p       (spawn myJ4Command)
    , withHyper      xK_w       (spawn "dmenu_win_switcher.sh")
    , withHyper      xK_b       (spawn "dmenu_bookmark_groups.sh")
    , withHyperShift xK_b       (spawn "dmenu_bookmarks.sh")
    , withHyper      xK_r       (spawn $ myTerminal ++ " -e ranger")
    , withHyper      xK_x       (spawn $ myTerminal ++ " -t scratchpad -e $HOME/.bin/get_xprop.sh")
    -- Emacs / org mode
    , withHyper      xK_F1      (raiseEmacsAndRun "(org-capture)")
    , withHyper      xK_F2      (raiseEmacsAndRun "(org-agenda)")
    , withHyper      xK_F3      (raiseEmacsAndRun "(org-agenda nil \"c1\")")
    , withHyper      xK_F4      (raiseEmacsAndRun "(org-agenda nil \"cW\")")
    -- Other apps
    , withHyper      xK_F5      (runOrRaise "spotify" (className =? "Spotify"))
    , withHyper      xK_F6      (raiseNextMaybe (spawn "firefox") (className =? "Firefox"))
    , withHyper      xK_F7      (raiseNext (title =?? "ASMR"))
    , withHyper      xK_F11     (spawn "dmenu_asmr.py")
    , withHyper      xK_F12     (S.promptSearch myXPConfig S.duckduckgo)
    -- Media keys
    , withHyper      xK_Home    (spawn "amixer set Master 6dB+")
    , withHyper      xK_End     (spawn "amixer set Master 6dB-")
    , withHyper      xK_Delete  (spawn "$HOME/.bin/spotify.sh prev")
    , withHyper      xK_Next    (spawn "$HOME/.bin/spotify.sh next")
    , withHyper      xK_Prior   (spawn "$HOME/.bin/spotify.sh play-pause")
    -- KEYCHORD - H-d {e,a} -- open ebooks or literature
    , withHyper      xK_d       (submap . M.fromList $ [
                      (only xK_e (spawn "dmenu_ebooks.sh"))
                    , (only xK_a (spawn "dmenu_articles.sh"))])
    ]

    -- mod-[1..9] view workspace on current monitor
    -- win-[1..9] view workspace on main monitor 
    -- alt-[1..9] view workspace on second monitor
    -- mod-shift-[1..9] move window to workspace 
    ++ [withHyper key (windows $ W.greedyView ws)                    
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]
    ++ [withWin key (sequence_ [
                             flip whenJust (windows . W.view) =<< screenWorkspace 0,
                             windows $ W.greedyView ws])                
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]
    ++ [withAlt key (sequence_ [
                             flip whenJust (windows . W.view) =<< screenWorkspace 1,
                             windows $ W.greedyView ws])       
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]
    ++ [withHyperShift key (windows $ W.shift ws)                      
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]
    -- mod-{a,s} focus physical screen
    -- mod-shift-{a,s} move window physical screen
    ++ [withHyper key (flip whenJust (windows . W.view) =<< screenWorkspace sc)
        | (key, sc) <- zip [xK_a, xK_s] [0..]]
    -- This is equivalent to /shift focused window to whatever workspace is on monitor N/
    -- Do i really need this keybind? I have relatively set keybinds
    ++ [withHyperShift key (flip whenJust (windows . W.shift) =<< screenWorkspace sc)
        | (key, sc) <- zip [xK_a, xK_s] [0..]]
    where
      keybind mask   key command = ((mask, key), command)
      withHyper      key command = keybind mod3Mask key command
      withHyperShift key command = keybind (mod3Mask .|. shiftMask) key command
      withHyperAlt   key command = keybind (mod3Mask .|. keyAlt) key command
      withHyperWin   key command = keybind (mod3Mask .|. keyWindows) key command
      withAlt        key command = keybind keyAlt key command
      withAltShift   key command = keybind (keyAlt .|. shiftMask) key command
      withWin        key command = keybind keyWindows key command
      withWinShift   key command = keybind keyWindows key command
      only key command = keybind 0 key command
      fsNoBar                    = sendMessage (Toggle "Full") <+> sendMessage ToggleStruts

------------------------------------------------------------------------
-- Layouts:
myLayout = twoThirdsMasterTiled ||| tiled ||| twoPane
  where
     -- Layouts
     cleanFull = noBorders Full
     centerOverTile = toggleLayouts cleanFull $ centerMaster tiled
     twoThirdsMasterTiled = toggleLayouts cleanFull $ spacing 10 $ Tall nmaster delta ratio2
     tiled   = toggleLayouts cleanFull $ spacing 10 $ Tall nmaster delta ratio
     twoPane = toggleLayouts cleanFull $ spacing 10 $ TwoPanePersistent Nothing delta ratio
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
    modMask            = mod3Mask,
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

