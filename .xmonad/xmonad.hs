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
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.WorkspaceCompare(getSortByIndex)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- COLOURS & other style stuff
myColourPurple = "#8620e6"
myColourPink = "#f438ee"
myColourGrey = "#333333"
myColourLightGrey = "#dddddd"

myFont = "xft:Hack:pixelsize=14:antialias=true:hinting=true"
myFontSmall = "xft:Hack:pixelsize=12:antialias=true:hinting=true"

myTerminal = "alacritty"
myWorkspaces = map show [1..9]

-- Execution of programs
myDmenuConfig = "dmenu -l 10 -i -fn 'Hack-14' -sb '#8620e6' -nhb '#8620e6' -nhf '#f438ee' -shb '#8620e6' -shf '#ffffff' -p 'App:' -m 0"
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

-- KEYBINDING
-- Key aliases, as a reminder for keybinds (mod2 is numlock)
keyAlt = mod1Mask
keyHyper = mod3Mask
keyWindows = mod4Mask
myModMask = keyHyper


makeSimpleKeymap keyMask simpleKeymapList = map (first $ (,) mask) simpleKeymapList
  where
    mask = case keyMask of
      Just m -> m
      Nothing -> 0

makeSimpleSubmap keyMask commandList = submap . M.fromList $ makeSimpleKeymap keyMask commandList

makeKeyChords keyMask parentKey childrenCommands = makeSimpleKeymap keyMask [(parentKey, makeSimpleSubmap Nothing childrenCommands)]

myMouseBindings conf = M.fromList . makeSimpleKeymap (Just myModMask) $
    [ (button1, (\w -> focus w >> mouseMoveWindow w)) -- Left = move
    , (button2, (\w -> focus w >> windows W.swapMaster)) -- Middle = make master
    , (button3, (\w -> focus w >> mouseResizeWindow w)) -- Right = resize
    -- Mouse wheel is sensitive, so use  smaller volume increments
    , (button4, (\w -> spawn "amixer set Master 3dB+")) -- Scroll up = vol up
    , (button5, (\w -> spawn "amixer set Master 3dB-"))] -- Scroll down = vol down

-- AVAILABLE - F5..F12  (and shift-F1..F12)
--             `~ 0 S-0 -_ =+ Insert
--             tabTAB W eE R T yY uU iI oO \
--             D F gG H ;: '"
--             zZ xX c vV B nN mM <> /?
--             [_,S-][left, right, up, down]
--
-- Using Maybe KeyMask, as it felt more intuitive to read than '0'
myKeys conf = M.fromList $
    makeSimpleKeymap (Just (myModMask .|. shiftMask)) [
      (xK_q     , io (exitWith ExitSuccess)) -- QUIT
    , (xK_Return, spawn myTerminal)
    , (xK_c     , kill) -- close the focused window
    , (xK_space , setLayout $ XMonad.layoutHook conf) -- reset to default layout
    , (xK_j     , windows W.swapDown) -- move window up
    , (xK_k     , windows W.swapUp  ) -- move window down
    ]
    ++
    makeSimpleKeymap (Just myModMask) [
      (xK_q             , restart "xmonad" True)
    -- Window and layout management
    , (xK_j             , windows W.focusDown) -- focus up stack
    , (xK_k             , windows W.focusUp  ) -- focus down stack
    , (xK_bracketleft   , gotoNonEmptyWS Prev) 
    , (xK_bracketright  , gotoNonEmptyWS Next)
    , (xK_m             , windows W.focusMaster  ) -- jump to master
    , (xK_Return        , windows W.swapMaster) -- make focused app master
    , (xK_h             , sendMessage Shrink) -- fewer windows in master
    , (xK_l             , sendMessage Expand) -- more windows in master
    , (xK_space         , sendMessage NextLayout) -- Use next configured layout
    , (xK_f             , sendMessage (Toggle "Full")) -- toggle fullscreen on focused window
    , (xK_t             , withFocused $ windows . W.sink) -- make float tiled again
    , (xK_comma         , sendMessage (IncMasterN 1)) -- increase num master windows
    , (xK_period        , sendMessage (IncMasterN (-1))) -- decrease num master windows
    -- Launchers
    , (xK_p             , spawn myJ4Command)
    , (xK_w             , spawn "dmenu_win_switcher.sh")
    , (xK_b             , spawn "dmenu_bookmark_groups.sh")
    , (xK_r             , spawn $ myTerminal ++ " -e ranger")
    , (xK_x             , spawn $ myTerminal ++ " -t scratchpad -e $HOME/.bin/get_xprop.sh")
    -- Emacs / org mode
    , (xK_F1            , raiseEmacsAndRun "(org-capture)")
    , (xK_F2            , raiseEmacsAndRun "(org-agenda)")
    , (xK_F3            , raiseEmacsAndRun "(org-agenda nil \"c1\")")
    , (xK_F4            , raiseEmacsAndRun "(org-agenda nil \"cW\")")
    -- Other apps
    , (xK_F5            , runOrRaise "spotify" (className =? "Spotify"))
    , (xK_F6            , raiseNextMaybe (spawn "firefox") (className =? "Firefox"))
    -- Media keys
    , (xK_Home          , spawn "amixer set Master 6dB+")
    , (xK_End           , spawn "amixer set Master 6dB-")
    , (xK_Delete        , spawn "$HOME/.bin/spotify.sh prev")
    , (xK_Next          , spawn "$HOME/.bin/spotify.sh next")
    , (xK_Prior         , spawn "$HOME/.bin/spotify.sh play-pause")
    ]
    ++ makeKeyChords (Just myModMask) xK_d [
                                     (xK_e, spawn "dmenu_ebooks.sh"),
                                     (xK_a, spawn "dmenu_articles.sh")]

    -- mod-[1..9] view workspace
    -- mod-win-[1..9] view workspace on main monitor 
    -- mod-alt-[1..9] view workspace on second monitor
    -- (win is left of alt, so left monitor)
    ++ makeSimpleKeymap (Just myModMask) [
        (key, windows $ W.greedyView ws)                    
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]
    ++ makeSimpleKeymap (Just (myModMask .|. keyWindows)) [            
        (key, sequence_ [flip whenJust (windows . W.view) =<< screenWorkspace 0,
                         windows $ W.greedyView ws])                
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]
    ++ makeSimpleKeymap (Just (myModMask .|. keyAlt)) [
        (key, sequence_ [flip whenJust (windows . W.view) =<< screenWorkspace 1,
                         windows $ W.greedyView ws])       
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]

    -- mod-shift-[1..9] move window to workspace
    ++ makeSimpleKeymap (Just (myModMask .|. shiftMask)) [
        (key, windows $ W.shift ws)                      
        | (key, ws) <- zip [xK_1..xK_9] myWorkspaces]

    -- mod-{a,s} focus physical screen
    -- mod-shift-{a,s} move window physical screen
    ++ makeSimpleKeymap (Just myModMask) [
        (key, flip whenJust (windows . W.view) =<< screenWorkspace sc)
        | (key, sc) <- zip [xK_a, xK_s] [0..]]
    
    ++ makeSimpleKeymap (Just (myModMask .|. shiftMask)) [
        (key, flip whenJust (windows . W.shift) =<< screenWorkspace sc)
        | (key, sc) <- zip [xK_a, xK_s] [0..]]

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
    spawnOnce "trayer --edge top --align center --widthtype request --padding 0 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --tint 0x000000 --alpha 100 --height 21 &"
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

