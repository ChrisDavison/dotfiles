-- IMPORTS

import XMonad
import System.Exit

import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce (spawnOnce)

import XMonad.Actions.WindowGo
import XMonad.Actions.Submap (submap)
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS (toggleWS', moveTo, findWorkspace, shiftTo, WSType( HiddenNonEmptyWS, EmptyWS ))

import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks

import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.NoBorders (noBorders)

import XMonad.Util.WorkspaceCompare(getSortByIndex)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


myDmenuConfig = "dmenu -i -fn 'Hack-14' -sb '#8620e6' -nhb '#8620e6' -nhf '#f438ee' -shb '#8620e6' -shf '#ffffff' -p 'App:' -m 0"

myJ4Command = "j4-dmenu-desktop --dmenu=\"" ++ myDmenuConfig ++ "\""

myGridSelect = def {
  gs_cellheight = 60
  , gs_cellwidth = 200
  , gs_font = "xft:Hack:pixelsize=14:antialias=true:hinting=true"
  }

-- Function to move to next non-empty WS skipping NSP.
nextNonEmptyWS = findWorkspace getSortByIndex Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)

-- Function to move to previous non-empty WS skipping NSP.
prevNonEmptyWS = findWorkspace getSortByIndex Prev HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)                 

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn myJ4Command)
    , ((modm .|. shiftMask, xK_p), spawn "dmenu_win_switcher.sh")
    , ((modm , xK_b), spawn "dmenu_bookmark_groups.sh")
    , ((modm , xK_d), submap . M.fromList $
        [ ((0, xK_e), spawn "dmenu_ebooks.sh"),
          ((0, xK_a), spawn "dmenu_articles.sh")])

    -- grid application selector
   , ((modm, xK_g), goToSelected myGridSelect)

    -- launch gmrun
    -- , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window 
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Change focus
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_i     ), prevNonEmptyWS)
    , ((modm,               xK_o     ), nextNonEmptyWS)

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window 
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Change size of master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Change number of windows in master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap (used with avoidStruts from Hooks.ManageDocks)
    -- , ((modm , xK_b ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), restart "xmonad" True)

    -- Media keys
    , ((modm ,  xK_Home ), spawn "amixer set Master 3dB+")
    , ((modm ,  xK_End ), spawn "amixer set Master 3dB-")
    , ((modm ,  xK_Prior ), spawn "~/.bin/spotify.sh play-pause")
    , ((modm ,  xK_Next ), spawn "~/.bin/spotify.sh next")
    , ((modm ,  xK_Delete ), spawn "~/.bin/spotify.sh previous")

    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        -- , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{a,s}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{a,s}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_s] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]



------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = twoThirdsMasterTiled ||| noBorders Full ||| centerMaster tiled ||| tiled
  where
     -- default tiling algorithm partitions the screen into two panes
     twoThirdsMasterTiled = spacing 10 $ Tall nmaster delta ratio2
     tiled   = spacing 10 $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio2 = 2/3
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Bitwarden"      --> doFloat
    , className =? "Pavucontrol"    --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Startup hook
myStartupHook = do
    spawnOnce "redshift"
    spawnOnce "~/.fehbg"
    spawnOnce "compton &"
    spawnOnce "/opt/Mullvad VPN/mullvad-vpn"
    spawnOnce "dropbox start"

------------------------------------------------------------------------
-- Configure workspace names in xmobar
myLogHook pipe = dynamicLogWithPP xmobarPP {
    ppOutput = hPutStrLn pipe
    --ppTitle = xmobarColor "green" "" . shorten 50
    , ppCurrent = \w -> xmobarColor "#af59ff" "" w
    , ppTitle = \c -> ""
    , ppLayout = \c -> ""
    }
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
myConfig pipe = defaultConfig {
      -- simple stuff
        -- preferred terminal
        terminal           = "alacritty",
        focusFollowsMouse  = True,
        borderWidth        = 2,
        modMask            = mod3Mask,
        -- workspaces = map show [1..9]
        workspaces         = ["1:emacs", "2:web"] ++ map show [3..7] ++ ["8:music", "9:video"],
        normalBorderColor  = "#dddddd",
        focusedBorderColor = "#8620e6",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts $ myLayout,
        manageHook         = manageDocks <+> myManageHook,
        logHook            = myLogHook pipe,
        startupHook        = myStartupHook
    }

xmobarConf = "xmobar -x 0 /home/davison/.config/xmobar/xmobarrc"

-----------------------------------------------------------------------
-- MAIN
main =  xmonad . docks . ewmh . myConfig =<< spawnPipe xmobarConf

