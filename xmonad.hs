--xmonad config

import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myTerminal = "/usr/bin/urxvt"

myScreensaver = "/usr/bin/xscreensaver-command -l"

mySelectScreenshot = "select-screenshot"

myScreenshot = "screenshot"

myXmobarrc = "~/.xmonad/xmobar-single.hs"

myWorkspaces = ["1:term", "2:web", "3:code","4:media", "5:vm"] ++ map show [6..9]

myManageHook = composeAll
  [className =? "Chromium"         --> doShift "2:web"
  ,className =? "Google-Chrome"    --> doShift "2:web"
  ,className =? "Firefox"          --> doShift "2:web"
  ,className =? "Emacs"		   --> doShift "3:code"
  ,resource  =? "desktop_window"   --> doIgnore
  ,className =? "Steam"            --> doFloat
  ,className =? "Gimp"             --> doFloat
  ,className =? "stalonetray"      --> doIgnore
  , isFullscreen                   --> (doF W.focusDown <+> doFullFloat)
  ]

myLauncher = "exe=`dmenu_path | dmenu` && eval \"exec $exe\""

myLayout = avoidStruts (
  ThreeColMid 1 (3/100) (1/2) |||
  Tall 1 (3/100) (1/2) |||
  Mirror (Tall 1 (3/100) (1/2)) |||
  tabbed shrinkText tabConfig |||
  Full |||
  spiral (6/7) |||
  noBorders (fullscreenFull Full))

myNormalBorderColor = "#7C7C7C"
myFocusedBorderColor = "#FFB6B0"

tabConfig = defaultTheme {
  activeBorderColor = "#7C7C7C",
  activeTextColor = "#CEFFAC",
  activeColor = "#000000",
  inactiveBorderColor = "#7C7C7C",
  inactiveTextColor = "#EEEEEE",
  inactiveColor = "#000000"
                         }

xmobarTitleColor = "#FFB6B0"

xmobarCurrentWorkspaceColor = "#CEFFAC"

myBorderWidth = 1

myModMask = mod1Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

  [ ((modMask .|. shiftMask, xK_Return),
    spawn $ XMonad.terminal conf)
    ,((modMask .|. controlMask, xK_l),
     spawn myScreensaver)
    ,((modMask, xK_p),
     spawn myLauncher)
    ,((modMask, xK_p),
     spawn myLauncher)
  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  , ((modMask .|. shiftMask, xK_p),
     spawn mySelectScreenshot)
  -- Take a full screenshot using the command specified by myScreenshot.
  , ((modMask .|. controlMask .|. shiftMask, xK_p),
     spawn myScreenshot)
  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn "amixer -q set Master toggle")
  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn "amixer -q set Master 5%-")
  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "amixer -q set Master 5%+")
  -- Mute volume.
  , ((modMask .|. controlMask, xK_m),
     spawn "amixer -q set Master toggle")
  -- Decrease volume.
  , ((modMask .|. controlMask, xK_j),
     spawn "amixer -q set Master 5%-")
  -- Increase volume.
  , ((modMask .|. controlMask, xK_k),
     spawn "amixer -q set Master 5%+")
  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")
  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")
  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")
  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --
  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)
  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)
  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)
  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)
  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)
  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)
  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )
  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )
  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)
  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )
  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )
  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)
  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)
  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)
  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))
  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))
  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))
  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]
  ++
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]
-- Run xmonad with all the defaults we set up.
--

main = do
  xmproc <- spawnPipe ("xmobar " ++ myXmobarrc)
  xmonad $ defaults {
      logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = "   "
      }
      , manageHook = manageDocks <+> myManageHook
      , startupHook = setWMName "LG3D"
      , handleEventHook = docksEventHook
  }


------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook
}
