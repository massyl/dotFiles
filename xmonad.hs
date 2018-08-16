{-# LANGUAGE BangPatterns #-}

--
-- Configuration file for XMonad + MATE
--
--  Usage:
--      * Copy this file to ~/.xmonad/
--      * Run:    $ xmonad --recompile
--
--  Author: Massyl Nait Mouloud
--  Inspired by:
--
--
--  License: BSD
--
import           Data.Monoid(Endo)
import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Util.Run (safeSpawn,spawnPipe)
import qualified Data.Map as M
import           System.Environment (getEnvironment)
import           XMonad.Util.EZConfig
import           XMonad.Hooks.DynamicLog
import           System.IO
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.EZConfig (additionalKeys)
import           XMonad.Util.WorkspaceCompare(getSortByXineramaRule)
import           System.Exit
import           XMonad.Actions.WindowGo
import qualified XMonad.StackSet as W
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.ManageDocks
import qualified XMonad.Layout.IndependentScreens as LIS
import           XMonad.Actions.UpdatePointer
import           XMonad.Layout.Spacing
import System.Environment


customWs:: [String]
customWs = ["1:func", "2:java", "3:web", "4:vms"] ++ map show [5..10]

-- | -------------------------------------------------------------------------------
--
--  go to login screen and keep current session
------------------------------------------------------------------------------------
logout :: MonadIO m => KeyMask -> ((KeyMask, KeySym), m ())
logout modm = ((modm .|. shiftMask, xK_q), spawn "dm-tool switch-to-greeter")

-- | -------------------------------------------------------------------------------
--
--  suspend current  session
------------------------------------------------------------------------------------
suspend :: MonadIO m => KeyMask -> ((KeyMask, KeySym), m ())
suspend modm= ((modm .|. shiftMask, xK_s), spawn "sudo pm-suspend")

-- | -------------------------------------------------------------------------------
--
--  hibernate current session
------------------------------------------------------------------------------------
hibernate :: MonadIO m => KeyMask -> ((KeyMask, KeySym), m ())
hibernate modm =((modm .|. shiftMask, xK_h), spawn "sudo pm-hibernate")

-- | -------------------------------------------------------------------------------
--
--   Mute volume
------------------------------------------------------------------------------------
toggle_volume :: MonadIO m => KeyMask -> ((KeyMask, KeySym), m ())
toggle_volume modm = ((modm .|. controlMask, xK_m), spawn "amixer -q set Master toggle")

-- | -------------------------------------------------------------------------------
--
--   Decrease volume by 10%
------------------------------------------------------------------------------------
decrease_volume
  :: MonadIO m => KeyMask -> ((KeyMask, KeySym), m ())
decrease_volume modm=  ((modm .|. controlMask, xK_d),  spawn "amixer -q set Master 10%-")

-- | -------------------------------------------------------------------------------
--
--   Increase volume by 10%
------------------------------------------------------------------------------------
increase_volume
  :: MonadIO m => KeyMask -> ((KeyMask, KeySym), m ())
increase_volume modm = ((modm .|. controlMask, xK_i), spawn "amixer -q set Master 10%+")

-- | -------------------------------------------------------------------------------
--
--    Disable right mouse button
------------------------------------------------------------------------------------
disable_right_mouse
  :: MonadIO m => KeyMask -> ((KeyMask, KeySym), m ())
disable_right_mouse modm = ((modm .|. shiftMask, xK_m), spawn "xmodmap -e 'pointer = 1 2 0 4 5 6 7 8 9'")

-- | -------------------------------------------------------------------------------
--
--   Enable right button or restaure defaut mouse configuration
------------------------------------------------------------------------------------
enable_right_mouse
  :: MonadIO m => KeyMask -> ((KeyMask, KeySym), m ())
enable_right_mouse modm =  ((modm .|. shiftMask, xK_e), spawn "xmodmap -e 'pointer = default'")

-- | -------------------------------------------------------------------------------
--
--   Forget what's keycode 135 is. Give it a meaning name when after
------------------------------------------------------------------------------------
code_135 :: MonadIO m => KeyMask -> ((KeyMask, KeySym), m ())
code_135 modm = ((modm .|. shiftMask, xK_n), spawn "xmodmap -e 'keycode 135 = NoSymbol'")

-- | -------------------------------------------------------------------------------
--
--  Disable trackpad touch
------------------------------------------------------------------------------------
disable_trackpad
  :: MonadIO m => KeyMask -> ((KeyMask, KeySym), m ())
disable_trackpad modm = ((modm .|. shiftMask, xK_t), spawn "xinput set-prop 'SynPS/2 Synaptics TouchPad' 'Device Enabled' 0")

-- | -------------------------------------------------------------------------------
--
--   Enable trackpad touch
------------------------------------------------------------------------------------
enable_trackpad
  :: MonadIO m => KeyMask -> ((KeyMask, KeySym), m ())
enable_trackpad modm = ((modm .|. shiftMask, xK_y), spawn "xinput set-prop 'SynPS/2 Synaptics TouchPad' 'Device Enabled' 1")

-- | -------------------------------------------------------------------------------
--
--
------------------------------------------------------------------------------------
mateKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
mateKeys (XConfig {modMask = modm}) = M.fromList $
  fmap ($ modm) [logout, suspend, hibernate, toggle_volume, decrease_volume, increase_volume, disable_right_mouse,enable_right_mouse,code_135,disable_trackpad,enable_trackpad]
  ++
  [((m .|. modm, k), windows $ f i) | (i, k) <- zip customWs [xK_1 .. xK_9], (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


-- | -------------------------------------------------------------------------------
--
--  Run's firefox if not yet running or move focus to it if already running
------------------------------------------------------------------------------------
firefox = (("M4-f") , runOrRaise "firefox" (className =? "Firefox-esr" <||> className =? "Navigator" <||> className =? "Firefox-bin" <||> className =? "Firefox"))

-- | -------------------------------------------------------------------------------
--
--  Run's emacs if not yet running or move focus to it if already running
------------------------------------------------------------------------------------
emacs  =  (("M4-x"), runOrRaise "emacs" (className =? "Emacs"))

-- | -------------------------------------------------------------------------------
--
--  Run's eclipse if not yet running or move focus to it if already running
------------------------------------------------------------------------------------
eclipse = (("M4-c"), runOrRaise "/home/massyl/softs/eclipse/eclipse.sh" (className =? "Eclipse"))

-- | -------------------------------------------------------------------------------
--
--  Kill xmonad session
------------------------------------------------------------------------------------
kill_xmonad = (("M4-z"), kill)

-- | -------------------------------------------------------------------------------
--
--  Runs dmenu
------------------------------------------------------------------------------------
dmenu = (("M4-p"), spawn "dmenu_run | dmenu -b")

-- | -------------------------------------------------------------------------------
--
-- Defines custom keys
------------------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys = [firefox, emacs, eclipse, kill_xmonad, dmenu]

-- | -------------------------------------------------------------------------------
--
-- Custom layout. Must be tuned again
------------------------------------------------------------------------------------
-- customLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full
customLayout = avoidStrutsOn [U,L] $ tiled ||| Mirror tiled ||| Full
  where
    tiled = spacing 2 $ Tall nmaster delta ratio
    nmaster = 2
    ratio = 1/3
    delta = 5/100

-- | -------------------------------------------------------------------------------
--
-- Custom hook functions that will move executables to the right workspace
------------------------------------------------------------------------------------
ownManageHook :: Query (Endo WindowSet)
ownManageHook = composeAll
    [ className =? "Emacs"       --> doShift "1:func"
     ,className =? "Eclipse"     --> doShift "2:java"
     ,className =? "Firefox-esr" --> doShift "3:web"
     ,className =? "Navigator"   --> doShift "3:web"
     ,className =? "Firefox"     --> doShift "3:web"
    ]

-- | -------------------------------------------------------------------------------
--
--  Mate specific xmonad desktop configuration
------------------------------------------------------------------------------------
mateConfig = docks $ desktopConfig
    { terminal = "gnome-terminal --hide-menubar"
    , keys     = mateKeys <+> keys desktopConfig
    , workspaces = customWs
    , manageHook = manageDocks <+> ownManageHook <+> manageHook def --defaultConfig
    , startupHook = setWMName "LG3D"
    }

main :: IO ()
main = do
    xmproc <- spawnPipe "/home/massyl/.local/bin/xmobar /home/massyl/.xmobarrc"
    xmonad $! mateConfig
                { modMask = mod4Mask
                ,layoutHook = customLayout
                ,logHook = dynamicLogWithPP xmobarPP
                           { ppOutput = hPutStrLn xmproc
                           , ppTitle = xmobarColor "green" "" . shorten 50
                           -- , ppSort    = getSortByXineramaRule
                           } >> updatePointer (1, 1) (0, 0)

                , borderWidth = 4
                , focusedBorderColor = "#7FBC71"
                , manageHook = manageDocks <+> ownManageHook
                } `additionalKeysP` myKeys

