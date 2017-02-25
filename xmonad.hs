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
import           System.Exit
import           XMonad.Actions.WindowGo
import qualified XMonad.StackSet as W
import           XMonad.Hooks.SetWMName
import qualified XMonad.Layout.IndependentScreens as LIS
import           XMonad.Actions.UpdatePointer
import           XMonad.Layout.Spacing
import System.Environment

customWs:: [String]
customWs = ["1:func", "2:java", "3:web", "4:vms"] ++ map show [5..9]

ownManageHook :: Query (Endo WindowSet)
ownManageHook = composeAll
    [ className =? "Emacs" --> doShift "1:func"
     ,className =? "Eclipse" --> doShift "2:java"
     ,className =? "Firefox" --> doShift "3:web"]

mateConfig = desktopConfig
    { terminal = "gnome-terminal"
    , keys     = mateKeys <+> keys desktopConfig
    , workspaces = customWs
    , manageHook = manageDocks <+> ownManageHook
    , startupHook = setWMName "LG3D"
    }

mateKeys (XConfig {modMask = modm}) = M.fromList $
    [((modm .|. shiftMask, xK_l), spawn "gnome-screensaver-command -l")
    --go to login screen and keep current session 
    ,((modm .|. shiftMask, xK_q), spawn "dm-tool stwitch-to-greeter")
    ,((modm .|. shiftMask, xK_s), spawn "sudo pm-suspend")
    ,((modm .|. shiftMask, xK_h), spawn "sudo pm-hibernate")
    -- Mute volume.
  , ((modm .|. controlMask, xK_m), spawn "amixer -q set Master toggle")
  -- Decrease volume.
  , ((modm .|. controlMask, xK_d),  spawn "amixer -q set Master 10%-")
  -- Increase volume.
  , ((modm .|. controlMask, xK_i), spawn "amixer -q set Master 10%+")
    --disables right mouse button
  ,((modm .|. shiftMask, xK_m), spawn "xmodmap -e 'pointer = 1 2 0 4 5 6 7 8 9'")
   --restaures the default configuration of mouse (mainely to enbale the right button)
  ,((modm .|. shiftMask, xK_e), spawn "xmodmap -e 'pointer = default'")
  ,((modm .|. shiftMask, xK_n), spawn "xmodmap -e 'keycode 135 = NoSymbol'")
   -- disable trackpad and enables it (t and y)
  ,((modm .|. shiftMask, xK_t), spawn "xinput set-prop 'SynPS/2 Synaptics TouchPad' 'Device Enabled' 0")
  ,((modm .|. shiftMask, xK_y), spawn "xinput set-prop 'SynPS/2 Synaptics TouchPad' 'Device Enabled' 1")
    ]++
    [((m .|. modm, k), windows $ f i)
         | (i, k) <- zip customWs [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

main :: IO ()
main = do
    -- unsetEnv "GHC_PACKAGE_PATH"
    -- getEnv "PREVPATH" >>= setEnv "PATH"
    -- unsetEnv "PREVPATH"
    xmproc <- spawnPipe "xmobar"
    xmonad $  mateConfig
                { modMask = mod4Mask
                 ,layoutHook = customLayout
                 ,logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        } >> updatePointer (0.5, 0.5) (1, 1)

              , borderWidth = 4
                 , focusedBorderColor = "#7FBC71"
                 , manageHook = manageDocks <+> ownManageHook
                } `additionalKeysP` myKeys

myKeys :: [(String, X ())]
myKeys = [ (("M4-f") , runOrRaise "firefox" (className =? "Firefox" <||> className =? "Firefox-bin" <||> className =? "Navigator"))
           ,(("M4-x"), runOrRaise "emacs" (className =? "Emacs"))
           ,(("M4-c"),
             runOrRaise "/home/massyl/softs/eclipse/eclipse.sh" (className =? "Eclipse"))
           ,(("M4-z"), kill)
           ,(("M4-p"), spawn "dmenu_run | dmenu -b")
         ]

customLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
    tiled = spacing 5 $ Tall nmaster delta ratio
    nmaster = 2
    ratio = 1/3
    delta = 5/100
