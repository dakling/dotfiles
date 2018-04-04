import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
-- import XMonad.Config.Xfce
import System.IO
import qualified Data.Map        as M
 
main = xmonad =<< dzen defaultConfig
   {
    layoutHook = avoidStruts  $  layoutHook defaultConfig
    , manageHook = manageDocks <+> manageHook defaultConfig
    , modMask = mod4Mask
    , keys = myKeys

    --default applications
    , terminal = "termite"
    --theming
    , borderWidth = 2
    , normalBorderColor  = "#333333"
    , focusedBorderColor = "#00adee"
    , startupHook = myStartupHook
    }


--keybinds
myKeys x = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
newKeys conf@(XConfig {XMonad.modMask = modm}) = 
    [
        ((modm, xK_c), kill)
        , ((modm, xK_F2), (spawn $ "firefox"))
    ]
--autostart
myStartupHook = do
    xmproc <- spawnPipe "monitor_home"
    xmproc <- spawnPipe "albert"
    return ()
