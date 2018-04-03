import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
-- import XMonad.Config.Xfce
import System.IO
import qualified Data.Map        as M
 
main = do
--autostart
    xmproc <- spawnPipe "monitor_home"
    xmproc <- spawnPipe "albert"
    h <- spawnPipe "xmobar -d"

    xmonad $ defaultConfig{
    layoutHook = avoidStruts  $  layoutHook defaultConfig
    -- this adds Xmobar to Xmonad
    , logHook = dynamicLogWithPP $
            xmobarPP {
                        ppOutput = hPutStrLn h
                    }
    , manageHook = manageDocks <+> manageHook defaultConfig
    , modMask = mod4Mask
    , keys = myKeys

    --default applications
    , terminal = "termite"
    --theming
    , borderWidth = 2
    , normalBorderColor  = "#333333"
    , focusedBorderColor = "#00adee"
    }


--keybinds
myKeys x = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
newKeys conf@(XConfig {XMonad.modMask = modm}) = 
    [
        ((modm, xK_c), kill)
        -- , ((modm, xK_c), spawnPipe "firefox")
    ]
