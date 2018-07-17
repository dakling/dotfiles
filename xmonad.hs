import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.Submap
import Graphics.X11.ExtraTypes.XF86
import Network.HostName
-- import qualified XMonad.StackSet as W
-- import XMonad.Config.Xfce
import System.IO
import qualified Data.Map        as M
 
main = do 
    hostname <- getHostName
    xmonad =<< statusBar (myBar hostname) myPP toggleStrutsKey myConfig

-- check on which pc we are running
atWork hostname = hostname == "klingenberg-pc"
-- Command to launch the bar.
myBar hostname
    | atWork hostname == True = "xmobar -x 1" 
    | otherwise = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#00adee" "" . wrap "<" ">" 
                , ppTitle = xmobarColor "#00adee" "" 
                }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig
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
    , handleEventHook = fullscreenEventHook -- allow fullscreen
    }


--keybinds
myKeys x = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
newKeys conf@(XConfig {XMonad.modMask = modm}) = 
    [
        ((modm, xK_c), kill)
        -- , ((modm .|. shiftMask, xK_Return), (windows W.swapMaster)) -- %! Swap the focused window and the master window
        , ((modm, xK_F1), (spawn $ "termite")) 
        , ((modm, xK_F2), (spawn $ "firefox"))
        , ((modm, xK_F3), (spawn $ "rangerStandalone"))
        , ((modm, xK_F4), (spawn $ "thunderbird"))
        , ((modm, xK_F5), (spawn $ "termite -e pacui"))
        , ((modm, xK_d), (spawn $ "albert show"))
        , ((modm, xK_x), submap . M.fromList $
            [((0,xK_i), (spawn $ "firefox"))
            ,((0,xK_d), (spawn $ "pcmanfm"))
            ,((0,xK_p), (spawn $ "pamac-manager"))
            ,((0,xK_e), (spawn $ "thunderbird"))
            ])
        , ((modm .|. shiftMask, xK_x), submap . M.fromList $
            [((0,xK_c), (spawn $ "nvim-termite ~/.xmonad/xmonad.hs"))
            ,((0,xK_v), (spawn $ "nvim-termite ~/.nvim/init.vim"))
            ,((0,xK_z), (spawn $ "nvim-termite ~/.zshrc"))
          ])
        , ((0, xF86XK_AudioLowerVolume   ), (spawn "amixer set Master 2-"))
        , ((0, xF86XK_AudioRaiseVolume   ), (spawn "amixer set Master 2+"))
        , ((0, xF86XK_AudioMute          ), (spawn "amixer set Master toggle"))
      ]
--autostart
myStartupHook = do
    xmproc <- spawnPipe "xsetroot -cursor_name left_ptr"
    xmproc <- spawnPipe "monitor_home"
    xmproc <- spawnPipe "albert"
    xmproc <- spawnPipe "stalonetray"
    xmproc <- spawnPipe "nitrogen --head=1 --random .config/backgrounds --set-scaled"
    xmproc <- spawnPipe "nitrogen --head=0 --random .config/backgrounds --set-scaled"
    xmproc <- spawnPipe "nm-applet"
    xmproc <- spawnPipe "pa-applet"
    xmproc <- spawnPipe "fix_touchscreen"
    xmproc <- spawnPipe "easystroke enable"
    xmproc <- spawnPipe "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    xmproc <- spawnPipe "compton -b"
    xmproc <- spawnPipe "setxkbmap -option ctrl:nocaps"  
    xmproc <- spawnPipe "xcape -e 'Control_L=Escape'"
    xmproc <- spawnPipe "/usr/share/HESSENBOX_DA/HESSENBOX_DA-Client.sh"
    xmproc <- spawnPipe "signal-desktop --start-in-tray"
    xmproc <- spawnPipe "pamac-tray"
    xmproc <- spawnPipe "clipit"
    xmproc <- spawnPipe "dropbox"
    xmproc <- spawnPipe "onboard"
    return ()
