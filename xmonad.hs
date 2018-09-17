import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Layout.NoBorders
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
    layoutHook = smartBorders  $  layoutHook defaultConfig
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
        , ((modm .|. shiftMask, xK_Return), (spawn $ "open-terminal-here")) 
        , ((modm, xK_F1), (spawn $ "termite")) 
        , ((modm, xK_F2), (spawn $ "firefox"))
        , ((modm, xK_F3), (spawn $ "rangerStandalone"))
        , ((modm, xK_F4), (spawn $ "thunderbird"))
        , ((modm, xK_F5), (spawn $ "termite -e pacui"))
        , ((modm, xK_o), (spawn $ "onboard"))
        , ((modm, xK_d), (spawn $ "dmenu_extended_run"))
        -- , ((modm,xK_Left),  (spawn $ "xdotool mousemove_relative -- -1 0"))
        -- , ((modm,xK_Right), (spawn $ "xdotool mousemove_relative -- 1 0"))
        -- , ((modm,xK_Up),    (spawn $ "xdotool mousemove_relative -- 0 -1"))
        -- , ((modm,xK_Down),  (spawn $ "xdotool mousemove_relative -- 0 1"))
        , ((modm, xK_x), submap . M.fromList $
            [((0,xK_i), (spawn $ "epiphany"))
            ,((0,xK_d), (spawn $ "pcmanfm"))
            ,((0,xK_p), (spawn $ "pamac-manager"))
            ,((0,xK_e), (spawn $ "thunderbird"))
            ])
        , ((modm .|. shiftMask, xK_x), submap . M.fromList $
            [((0,xK_c), (spawn $ "nvim-termite ~/.dotfiles/dotfiles/xmonad.hs"))
            ,((0,xK_v), (spawn $ "nvim-termite ~/.dotfiles/dotfiles/nvim/init.vim"))
            ,((0,xK_z), (spawn $ "nvim-termite ~/.dotfiles/dotfiles/zshrc"))
          ])
        , ((modm, xK_r), submap . M.fromList $
            [((0,xK_s), (spawn $ "shutdown now"))
            ,((0,xK_r), (spawn $ "reboot"))
            ,((0,xK_l), (spawn $ "i3exit lock"))
            ])
        -- , ((0, xF86XK_AudioLowerVolume   ), (spawn "amixer set Master 2-"))
        -- , ((0, xF86XK_AudioRaiseVolume   ), (spawn "amixer set Master 2+"))
        -- , ((0, xF86XK_AudioMute          ), (spawn "amixer set Master toggle"))
        , ((0, xK_Page_Down              ), (spawn "xdotool click 5"))
        , ((0, xK_Page_Up                ), (spawn "xdotool click 4"))
        , ((0, xK_Menu                   ), (spawn "xdotool click 2"))
      ]
--autostart
myStartupHook = do
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "monitor_home"
    -- spawnOnce "albert"
    spawnOnce "stalonetray"
    spawnOnce "nitrogen --head=0 --random ~/.config/backgrounds --set-scaled"
    spawnOnce "nitrogen --head=1 --random ~/.config/backgrounds --set-scaled"
    spawnOnce "nm-applet"
    spawnOnce "pa-applet"
    spawnOnce "fix_touchscreen"
    spawnOnce "easystroke enable"
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    spawnOnce "compton -b"
    spawnOnce "setxkbmap -option ctrl:nocaps"  
    spawnOnce "xcape -e 'Control_L=Escape'"
    spawnOnce "/usr/share/HESSENBOX_DA/HESSENBOX_DA-Client.sh"
    spawnOnce "signal-desktop --start-in-tray"
    spawnOnce "pamac-tray"
    spawnOnce "clipit"
    spawnOnce "dropbox"
    spawnOnce "onboard"
    return ()
