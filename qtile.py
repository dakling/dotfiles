# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from libqtile import bar, hook, layout, widget
from libqtile.config import Click, Drag, Group, Key, KeyChord, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from libqtile import extension
from libqtile.log_utils import logger
# from pynput.keyboard import Key as PKey, Controller
import pyautogui
import subprocess

try:
    import aiomanhole # type: ignore
except ImportError:
    aiomanhole = None



# global variables

mod = "mod4"
alt = "mod1"
terminal = guess_terminal()
browser = "firefox"

bg_color = "#222222"
fg_color = "#FFFFFF"
fg_color_alt = "#828080"

# custom functions

def change_focus_emacs(qtile, direction):
    w = qtile.current_screen.group.current_window
    win_class = w.get_wm_class()
    if win_class is not None and 'emacs' == win_class[0].lower():
        if qtile.core.name == "x11":
            pyautogui.keyDown('alt')
            pyautogui.keyUp('h')
            # pyautogui.keyUp('j')
            # pyautogui.keyUp('k')
            # pyautogui.keyUp('l')
            pyautogui.keyDown(direction["key"])
            pyautogui.keyUp(direction["key"])
            pyautogui.keyUp('alt')
        elif qtile.core.name == "wayland":
            # qtile.cmd_simulate_keypress([mod, "control", alt], direction["key"])
            # qtile.cmd_simulate_keypress([], "i")
            # qtile.cmd_spawn("emacsclient" " --eval (quote (progn (helm-find-files)))")
            # qtile.cmd_spawn("wtype" " -M ctrl -M win " + direction["key"])
            subprocess.run("wtype -M ctrl -M alt " + direction["key"])
            # subprocess.run("wtype -M alt -M logo h")
            # subprocess.run("wtype" " i " + direction["key"])
            # direction["cmd"]()
    else:
        direction["cmd"]()

@lazy.function
def left_emacs(qtile):
    try:
        change_focus_emacs(qtile, {"key": 'f12',
                                   "cmd": qtile.current_group.layout.cmd_left,
                                })
    except AttributeError:
        change_focus_emacs(qtile, {"key": 'f12',
                                   "cmd": qtile.current_group.layout.cmd_previous,
                                   })

@lazy.function
def right_emacs(qtile):
    try:
        change_focus_emacs(qtile, {"key": 'f11',
                                   "cmd": qtile.current_group.layout.cmd_right,
                                })
    except AttributeError:
        change_focus_emacs(qtile, {"key": 'f11',
                                   "cmd": qtile.current_group.layout.cmd_next}
                                   )

@lazy.function
def up_emacs(qtile):
    try:
        change_focus_emacs(qtile, {"key": 'f9',
                                   "cmd": qtile.current_group.layout.cmd_up,
                                })
    except AttributeError:
        change_focus_emacs(qtile, {"key": 'f9'})

@lazy.function
def down_emacs(qtile):
    try:
        change_focus_emacs(qtile, {"key": 'f10',
                                   "cmd": qtile.current_group.layout.cmd_down,
                                })
    except AttributeError:
        change_focus_emacs(qtile, {"key": 'f10'})

@lazy.function
def to_other_screen(qtile):
    current_screen = qtile.current_screen
    logger.warning(current_screen)
    new_screen = 0 if current_screen == 1 else 1
    # qtile.current_window.to_screen(new_screen)


keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", left_emacs(), desc="Move focus to left"),
    Key([mod], "l", right_emacs(), desc="Move focus to right"),
    Key([mod], "j", down_emacs(), desc="Move focus down"),
    Key([mod], "k", up_emacs(), desc="Move focus up"),
    Key([mod], "Left", lazy.layout.left(), desc="Move focus to left (ignoring emacs frames)"),
    Key([mod], "Right", lazy.layout.right(), desc="Move focus to right (ignoring emacs frames)"),
    Key([mod], "Down", lazy.layout.down(), desc="Move focus down (ignoring emacs frames)"),
    Key([mod], "Up", lazy.layout.up(), desc="Move focus up (ignoring emacs frames)"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod], "o", lazy.next_screen(), desc="Go to next screen"),
    Key([mod, "control"], "o", lazy.prev_screen(), desc="Go to previous screen"),
    # Key([mod, "shift"], "o", lazy.window.toscreen(0 if lazy.current_screen == 1 else 1), desc="Move window to next screen"),
    Key([mod, "shift"], "o", to_other_screen(), desc="Move window to next screen"),
    # Key([mod, "shift"], "o", logger.warning(lazy.group.current_screen), desc="Move window to next screen"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "control"], "m",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    Key([mod, "control"], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod, alt], "m", lazy.to_layout_index(2), desc="Use max layout"),
    Key([mod], "v", lazy.to_layout_index(0), desc="Use column layout"),
    Key([mod], "s", lazy.to_layout_index(1), desc="Use vertical tile layout"),
    Key([mod], "c", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "shift"], "m", lazy.window.toggle_fullscreen() , desc="Toggle fullscreen of focused window"),
    # Key([mod], "w", lazy.run_extension(extension.WindowList()) , desc="Show list of all open windows"),
    Key([mod, "shift"], "space", lazy.window.toggle_floating() , desc="Toggle floating of focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod, "shift"], "d", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([mod], "d", lazy.spawn("rofi -show combi"), desc="Spawn a command using rofi"),

    Key([mod], "e", lazy.spawn("emacsclient -nc"), desc="Spawn emacsclient"),
    Key([mod, "shift"], "e", lazy.spawn("emacs"), desc="Spawn emacs"),
    Key([mod], "F1", lazy.spawn(terminal), desc="Spawn terminal"),
    Key([mod], "F2", lazy.spawn(browser), desc="Spawn browser"),

    KeyChord([mod], "space", [
        KeyChord(["shift"], "s", [
            Key(["shift"], "s", lazy.spawn("shutdown now")),
            Key(["shift"], "r", lazy.spawn("reboot")),
        ],
                 mode="System")
    ],
             mode="Top")
]

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

layouts = [
    layout.Columns(border_focus_stack=[fg_color, bg_color],
                   border_focus=[fg_color_alt, bg_color],
                   border_width=1,
                   split=False,
                   border_on_single=True,
                   wrap_focus_rows=False,
                   wrap_focus_columns=False,
                   wrap_focus_stacks=True),
    # layout.Stack(num_stacks=1),
    # layout.Stack(num_stacks=2),
    # layout.Stack(num_stacks=3),
    layout.VerticalTile(
                   border_focus=[fg_color_alt, bg_color],
    ),
    layout.Max(),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="sans",
    fontsize=14,
    padding=1,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                # widget.CurrentLayout(),
                widget.CurrentScreen(),
                widget.GroupBox(),
                widget.Prompt(),
                widget.TaskList(),
                widget.Chord(
                    chords_colors={
                        "launch": (bg_color, fg_color),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                # widget.TextBox("default config", name="default"),
                # widget.TextBox("Press &lt;S-d&gt; to spawn", foreground="#d75f5f"),
                # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
                # widget.StatusNotifier(),
                widget.Notify(),
                widget.Systray(),
                widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
                # widget.QuickExit(),
            ],
            24,
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
        wallpaper="/home/klingenberg/Pictures/go-bg.jpg",
        wallpaper_mode="fill",
    ),
    Screen(
        top=bar.Bar(
            [
                # widget.CurrentLayout(),
                widget.CurrentScreen(),
                widget.GroupBox(),
                widget.Prompt(),
                widget.TaskList(),
                widget.Chord(
                    chords_colors={
                        "launch": (bg_color, fg_color),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                # widget.TextBox("default config", name="default"),
                # widget.TextBox("Press &lt;S-d&gt; to spawn", foreground="#d75f5f"),
                # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
                # widget.StatusNotifier(),
                widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
                # widget.QuickExit(),
            ],
            24,
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
        wallpaper="/home/klingenberg/Pictures/go-bg.jpg",
        wallpaper_mode="fill",
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

if aiomanhole:
    @hook.subscribe.startup_complete
    def set_manhole():
        aiomanhole.start_manhole(port=7113, namespace={"qtile": qtile})

@hook.subscribe.startup_once
def autostart():
    processes = [
        ['blueman-applet'],
        ['dunst'],
        ["kdeconnect-indicator"],
        ["nm-applet"],
        ["udiskie", "--tray"],
        ["pa-applet"],
        ["/usr/bin/polkit-dumb-agent"],
    ]
    # if lazy.core.name == "x11":
    #     processes.append(["setxkbmap", "de" "nodeadkeys"])

    for p in processes:
        subprocess.Popen(p)

# TODO kanshi
