;; -*-lisp-*-
;;
;; Here is a sample .stumpwmrc file

(in-package :stumpwm)

(load "~/.sbclrc")

(ignore-errors
 (ql:quickload :slynk)
 (slynk:create-server :dont-close t))

;; ;; fonts
(ql:quickload "clx-truetype")
(load-module "ttf-fonts")
(setf clx-truetype::+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
(xft:cache-fonts)

; (set-font (make-instance 'xft:font :family "Fira Code" :subfamily "Medium" :size 10))

(unless
   (ignore-errors (set-font (make-instance 'xft:font :family "Fira Code" :subfamily "Light" :size 10)))
 (set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 10)))

(ql:quickload "xembed")
(load-module "stumptray")

;; (require "maildir")

;; (load-module "clipboard-history")

;; (define-key *root-map* (kbd "C-y") "show-clipboard-history")
;; start the polling timer process
;; (clipboard-history:start-clipboard-manager)

;; change the prefix key to something else
;; (set-prefix-key (kbd "s-SPC"))
(set-prefix-key (kbd "s--"))

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

;; define my own commands
(defcommand start-emacs-daemon () ()
  (run-shell-command "systemctl --user start emacs"))

(defcommand run-emacs-client (&optional command wait-for-result?) (:rest)
  (run-shell-command
   (if command
       (format nil "emacsclient -nc -e \"(~a)\"" command)
       "emacsclient -nc")
   wait-for-result?))

(defcommand stop-emacs-daemon (&optional wait?) ()
  (run-emacs-client "save-buffers-kill-emacs" wait?))

(defcommand run-emacs (&optional command) (:rest)
  (run-shell-command
   (if command
       (format nil "emacs --eval \"(~a)\"" command)
       "emacs")))

(defcommand run-terminal (&optional command) (:rest)
  (run-shell-command
   (concatenate
    'string
    "alacritty "
    (if command
        (concatenate 'string " -e " command)
        ""))))

(defcommand shutdown () ()
  ;; (stop-emacs-daemon t)
  (run-terminal "shutdown now"))

(defcommand reboot () ()
  ;; (stop-emacs-daemon t)
  (run-terminal "reboot"))

(defun map-tablet-to-screen (&optional (screen "eDP-1"))
  (run-terminal (format nil "xinput --map-to-output $(xinput --list --id-only \"Wacom One by Wacom M Pen Pen (0)\") ~a" screen)))

(defcommand map-tablet-to-laptop-screen () ()
  (map-tablet-to-screen "eDP-1"))

(defcommand map-tablet-to-external-screen () ()
  (map-tablet-to-screen "HDMI-1"))

(defun emacs-is-current-window-p ()
  (and (current-window)
       (stumpwm::class-re-p (current-window) "Emacs")))

(defun direction-to-evil-key (direction)
  (cond
    ((equalp direction :right) "l")
    ((equalp direction :left) "h")
    ((equalp direction :up) "k")
    ((equalp direction :down) "j")))

(defun emacs-evil-window-move (direction)
  (meta (kbd (format nil "s-~a" (direction-to-evil-key direction)))))

(defun emacs-window-close ()
  (meta (kbd "s-c")))

(defun emacs-window-maximize ()
  (meta (kbd "s-m")))

(defun emacs-vsplit ()
  (meta (kbd "s-s")))

(defun emacs-hsplit ()
  (meta (kbd "s-v")))

(defcommand move-window-or-emacs-buffer (direction) ((:direction "Direction: "))
  (if (emacs-is-current-window-p)
      (emacs-evil-window-move direction)
      (move-focus direction)))

(defcommand close-window-or-emacs-buffer () ()
  (if (emacs-is-current-window-p)
      (emacs-window-close)
      (eval-command "delete")))

(defcommand maximize-window-and-emacs-window () ()
  (when (emacs-is-current-window-p)
    (emacs-window-maximize))
  ;; unless (only-one-frame-p)
    (eval-command "only"))

;; TODO maybe better to do a normal split and automatically open emacs with the same buffer?
(defcommand vsplit-maybe-emacs () ()
  (if (emacs-is-current-window-p)
      (emacs-vsplit)
      (eval-command "vsplit")))

(defcommand hsplit-maybe-emacs () ()
  (if (emacs-is-current-window-p)
      (emacs-hsplit)
      (eval-command "hsplit")))

(defcommand emacs-M-x () ()
  (if (emacs-is-current-window-p)
      (meta (kbd "M-x"))
      (run-emacs-client "execute-extended-command")))

(defcommand emacs-pass () ()
  (if (emacs-is-current-window-p)
      (meta (kbd "s-P"))
      (run-emacs-client "password-store-insert")))

(defcommand emacs-find-file () ()
  (if (emacs-is-current-window-p)
      (meta (kbd "s-f"))
      (run-emacs-client "find-file")))

(defcommand emacs-find-buffer () ()
  (if (emacs-is-current-window-p)
      (meta (kbd "s-b"))
      (run-emacs-client "consult-buffer")))

(defcommand emacs-terminal () ()
  (if (emacs-is-current-window-p)
      (meta (kbd "s-F1"))
      ;; (run-emacs-client "+vterm/here t")
      (run-emacs-client "+eshell/here")))

(defcommand emacs-everywhere () ()
  (run-shell-command "doom everywhere")
  ;; (run-emacs-client "emacs-everywhere")
  )

(defcommand my/pause () ()
  (when (current-window)
    (cond
      ((search "DAZN" (window-title (current-window))) (meta (kbd "SPC")))
      ((stumpwm::class-re-p (current-window) "Firefox") (meta (kbd "K"))))))

;; clean up a little
(undefine-key *root-map* (kbd "c"))
(undefine-key *root-map* (kbd "!"))
(undefine-key *root-map* (kbd "C-m"))
(undefine-key *root-map* (kbd "C-a"))
(undefine-key *root-map* (kbd "C-b"))
(undefine-key *root-map* (kbd "C-n"))
(undefine-key *root-map* (kbd "C-SPC"))
(undefine-key *root-map* (kbd "w"))
;; (undefine-key *root-map* (kbd ";"))
;; (undefine-key *root-map* (kbd ":"))
;; open stuff
;; (define-key *top-map* (kbd "s-v") "hsplit-maybe-emacs")
;; (define-key *top-map* (kbd "s-s") "vsplit-maybe-emacs")
(define-key *top-map* (kbd "s-v") "hsplit")
(define-key *top-map* (kbd "s-s") "vsplit")
(define-key *top-map* (kbd "s-V") "hsplit")
(define-key *top-map* (kbd "s-S") "vsplit")
(define-key *top-map* (kbd "s-f") "emacs-find-file")
(define-key *top-map* (kbd "s-b") "emacs-find-buffer")
(define-key *top-map* (kbd "s-a") "emacs-everywhere")
(define-key *top-map* (kbd "s-Right") "move-focus right")
(define-key *top-map* (kbd "s-Left") "move-focus left")
(define-key *top-map* (kbd "s-Up") "move-focus up")
(define-key *top-map* (kbd "s-Down") "move-focus down")
;; spacemacsy style
(define-key *root-map* (kbd "M") "lastmsg")
;; audio keys
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec pamixer --decrease 5")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec pamixer --increase 5")
(define-key *top-map* (kbd "XF86AudioMute") "exec pamixer --toggle-mute")
(define-key *top-map* (kbd "XF86AudioPause") "my/pause")
(define-key *top-map* (kbd "XF86AudioPlay") "my/pause")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "exec sudo light -U 5")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "exec sudo light -A 5")
;;misc
(define-key *root-map* (kbd ",") "colon")
(define-key *root-map* (kbd ".") "eval")
(define-key *root-map* (kbd "s-w") "banish")
(define-key *root-map* (kbd "s-r") "loadrc")
(define-key *root-map* (kbd "SPC") "emacs-M-x")
(define-key *root-map* (kbd "RET") "toggle-always-show")

(defvar *window-map*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "TAB") "other-window")
    (stumpwm:define-key m (stumpwm:kbd "w") "other-window")
    (stumpwm:define-key m (stumpwm:kbd "v") "hsplit")
    (stumpwm:define-key m (stumpwm:kbd "s") "vsplit")
    (stumpwm:define-key m (stumpwm:kbd "l") "move-focus right")
    (stumpwm:define-key m (stumpwm:kbd "h") "move-focus left")
    (stumpwm:define-key m (stumpwm:kbd "k") "move-focus up")
    (stumpwm:define-key m (stumpwm:kbd "j") "move-focus down")
    (stumpwm:define-key m (stumpwm:kbd "d") "remove-split")
    (stumpwm:define-key m (stumpwm:kbd "m") "only")
    m))
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "w") '*window-map*)

(defvar *system-map*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "S") "shutdown")
    (stumpwm:define-key m (stumpwm:kbd "R") "reboot")
    m))
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "S") '*system-map*)

;; Ssh somewhere
;; (define-key *root-map* (kbd "C-s") "colon1 exec termite -e ssh ")
;; Lock screen
;; (define-key *root-map* (kbd "C-l") "exec xlock")

(define-key *top-map* (kbd "s-l") "move-window-or-emacs-buffer right")
(define-key *top-map* (kbd "s-h") "move-window-or-emacs-buffer left")
(define-key *top-map* (kbd "s-k") "move-window-or-emacs-buffer up")
(define-key *top-map* (kbd "s-j") "move-window-or-emacs-buffer down")
(define-key *top-map* (kbd "s-L") "move-window right")
(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-M-l") "move-window right")
(define-key *top-map* (kbd "s-M-h") "move-window left")
(define-key *top-map* (kbd "s-M-k") "move-window up")
(define-key *top-map* (kbd "s-M-j") "move-window down")
(define-key *top-map* (kbd "H-l") "move-window right")
(define-key *top-map* (kbd "H-h") "move-window left")
(define-key *top-map* (kbd "H-k") "move-window up")
(define-key *top-map* (kbd "H-j") "move-window down")
(define-key *top-map* (kbd "s-c") "close-window-or-emacs-buffer")
(define-key *top-map* (kbd "s-C") "delete")
(define-key *top-map* (kbd "s-M-c") "remove-split")
(define-key *top-map* (kbd "H-c") "remove-split")
(define-key *top-map* (kbd "s-m") "maximize-window-and-emacs-window")
;; (define-key *top-map* (kbd "s-d") "colon1 exec ")
(define-key *top-map* (kbd "s-d") "exec rofi -show combi")
(define-key *top-map* (kbd "s-x") "emacs-M-x")
(define-key *top-map* (kbd "s-P") "emacs-pass")
(define-key *top-map* (kbd "H-p") "emacs-pass")
(define-key *top-map* (kbd "s-e") "run-emacs-client %s")
(define-key *top-map* (kbd "s-E") "exec emacs")
(define-key *top-map* (kbd "s-M-e") "exec emacs")
(define-key *top-map* (kbd "H-e") "exec emacs")
;;; reduce dependency on function row keys
(define-key *top-map* (kbd "s-F1") "emacs-terminal")
(define-key *top-map* (kbd "s-S-F1") "run-terminal")
(define-key *top-map* (kbd "s-M-F1") "run-terminal")
(define-key *top-map* (kbd "H-F1") "run-terminal")
(define-key *top-map* (kbd "s-RET") "run-terminal")
(define-key *top-map* (kbd "s-F2") "exec firefox")
(define-key *top-map* (kbd "s-S-F2") "exec firefox")
(define-key *top-map* (kbd "s-M-F2") "exec firefox")
(define-key *top-map* (kbd "H-F2") "exec firefox")
;; (define-key *top-map* (kbd "s-F2") "exec nyxt")
(define-key *top-map* (kbd "s-F3") "run-emacs-client deer")
(define-key *top-map* (kbd "s-S-F3") "exec spacefm")
(define-key *top-map* (kbd "s-M-F3") "exec spacefm")
(define-key *top-map* (kbd "H-F3") "exec spacefm")
(define-key *top-map* (kbd "s-F4") "run-emacs-client mu4e")

(defvar *program-map*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "i") "exec firefox")
    (stumpwm:define-key m (stumpwm:kbd "d") "exec spacefm")
    (stumpwm:define-key m (stumpwm:kbd "t") "run-terminal")
    (stumpwm:define-key m (stumpwm:kbd "m") "run-emacs-client mu4e")
    m))
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-C-SPC") '*program-map*)


(define-key *top-map* (kbd "s-n") "gnew")
(define-key *top-map* (kbd "s-w") "grouplist")

(loop for i from 1 upto 9
      do (progn
           (define-key *top-map* (kbd (format nil "s-~a" i)) (format nil "gselect ~a" i))
           (define-key *top-map* (kbd (format nil "H-~a" i)) (format nil "gmove ~a" i))
           (define-key *top-map* (kbd (format nil "s-M-~a" i)) (format nil "gmove ~a" i))))
(define-key *top-map* (kbd "s-!") "gmove 1")
(define-key *top-map* (kbd "s-\"") "gmove 2")
(define-key *top-map* (kbd "s-section") "gmove 3")
(define-key *top-map* (kbd "s-$") "gmove 4")
(define-key *top-map* (kbd "s-%") "gmove 5")
(define-key *top-map* (kbd "s-&") "gmove 6")
(define-key *top-map* (kbd "s-/") "gmove 7")
(define-key *top-map* (kbd "s-(") "gmove 8")
(define-key *top-map* (kbd "s-)") "gmove 9")

;; program keymap
;; (defvar *program-map*
;;   (let ((m (stumpwm:make-sparse-keymap)))
;;     (stumpwm:define-key m (stumpwm:kbd "i") "exec firefox")
;;     (stumpwm:define-key m (stumpwm:kbd "d") "exec spacefm")
;;     m ; NOTE: this is important
;;     ))
;; (stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-x") '*program-map*)


;; Web jump (works for Google and Imdb)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
    (substitute #\+ #\Space search)
    (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "duckduckgo" "firefox https://duckduckgo.com/?q=")
(define-key *root-map* (kbd "b") "duckduckgo")

;;; Define window placement policy...

;; Clear rules
(clear-window-placement-rules)

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.
;; TODO
;; (define-frame-preference "Default")
;; (define-frame-preference "Mail"
;;   (0 t nil :class "firefox")
;;   (1 t nil :class "emacsclient -nc -s instance1 -e \" (mu4e)\""))
;; (define-frame-preference "Admin"
;;   (0 t nil :class "pavcucontol"))

;; cursor
(setf *grab-pointer-foreground* (xlib:make-color :red 0.1 :green 0.25 :blue 0.5))
(setf *grab-pointer-background* (lookup-color (current-screen) "DeepSkyBlue"))
(setf *grab-pointer-character* 88)
(setf *grab-pointer-character-mask* 88)

;; I like messages to be centered on the screen.
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

;; I thought that this mode-line was fabulous!
(defvar *battery-status-command*
  "acpi -b | grep \"Battery 1:\" | cut -b 18-")

(defvar *vol-status-command*
  "pactl list sinks | grep Volume | head -n 1 | cut -b 30-34")

(setf *time-modeline-string* "%a %b %e %k:%M")

(setf *screen-mode-line-format*
      (list "[^B%n^b] %w^>"
            ;; "| Bat: "
            ;; '(:eval (run-shell-command *battery-status-command* t))
            ;; " | Vol. "
            ;; '(:eval (run-shell-command *vol-status-command* t))
            " | %d |"
            "             "))

(setf *window-format* "%m%n%s%c")

(setf *mode-line-timeout* 1)

(let ((bg-color "#222222")
      (fg-color "#FCE566")
      ;; (fg-color "#5AD4E6")
      )
 (set-bg-color bg-color)
 (set-fg-color fg-color)
 (set-border-color fg-color)
 (set-focus-color fg-color)
 (setf *mode-line-background-color* bg-color)
 (setf *mode-line-foreground-color* fg-color))

(setf *mouse-focus-policy* :click)

;; ;; custom functions
;; (defun start-tagesschau ()
;;   ;; TODO
;;   (move-focus :up)
;;   (move-focus :up)
;;   (move-focus :up)
;;   (move-focus :up)
;;   (move-focus :up)
;;   (move-focus :up)
;;   (move-focus :up)
;;   (run-shell-command "firefox https://live.daserste.de/")
;;   (run-shell-command "sleep 10")
;;   (run-shell-command "xdotool mousemove 956 611")
;;   (run-shell-command "sleep 0.5")
;;   (run-shell-command "xdotool click 1")
;;   (run-shell-command "sleep 0.5")
;;   (run-shell-command "xdotool mousemove 1633 936")
;;   (run-shell-command "sleep 0.5")
;;   (run-shell-command "xdotool click 1")
;;   (run-shell-command "sleep 0.5")
;;   (run-shell-command "xdotool mousemove 1919 977")
;;   (loop while t do
;;          (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
;;              (get-decoded-time)
;;              (if (and (= 20 hour) (< 15 minute 16))
;;               (stop-tagesschau)
;;               (run-shell-command "sleep 5")))))

;; (start-tagesschau)

;; (defun stop-tagesschau ()
;;   (delete))


;; autostart

(let ((autostart-command-list
        (list
         ;; "bash ~/.screenlayout/default.sh"
         "xmodmap -e 'clear mod4'"
         "xmodmap -e 'add mod4 = Super_L'"
         ;; "emacs --daemon=instance1"
         "pkill dropbox"
         "dropbox"
         "nm-applet"
         "udiskie --tray"
         "blueman-applet"
         "pa-applet"
         "/usr/bin/polkit-dumb-agent"
         "setxkbmap de -option ctrl:nocaps nodeadkeys"
         "xcape -e 'Control_L=Escape'"
         ;; "xcape -e 'Shift_L=Escape'"
         "feh --bg-scale ~/Pictures/arch-bg.jpg")))
  (dolist (cmd autostart-command-list)
    (run-shell-command cmd)))

;; TODO find out if this can be done more nicely
(defun init-groups ()
  (gnew "mail")                         ;2
  (gnew "system")                       ;3
  (gnew "programming")                  ;4
  (gnew "work")                         ;5
  (gnew "hacking")                      ;6
  (gnew "entertainment")                ;7
  (gnew "baduk")                        ;8
  (eval-command "gselect 1"))                      ;

(init-groups)

(refresh-heads)

;; turn on the mode line

(enable-mode-line (stumpwm:current-screen)
                  (stumpwm::head-by-number (stumpwm:current-screen) 0) t)

(stumptray::stumptray)
