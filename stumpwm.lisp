;; -*-lisp-*-
;;
;; Here is a sample .stumpwmrc file


(in-package :stumpwm)

(load "~/.sbclrc")

;; (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                        (user-homedir-pathname))))
;;   (when (probe-file quicklisp-init)
;;     (load quicklisp-init)))
;; (require "asdf")

;; (asdf:load-system :slynk)
;; (slynk:create-server :dont-close t)

;; (ql:quickload :clx-truetype)
;; (load-module "ttf-fonts")
;; (xft:cache-fonts)

;; (ql:quickload :xembed)
;; (load-module "stumptray")

;; (load-module "maildir")

;; (load-module "clipboard-history")

;; (define-key *root-map* (kbd "C-y") "show-clipboard-history")
;; start the polling timer process
;; (clipboard-history:start-clipboard-manager)

;; change the prefix key to something else
(set-prefix-key (kbd "s-SPC"))

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

;; define my own commands
(defun emacs-is-current-window-p ()
  (and (current-window) (search "Emacs" (window-title (current-window)))))

(defun emacs-evil-window-move (direction)
  (let ((cmd-string (concatenate 'string "emacsclient -s instance1 -e \"(evil-window-" (subseq (string-downcase (format nil "~s" direction)) 1) " 1)\"")))
    (run-shell-command cmd-string t)))

(defun emacs-window-close ()
  (let ((cmd-string "emacsclient -s instance1 -e \"(my/close-buffer)\""))
    (run-shell-command cmd-string t)))

(defcommand move-window-or-emacs-buffer (direction) ((:direction "Direction: "))
  (let ((try-emacs
          (when (emacs-is-current-window-p)
            (emacs-evil-window-move direction))))
    (when (or (not try-emacs) (= 1 (length try-emacs)))
      (move-focus direction))))

(defcommand close-window-or-emacs-buffer () ()
  (if (emacs-is-current-window-p)
      (emacs-window-close)
      (eval-command "delete")))

;; clean up a little
(undefine-key *root-map* (kbd "c"))
(undefine-key *root-map* (kbd "!"))
(undefine-key *root-map* (kbd "C-m"))
(undefine-key *root-map* (kbd "C-a"))
(undefine-key *root-map* (kbd "C-b"))
(undefine-key *root-map* (kbd "C-n"))
(undefine-key *root-map* (kbd "C-SPC"))
;; (undefine-key *root-map* (kbd ";"))
;; (undefine-key *root-map* (kbd ":"))
;; open stuff
(define-key *root-map* (kbd "d") "colon1 exec ")
(define-key *root-map* (kbd "t") "exec termite")
(define-key *root-map* (kbd "e") "exec emacsclient -nc -s instance1")
(define-key *root-map* (kbd "s-e") "exec emacs")
;; vertical split should be the default
(define-key *top-map* (kbd "s-v") "hsplit")
(define-key *top-map* (kbd "s-s") "vsplit")
(define-key *top-map* (kbd "s-V") "hsplit")
(define-key *top-map* (kbd "s-S") "vsplit")
(define-key *root-map* (kbd "v") "hsplit")
(define-key *root-map* (kbd "s") "vsplit")
;; spacemacsy style
(define-key *root-map* (kbd "m") "only")
(define-key *root-map* (kbd "M") "lastmsg")
;; audio keys
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec amixer set Master 5%-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec amixer set Master 5%+")
(define-key *top-map* (kbd "XF86AudioMute") "exec amixer set Master toggle")
;;misc
(define-key *root-map* (kbd ",") "colon")
(define-key *root-map* (kbd ".") "eval")
(define-key *root-map* (kbd "s-w") "banish")
(define-key *root-map* (kbd "s-r") "loadrc")

;; Ssh somewhere
;; (define-key *root-map* (kbd "C-s") "colon1 exec termite -e ssh ")
;; Lock screen
;; (define-key *root-map* (kbd "C-l") "exec xlock")

;; xmonad habits
(define-key *top-map* (kbd "s-l") "move-window-or-emacs-buffer right")
(define-key *top-map* (kbd "s-h") "move-window-or-emacs-buffer left")
(define-key *top-map* (kbd "s-k") "move-window-or-emacs-buffer up")
(define-key *top-map* (kbd "s-j") "move-window-or-emacs-buffer down")
(define-key *top-map* (kbd "s-L") "move-window right")
(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-c") "close-window-or-emacs-buffer")
(define-key *top-map* (kbd "s-m") "only")
(define-key *top-map* (kbd "s-F1") "exec termite")
(define-key *top-map* (kbd "s-F2") "exec firefox")
(define-key *top-map* (kbd "s-F3") "exec emacsclient -nc -s instance1 -e \"(deer)\"")

;; program keymap
(defvar *program-map*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "i") "exec firefox")
    (stumpwm:define-key m (stumpwm:kbd "d") "exec pcmanfm")
    m ; NOTE: this is important
    ))
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-x") '*program-map*)


;; Web jump (works for Google and Imdb)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
    (substitute #\+ #\Space search)
    (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "duckduckgo" "qutebrowser https://duckduckgo.com/?q=")
;; C-t M-s is a terrble binding, but you get the idea.
(define-key *root-map* (kbd "b") "duckduckgo")

;; Message window font
;; (set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")
;; (set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Oblique" :size 11))

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
;; (define-frame-preference "Default"
;;   ;; frame raise lock (lock AND raise == jumpto)
;;   (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
;;   (1 t nil :class "Termite"))

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
  "acpi -b | awk -F '[ ,]' '{printf \"%s%s\", $3, $5}' | sed s/Discharging/\-/ | sed s/Unknown// | sed s/Full// | sed s/Charging/+/")

(defvar *vol-status-command*
  "pulseaudio-ctl full-status | cut -b 1-3")

(setf *time-modeline-string* "%a %b %e %k:%M")

(setf *screen-mode-line-format*
      (list "[^B%n^b] %W^>"
            '(:eval (run-shell-command *battery-status-command* t))
            ;; " | Vol. "
            ;; '(:eval (run-shell-command *vol-status-command* t))
            "| %D | %d"))

(setf *window-format* "%m%n%s%c")

(setf *mode-line-timeout* 60)

;; turn on the mode line
(enable-mode-line (stumpwm:current-screen)
                          (stumpwm:current-head) t)


;; autostart

(let ((autostart-command-list
        (list
  "xmodmap -e 'clear mod4'"
  "xmodmap -e 'add mod4 = Super_L'"
  "emacs --daemon=instance1"
  "dropbox"
  "nm-applet"
  "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  "setxkbmap -option ctrl:nocaps"  
  "xcape -e 'Control_L=Escape'"
  "bash ~/.screenlayout/default.sh"
  )))
  (dolist (cmd autostart-command-list)
    (run-shell-command cmd)))
