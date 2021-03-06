;; -*-lisp-*-
;;
;; Here is a sample .stumpwmrc file

;; TODOs
;; - DONE(?) default set of groups at startup
;; - DONE(?) ensure that emacsclient cmds are sent to emacs instance of the active window
;; - DONE(?)shutdown cmds
;; - DONE(?)  enable modeline on one/both screens in a controlled way

(in-package :stumpwm)

;; (load "~/.sbclrc")

;; (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                        (user-homedir-pathname))))
;;   (when (probe-file quicklisp-init)
;;     (load quicklisp-init)))

; (sb-posix:putenv "SBCL_HOME=")

;; (let* ((guix-profile (pathname-as-directory (getenv "GUIX_PROFILE")))
;;        (module-dir (merge-pathnames "share/common-lisp/sbcl/" guix-profile)))
;;   (set-module-dir module-dir))

; (load-module "ttf-fonts")

(sb-posix:putenv "SBCL_HOME=/home/klingenberg/.guix-profile/lib/sbcl")
;; (sb-posix:putenv "SBCL_HOME=/run/current-system/profile/lib/sbcl/")
;; (require "asdf")
;; (load "/home/klingenberg/.guix-profile/share/emacs/site-lisp/")
;; (load "/run/current-system/profile/share/common-lisp/sbcl-bundle-systems/slynk.asd")
;; (load "/run/current-system/profile/share/common-lisp/sbcl-bundle-systems/stumpwm.asd")
;; (asdf:load-asd "/home/klingenberg/.emacs.d/.local/straight/build/sly/slynk/slynk.asd")
;; (require "slynk")
;; (asdf:load-system "slynk")
;; (slynk:create-server :dont-close t)

;; fonts
(require :ttf-fonts)
(setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
(setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
(xft:cache-fonts)
;; (set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 10))
(set-font (make-instance 'xft:font :family "Fira Mono" :subfamily "Regular" :size 10))

;; (require "xembed")
; (load-module "stumptray")
(require "stumptray")

;; (require "maildir")

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
(defcommand start-emacs-daemon () ()
  (eval-command "exec emacs --daemon=instance1")
  (eval-command "exec emacs --daemon"))

(defcommand run-emacs-client (&optional command) (:rest)
  (eval-command
   ;; (if command
   ;;     (format nil "exec emacsclient -nc -e \"(~a)\"" command)
   ;;   "exec emacsclient -nc")
   (if command
       (format nil "exec emacsclient -nc -s instance1 -e \"(~a)\"" command)
       "exec emacsclient -nc -s instance1")
   ))

(defcommand stop-emacs-daemon () ()
  (run-emacs-client "save-buffers-kill-emacs"))

(defcommand run-emacs (&optional command) (:rest)
  (eval-command
   (if command
       (format nil "exec emacs --eval \"(~a)\"" command)
       "exec emacs")))

(defcommand run-terminal (&optional command) (:rest)
  (eval-command
   (if command
       (format nil "exec termite -e \"~a\"" command)
       "exec termite")))

(defcommand shutdown () ()
  (stop-emacs-daemon)
  (run-terminal "sudo shutdown"))

(defcommand reboot () ()
  (stop-emacs-daemon)
  (run-terminal "sudo reboot"))

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

;; (defun emacs-evil-window-move (direction)
;;   (let ((cmd-string (concatenate 'string "emacsclient -s instance1 -e \"(evil-window-" (subseq (string-downcase (format nil "~s" direction)) 1) " 1)\"")))
;;     (run-shell-command cmd-string t)))

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
      (run-emacs-client "counsel-M-x nil")))

(defcommand emacs-pass () ()
  (if (emacs-is-current-window-p)
      (meta (kbd "s-P"))
      (run-emacs-client "ivy-pass")))

(defcommand emacs-find-file () ()
  (if (emacs-is-current-window-p)
      (meta (kbd "s-f"))
      (run-emacs-client "counsel-find-files nil")))

(defcommand emacs-find-buffer () ()
  (if (emacs-is-current-window-p)
      (meta (kbd "s-b"))
      (run-emacs-client "ivy-switch-buffer")))

(defcommand emacs-terminal () ()
  (if (emacs-is-current-window-p)
      (meta (kbd "s-F1"))
      ;; (run-emacs-client "+vterm/here t")
      (run-emacs-client "+eshell/here")))

(defcommand emacs-everywhere () ()
  (eval-command "exec emacsclient -e \"(emacs-everywhere)\"")
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
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec amixer set Master 5%-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec amixer set Master 5%+")
(define-key *top-map* (kbd "XF86AudioMute") "exec amixer set Master toggle")
(define-key *top-map* (kbd "XF86AudioPause") "my/pause")
(define-key *top-map* (kbd "XF86AudioPlay") "my/pause")
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
(define-key *top-map* (kbd "s-c") "close-window-or-emacs-buffer")
(define-key *top-map* (kbd "s-C") "delete")
(define-key *top-map* (kbd "s-C-c") "remove-split")
(define-key *top-map* (kbd "s-m") "maximize-window-and-emacs-window")
;; (define-key *top-map* (kbd "s-d") "colon1 exec ")
(define-key *top-map* (kbd "s-d") "exec rofi -show combi")
(define-key *top-map* (kbd "s-x") "emacs-M-x")
(define-key *top-map* (kbd "s-P") "emacs-pass")
(define-key *top-map* (kbd "s-e") "run-emacs-client %s")
(define-key *top-map* (kbd "s-E") "exec emacs")
(define-key *top-map* (kbd "s-F1") "emacs-terminal")
(define-key *top-map* (kbd "s-S-F1") "run-terminal")
(define-key *top-map* (kbd "s-F2") "exec firefox")
;; (define-key *top-map* (kbd "s-F2") "exec nyxt")
(define-key *top-map* (kbd "s-F3") "run-emacs-client deer")
(define-key *top-map* (kbd "s-S-F3") "exec pcmanfm")
(define-key *top-map* (kbd "s-F4") "run-emacs-client mu4e")

(define-key *top-map* (kbd "s-n") "gnew")
(define-key *top-map* (kbd "s-w") "grouplist")

(loop for i from 1 upto 9
      do (define-key *top-map* (kbd (format nil "s-~a" i)) (format nil "gselect ~a" i)))
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
;;     (stumpwm:define-key m (stumpwm:kbd "d") "exec pcmanfm")
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
  "acpi -b | awk -F '[ ,]' '{printf \"%s%s\", $3, $5}' | sed s/Discharging/\-/ | sed s/Unknown// | sed s/Full// | sed s/Charging/+/")

(defvar *vol-status-command*
  "pactl list sinks | grep Volume | head -n 1 | cut -b 30-34")

(setf *time-modeline-string* "%a %b %e %k:%M")

(setf *screen-mode-line-format*
      (list "[^B%n^b] %w^>"
            ;; '(:eval (run-shell-command *battery-status-command* t))
            ;; " | Vol. "
            ;; '(:eval (run-shell-command *vol-status-command* t))
            " | %d |"
            "         "))

(setf *window-format* "%m%n%s%c")

(setf *mode-line-timeout* 1)

(setf *mouse-focus-policy* :click)

;; autostart

(let ((autostart-command-list
        (list
  "xmodmap -e 'clear mod4'"
  "xmodmap -e 'add mod4 = Super_L'"
  "emacs --daemon=instance1"
  "pkill dropbox"
  "dropbox"
  "nm-applet"
  "blueman-applet"
  "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  "setxkbmap de -option ctrl:nocaps nodeadkeys"
  "xcape -e 'Control_L=Escape'"
  "fix_touchscreen"
  "bash ~/.screenlayout/default.sh")))
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
;; TODO check if numbers persist across X-restarts/reboots -> if so, wrap the following in a function -> seems that way
(enable-mode-line (stumpwm:current-screen)
                  (stumpwm::head-by-number (stumpwm:current-screen) 0) t)
;; (toggle-mode-line (stumpwm:current-screen)
;;                   (stumpwm::head-by-number (stumpwm:current-screen) 0) t)

(stumptray::stumptray)
