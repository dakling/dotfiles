;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Dario Klingenberg"
      user-mail-address "dario.klingenberg@web.de")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-solarized-dark)
(setq doom-theme 'doom-city-lights)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;
;; Beginning of my configuration

(defun system-name= (&rest names)
  (cl-some
   (lambda (name)
     (string-equal name (system-name)))
   names))

(unless (system-name= "lina")
    (progn
      (use-package! exwm
        :init
        (server-start)
        :config
        ;; Add workspace to modeline
        (add-to-list 'global-mode-string
                     '(:eval (format (concat "<%s> "
                                             (unless (null (my/exwm-get-other-workspace)) "[%s] "))
                                     exwm-workspace-current-index
                                     (my/exwm-get-other-workspace))))
        (defun +exwm/rename-buffer-to-title-h ()
          "Make sure that the exwm buffers name convays its content."
          (exwm-workspace-rename-buffer
           (format "%s - %s" exwm-class-name exwm-title)))
        (defun +exwm/update-class-h ()
          (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name)
                      (string= "Firefox" exwm-class-name))
            (exwm-workspace-rename-buffer exwm-class-name)))
        (defun +exwm/update-title-h ()
          (cond ((or (not exwm-instance-name)
                     (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                     (string= "gimp" exwm-instance-name)
                     (string= "Firefox" exwm-class-name))
                 (exwm-workspace-rename-buffer exwm-title))))
        (evil-set-initial-state 'exwm-mode 'emacs)
        (add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)
        (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
        (add-hook 'exwm-update-title-hook #'+exwm/rename-buffer-to-title-h)
        (add-hook 'exwm-update-class-hook #'+exwm/update-class-h)
        (add-hook 'exwm-update-title-hook #'+exwm/update-title-h)
        (add-hook 'exwm-mode #'doom-mark-buffer-as-real-h)
        (setq mouse-autoselect-window nil
              focus-follows-mouse nil)
        (exwm-enable))

      (use-package! exwm-input
        ;; :after-call exwm-randr
        :config
        (define-key exwm-mode-map (kbd "C-c") nil)
        (setq exwm-input-global-keys
              `(([?\s-r] . exwm-reset)
                ([?\s-e] . exwm-input-release-keyboard)
                ([?\s-F] . exwm-layout-set-fullscreen)
                ([?\s-a] . exwm-workspace-switch)
                ([?\s-A] . exwm-workspace-move-window)
                (\,@(mapcar (lambda (i)
                              `(,(kbd (format "s-%d" i)) .
                                (lambda () (interactive)
                                  (exwm-workspace-switch-create ,i))))
                            (number-sequence 0 9)))
                (\,@(mapcar (lambda (i)
                              `(,(kbd (format "M-%d" i)) .
                                (lambda () (interactive)
                                  (exwm-workspace-switch-create ,i))))
                            (number-sequence 0 9)))
                ;; ,@(mapcar (lambda (i)
                ;; 	      `(,(kbd (format "s-%s" i)) .
                ;; 		(lambda () (interactive)
                ;; 		  (exwm-workspace-move-window ,i))))
                ;; 	    (list '! \" § $ % & / ( ) =))
                ;; (number-sequence 0 9))
                ([?\s-o] . my/exwm-switch-to-other-workspace)
                ([?\s-O] . my/exwm-move-window-to-other-workspace)
                ([?\s-w] . other-window)
                ([?\s-d] . dmenu)
                ([?\s-x] . helm-M-x)
                ([?\s-f] . helm-find-files)
                ([?\s-p] . helm-projectile)
                ([?\s-b] . helm-buffers-list)
                ([?\s-l] . evil-window-right)
                ([?\s-h] . evil-window-left)
                ([?\s-j] . evil-window-down)
                ([?\s-k] . evil-window-up)
                ([?\s-v] . split-window-right)
                ([?\s-s] . split-window-below)
                ([?\s-c] . my/close-buffer)
                ([?\s-q] . my/get-rid-of-mouse)
                ([?\s-m] . delete-other-windows)
                ([s-f1] . (lambda () (interactive) (eshell 'N)))
                ([C-s-f1] . eshell)
                ([s-f2] . (lambda () (interactive) (start-process "" nil browser)))
                ([s-f3] . deer)
                ([s-f4] . (lambda () (interactive) (mu4e)))
                ([s-f12] . (lambda () (interactive) (start-process "" nil "/usr/bin/slock")))))
        (push ?\s-\  exwm-input-prefix-keys)
        ;; (push ?\M-m  exwm-input-prefix-keys)
        (exwm-input-set-key (kbd "<XF86MonBrightnessUp>")
                            #'my/brightness+)
        (exwm-input-set-key (kbd "<XF86MonBrightnessDown>")
                            #'my/brightness-)
        (exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                            'pulseaudio-control-decrease-volume)
        (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                            'pulseaudio-control-increase-volume)
        (exwm-input-set-key (kbd "<XF86AudioMute>")
                            'pulseaudio-control-toggle-current-sink-mute))

      (use-package! exwm-systemtray
        ;; :after-call exwm-mode-hook
        :config (exwm-systemtray-enable))

      (use-package! exwm-randr
        ;; :after-call exwm-mode-hook
        :init
        (cond
         ((system-name= "klingenberg-tablet") (progn (set 'monitor1 "eDP1")
                                                     (set 'monitor2 "HDMI2")
                                                     (set 'placement "below")))
         ((system-name= "klingenbergLaptop") (progn (set 'monitor1 "LVDS1")
                                                    (set 'monitor2 "VGA1")
                                                    (set 'placement "below")))
         ((system-name= "klingenberg-laptop") (progn (set 'monitor1 "LVDS1")
                                                     (set 'monitor2 "VGA1")
                                                     (set 'placement "below")))
         (t (progn (set 'monitor2 "VGA-1")
                   (set 'monitor1 "HDMI-1")
                   (set 'placement "left-of"))))
        (setq exwm-randr-workspace-monitor-plist (list 0 monitor1
                                                       2 monitor1
                                                       4 monitor1
                                                       6 monitor1
                                                       8 monitor1
                                                       1 monitor2
                                                       3 monitor2
                                                       5 monitor2
                                                       7 monitor2
                                                       9 monitor2))
        :config
        (defun my/exwm-get-other-workspace ()
          (cond ((not (= 2 (length (seq-filter #'identity (mapcar #'exwm-workspace--active-p exwm-workspace--list))))) nil) ;currently only works for two monitors
                ((= exwm-workspace-current-index
                    (cl-position t (mapcar #'exwm-workspace--active-p exwm-workspace--list) :from-end t))
                 (cl-position t (mapcar #'exwm-workspace--active-p exwm-workspace--list) :from-end nil))
                ((= exwm-workspace-current-index
                    (cl-position t (mapcar #'exwm-workspace--active-p exwm-workspace--list) :from-end nil))
                 (cl-position t (mapcar #'exwm-workspace--active-p exwm-workspace--list) :from-end t))))
        (defun my/exwm-switch-to-other-workspace () (interactive)
               (exwm-workspace-switch (my/exwm-get-other-workspace)))
        (defun my/exwm-move-window-to-other-workspace () (interactive)
               (exwm-workspace-move-window (my/exwm-get-other-workspace)))


        (defun my/exwm-xrandr ()
          "Configure screen with xrandr."
          (shell-command
           (if (file-exists-p "~/.screenlayout/default.sh")
               "~/.screenlayout/default.sh" ; prefer saved command by arandr by default
             (concat "xrandr --output "
                     monitor1
                     " --primary --auto --"
                     placement
                     " "
                     monitor2
                     " --auto")))
          (my/fix-touchscreen))

        (add-hook 'exwm-randr-screen-change-hook #'my/exwm-xrandr)
        (progn
          (exwm-randr-enable)))

      (use-package! exwm-workspace
        ;; :after-call exwm-mode-hook
        :init
        (progn
          (setq exwm-workspace-number 10)
          (setq exwm-workspace-show-all-buffers t)
          (setq exwm-layout-show-all-buffers t)))))

;;; Setting some variables
(setq evil-collection-setup-minibuffer t)

(setq display-time-24hr-format t
      display-time-default-load-average nil)
(display-time-mode 1)

(setq initial-major-mode 'lisp-interaction-mode)

(cond
 ((system-name= "klingenberg-pi")
  (add-load-path! "/run/current-system/sw/share/emacs/site-lisp/mu4e"))
 ((system-name= "klingenberg-tablet")
  (add-load-path! "/run/current-system/profile/share/emacs/site-lisp/")))

;; (when (system-name= "klingenberg-tablet")
;;   (add-to-list 'exec-path "/home/klingenberg/.nix-profile/bin")
;;   (setenv "PATH"
;;           (concat (getenv "HOME") "/.nix-profile/bin:" (getenv "PATH"))))

;; TODO plover avtivation/deactivation can probably be done in a more elegant way
(defun my/plover-activate ()
  (interactive)
  (start-process "plover" nil "plover" "-g" "none"))

(defun my/plover-deactivate ()
  (interactive)
  (shell-command "killall plover"))

(defun my/auto-plover-on ()
  (interactive)
  (add-hook 'evil-insert-state-entry-hook #'my/plover-activate)
  (add-hook 'evil-insert-state-exit-hook #'my/plover-deactivate))

(defun my/auto-plover-off ()
  (interactive)
  (remove-hook 'evil-insert-state-entry-hook #'my/plover-activate)
  (remove-hook 'evil-insert-state-exit-hook #'my/plover-deactivate))

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)


(after! sly
  (setq inferior-lisp-program (cond
                               ((system-name= "klingenberg-tablet")  "~/.local/bin/.run-sbcl.sh")
                               (t "/usr/bin/sbcl --load /home/klingenberg/quicklisp.lisp"))))

(setq browse-url-browser-function 'browse-url-firefox)

(customize-set-variable 'compilation-scroll-output t)

(setq helm-swoop-pre-input-function (lambda () (car evil-ex-search-pattern)))

(after! evil-snipe (evil-snipe-mode -1))

(customize-set-variable 'avy-all-windows t)

(customize-set-variable 'avy-single-candidate-jump t)

(after! org
 (setq org-file-apps
       (remove (assoc "\\.pdf\\'" org-file-apps)
               org-file-apps)))

(map!
   :localleader
   :after org
   :map org-mode-map
   :n "x" (lambda () (interactive)
             (let ((current-prefix-arg '-)) ; simulate pressing C-u
               (call-interactively 'org-export-dispatch))))

(setq org-roam-capture-templates
      '(("d" "default" plain #'org-roam-capture--get-point "%? \n %i \n %a"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t)))

(setq smerge-command-prefix "+")

;; don't pop up async shell commnand buffers by default
(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(after! format-all-mode
  ;; (remhash 'c++-mode format-all--mode-table)
  (setq +format-on-save-enabled-modes 'omnisharp-mode))
;;; Defining some useful functions
(defun shutdown ()
  (interactive)
  (cond
   ((system-name= "klingenberg-laptop" "klingenberg-tablet") (async-shell-command "sudo shutdown"))
   (t (shell-command "shutdown now"))))

(defun reboot ()
  (interactive)
  (async-shell-command "sudo reboot"))

(defvar browser
  (cond
   ;; ((system-name= "klingenberg-tablet") "next")
   ((system-name= "klingenberg-laptop") "epiphany")
   ;; ((system-name= "klingenberg-tablet") "nyxt")
   (t "firefox")))

(defun my/brightness+ ()
  (interactive)
  (shell-command "xbacklight -inc 10"))

(defun my/brightness- ()
  (interactive)
  (shell-command "xbacklight -dec 10"))

(defun my/fix-touchscreen ()
  (when (system-name= "klingenberg-tablet")
    (shell-command "xinput --map-to-output $(xinput list --id-only \"ELAN Touchscreen\") eDP1")
    ;; (shell-command "xinput --map-to-output $(xinput list --id-only \"HDX HDX DIGITIZER Pen (0)\") eDP1")
    ))

(defun my/eww-open-league-table ()
    "Do an internet search for soccer league table."
    (interactive)
    (let* ((country-search-string-table
            '(("germany" "bundesliga tabelle")
              ("spain" "la liga tabelle")
              ("italy" "seria a tabelle")
              ("france" "ligue 1 tabelle")
              ("england" "premier league tabelle")))
           (country (completing-read "which country? " (mapcar #'car country-search-string-table))))
      (eww (cadr (assoc country country-search-string-table)))))

(use-package! async-await)
;; adapted from snippet by oremacs
(defun my/youtube-dl ()
  (interactive)
  (let* ((url (or (plist-get eww-data :url)
                  (current-kill 0)))
         (str (replace-regexp-in-string
               "-"
               "-"
               (replace-regexp-in-string
                "\\&list.*"
                ""
                (replace-regexp-in-string
                 "https://www.invidio.us/watch\\?v="
                 ""
                 (replace-regexp-in-string
                  "https://www.youtube.com/watch\\?v="
                  ""
                  url)))))
         (download-dir "~/Videos/"))
    (message "Downloading video, will open as soon as download is complete...")
    (set-process-sentinel
     (start-process-shell-command
      "youtube-download"
      "*youtube-download*"
      "youtube-dl"
      (concat "-o " download-dir "%\\(title\\)s%\\(id\\)s")
      (concat "\"" str "\""))
     (lambda (_ _)
       (helm-open-file-with-default-tool (car (directory-files
                                               "~/Videos/"
                                               ;; download-dir
                                               t str)))))))

(defun fdy-mount (source target)
  "Mount a directory from fdy windows remote server."
  (async-shell-command (concat
                        "sudo /usr/bin/mount //dc1/"
                        source
                        " "
                        target
                        " -t cifs -o username=klingenberg,noexec,uid=klingenberg")))
(defun my/close-buffer ()
  (interactive)
  (unless (equalp (buffer-name) "*scratch*")
    (kill-this-buffer))
  (when (< 1 (length (window-list)))
    (evil-window-delete)))

(defun qmount (location)
  "Shortcuts for mounting frequent locations,"
  (interactive)
  (apply #'fdy-mount
         (cond ((string= location "lectures") '("misc/fdy-lectures.git" "~/git/mnt/fdy-lectures.git"))
               ((string= location "klausuren") '("lehre/TM1/Klausuren.git" "~/git/mnt/Klausuren.git"))
               ((string= location "bosss") '("bosss/users/klingenberg/root.git" "~/git/mnt/bosss.git"))
               ((string= location "publications") '("misc/fdy-publications.git" "~/git/mnt/fdy-publications.git"))
               ((string= location "misc") '("misc" "~/misc"))
               ((string= location "scratch") '("scratch" "~/scratch"))
               ((string= location "backup") '("backup" "~/backup"))
               ((string= location "lehre") '("lehre" "~/lehre")))))

(defun my/create-super-bindings ()
  "Create bindings starting with super for use outside exwm."
  (map!
   :n
   ;; :states '(insert emacs hybrid normal visual motion operator replace)
   "s-w" '(other-window :which-key "other window")
   "s-d" 'dmenu
   "s-x" 'helm-M-x
   "s-f" 'helm-find-files
   "s-p" 'helm-projectile
   "s-b" 'helm-mini
   "s-l" 'evil-window-right
   "s-h" 'evil-window-left
   "s-j" 'evil-window-down
   "s-k" 'evil-window-up
   "s-L" 'enlarge-window-horizontally
   "s-H" 'shrink-window-horizontally
   "s-J" 'enlarge-window
   "s-K" 'shrink-window
   "s-v" 'split-window-right
   "s-s" 'split-window-below
   "s-c" 'my/close-buffer
   "s-q" 'my/get-rid-of-mouse
   "s-m" 'delete-other-windows
   "s-n" 'my/plover-activate
   "s-N" 'my/plover-deactivate
   "s-<f1>" '(lambda () (interactive) (eshell 'N))
   "C-s-<f1>" 'eshell
   "s-<f2>" '(lambda () (interactive)
               (start-process "" nil browser))
   "s-<f3>" 'deer
   "s-<f4>" '(lambda () (interactive)
               (mu4e))
   "s-<f12>" '(lambda () (interactive)
                (start-process "" nil "/usr/bin/slock"))))

(my/create-super-bindings)

;; (use-package! mu4e-conversation)

;; (global-mu4e-conversation-mode)

;; disable mu4e-org
;; (remove-hook 'message-send-hook #'doom--setq-org-mu4e-convert-to-html-for-message-send-h)
;; (remove-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)
;; (setq org-mu4e-convert-to-html nil)

(defun my/mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my/mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my/mu4e-account-alist)
                             nil t nil nil (caar my/mu4e-account-alist))))
         (account-vars (cdr (assoc account my/mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

;; ask for account when composing mail
(add-hook 'mu4e-compose-pre-hook 'my/mu4e-set-account)

(setq my/mu4e-account-alist
      '(("fdy"
         (mu4e-sent-messages-behavior sent)
         (mu4e-compose-signature-auto-include t)
         (mu4e-compose-signature
          "Technische Universität Darmstadt
Dario Klingenberg, M.Sc.
Fachgebiet für Strömungsdynamik
Fachbereich Maschinenbau
Fachgebiet für Strömungsdynamik (FDY)
Otto-Berndt-Straße 2 (L1|01 322)
64287 Darmstadt

E-Mail: klingenberg@fdy.tu-darmstadt.de
Telefon: +49 6151 16-26207
Fax: +49 6151 16-26203
Web: http://www.fdy.tu-darmstadt.de")
         (mu4e-sent-folder "/fdy/Sent Items")
         (mu4e-drafts-folder "/fdy/Drafts")
         (smtpmail-smtp-server "smtp.tu-darmstadt.de")
         (smtpmail-smtp-service 465)
         (smtpmail-stream-type ssl)
         (user-mail-address "klingenberg@fdy.tu-darmstadt.de")
         (user-full-name "Dario Klingenberg"))
        ("gsc"
         (mu4e-sent-messages-behavior sent)
         (mu4e-compose-signature-auto-include t)
         (mu4e-compose-signature
          "Technische Universität Darmstadt
Dario Klingenberg, M.Sc.
Graduate School Computational Engineering
Dolivostraße 15
64293 Darmstadt

E-Mail: klingenberg@gsc.tu-darmstadt.de
Telefon: +49 6151 16-24381
Fax: +49 6151 16-24404
Web: http://www.gsc.ce.tu-darmstadt.de/")
         (mu4e-sent-folder "/gsc/Sent Items")
         (mu4e-drafts-folder "/gsc/Drafts")
         (smtpmail-smtp-server "smtp.tu-darmstadt.de")
         (smtpmail-smtp-service 465)
         (smtpmail-stream-type ssl)
         (user-mail-address "klingenberg@gsc.tu-darmstadt.de")
         (user-full-name "Dario Klingenberg"))
        ("gmail"
         ;; Under each account, set the account-specific variables you want.
         (mu4e-sent-messages-behavior delete)
         (mu4e-compose-signature-auto-include nil)
         (mu4e-sent-folder "/gmail/sent")
         (mu4e-drafts-folder "/gmail/Drafts")
         (user-mail-address "dario.klingenberg@gmail.com")
         (smtpmail-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-service 465)
         (smtpmail-stream-type ssl)
         (user-full-name "Dario Klingenberg"))
        ("web"
         (mu4e-sent-messages-behavior sent)
         (mu4e-compose-signature-auto-include nil)
         (mu4e-sent-folder "/web/Sent Items")
         (mu4e-drafts-folder "/web/Drafts")
         (smtpmail-smtp-server "smtp.web.de")
         (smtpmail-smtp-service 587)
         (smtpmail-stream-type starttls)
         (user-mail-address "dario.klingenberg@web.de")
         (user-full-name "dario"))))

;; taken from reddit
(use-package! mu4e-alert
  :after-call mu4e-index-updated-hook
  :config
  (setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND NOT maildir:/Web/INBOX/")
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-set-default-style 'libnotify)
  (alert-add-rule
   :category "mu4e-alert"
   :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode)))
   :continue t))

(after! mu4e
  (setq mu4e-update-interval 120)
  (setq mu4e-compose-signature-auto-include t)
  (setq mu4e-enable-notifications t)
  (customize-set-variable 'mu4e-headers-leave-behavior 'apply)
  (setq mu4e-view-use-gnus t)
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  ;; (progn
  ;;   ;; For org-mu4e-compose-mode
  ;;   (add-hook 'org-mode-hook
  ;;             #'evil-collection-mu4e-org-set-header-to-normal-mode)
  ;;   (add-hook 'mu4e-compose-pre-hook
  ;;             #'evil-collection-mu4e-org-set-header-to-insert-mode))
  )

;; keybindings
;;
;; alternative leader for exwm
(setq doom-leader-alt-key "s-SPC")

(setq doom-localleader-key "-")

(map! "C-g" #'keyboard-quit)

(map! :leader
      "SPC" #'execute-extended-command
      "++" #'+popup/toggle
      "+ RET" #'+popup/other
      "lm" #'bookmark-set
      "ll" #'bookmark-jump
      "er" #'eval-expression
      "ss" #'helm-swoop
      "SS" #'shutdown
      "SR" #'reboot
      "w TAB" #'evil-switch-to-windows-last-buffer)

(map!
 :n "gb" #'pop-tag-mark
 :n "s" #'avy-goto-char-timer
 :n "S" #'avy-goto-char-timer)

(map! :leader :map (elisp)
      "ef" #'eval-defun
      "ep" #'eval-print-last-sexp)

(after!
  lispy
  (lispy-set-key-theme '(lispy c-digits))) ;; disable single-key bindings

(after!
  lispyville
  (setq lispyville-key-theme '(operators
                               c-w
                               additional
                               prettify
                               additional-insert
                               (escape insert)
                               slurp/barf-cp))
  (map!
   :map lispyville-mode-map
   :ni "M-h" #'lispy-left
   :ni "M-l" #'lispyville-next-closing
   :n "M-j" #'lispy-down
   :n "M-k" #'lispy-up
   :ni "M-J" #'lispyville-drag-forward
   :ni "M-K" #'lispyville-drag-backward
   :ni "M-H" #'lispyville-<
   :ni "M-L" #'lispyville->
   :ni "C-M-h" #'lispy-move-left
   :ni "C-M-l" #'lispy-move-right
   :ni "M-r" #'lispy-raise-sexp
   :ni "M-d" #'lispyville-wrap-round
   :ni "C-<return>" #'lispy-split
   :n "gc" #'lispyville-comment-or-uncomment)

  (map!
   :localleader
   :map (emacs-lisp-mode-map lisp-mode-map scheme-mode-map)
   "el" #'lispy-eval
   "1" #'lispy-describe-inline
   "2" #'lispy-arglist-inline
   "x" #'lispy-x))

;; scheme
(use-package! geiser
  :commands (run-geiser)
  :config
  ;; (with-eval-after-load 'geiser-guile
  ;;   (add-to-list 'geiser-guile-load-path "~/guix"))
  (with-eval-after-load 'yasnippet
    (add-to-list 'yas-snippet-dirs "~/guix/etc/snippets"))
  (setq flycheck-scheme-chicken-executable "chicken-csc")
  (setq geiser-chicken-binary "chicken-csi")
  (setq geiser-active-implementations '(guile chicken))
  (map!
   :localleader
   :map scheme-mode-map
   :n "'" #'geiser
   :n "ef" #'geiser-eval-definition
   :n "ee" #'geiser-eval-last-sexp
   :n "eb" #'geiser-eval-buffer))

;; latex
(setq +latex-viewers '(pdf-tools))
(setq reftex-default-bibliography "~/Documents/programming/latex_macros/bibliography.bib")
(map!
 :localleader
 :map (TeX-mode-map LaTeX-mode-map)
 "-" #'TeX-recenter-output-buffer
 "." #'LaTeX-mark-environment
 "*" #'LaTeX-mark-section
 "a" #'TeX-command-run-all
 "b" #'TeX-command-master
 "e" #'TeX-next-error
 "k" #'TeX-kill-job
 "m" #'TeX-insert-macro
 "v" #'TeX-view
 "hd" #'TeX-doc
 "xb" #'latex/font-bold
 "xc" #'latex/font-code
 "xe" #'latex/font-emphasis
 "xi" #'latex/font-italic
 "xr" #'latex/font-clear
 "xo" #'latex/font-oblique
 "xfc" #'latex/font-small-caps
 "xff" #'latex/font-sans-serif
 "xfr" #'latex/font-serif
 "rt" #'reftex-toc
 "rr" #'reftex-cleveref-cref
 "rc" #'reftex-citation
 "ol" (lambda () (interactive) (find-file "definLocal.tex"))
 "og" (lambda () (interactive) (find-file (getenv "LatexGlobalConfig")))
 "ob" (lambda () (interactive) (find-file "bibliography.bib")))

(use-package! evil-tex
  :after-call LaTeX-mode-hook
  :config
  (add-hook 'LaTeX-mode-hook #'evil-tex-mode))

;; f#
(map! :localleader
      :after fsharp-mode
      :map fsharp-mode-map
      :n "'" #'run-fsharp
      :n "e" nil
      :n "ef" #'fsharp-eval-phrase
      :n "er" #'fsharp-eval-region
      :n "ce" #'compile
      :n "cr" #'recompile
      :n "cc" #'recompile)

;;c#

(cond
 ((system-name= "klingenberg-pi")
  (setq omnisharp-server-executable-path "/run/current-system/sw/bin/omnisharp"))
 ((system-name= "klingenberg-tablet")
  (setq omnisharp-server-executable-path "~/.nix-profile/bin/omnisharp")))

(defun my/csharp-list-to-array ()
  (replace-regexp "List<\\(.*\\)>" "\\1[]"
                  nil
                  (line-beginning-position)
                  (line-end-position)))

(defun my/csharp-array-to-list ()
  (replace-regexp "\\([A-z]*\\)\\[\\]" "List<\\1>"
                  nil
                  (line-beginning-position)
                  (line-end-position)))


(defun my/csharp-toggle-list-and-array ()
  (interactive)
  (let ((min (line-beginning-position))
        (max (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (cond ((re-search-forward "List<\\(.*\\)>" max t)
             (my/csharp-list-to-array))
            ((re-search-forward "\\(.*\\)\\[\\]" max t)
             (my/csharp-array-to-list))
            (t (message "neither array nor string found on current line"))))))

(use-package! csharp-repl
  :unless (system-name= "klingenberg-pi")
  :load-path "~/Documents/programming/elisp/emacs-csharp-repl/")

(defun my/personal-bosss-file-p ()
  (and (buffer-file-name)
       (file-in-directory-p (buffer-file-name) "~/BoSSS-experimental/internal/src/private-kli/")))

(defun my/bosss-file-p ()
  (or
   (and (buffer-file-name)
        (file-in-directory-p (buffer-file-name) "~/BoSSS/"))
   (my/personal-bosss-file-p)))

(defun my/add-header ()
  (interactive)
  (let ((header-text
         (concat
          "/* =======================================================================
Copyright " (format-time-string "%Y") " Technische Universitaet Darmstadt, Fachgebiet fuer Stroemungsdynamik (chair of fluid dynamics)

Licensed under the Apache License, Version 2.0 (the \"License\");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an \"AS IS\" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

")))
    (save-excursion
      (goto-line 0)
      (when (my/personal-bosss-file-p)
        (unless (or (search-forward (substring header-text 93) nil t) ; check if header already exists, start a bit later to ignore year
                    (derived-mode-p #'bosss-mode)) ; check if this is just a worksheet
          (princ header-text (current-buffer)))))))

(defun my/format-on-save-enable ()
  "Code-format the entire buffen on save."
  (interactive)
  (if (my/personal-bosss-file-p)
      ;; (format-all-mode 1)               ; not really necessary
      (add-hook 'before-save-hook #'omnisharp-code-format-entire-file)
    (format-all-mode -1)))

(defun my/format-on-save-disable ()
  "Disable Code-formatting of the entire buffen on save."
  (interactive)
  (remove-hook 'before-save-hook #'omnisharp-code-format-entire-file)
  (format-all-mode -1))

(defun my/indent-buffer-without-bosss-header ()
  "Indent file, but ignore header"
  (interactive)
  (save-excursion
    (goto-line 16)
    (let ((beg (point)))
      (evil-indent beg (point-max)))))

(defun my/add-file-to-project (file project)
  "Add FILE to PROJECT."
  (find-file project)
  (goto-line 0)
  (search-forward "<Compile Include=")
  (evil-open-above 1)
  (princ (concat "<Compile Include=\""
                 (file-relative-name file
                                     (file-name-directory project))
                 "\"/>")
         (current-buffer))
  (save-buffer)
  (magit-stage-file file)
  (kill-buffer (current-buffer)))

(defun my/add-current-file-to-project ()
  "Add current file to my project."
  (interactive)
  (my/add-file-to-project (buffer-file-name) (my/csharp-find-current-project)))

(defun my/remove-file-from-project (file project)
  "Remove FILE from PROJECT."
  (find-file project)
  (goto-line 0)
  (search-forward "<Compile Include=")
  (search-forward (file-name-nondirectory file))
  (kill-whole-line)
  (save-buffer)
  (delete-file file)
  (magit-stage-file file)
  (kill-buffer (current-buffer)))

(defun my/remove-current-file-from-project ()
  "Remove current file to my project."
  (interactive)
  (my/remove-file-from-project (buffer-file-name) (my/csharp-find-current-project)))

(defun my/run-bosss-control-file (solver control-file &optional debug)
  "Run SOLVER with CONTROL-FILE, optionally using sbd to DEBUB"
  (async-shell-command
   (if debug
       (concat "sdb \"args -c " control-file "\" \"run " solver "\"")
     (concat "mono " solver " -c " control-file))))

(defun my/run-tests (path-to-assembly)
  "Implement tests manually as default functions do not work"
  (interactive)
  (async-shell-command (concat "nunit3-console " path-to-assembly)))

(add-hook 'csharp-mode-hook (lambda ()
                              (push '(?< . ("< " . " >")) evil-surround-pairs-alist)))
(add-hook 'csharp-mode-hook #'my/add-header)
(add-hook 'csharp-mode-hook #'my/format-on-save-enable)

(setq bosss-master-solution "/home/klingenberg/BoSSS-experimental/internal/src/Master.sln")
(defun my/csharp-find-current-project ()
  "Find the closest csproj file relative to the current directory."
  (cl-labels
      ((find-csproj-file (dir)
                         (directory-files dir nil ".*csproj"))
       (iter (dir)
             (cond
              ((find-csproj-file dir) (expand-file-name
                                       (car (find-csproj-file dir))
                                       dir)) ; if a .csproj file is found in the current directory, return its absolute path
              ((string-equal "/" (expand-file-name dir)) nil) ; prevent infinite loops
              (t (iter (concat dir "/../")))))) ; if there is no .csproj file, look one directory higher
    (iter (file-name-directory (buffer-file-name)))))


(map!
 :localleader
 :map csharp-mode-map
 "cd" (lambda () (interactive) (compile (concat "msbuild /p:Configuration=Debug " (my/csharp-find-current-project))))
 "cr" (lambda () (interactive) (compile (concat "msbuild /p:Configuration=Release " bosss-master-solution)))
 "ce" (lambda () (interactive) (compile (concat "msbuild /p:Configuration=Debug " bosss-master-solution)))
 ;; "cc" #'recompile
 "=" #'my/indent-buffer-without-bosss-header
 "et" (lambda () (interactive) (my/run-tests (my/csharp-find-current-project)))
 "eo" #'run-csharp-repl-other-frame
 "R" #'run-csharp-repl-other-window
 "er" #'csharp-repl-send-region
 "eb" #'csharp-repl-send-buffer)

;; bosss
(use-package! bosss
  :unless (system-name= "klingenberg-pi")
  :load-path "~/Documents/programming/elisp/emacs-bosss/"
  :init
  (add-to-list 'auto-mode-alist '("\\.bws\\'" . bosss-mode))
  (setq bosss-pad-path "/home/klingenberg/BoSSS-experimental/public/src/L4-application/BoSSSpad/bin/Release/BoSSSpad.exe")
  (setq bosss-path-reference (mapcar (lambda (proj) (concat "/home/klingenberg/BoSSS-experimental/internal/src/private-kli/" proj))
                                     '("RANSCommon/bin/Release/RANS_Solver.dll"
                                       "KOmegaModelSolver/bin/Release/KOmegaSolver.exe"
                                       "KOmegaStatSymmModelSolver/bin/Release/KOmegaSSSolver.exe"
                                       "TurbulenceModelParameterOptimization/bin/Release/ParameterOptimization.exe")))
  :config
  (map! :map bosss-mode-map
        :n "M-j" '(bosss-next-field :which-key "next field")
        :n "M-k" '(bosss-previous-field :which-key "previous field"))
  (map!
   :localleader
   :map #'bosss-mode-map
   :n "ro" #'run-bosss-repl-other-window
   :n "R"  #'run-bosss-repl-other-window
   :n "rn" #'bosss-repl-start-bosss-pad
   :n "ef" #'bosss-repl-send-current-field
   :n "ee" #'bosss-repl-send-region
   :n "eb" #'bosss-repl-send-buffer
   :n "en" #'bosss-eval-and-next-field
   :n "lp" #'bosss-repl-load-my-assembly
   :n "in" #'bosss-create-new-field))

(map! :map company-mode-map
      :i "M-l" #'company-complete-selection
      :i "M-j" #'company-select-next-or-abort
      :i "M-k" #'company-select-previous-or-abort)

(map!
 :after evil-snipe
 :v "s" #'evil-surround-region)

(use-package! helm
  :diminish helm-mode
  :config
  (map!
   :map helm-map
   "M-j" #'helm-next-line
   "M-k" #'helm-previous-line
   "M-h" #'helm-find-files-up-one-level
   "M-l" #'helm-execute-persistent-action
   "M-H" #'left-char
   "M-L" #'right-char
   "M-SPC" #'helm-toggle-visible-mark-forward)
  (map!
   :map helm-find-files-map
   "M-l" #'helm-ff-RET
   "C-l" nil
   "M-y" #'helm-ff-run-copy-file
   "M-r" #'helm-ff-run-rename-file
   "M-s" #'helm-ff-run-find-file-as-root
   "M-o" #'helm-ff-run-switch-other-window
   "M-O" #'helm-ff-run-switch-other-frame
   "M-RET" #'helm-ff-run-open-file-with-default-tool)
  (map!
   :map helm-buffer-map
   "M-d" #'helm-buffer-run-kill-persistent)
  (setq completion-styles `(basic partial-completion emacs22 initials
                                  ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-file-cache-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-quick-update t)
  (setq helm-recentf-fuzzy-match t))

(use-package! helm-swoop
  :after-call helm-mode-hook
  :config
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-current-mode-from-helm-swoop))

(use-package! org-roam-server
  :after-call org-roam-mode-hook
  :config
  (map! :map doom-leader-notes-map
        "rg" (lambda () (interactive) (org-roam-server-mode 1) (browse-url-firefox "127.0.0.1:8080")))
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20))

(use-package! smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;; (use-package! mini-modeline
;;   :after doom-modeline
;;   :config
;;   (defun my-setup-mini-modeline()
;;     (add-hook 'minibuffer-inactive-mode-hook
;;               (lambda ()
;;                 (make-local-variable 'face-remapping-alist)
;;                 (add-to-list 'face-remapping-alist '(default (:background "#21252b")))))
;;     (doom-modeline-def-modeline 'minibuffer-line
;;       '(modals workspace-name window-number matches buffer-info remote-host buffer-position word-count selection-info)
;;       '(misc-info major-mode process vcs lsp checker " \n"))
;;     (setq mini-modeline-l-format '(:eval (doom-modeline-format--minibuffer-line))))
;;   (add-hook 'mini-modeline-mode-hook 'my-setup-mini-modeline)
;;   (add-hook 'after-init-hook 'mini-modeline-mode))

(use-package! pulseaudio-control
  :custom
  (pulseaudio-control-volume-step "5%")
  :config
  (setq pulseaudio-control--volume-maximum '(("percent" . 110)
                                             ("decibels" . 2.5)
                                             ("raw" . 72000))))
(use-package! pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
  (evil-collection-init 'pdf)
  (setq pdf-view-midnight-colors '("WhiteSmoke" . "gray16"))
  (map!
   :map pdf-view-mode-map
   :n "-" nil)
  (map!
   :localleader
   :map pdf-view-mode-map
   "at" #'pdf-annot-add-text-annotation
   "ah" #'pdf-annot-add-highlight-markup-annotation
   "ao" #'pdf-annot-add-strikeout-markup-annotation
   "aD" #'pdf-annot-delete))

(use-package! telega
  :commands telega
  :config
  (telega-notifications-mode 1))

(use-package! elfeed
  :commands (elfeed elfeed-update)
  :config
  (setq elfeed-feeds
        '(("https://www.zeitsprung.fm/feed/ogg/" podcast zeitsprung)
          ("https://kickermeetsdazn.podigee.io/feed/mp3/" podcast kmd fussball)
          ("https://audioboom.com/channels/2399216.rss" podcast nstaaf)
          ("http://fokus-fussball.de/feed/mp3/" podcast collina erben fussball)
          ("https://liebling-bosman.podigee.io/feed/mp3" podcast liebling bosman fussball)
          ("https://tribuenengespraech.podigee.io/feed/vorbis" podcast rasenfunk tribünengespräch fussball)
          ("https://feeds.feedburner.com/hacks-on-tap" podcast hacks on tap politics)
          ("https://ambrevar.xyz/atom.xml" blog emacs programming)
          ("https://nyxt.atlas.engineer/feed" lisp programming nyxt next)
          ("https://guix.gnu.org/feeds/blog/arm.atom" lisp programming guix blog)
          ("https://www.kernel.org/feeds/kdist.xml" linux kernel updates)
          ("http://www.reddit.com/r/emacs/.rss" emacs reddit)
          ("http://www.reddit.com/r/DoomEmacs/.rss" emacs reddit)
          ("http://www.reddit.com/r/lisp/.rss" programming reddit)
          ("http://www.reddit.com/r/common_lisp/.rss" programming reddit)
          ("http://www.reddit.com/r/scheme/.rss" programming reddit)
          ("http://www.reddit.com/r/linux/.rss" programming reddit)
          ("http://www.reddit.com/r/archlinux/.rss" programming reddit)
          ("http://www.reddit.com/r/nixos/.rss" programming reddit)
          ("http://www.reddit.com/r/Plover/.rss" programming reddit)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyHDQ5C6z1NDmJ4g6SerW8g" youtube mailab)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UChkVOG0PqkXIHHmkBGG15Ig" youtube walulis)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCo4693Ony4slDY5hR0ny-bw" youtube walulis)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVTyTA7-g9nopHeHbeuvpRA" youtube meyers)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3XTzVzaHQEd30rQbuvCtTQ" youtube lastweektonight)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw" youtube math 3blue1brown 3b1b)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCoxcjq-8xIDTYp3uz647V5A" youtube math numberphile)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSju5G2aFaWMqn-_0YBtq5A" youtube math standupmaths matt parker)
          ("https://www.youtube.com/feeds/videos.xml?playlist_id=PL8FB14A2200B87185" youtube playlist yale economics)))

  ;; Taken from https://joshrollinswrites.com/help-desk-head-desk/20200611/
  (defun elfeed-v-mpv (url)
    "Watch a video from URL in MPV"
    (async-shell-command (format "mpv \"%s\"" url)))

  (defun elfeed-view-mpv (&optional use-generic-p)
    "Youtube-feed link"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (elfeed-v-mpv it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))
  ;; Taken from https://noonker.github.io/posts/2020-04-22-elfeed/
  (defun yt-dl-it (url)
    "Downloads the URL in an async shell"
    (let ((default-directory "~/Videos"))
      (async-shell-command (format "youtube-dl %s" url))))

  (defun elfeed-youtube-dl (&optional use-generic-p)
    "Youtube-DL link"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (yt-dl-it it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (defun elfeed-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (eww-browse-url it)
               do (delete-other-windows))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (defun elfeed-firefox-open (&optional use-generic-p)
    "open with firefox"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (browse-url-firefox it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (defun elfeed-reddit-open (&optional use-generic-p)
    "open with md4rd"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (md4rd--fetch-comments (format "%s.json" it)))
      (mapc #'elfeed-search-update-entry entries)
      ;; (unless (use-region-p) (forward-line))
      ))

  (defun elfeed-open-item-generic (entry)
    (cond
     ((elfeed-tagged-p 'youtube entry) (elfeed-view-mpv))
     ((elfeed-tagged-p 'reddit entry) (elfeed-reddit-open))
     ((elfeed-tagged-p 'podcast entry) (let ((elfeed-show-entry entry))
                                         (elfeed-show-play-enclosure
                                          (elfeed--enclosure-maybe-prompt-index entry))))
     (t (elfeed-eww-open))))

  (defun elfeed-open-generic ()
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (elfeed-open-item-generic entry))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (map! :map elfeed-search-mode-map
        :n "o" #'elfeed-open-generic
        :n "e" #'elfeed-eww-open
        :n "b" #'elfeed-firefox-open
        :n "r" #'elfeed-reddit-open
        :n "v" #'elfeed-view-mpv
        :n "d" #'elfeed-youtube-dl))

(after! eww
 (map!
   :map eww-mode-map
   :n "M-h" #'eww-back-url
   :n "M-l" #'eww-forward-url
   :n "M-y" #'eww-copy-page-url
   :n "f" #'ace-link-eww))

(use-package! md4rd
  :commands (md4rd md4rd--fetch-comments)
  :config
  (add-hook 'md4rd-mode-hook 'md4rd-indent-all-the-lines))

(use-package! emms
  :commands (emms)
  :config
  (emms-standard)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music"))

(use-package! pinentry
  :init
  (pinentry-start))

(use-package! diminish
  :config
  (mapcar #'diminish '(reftex-mode
                       auto-revert-mode
                       undo-tree-mode
                       eldoc-mode
                       pdf-view-midnight-minor-mode
                       subword-mode
                       flyspell-mode
                       defining-kbd-macro)))

;; (use-package! key-chord
;;   :config
;;   (key-chord-mode 1)
;;   (key-chord-define csharp-mode-map "cl"  "class")
;;   (key-chord-define csharp-mode-map "or"  "override")
;;   (key-chord-define csharp-mode-map "st"  "static")
;;   (key-chord-define csharp-mode-map "pu"  "public")
;;   (key-chord-define csharp-mode-map "pr"  "private")
;;   (key-chord-define csharp-mode-map "db"  "double")
;;   (key-chord-define csharp-mode-map "in"  "int")
;;   (key-chord-define csharp-mode-map ";;"  "\C-e;"))

;; load my custom scripts
(load "~/Dropbox/Helen+Dario/washing-machine-timer.el" t t)
(load "~/Dropbox/Helen+Dario/einkaufsliste/interactiveEnterLisp.el" t t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(avy-all-windows t)
 '(avy-single-candidate-jump t)
 '(compilation-scroll-output t)
 '(custom-safe-themes
   '("d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "fe94e2e42ccaa9714dd0f83a5aa1efeef819e22c5774115a9984293af609fce7" default))
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#80d200")
     ("FAIL" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#aaeeee")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-header)
 '(pulseaudio-control-volume-step "5%")
 '(vc-annotate-background-mode nil)
 '(xterm-color-names
   ["#000000" "#ff8059" "#44bc44" "#eecc00" "#29aeff" "#feacd0" "#00d3d0" "#a8a8a8"])
 '(xterm-color-names-bright
   ["#181a20" "#f4923b" "#80d200" "#cfdf30" "#72a4ff" "#f78fe7" "#4ae8fc" "#ffffff"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
