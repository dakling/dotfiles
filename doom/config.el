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
(setq doom-theme 'doom-solarized-dark)


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


;;; Setting some variables
(setq evil-collection-setup-minibuffer t)

(setq display-time-24hr-format t
      display-time-default-load-average nil)
(display-time-mode 1)

(setq initial-major-mode 'lisp-interaction-mode)

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

(setq browse-url-browser-function 'eww-browse-url)

(customize-set-variable 'compilation-scroll-output t)

;;; Defining some useful functions
(defun shutdown ()
  (interactive)
  (cond
   ((system-name= "klingenberg-laptop") (async-shell-command "sudo shutdown"))
   (t (shell-command "shutdown now"))))

(defun reboot ()
  (interactive)
  (async-shell-command "sudo reboot now"))

(defun system-name= (&rest names)
  (cl-some
   (lambda (name)
     (string-equal name (system-name)))
   names))

(defvar browser
  (cond
   ;; ((system-name= "klingenberg-tablet") "next")
   ((system-name= "klingenberg-laptop") "epiphany")
   (t "firefox")))

(defun my/brightness+ ()
  (interactive)
  (shell-command "xbacklight -inc 10"))

(defun my/brightness- ()
  (interactive)
  (shell-command "xbacklight -dec 10"))

(defun my/fix-touchscreen ()
  (when (system-name= "klingenberg-tablet")
    (shell-command "xinput --map-to-output $(xinput list --id-only \"ELAN Touchscreen\") eDP1")))

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

(if (not (system-name= "lina"))
    (progn
      (use-package! exwm
        :init
        (server-start)
        :config
        (evil-set-initial-state 'exwm-mode 'emacs)
        (setq mouse-autoselect-window nil
              focus-follows-mouse nil)
        (exwm-enable))

      (use-package! exwm-input
        :after exwm-randr
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
                ([?\s-b] . helm-mini)
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
                ([s-f4] . (lambda () (interactive) (gnus)))
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
        :after exwm
        :config (exwm-systemtray-enable))

      (defvar exwm-connected-displays)
      (use-package! exwm-randr
        :after exwm
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
          (my/fix-touchscreen)
          (setq exwm-connected-displays (length (cadr (exwm-randr--get-outputs)))))

        (add-hook 'exwm-randr-screen-change-hook #'my/exwm-xrandr)
        (progn
          (exwm-randr-enable)))

      (use-package! exwm-workspace
        :after exwm
        :init
        (progn
          (setq exwm-workspace-number 10)
          (setq exwm-workspace-show-all-buffers t)
          (setq exwm-layout-show-all-buffers t)))))

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
   "s-<f1>" '(lambda () (interactive) (eshell 'N))
   "C-s-<f1>" 'eshell
   "s-<f2>" '(lambda () (interactive)
               (start-process "" nil browser))
   "s-<f3>" 'deer
   "s-<f4>" '(lambda () (interactive)
               (gnus))
   "s-<f12>" '(lambda () (interactive)
                (start-process "" nil "/usr/bin/slock"))))

;; (my/create-super-bindings)

;; mail - gnus
;; (after! gnus
;;  (gnus-demon-add-handler 'gnus-demon-scan-news 5 nil)
;;  (gnus-demon-init))

;; ;; Don't ask auto-save stuff when opening
;; (setq gnus-always-read-dribble-file t)
;;                                         ; No primary server:
;; (setq gnus-select-method '(nnnil ""))

;;                                         ; Get local email, and store it in nnml; connect via IMAP to imap.gmail.com...:
;; (setq gnus-secondary-select-methods '((nnml "")
;;                                       (nnimap "mail.tu-darmstadt.de")
;;                                       (nnimap "imap.gmail.com")
;;                                       (nnimap "imap.web.de")
;;                                       (nnimap "mail.gsc.ce.tu-darmstadt.de")
;;                                       ;;(nnrss "https://www.zeitsprung.fm/feed/mp3/")
;;                                       ;; (nnreddit "")
;;                                       ))

;;                                         ; Archive outgoing email in Sent folder on imap.mcom.com:
;; ;; (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
;; ;;       gnus-message-archive-group "[Gmail]/Sent Mail")

;;                                         ; Mark gcc'ed (archived) as read:
;; (setq gnus-gcc-mark-as-read t)

;;                                         ; Send email via Gmail:
;; ;; (setq message-send-mail-function 'smtpmail-send-it
;; ;;       smtpmail-default-smtp-server "smtp.gmail.com")


;; ;; Available SMTP accounts.
;; (defvar smtp-accounts
;;   '((ssl "klingenberg@fdy.tu-darmstadt.de" "smtp.tu-darmstadt.de" 465 "km88econ" nil)
;;     (ssl "dario.klingenberg@gmail.com" "smtp.gmail.com" 465 "dario.klingenberg" nil)
;;     (starttls "dario.klingenberg@web.de" "smtp.web.de" 587 "dario.klingenberg" nil)
;;     (ssl "klingenberg@gsc.tu-darmstadt.de" "smtp.gsc.ce.tu-darmstadt.de" 465 "klingenberg" nil)))

;; ;; Let Gnus change the "From:" line by looking at current group we are in.
;; (setq gnus-posting-styles
;;       '(((mail-header-from "*fdy*") (signature-file "~/.config/emacs/fdy-signature"))
;;         ("fdy" (address "klingenberg@fdy.tu-darmstadt.de") (signature-file "~/.config/emacs/fdy-signature"))
;;         ("gsc" (address "klingenberg@gsc.tu-darmstadt.de") (signature-file "~/.config/emacs/gsc-signature"))
;;         ("gmail" (address "dario.klingenberg@gmail.com") (signature-file nil))
;;         ("web" (address "dario.klingenberg@web.de") (signature-file nil))))

;; ;; Default smtpmail.el configurations.
;; (require 'cl)
;; (require 'smtpmail)
;; (setq send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it
;;       mail-from-style nil
;;       user-full-name "Dario Klingenberg"
;;       user-mail-address "klingenberg@fdy.tu-darmstadt.de"
;;       smtpmail-debug-info t
;;       smtpmail-debug-verb t)

;; (defun set-smtp (mech server port user password)
;;   "Set related SMTP variables for supplied parameters."
;;   (setq smtpmail-smtp-server server
;;         smtpmail-smtp-service port
;;         smtpmail-auth-credentials (list (list server port user password))
;;         smtpmail-auth-supported (list mech)
;;         smtpmail-starttls-credentials nil)
;;   (message "Setting SMTP server to `%s:%s' for user `%s'."
;;            server port user))

;; (defun set-smtp-ssl (server port user password &optional key cert)
;;   "Set related SMTP and SSL variables for supplied parameters."
;;   (setq starttls-use-gnutls t
;;         starttls-gnutls-program "gnutls-cli"
;;         starttls-extra-arguments nil
;;         smtpmail-smtp-server server
;;         smtpmail-smtp-service port
;;         smtpmail-stream-type 'ssl
;;         smtpmail-auth-credentials (list (list server port user password))
;;         smtpmail-starttls-credentials (list (list server port key cert)))
;;   (message
;;    "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
;;    server port user))

;; (defun set-smtp-starttls (server port user password &optional key cert)
;;   "Set related SMTP and Starttls variables for supplied parameters."
;;   (setq starttls-use-gnutls t
;;         starttls-gnutls-program "gnutls-cli"
;;         starttls-extra-arguments nil
;;         smtpmail-smtp-server server
;;         smtpmail-smtp-service port
;;         smtpmail-stream-type 'starttls
;;         smtpmail-auth-credentials (list (list server port user password))
;;         smtpmail-starttls-credentials (list (list server port key cert)))
;;   (message
;;    "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
;;    server port user))

;; (defun change-smtp ()
;;   "Change the SMTP server according to the current from line."
;;   (save-excursion
;;     (loop with from = (save-restriction
;;                         (message-narrow-to-headers)
;;                         (message-fetch-field "from"))
;;           for (auth-mech address . auth-spec) in smtp-accounts
;;           when (string-match address from)
;;           do (cond
;;               ((memq auth-mech '(cram-md5 plain login))
;;                (return (apply 'set-smtp (cons auth-mech auth-spec))))
;;               ((eql auth-mech 'ssl)
;;                (return (apply 'set-smtp-ssl auth-spec)))
;;               ((eql auth-mech 'starttls)
;;                (return (apply 'set-smtp-starttls auth-spec)))
;;               (t (error "Unrecognized SMTP auth. mechanism: `%s'" auth-mech)))
;;           finally (error "Cannot infer SMTP information"))))

;; (defadvice smtpmail-via-smtp
;;     (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer))
;;   "Call `change-smtp' before every `smtpmail-via-smtp'."
;;   (with-current-buffer smtpmail-text-buffer (change-smtp)))

;; (setq message-dont-reply-to-names "klingenberg")

;; (ad-activate 'smtpmail-via-smtp)
;; ;; (ad-deactivate 'smtpmail-via-smtp)

;;                                         ; Use topics per default:
;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;;                                         ; Show more MIME-stuff:
;; (setq gnus-mime-display-multipart-related-as-mixed t)
;;                                         ; Smileys:
;; (setq smiley-style 'medium)
;;                                         ; Don't get the first article automatically:
;; (setq gnus-auto-select-first nil)
;;                                         ; Don't show that annoying arrow:
;; (setq gnus-summary-display-arrow nil)

;; ;; (setq nnimap-split-inbox "INBOX") ;; (1)
;; ;; (setq nnimap-split-predicate "UNDELETED") ;; (2)
;; ;; (setq nnimap-split-rule
;; ;;        '(
;; ;;          ("INBOX.web" "^To:.*web.de")
;; ;;          ("INBOX.gmail" "^To:.*gmail.com")
;; ;;          ("INBOX.fdy" "^To:.*fdy.tu-darmstadt.de")
;; ;;          ("INBOX.gsc" "^To:.*gsc.tu-darmstadt.de")
;; ;;         ))

;; (setq gnus-topic-topology '(("Gnus" visible)
;;                             (("important" visible)
;;                              (("work" visible)))
;;                             (("misc" visible))))

;; ;; TODO - test
;; ;; (mapcar (lambda (topic)
;; ;;           (gnus-topic-set-parameters
;; ;;            topic
;; ;;            '((gnus-use-adaptive-scoring nil)
;; ;;              (gnus-use-scoring nil)
;; ;;              (visible . t)
;; ;;              (display . all)
;; ;;              (modeline-notify . t))))
;; ;;         '("important" "work"))

;;                                         ; Email splitting rules:
;; (setq nnmail-split-fancy
;;       '(|
;;         (any "klingenberg@fdy.tu-darmstadt.de" "work")
;;         (any "klingenberg@gsc.tu-darmstadt.de" "work")
;;         (any "klingenberg@gmail.tu-darmstadt.de" "important")
;;         "INBOX"))
;;                                         ; Use fancy splitting:
;; (setq nnmail-split-methods 'nnmail-split-fancy)

;; ;; auto-complete addresses
;; (use-package! bbdb
;;   :config
;;   (add-to-list 'company-backends 'company-bbdb)
;;   (bbdb-initialize 'gnus 'message)
;;   (bbdb-mua-auto-update-init 'gnus 'message))

;; ;; (general-define-key
;; ;;  :keymaps '(gnus-group-mode-map gnus-topic-mode-map)
;; ;;  :states '(override normal)
;; ;;  "s" '(gnus-group-make-nnir-group :which-key "search")
;; ;;  "o" '(gnus-group-list-all-groups :which-key "all groups")
;; ;;  "O" '(gnus-group-list-groups :which-key "hide groups")
;; ;;  "D" '(gnus-group-delete-group :which-key "delete groups")
;; ;;  "M-G" '(gnus-group-get-new-news :which-key "refresh all groups"))

;; ;; (general-define-key
;; ;;  :keymaps '(gnus-summary-mode-map gnus-article-mode-map)
;; ;;  :states '(override normal)
;; ;;  "r" '(gnus-summary-reply-with-original :which-key "reply")
;; ;;  "R" '(gnus-summary-wide-reply-with-original :which-key "reply to all"))

;; (use-package! gnus-desktop-notify
;;   :config
;;   (gnus-desktop-notify-mode)
;;   (gnus-demon-add-scanmail))

;; (require 'gnus-icalendar)
;; (gnus-icalendar-setup)
;; (setq gnus-icalendar-org-capture-file "~/Documents/TODO.org")
;; (setq gnus-icalendar-org-capture-headline '("IMPORTANT DATES")) ;;make sure to create Calendar heading first
;; (gnus-icalendar-org-setup)

                                        ; (setq +mu4e-backend 'offlineimap)

(defun my/mu4e-setup ()

  ;; (use-package! mu4e-conversation)

  ;; (global-mu4e-conversation-mode)
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
  ;; (setq mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e")
  ;; (setq mu4e-maildir "~/Mail")
  ;; (setq mu4e-trash-folder "/Trash")
  ;; (setq mu4e-refile-folder "/Archive")
  (setq mu4e-update-interval 120)
  ;; (setq mu4e-hide-index-messages t) ; do not show minibuffer messages after updates
  ;; (setq mu4e-index-update-error-warning nil)
  (setq mu4e-compose-signature-auto-include t)
  ;; (setq mu4e-view-show-images t)
  (setq mu4e-enable-notifications t)
  ;; (setq org-mu4e-convert-to-html t)
  ;; (setq send-mail-function 'smtpmail-send-it)
  ;; (setq message-send-mail-function 'smtpmail-send-it)
  ;; (setq smtpmail-stream-type 'ssl)
  ;; (setq mu4e-view-show-addresses t)
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
           (smtpmail-smtp-server "smtp.gsc.ce.tu-darmstadt.de")
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
  ;; (run-at-time t mu4e-update-interval #'(lambda ()
  ;;                                         (progn
  ;;                                           (mu4e-update-mail-and-index t))))
                                        ; this should not be needed, but it is

  ;; taken from reddit
  (use-package! mu4e-alert
    :config
    (setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND NOT maildir:/Web/INBOX/")
    (mu4e-alert-enable-mode-line-display)
    (mu4e-alert-enable-notifications)
    (mu4e-alert-set-default-style 'libnotify)
    (alert-add-rule
     :category "mu4e-alert"
     :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode)))
     :continue t))

  ;; (require 'mu4e-icalendar)
  ;; (mu4e-icalendar-setup)
  ;; (setq mu4e-view-use-gnus nil)

  ;; (defun convert-vcal-to-org ()
  ;;   (interactive)
  ;;   (read-only-mode -1)
  ;;   (erase-buffer)
  ;;   (let* ((ical2org (executable-find "ical2org")))
  ;;     (insert (shell-command-to-string
  ;;              (concat "awk -f " ical2org " <" (shell-quote-argument (buffer-file-name)))))
  ;;     (keep-lines "^[\*(  )]" (point-min) (point-max))
  ;;     (flush-lines "^ *$" (point-min) (point-max))))

  ;; (define-derived-mode
  ;;   my/mu4e-cal-view-mode
  ;;   fundamental-mode
  ;;   "my/mu4e-cal-view-mode"
  ;;   "View cal in mu4e."
  ;;   (convert-vcal-to-org))

  ;; (add-to-list 'auto-mode-alist '("\\.vcs\\'" . my/mu4e-cal-view-mode))

  ;; (require 'gnus-icalendar)
  ;; (gnus-icalendar-setup)

  ;; (setq gnus-icalendar-org-capture-file "~/Documents/TODO.org")
  ;; (setq gnus-icalendar-org-capture-headline '("IMPORTANT DATES")) ;;make sure to create Calendar heading first
  ;; (gnus-icalendar-org-setup)
  )

(defun evil-collection-mu4e-org-set-header-to-normal-mode ()
  (evil-set-initial-state 'mu4e-compose-mode 'normal))

(defun evil-collection-mu4e-org-set-header-to-insert-mode ()
  (evil-set-initial-state 'mu4e-compose-mode 'insert))

(after!
  mu4e
  (progn
    (my/mu4e-setup)
    ;; For org-mu4e-compose-mode
    (add-hook 'org-mode-hook
              #'evil-collection-mu4e-org-set-header-to-normal-mode)
    (add-hook 'mu4e-compose-pre-hook
              #'evil-collection-mu4e-org-set-header-to-insert-mode)))

;; keybindings
;;
;; alternative leader for exwm
(setq doom-leader-alt-key "s-SPC")

(setq doom-localleader-key "-")

;; (map! "ESC" #'keyboard-quit)

(map! :leader
      "SPC" #'execute-extended-command
      "++" #'+popup/toggle
      "+ RET" #'+popup/other
      "lm" #'bookmark-set
      "ll" #'bookmark-jump
      "er" #'eval-expression
      "ss" #'shutdown
      "sr" #'reboot
      "w TAB" #'evil-switch-to-windows-last-buffer)

(map!
 :n "gb" #'pop-tag-mark)

(map! :leader :map (elisp)
      "ef" #'eval-defun)

(after!
  lispy
  (lispy-set-key-theme '(lispy c-digits)) ;; disable single-key bindings

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
   ;; (kbd "M-h") #'lispyville-previous-opening
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
   ;; (kbd "M-8") #'lispyville-wrap-brackets
   ;; (kbd "M-7") #'lispyville-wrap-braces
   ;; (kbd "M-9") #'lispyville-wrap-brackets
   ;; (kbd "M-0") #'lispyville-wrap-braces
   :ni "C-<return>" #'lispy-split
   ;; (kbd "M-<backspace>") 'lispyville-delete-backward-word
   ;; (kbd "/") #'lispy-occur
   :n "gc" #'lispyville-comment-or-uncomment)

  (map!
   :localleader
   :map (emacs-lisp-mode-map lisp-mode-map scheme-mode-map)
   "el" #'lispy-eval
   "1" #'lispy-describe-inline
   "2" #'lispy-arglist-inline
   "x" #'lispy-x))

;; scheme
(setq flycheck-scheme-chicken-executable "chicken-csc")
(setq geiser-chicken-binary "chicken-csi")
(setq geiser-active-implementations '(chicken))

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
 ;; "s" '(my/LaTeX-star-environment-dwim :which-key "toggle starred environment")
 ;; "t" '(my/toggle-parens-and-left-right :which-key "toggle parens and \left( \right)")
 ;; "cse" '((lambda () (interactive) (LaTeX-environment 1)) :which-key "change current environment")
 ;; TeX-doc is a very slow function
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

;;c#
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
  :load-path "~/Documents/programming/elisp/emacs-csharp-repl/")

(defun my/bosss-file-p ()
  (or
   (file-in-directory-p (buffer-file-name) "~/BoSSS/")
   (file-in-directory-p (buffer-file-name) "~/BoSSS-experimental/internal/src/private-kli/")))

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
      (when (my/bosss-file-p)
        (unless (or (search-forward (substring header-text 93) nil t) ; check if header already exists, start a bit later to ignore year
                    (derived-mode-p #'bosss-mode)) ; check if this is just a worksheet
          (princ header-text (current-buffer)))))))

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

;; (add-hook 'csharp-mode-hook #'subword-mode)
;; (add-hook 'csharp-mode-hook #'company-mode)
;; (add-hook 'csharp-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'csharp-mode-hook (lambda ()
                              (push '(?< . ("< " . " >")) evil-surround-pairs-alist)))
(add-hook 'csharp-mode-hook #'my/add-header)
;; (add-hook 'csharp-mode-hook (lambda ()
;;                               (add-hook 'before-save-hook #'my/indent-buffer-without-bosss-header nil t)))

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
 "er" #'csharp-repl-send-region)

;; bosss
(use-package! bosss
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
  (map!
   :localleader
   :map bosss-mode-map
   "j" '(bosss-next-field :which-key "next field")
   "k" '(bosss-previous-field :which-key "previous field")
   "ro" '(run-bosss-repl-other-window :which-key "start repl in other window")
   "rn" '(bosss-repl-start-bosss-pad :which-key "run bossspad")
   "ef" '(bosss-repl-send-current-field :which-key "send region to repl")
   "ee" '(bosss-repl-send-region :which-key "send region to repl")
   "eb" '(bosss-repl-send-buffer :which-key "send buffer to repl")
   "en" '(bosss-eval-and-next-field :which-key "eval and next field")
   "lp" '(bosss-repl-load-my-assembly :which-key "load my assembly")
   "in" '(bosss-create-new-field :which-key "create new input field")))

(map! :map company-mode-map
      :i "M-j" #'company-select-next-or-abort
      :i "M-k" #'company-select-previous-or-abort)

(use-package! helm-exwm)

(use-package! helm
  :diminish helm-mode
  :after helm-exwm
  :config
  (map!
   :map helm-map
   "M-j" #'helm-next-line
   "M-k" #'helm-previous-line
   "M-H" #'left-char
   "M-L" #'right-char
   "M-SPC" #'helm-toggle-visible-mark-forward)
  (map!
   :map helm-find-files-map
   "M-l" #'helm-ff-RET
   "M-h" #'helm-find-files-up-one-level
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
  (setq helm-recentf-fuzzy-match t)
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf)))

(use-package! mini-modeline
  :custom
  mini-modeline-enhance-visual nil
  :config
  (setq display-time-default-load-average nil)
  (setq display-time-load-average-threshold 10000000)
  (setq mini-modeline-r-format
        '("%e" mode-line-front-space
          ;; mode-line-mule-info
          ;; mode-line-client
          ;; mode-line-modified
          ;; mode-line-remote
          ;; mode-line-frame-identification
          mode-line-buffer-identification
          vc-mode
          " " mode-line-position " "
          ;; evil-mode-line-tag
          ;; mode-line-modes
          mode-name
          " "
          mode-line-misc-info
          (:eval (format (concat "<%s> "
                                 (unless (null (my/exwm-get-other-workspace)) "[%s] "))
                         exwm-workspace-current-index
                         (my/exwm-get-other-workspace)))
          "    |"
          mode-line-end-spaces))
  (mini-modeline-mode 1))

(use-package! smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package! auto-dim-other-buffers
  :config
  (setq auto-dim-other-buffers-face nil)
  (auto-dim-other-buffers-mode 1))

(use-package! pulseaudio-control
  :custom
  (pulseaudio-control-volume-step "5%")
  :config
  (setq pulseaudio-control--volume-maximum '(("percent" . 110)
                                             ("decibels" . 2.5)
                                             ("raw" . 72000))))

(use-package! telega
  :config
  (telega-notifications-mode 1))

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

;; eshell
(defun my-eshell/setup ()
  (add-hook 'evil-insert-state-entry-hook #'+eshell-goto-prompt-on-insert-a nil t))

(after! eshell
  (add-hook 'eshell-mode-hook #'my-eshell/setup))


;; HACK
;; (map!
;;   :map eshell-mode-map
;;   :n "I" (lambda () (interactive)
;;            (progn
;;              (eshell-bol)
;;              (evil-insert 0))))

;; load my custom scripts
(load "~/Dropbox/Helen+Dario/washing-machine-timer.el" t t)
(load "~/Dropbox/Helen+Dario/einkaufsliste/interactiveEnterLisp.el" t t)
