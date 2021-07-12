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
(setq doom-font (font-spec :family "Fira Code")
      doom-variable-pitch-font (font-spec :family "Fira Code"))
;; (setq doom-font (font-spec :family "DejaVu Sans Mono"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-solarized-dark)
(setq doom-theme 'doom-one)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


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

;;; Setting some variables
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq evil-respect-visual-line-mode t)  ; TODO check if this must be moved to init.el
(setq evil-collection-setup-minibuffer t)

(setq +evil-want-o/O-to-continue-comments nil)

(setq display-time-24hr-format t
      display-time-default-load-average nil)
(display-time-mode 1)

(setq initial-major-mode 'lisp-interaction-mode
      doom-scratch-initial-major-mode 'lisp-interaction-mode)

(setq +lookup-provider-url-alist
      '(("DuckDuckGo" "https://duckduckgo.com/?q=%s")
        ("Google Translate" "https://translate.google.com/?sl=auto&tl=de&text=%s")
        ("Wikipedia" "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
        ("Wolfram alpha" "https://wolframalpha.com/input/?i=%s")
        ("Google" +lookup--online-backend-google "https://google.com/search?q=%s")
        ("Google (direct)" "https://google.com/search?q=%s")
        ("Google images" "https://www.google.com/images?q=%s")
        ("Google maps" "https://maps.google.com/maps?q=%s")
        ("DevDocs.io" "https://devdocs.io/#q=%s")
        ("StackOverflow" "https://stackoverflow.com/search?q=%s")
        ("Csharp (.Net)" "https://docs.microsoft.com/en-us/search/?terms=%s&scope=.NET")
        ("Github" "https://github.com/search?ref=simplesearch&q=%s")
        ("Youtube" "https://youtube.com/results?aq=f&oq=&search_query=%s")
        ("Doom Emacs issues" "https://github.com/hlissner/doom-emacs/issues?q=is%%3Aissue+%s")))

(setq browse-url-browser-function 'browse-url-firefox)

(setq-default abbrev-mode t)
(setq abbrev-file-name "~/Dropbox/Dario/abbrev.el")


(cond
 ((system-name= "klingenberg-laptop")
  (add-load-path! "/usr/share/emacs/site-lisp/")
  (add-load-path! "/usr/share/stumpwm/contrib/util/swm-emacs/"))
 ((system-name= "klingenberg-pi")
  (add-load-path! "/run/current-system/sw/share/emacs/site-lisp/mu4e"))
 ((system-name= "klingenberg-tablet")
  (add-load-path! "/run/current-system/profile/share/emacs/site-lisp/")
  (add-load-path! "~/.guix-profile/share/emacs/site-lisp/")))

(after! dired
  (setq ranger-cleanup-on-disable t
        ranger-cleanup-eagerly t))

(after! undo-fu
  (setq undo-fu-allow-undo-in-region t))

(after! sly
  (setq inferior-lisp-program (cond
                               ((system-name= "klingenberg-tablet")  "~/.local/bin/.run-sbcl.sh")
                               (t "/usr/bin/sbcl --load /home/klingenberg/quicklisp.lisp")))
  (defun my/connect-to-nyxt ()
    (when (equalp
           (buffer-file-name)
           "/home/klingenberg/.dotfiles/nyxt.lisp")
      (sly-connect "localhost" 4006)))
  (add-hook 'lisp-mode-hook #'my/connect-to-nyxt)
  (map! :map sly-mrepl-mode-map
        :ni "M-k" #'sly-mrepl-previous-prompt
        :ni "M-j" #'sly-mrepl-next-prompt)
  (map! :localleader
        :map sly-mrepl-mode-map
        :n "g" #'helm-comint-prompts-all))

(customize-set-variable 'compilation-scroll-output t)

(after! evil-snipe (evil-snipe-mode -1))

(customize-set-variable 'avy-all-windows t)

(customize-set-variable 'avy-single-candidate-jump t)

(setq magit-repository-directories '(("~/" . 1)))

(after! org
  (setq org-id-link-to-org-use-id t)
  (setq org-file-apps
        (remove (assoc "\\.pdf\\'" org-file-apps)
                org-file-apps))
  (setq org-todo-keywords (list "TODO" "SCOP" "PROG" "|" "DONE" "BLOC" "KILL"))
  (setq org-todo-keyword-faces
        '(("DONE" . font-lock-comment-face)
          ("SCOP" . +org-todo-onhold)))
  (setq org-capture-templates
        `(("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox") "* TODO %?
%i
%a" :prepend t) ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox") "* %u %?
%i
%a" :prepend t) ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file) "* %U %?
%i
%a" :prepend t) ("b" "BoSSS calculation" entry (file+headline "~/Documents-work/bosss/calculation-log.org" ,(with-temp-buffer
                                                                                                              (org-insert-time-stamp (current-time)))) "** RUNNING %T %(my/bosss-worksheet-get-project-name \"%f\")
- %(org-link-make-string (concat (bosss-get-most-recent-deploy-directory) \"/stdout.txt\")  \"stdout.txt\")
- PID: %(bosss-get-most-recent-pid)
- previous calculation:
- %?
%i" :prepend nil) ("p" "Templates for projects") ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?
%i
%a" :prepend t) ("pn" "Project-local notes" entry (file+headline +org-capture-project-notes-file "Inbox") "* %U %?
%i
%a" :prepend t) ("pc" "Project-local changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?
%i
%a" :prepend t) ("o" "Centralized templates for projects") ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?
 %i
 %a" :heading "Tasks" :prepend nil) ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?
 %i
 %a" :heading "Notes" :prepend t) ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?
 %i
 %a" :heading "Changelog" :prepend t))))

(map!
   :localleader
   :after org
   :map org-mode-map
   :n "ll" #'org-insert-link
   :n "lf" (lambda () (interactive)
             (let ((current-prefix-arg '-)) ; simulate pressing C-u
               (org-insert-link)))
   :n "x" (lambda () (interactive)
             (let ((current-prefix-arg '-)) ; simulate pressing C-u
               (call-interactively 'org-export-dispatch))))

(map!
 :after org
 :map org-mode-map
 :n "gb" 'org-mark-ring-goto)

(setq org-roam-capture-templates
      '(("d" "default" plain #'org-roam-capture--get-point "%? \n %i \n %a"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t)))

(use-package! org-super-links
  :after org
  :config
  (map! :localleader
        :map org-mode-map
        ("ss" #'org-super-links-link)
        ("ls" #'org-super-links-store-link)
        ("lS" #'org-super-links-insert-link)))

(setq smerge-command-prefix "#")

(set-popup-rules!
  '(("^\\*bosss\\*" :slot -1 :size 20 :select nil) ; popup bosss process buffer
    ("^\\*Async Shell Command\\*" :slot -1 :size 20)))

;;; Defining some useful functions
(defun shutdown ()
  (interactive)
  (run-hook-with-args-until-failure 'kill-emacs-query-functions)
  (cond
   ((system-name= "klingenberg-laptop" "klingenberg-tablet") (async-shell-command "sudo shutdown"))
   (t (shell-command "shutdown now"))))

(defun reboot ()
  (run-hook-with-args-until-failure 'kill-emacs-query-functions)
  (interactive)
  (async-shell-command "sudo reboot"))

(defun my/open-in-external-app ()
  (interactive)
  (let ((process-connection-type nil))
    ;; (helm-find-file-extern (buffer-file-name))
    (counsel-find-file-extern (buffer-file-name))))

(defun my/brightness+ ()
  (interactive)
  (shell-command "xbacklight -inc 10"))

(defun my/brightness- ()
  (interactive)
  (shell-command "xbacklight -dec 10"))

(defun my/fix-touchscreen ()
  (when (system-name= "klingenberg-tablet")
    (shell-command "xinput --map-to-output $(xinput list --id-only \"ELAN Touchscreen\") eDP-1")
    (ignore-errors
      (shell-command "xinput --map-to-output $(xinput list --id-only \"HDX HDX DIGITIZER Pen (0)\") eDP-1"))))

(defun my/tuxi ()
  (interactive)
  (cl-labels
      ((read-lines (filePath)
                   "Return a list of lines of a file at filePath."
                   (with-temp-buffer
                     (insert-file-contents filePath)
                     (split-string (buffer-string) "\n" t)))
       (write-lines (filename data)
                    (with-temp-file filename
                      (mapcar
                       (lambda (item)
                         (princ (concat item "\n") (current-buffer)))
                       data)))
       (update-cache (query old-cache-list)
                     (if (member query old-cache-list)
                         old-cache-list
                       (cons query old-cache-list))))
    (let*
        ((cache-file "~/.config/emacs/.local/cache/tuxi")
         (cache (read-lines cache-file))
         (query (completing-read "Enter query: " cache))
         (updated-cache (update-cache query cache))
         (result (shell-command-to-string (concat "tuxi -r " query))))
      (write-lines cache-file updated-cache)
      (message result))))

(defun my/eww-open-league-table ()
  "Do an internet search for soccer league table."
  (interactive)
  (let* ((country-search-string-table
          '(("germany" "bundesliga tabelle")
            ("spain" "la liga tabelle")
            ("italy" "seria a tabelle")
            ("france" "ligue 1 tabelle")
            ("england" "premier league table")
            ("england 2nd tier" "english championship table")))
         (country (completing-read "which country? " (mapcar #'car country-search-string-table))))
    (eww (cadr (assoc country country-search-string-table)))))

(defun my/make-alert (time mesg)
  (run-at-time
   time
   nil
   (lambda ()
     (let ((ans (y-or-n-p (concat "Reminder: " mesg " Remind again in 30 seconds (n) or dismiss reminder (y)?"))))
       (unless ans
         (my/make-alert 30 mesg)))))
  (message "Made alert for %s at %s" mesg time))


(after! bash-completion
  (setq bash-completion-nospace t))     ; TODO does not have any effect

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
                 "https://w*invidio.us/watch\\?v="
                 ""
                 (replace-regexp-in-string
                  "https://w*youtube.com/watch\\?v="
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
       (counsel-find-file-extern
        (car (directory-files
              "~/Videos/"
              ;; download-dir
              t str)))))))

(defun my/youtube-watch ()
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
                 "https://w*invidio.us/watch\\?v="
                 "https://youtube.com/watch\\?v="
                 url)))))
    (elfeed-v-mpv str)))

(defun fdy-mount (source target)
  "Mount a directory from fdy windows remote server."
  (async-shell-command (concat
                        "sudo /usr/bin/mount //dc1/"
                        source
                        " "
                        target
                        " -t cifs -o username=klingenberg,noexec,uid=klingenberg")))

(defun qmount ()
  "Shortcuts for mounting frequent locations,"
  (interactive)
  (let*
      ((mount-options
        '((lectures . ("misc/fdy-lectures.git" "~/git/mnt/fdy-lectures.git"))
          (klausuren . ("lehre/TM1/Klausuren.git" "~/git/mnt/Klausuren.git"))
          (bosss . ("bosss/users/klingenberg/root.git" "~/git/mnt/bosss.git"))
          (publications . ("misc/fdy-publications.git" "~/git/mnt/fdy-publications.git"))
          (misc . ("misc" "~/misc"))
          (scratch . ("scratch" "~/scratch"))
          (backup . ("backup" "~/backup"))
          (lehre . ("lehre" "~/lehre"))))
       (location (intern
                  (completing-read
                   "What to mount"
                   (mapcar #'car mount-options)))))
    (apply #'fdy-mount (alist-get location mount-options))))

(defun my/close-buffer ()
  (interactive)
  ;; (unless (equalp (buffer-name) "*scratch*")
  ;;   (kill-this-buffer))
  (if (< 1 (length (window-list)))
      (evil-window-delete)
    (stump/window-close)))

(defun my/run-command-ssh (server &rest cmds)
  "Run COMMAND on SERVER, assumes that you set it up properly"
  (async-shell-command (concat "ssh " server " '" (mapconcat 'identity cmds "; ")"'")))

(defun stump/move-focus (direction)
  (with-current-buffer "*scratch*"
      (shell-command-to-string (format "stumpish eval \\\(move-focus :%s\\\)" direction))))

(defun stump/emacs-window-right ()
  (interactive)
  (condition-case nil
      (evil-window-right 1)
    (error (stump/move-focus "right"))))

(defun stump/emacs-window-left ()
  (interactive)
  (condition-case nil
      (evil-window-left 1)
    (error (stump/move-focus "left"))))

(defun stump/emacs-window-up ()
  (interactive)
  (condition-case nil
      (evil-window-up 1)
    (error (stump/move-focus "up"))))

(defun stump/emacs-window-down ()
  (interactive)
  (condition-case nil
      (evil-window-down 1)
    (error (stump/move-focus "down"))))

(defun stump/window-close ()
  (with-current-buffer "*scratch*"
      (shell-command-to-string "stumpish delete")))

(defun my/create-super-bindings ()
  "Create bindings starting with super for use outside exwm."
  (map!
   :n
   ;; :states '(insert emacs hybrid normal visual motion operator replace)
   "s-w" '(other-window :which-key "other window")
   "s-l" 'stump/emacs-window-right
   "s-h" 'stump/emacs-window-left
   "s-j" 'stump/emacs-window-down
   "s-k" 'stump/emacs-window-up
   "s-L" 'enlarge-window-horizontally
   "s-H" 'shrink-window-horizontally
   "s-J" 'enlarge-window
   "s-K" 'shrink-window
   "s-v" 'split-window-right
   "s-s" 'split-window-below
   "s-c" 'my/close-buffer
   "s-q" 'my/get-rid-of-mouse
   "s-m" 'delete-other-windows
   "s-t" 'my/tuxi
   "s-y" 'ytdious
   ;; "s-<f1>" '+vterm/here
   ;; "C-s-<f1>" '+vterm/toggle
   "s-<f1>" '+eshell/here
   "C-s-<f1>" '+eshell/toggle
   "s-<f2>" '(lambda () (interactive)
             (funcall browse-url-browser-function "" "-new-tab"))
   "s-<f3>" 'deer
   "s-<f4>" '(lambda () (interactive)
             (mu4e))
   "s-<f12>" '(lambda () (interactive)
                (start-process "" nil "/usr/bin/slock")))
  (when t
    (map!
     :n
     "s-x" 'counsel-M-x
     "s-f" 'counsel-find-file
     "s-p" 'counsel-projectile
     "s-b" 'ivy-switch-buffer
     "s-g" 'helm-system-packages
     "s-P" '+pass/ivy))
  (when nil
    (map!
     :n
     "s-x" 'helm-M-x
     "s-f" 'helm-find-files
     "s-p" 'helm-projectile
     "s-g" 'helm-system-packages
     "s-b" 'helm-mini
     "s-P" 'helm-pass)))

(my/create-super-bindings)

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

(setq fdy-signature
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
Web: https://www.fdy.tu-darmstadt.de
")

(setq gsc-signature
          "Technische Universität Darmstadt
Dario Klingenberg, M.Sc.
Graduate School Computational Engineering
Dolivostraße 15
64293 Darmstadt

E-Mail: klingenberg@gsc.tu-darmstadt.de
Telefon: +49 6151 16-24381
Fax: +49 6151 16-24404
Web: https://www.gsc.ce.tu-darmstadt.de/
")

(setq my/mu4e-account-alist
      `(("fdy"
         (mu4e-sent-messages-behavior sent)
         ;; (mu4e-compose-signature-auto-include nil)
         (mu4e-compose-signature ,fdy-signature)
         ;; (org-msg-signature
;;           ,(concat
;;            "#+begin_signature
;; --" fdy-signature "
;; #+end_signature"))
         (mu4e-sent-folder "/fdy/Sent Items")
         (mu4e-drafts-folder "/fdy/Drafts")
         (smtpmail-smtp-server "smtp.tu-darmstadt.de")
         (smtpmail-smtp-service 465)
         (smtpmail-stream-type ssl)
         (user-mail-address "klingenberg@fdy.tu-darmstadt.de")
         (user-full-name "Dario Klingenberg"))
        ("gsc"
         (mu4e-sent-messages-behavior sent)
         ;; (mu4e-compose-signature-auto-include nil)
         (mu4e-compose-signature ,gsc-signature)
         ;; (org-msg-signature
;;           ,(concat
;;             "#+begin_signature
;; --" gsc-signature "
;; #+end_signature"))
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
         ;; (mu4e-compose-signature-auto-include nil)
         (mu4e-sent-folder "/gmail/sent")
         (mu4e-drafts-folder "/gmail/Drafts")
         (user-mail-address "dario.klingenberg@gmail.com")
         (smtpmail-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-service 465)
         (smtpmail-stream-type ssl)
         (user-full-name "Dario Klingenberg")
         (mu4e-compose-signature nil)
         ;; (org-msg-signature nil)
         )
        ("web"
         (mu4e-sent-messages-behavior sent)
         ;; (mu4e-compose-signature-auto-include nil)
         (mu4e-sent-folder "/web/Sent Items")
         (mu4e-drafts-folder "/web/Drafts")
         (smtpmail-smtp-server "smtp.web.de")
         (smtpmail-smtp-service 587)
         (smtpmail-stream-type starttls)
         (user-mail-address "dario.klingenberg@web.de")
         (user-full-name "dario")
         (mu4e-compose-signature nil)
         ;; (org-msg-signature nil)
         )))

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
  (setq shr-use-colors nil)
  (setq message-subject-re-regexp
        (concat
         "^[ \t]*"
         "\\("
         "\\("
         "[Aa][Nn][Tt][Ww]\\.?\\|"     ; antw
         "[Aa][Ww]\\|"                 ; aw
         "[Ff][Ww][Dd]?\\|"            ; fwd
         "[Ww][Gg]?\\|"                ; wg
         "[Rr][Ee]\\|"                 ; re
         "[Rr][\311\351][Ff]\\.?\\|"   ; ref
         "\\)"
         "\\(\\[[0-9]*\\]\\)"
         "*:[ \t]*"
         "\\)"
         "*[ \t]*"
         ))
  (remove-hook 'mu4e-compose-pre-hook 'org-msg-mode)
  (setq mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version))
  (setq mu4e-update-interval 120)
  (setq mu4e-compose-signature-auto-include t)
  (setq mu4e-enable-notifications t)
  (customize-set-variable 'mu4e-headers-leave-behavior 'apply)
  (setq mu4e-view-use-gnus t)
  ;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-sign-pgpmime)
  ;; (setq mml-secure-message-openpgp-sign-with-sender t)
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  (require 'org-agenda)
  (setq gnus-icalendar-org-capture-file "~/org/notes.org")
  (setq gnus-icalendar-org-capture-headline '("Inbox"))
  (gnus-icalendar-org-setup)
  ;;     org-msg
  ;; (add-hook 'org-msg-edit-mode-hook 'mml-secure-sign-pgpmime)
  ;; (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
  ;;       ;; org-msg-startup "hidestars indent inlineimages"
  ;;       ;; org-msg-greeting-fmt "Hallo %s,\n\n\n"
  ;;       org-msg-default-alternatives '(html text))
  )

;; keybindings
;;
;; alternative leader for exwm
;; (setq doom-leader-alt-key "s-SPC")

(setq doom-localleader-key "-")

(map!
 "C-g" #'keyboard-quit
 :n "gb" #'pop-tag-mark
 :n "s" #'avy-goto-char-timer
 :n "S" #'avy-goto-char-timer
 :n "+" #'evil-goto-mark)

(map! :leader
      "SPC" #'execute-extended-command
      "++" #'+popup/toggle
      "+ RET" #'+popup/other
      "lm" #'bookmark-set
      "er" #'eval-expression
      "SS" #'shutdown
      "SR" #'reboot
      "w TAB" #'evil-switch-to-windows-last-buffer)

(map!
 :map doom-leader-open-map
 "w" #'eww
 "d" #'dired-jump
 "D" #'+debugger/start)

(map! :leader :map (elisp)
      "ef" #'eval-defun
      "ep" #'eval-print-last-sexp)

;; outline-minor-mode messes with some of my lispy bindings
;; TODO why doesnt the map! macro work?
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-j") nil)
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-k") nil)
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-h") nil)
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-l") nil)
;; (map! :map outline-minor-mode-map ;TODO check if this messes up other situtations
;;       :n "M-j" nil
;;       :n "M-k" nil
;;       :n "M-h" nil
;;       :n "M-l" nil)

(after!
  lispy
  (lispy-set-key-theme '(lispy c-digits))
  (setq lispy-colon-p nil)) ;; disable single-key bindings

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
   "[" nil
   "]" nil
   :ni "C-<return>" #'lispy-split
   :n "gc" #'lispyville-comment-or-uncomment)
  (map!
   :map lispy-mode-map
   "[" nil
   "]" nil)

  (map!
   :localleader
   :map (emacs-lisp-mode-map lisp-mode-map scheme-mode-map)
   "el" #'lispy-eval
   "1" #'lispy-describe-inline
   "2" #'lispy-arglist-inline
   "x" #'lispy-x))

;; scheme
(use-package! geiser
  ;; :load-path "/run/current-system/profile/share/emacs/site-lisp/"
  ;; :commands (run-geiser)
  :config
  (setq flycheck-scheme-chicken-executable "chicken-csc")
  (setq geiser-chicken-binary "chicken-csi")
  (setq geiser-active-implementations '(chicken guile chez))
  (setq geiser-default-implementation 'guile)
  ;; (setq geiser-scheme-dir "~/")
  (defun chicken-doc (&optional obtain-function)
    (interactive)
    (let ((func (funcall (or obtain-function 'current-word))))
      (when func
        (process-send-string (scheme-proc)
                             (format "(require-library chicken-doc) ,doc %S\n" func))
        (save-selected-window
          (select-window (display-buffer (get-buffer scheme-buffer) t))
          (goto-char (point-max))))))
  ;; (after! geiser
  ;;   (add-to-list 'geiser-implementations-alist '((regexp "\\.sc$") chez))
  ;;   (add-to-list 'geiser-implementations-alist '((regexp "\\.sls$") chez)))
  (add-to-list 'auto-mode-alist
               '("\\.sls\\'" . scheme-mode)
               '("\\.sc\\'" . scheme-mode))
  (add-to-list 'auto-mode-alist
               '("\\.egg\\'" . scheme-mode))
  (map!
   :localleader
   :map scheme-mode-map
   :n "'" #'geiser
   :n "ef" #'geiser-eval-definition
   :n "ee" #'geiser-eval-last-sexp
   :n "eb" #'geiser-eval-buffer))

(use-package! guix
  :when (system-name= "klingenberg-tablet")
  ;; :commands (guix scheme-mode)
  :config
  (defun my/activate-guix-devel-mode ()
    (when (file-in-directory-p (buffer-file-name) "~/guix")
      (guix-devel-mode 1)))
  (add-hook 'scheme-mode-hook #'my/activate-guix-devel-mode)
  (map!
   :localleader
   :map guix-devel-mode-map
   :n "b" 'guix-devel-build-package-definition
   :n "s" 'guix-devel-build-package-source
   :n "d" 'guix-devel-download-package-source
   :n "l" 'guix-devel-lint-package
   :n "k" 'guix-devel-copy-module-as-kill
   :n "u" 'guix-devel-use-module
   :n "." 'guix-devel-code-block-edit))

;; (use-package! cider
;;   :config
;;   (map!
;;    :localleader
;;    :map clojure-mode-map
;;    :n "'" '+eval/open-repl-other-window
;;    :n "ef" 'cider-eval-defun-at-point))

;; doc-view mode
(map!
 :map 'doc-view-mode-map
 :n "j" #'doc-view-next-page
 :n "k" #'doc-view-previous-page
 :n "<down>" #'doc-view-next-page
 :n "<up>" #'doc-view-previous-page)

;; open docx as text
(defun my/docx->markdown (&optional file)
  (let ((pandoc (executable-find "pandoc")))
    (shell-command-to-string
     (concat pandoc " --wrap=none " (shell-quote-argument (or file (buffer-file-name))) " -t markdown"))))

(defun my/docx->markdown! ()
  (interactive)
  (read-only-mode -1)
  (erase-buffer)
  (insert (my/docx->markdown (buffer-file-name)))
  (not-modified)
  (read-only-mode 1))

(define-derived-mode
  pandoc-view-mode
  markdown-mode
  "pandoc-view-mode"
  "View pandoc processing of docx file using markdown mode."
  (my/docx->markdown!))
(map!
 :localleader
 :map doc-view-mode-map
 :n "p" #'pandoc-view-mode
 :n "t" #'pandoc-view-mode
 :n "l" #'my/open-in-external-app)
(map!
 :localleader
 :map pandoc-view-mode-map
 :n "d" #'doc-view-mode
 :n "t" #'doc-view-mode
 :n "l" #'my/open-in-external-app)

;; latex
(setq +latex-viewers '(pdf-tools))
(setq reftex-default-bibliography
      (cond
       ((system-name= "klingenberg-laptop" "klingenberg-tablet") "~/Documents-work/conferences/latex_macros/bibliography.bib")
       ((system-name= "klingenberg-pc") "~/Documents/conferences/latex_macros/bibliography.bib")))

(add-hook! (TeX-mode-hook LaTeX-mode-hook) (lamdbda () (auto-fill-mode 1)))

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
 "og" (lambda () (interactive) (find-file "~/texmf/tex/latex/local/dakling.sty"))
 "ob" (lambda () (interactive) (find-file reftex-default-bibliography)))

(use-package! evil-tex
  :after-call LaTeX-mode-hook
  :config
  (add-hook 'LaTeX-mode-hook #'evil-tex-mode))

;; octave
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(map! :localleader
      :map octave-mode-map
      "el" #'octave-send-line
      "ef" #'octave-send-defun
      "eb" #'octave-send-block
      "er" #'octave-send-region
      "ee" #'octave-send-buffer)

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

(use-package! gnu-apl-mode)

;;c#
(use-package! lsp
  :config
  ;;(progn
    ;;(let*
        ;;((version "v1.37.7")
         ;;(server-dir (concat "~/.config/emacs/.local/etc/lsp/omnisharp-roslyn/" version "/")))
      ;;(defun my/create-lsp-custom-executable ()
        ;;"Modify the omnisharp-server run-script as needed for Guix"
        ;;(when (system-name= "klingenberg-laptop" "klingenberg-tablet")
          ;;(with-temp-file (concat server-dir "run-custom")
            ;;(goto-char (point-min))
            ;;(insert-file-contents (concat server-dir "run"))
            ;;(search-forward "mono_cmd")
            ;;(kill-line)
            ;;(insert "=mono"))))
      ;;(my/create-lsp-custom-executable)
      ;;(cond
       ;;((system-name= "klingenberg-pi")
        ;;(setq omnisharp-server-executable-path "/run/current-system/sw/bin/omnisharp"))
       ;;((system-name= "klingenberg-laptop" "klingenberg-tablet")
        ;;;; (setq omnisharp-server-executable-path "~/.nix-profile/bin/omnisharp/")
        ;;(setq lsp-csharp-server-path (concat server-dir "/run-custom"))))))
  )

;; (after! omnisharp
;;   (progn
;;     (defun my/create-omnisharp-custom-executable ()
;;       "Modify the omnisharp-server run-script as needed for Guix"
;;       (when (system-name= "klingenberg-laptop" "klingenberg-tablet")
;;         (with-temp-file (concat (omnisharp--server-installation-dir) "/run-custom")
;;           (goto-char (point-min))
;;           (insert-file-contents (omnisharp--server-installation-path))
;;           (search-forward "mono_cmd")
;;           (kill-line)
;;           (insert "=mono"))))
;;     (my/create-omnisharp-custom-executable)
;;     (cond
;;      ((system-name= "klingenberg-pi")
;;       (setq omnisharp-server-executable-path "/run/current-system/sw/bin/omnisharp"))
;;      ((system-name= "klingenberg-laptop" "klingenberg-tablet")
;;       ;; (setq omnisharp-server-executable-path "~/.nix-profile/bin/omnisharp/")
;;       (setq omnisharp-server-executable-path (concat (omnisharp--server-installation-dir) "/run-custom"))
;;       ;; (setq omnisharp-server-executable-path nil)
;;       ))))

(after! lsp
  ;; (setq lsp-file-watch-threshold 30000)
  (setq lsp-file-watch-threshold nil))

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
  :when (system-name= "klingenberg-laptop" "klingenberg-tablet" "klingenberg-pc")
  ;; :load-path "~/Documents/programming/elisp/emacs-csharp-repl/"
  )

(defun my/personal-bosss-file-p ()
  (and (buffer-file-name)
       (cl-search "private-kli" (buffer-file-name))))

(defun my/personal-bosss-control-file-p ()
  (and (buffer-file-name)
       (my/personal-bosss-file-p)
       (cl-search "Controlfiles" (buffer-file-name))))

(defun my/bosss-file-p ()
  (or
   (and (buffer-file-name)
        (file-in-directory-p (buffer-file-name) "~/BoSSS-experimental/"))
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

;; (defun my/omnisharp-code-format-entire-file ()
;;   (when (and (my/personal-bosss-file-p) (not (my/personal-bosss-control-file-p)))
;;     (omnisharp-code-format-entire-file)))

(defun my/format-on-save-enable ()
  "Code-format the entire buffen on save."
  (interactive)
  (format-all-mode -1)
  ;; (add-hook 'before-save-hook #'my/omnisharp-code-format-entire-file)
  )

(defun my/format-on-save-disable ()
  "Disable Code-formatting of the entire buffen on save."
  (interactive)
  ;; (remove-hook 'before-save-hook #'my/omnisharp-code-format-entire-file)
  ;; (remove-hook 'before-save-hook #'omnisharp-code-format-entire-file)
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

(defun my/bosss-worksheet-get-project-name (file)
  "In a BoSSS worksheet FILE, obtain the project name of the current calculation. Warning: This function is not very sophisticated."
  (save-excursion
    (progn
      (find-file file)
      (goto-char (point-min))
      (when
          (search-forward-regexp "^string ProjName = " nil t)
        (end-of-line)
        (backward-word)
        (thing-at-point 'word t)))))

(defun my/scancel-lichtenberg ()
  "Scancel (kill) process with pid on lichtenberg, assumes that pid is (thing-at-point)."
  (interactive)
  (let ((sid (thing-at-point 'word t)))
    (my/run-command-ssh "lcluster" (concat "scancel " sid))))

(defun my/squeue-lichtenberg ()
  "List process state on lichtenberg."
  (interactive)
  (my/run-command-ssh "lcluster" "squeue" "sacct --format=\"JobID,JobName%30,State\" | grep RUNNING"))

(add-hook 'csharp-mode-hook #'my/add-header)
(add-hook 'csharp-mode-hook #'my/format-on-save-disable)
;; (add-hook 'omnisharp-mode-hook #'my/omnisharp-code-format-entire-file)

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
 "cd" (lambda () (interactive) (compile (concat "msbuild -verbosity:quiet -maxCpuCount /p:WarningLevel=0 /p:Configuration=Debug " (my/csharp-find-current-project))))
 "cr" (lambda () (interactive) (compile (concat "msbuild -verbosity:quiet -maxCpuCount /p:WarningLevel=0 /p:Configuration=Release " bosss-master-solution)))
 "ce" (lambda () (interactive) (compile (concat "msbuild -verbosity:quiet -maxCpuCount /p:WarningLevel=0 /p:Configuration=Debug " bosss-master-solution)))
 "cc" #'recompile
 "=" #'my/indent-buffer-without-bosss-header
 "et" (lambda () (interactive) (my/run-tests (my/csharp-find-current-project)))
 "eo" #'run-csharp-repl-other-frame
 "R" #'run-csharp-repl-other-window
 "er" #'csharp-repl-send-region
 "eb" #'csharp-repl-send-buffer)

;; bosss
(use-package! bosss
  :when (system-name= "klingenberg-laptop" "klingenberg-tablet" "klingenberg-pc")
  ;; :load-path "~/Documents/programming/elisp/emacs-bosss/"
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
   :n "rq" #'bosss-repl-quit
   :n "R"  #'run-bosss-repl-other-window
   :n "rn" #'bosss-repl-start-bosss-pad
   :n "ef" #'bosss-repl-send-current-field
   :n "ee" #'bosss-repl-send-region
   :n "eb" #'bosss-repl-send-buffer
   :n "en" #'bosss-eval-and-next-field
   :n "lp" #'bosss-repl-load-my-assembly
   :n "in" #'bosss-create-new-field))

;;(use-package! maxima
  ;;:config
  ;;(add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))
  ;;(map! :localleader :map maxima-mode-map
        ;;"ef" #'maxima-send-full-line
        ;;"ee" #'maxima-send-previous-form
        ;;"er" #'maxima-send-region
        ;;"eb" #'maxima-send-buffer))

;; org-kanban
(use-package! kanban
  :when (system-name= "klingenberg-tablet")
  :load-path  "~/Documents/programming/elisp/kanban.el/")

(map! :map company-mode-map
      :i "M-l" #'company-complete-selection
      :i "M-j" #'company-select-next-or-abort
      :i "M-k" #'company-select-previous-or-abort)

(map!
 :after evil-snipe
 :v "s" #'evil-surround-region)

(after! ivy
  (setq ivy-extra-directories nil)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq counsel-find-file-at-point t)
  ;; (defun my/open-shell-here (dir)
  ;;   (sly)
  ;;   (sly-mrepl-sync nil (directory-file-name dir)))
  (map!
   :map ivy-minibuffer-map
   "M-j" #'ivy-next-line
   "M-k" #'ivy-previous-line
   "M-h" #'ivy-backward-delete-char
   "M-l" #'ivy-alt-done
   "M-o" #'ivy-dispatching-call
   "TAB" #'ivy-partial
   "M-RET" #'ivy-call
   "M-TAB" #'ivy-mark
   "C-TAB" #'ivy-unmark
   "M-H" #'left-char
   "M-L" #'right-char)
  ;; (ivy-add-actions
  ;;  'counsel-find-file
  ;;  '(("y" counsel-find-file-copy "copy file(s)"
  ;;     (lambda (x)
  ;;       (counsel-find-file-copy (mapconcat 'identity x "\n"))))
  ;;    ("r" counsel-find-file-move "move file(s)"
  ;;     (lambda (x)
  ;;       (counsel-find-file-move (mapconcat 'identity x "\n"))))
  ;;    ("D" counsel-find-file-delete "delete file(s)"
  ;;     (lambda (x)
  ;;       (counsel-find-file-delete (mapconcat 'identity x "\n"))))
  ;;    ("m" ivy-mark "mark")
  ;;    ("u" ivy-ummark "unmark")
  ;;    ;; ("t" ivy-toggle-marks "toggle marks")
  ;;    ))
  (map!
   :map counsel-find-file-map
   :ni "DEL" #'backward-delete-char
   "M-e" (lambda () (interactive) (ivy-exit-with-action
                                   (lambda (dir-or-file)
                                     (let ((default-directory (file-name-directory dir-or-file)))
                                       (eshell t)))))
   "M-s" (lambda () (interactive) (ivy-exit-with-action #'counsel-find-file-as-root))
   "M-y" (lambda () (interactive) (ivy-exit-with-action
                                   (lambda (x) (interactive) (if (listp x)
                                                                 (counsel-find-file-copy (mapconcat 'identity x "\n"))
                                                               (counsel-find-file-copy x)))))
   "M-r" (lambda () (interactive) (ivy-exit-with-action
                                   (lambda (x) (interactive) (if (listp x)
                                                                 (counsel-find-file-move (mapconcat 'identity x "\n"))
                                                               (counsel-find-file-move x)))))
   "M-D" (lambda () (interactive) (ivy-exit-with-action #'counsel-find-file-delete))
   "M-Y" (lambda () (interactive) (ivy-exit-with-action (lambda (x) (interactive) (kill-new (directory-file-name x)))))
   "M-c" (lambda () (interactive) (ivy-exit-with-action (lambda (x) (interactive) (dired-compress-file x))))
   "M-RET" (lambda () (interactive) (ivy-exit-with-action #'counsel-find-file-extern))))

;; (use-package! helm
;;   :when (featurep 'helm)
;;   :diminish helm-mode
;;   :config
;;   (map!
;;    :map doom-leader-buffer-map
;;    "b" #'helm-mini)
;;   (map!
;;    :map helm-map
;;    "M-j" #'helm-next-line
;;    "M-k" #'helm-previous-line
;;    "M-h" #'helm-find-files-up-one-level
;;    "M-l" #'helm-execute-persistent-action
;;    "M-w" #'helm-select-action
;;    "M-H" #'left-char
;;    "M-L" #'right-char
;;    "M-TAB" #'helm-toggle-visible-mark-forward)
;;   (map!
;;    :map helm-find-files-map
;;    "M-l" #'helm-ff-RET
;;    "C-l" nil
;;    "M-y" #'helm-ff-run-copy-file
;;    "M-r" #'helm-ff-run-rename-file
;;    "M-s" #'helm-ff-run-find-file-as-root
;;    "M--" #'helm-ff-run-marked-files-in-dired
;;    "M-o" #'helm-ff-run-switch-other-window
;;    "M-O" #'helm-ff-run-switch-other-frame
;;    "M-RET" #'helm-ff-run-open-file-with-default-tool)
;;   (map!
;;    :map helm-buffer-map
;;    "M-d" #'helm-buffer-run-kill-persistent)
;;   (setq helm-move-to-line-cycle-in-source t))

;; TODO check if this is needed with doom
;; (use-package! org-roam-server
;;   :after-call org-roam-mode-hook
;;   :config
;;   (map! :map doom-leader-notes-map
;;         "rg" (lambda () (interactive) (org-roam-server-mode 1) (browse-url-firefox "127.0.0.1:8080")))
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-label-truncate t
;;         org-roam-server-label-truncate-length 60
;;         org-roam-server-label-wrap-length 20))
;; END TODO check if this is needed with doom

;; (use-package! edit-server
;;   :config (edit-server-start))

(use-package! vterm
  :config
  (map!
   :map vterm-mode-map
   :ni "M-p" #'vterm-send-C-p
   :ni "M-n" #'vterm-send-C-n
   :ni "M-k" #'vterm-previous-prompt
   :ni "M-j" #'vterm-next-prompt))

(use-package! pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-auto-slice-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
  (evil-collection-init 'pdf)
  (add-to-list 'desktop-locals-to-save 'pdf-view-register-alist)
  (setq pdf-view-midnight-colors '("WhiteSmoke" . "gray16"))
  (map!
   :map pdf-view-mode-map
   :n "J" #'pdf-view-next-page
   :n "K" #'pdf-view-previous-page
   :n "+" #'pdf-view-jump-to-register
   :n "-" nil)
  (map!
   :localleader
   :map pdf-view-mode-map
   "at" #'pdf-annot-add-text-annotation
   "ah" #'pdf-annot-add-highlight-markup-annotation
   "ao" #'pdf-annot-add-strikeout-markup-annotation
   "aD" #'pdf-annot-delete))

(use-package! pulseaudio-control
  :when (system-name= "klingenberg-tablet")
  :custom
  (pulseaudio-control-volume-step "5%")
  :config
  (setq pulseaudio-control--volume-maximum '(("percent" . 110)
                                             ("decibels" . 2.5)
                                             ("raw" . 72000))))

;; TODO start
(use-package! telega
  :after (rainbow-identifiers)
  :when (system-name= "klingenberg-laptop" "klingenberg-tablet")
  :commands telega
  :config
  (telega-notifications-mode 1))

;; TODO end

(use-package! slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "2021-ZIH"
   :default t
   :token (auth-source-pick-first-password
           :host "2021-zih.slack.com"
           :user "klingenberg@fdy.tu-darmstadt.de")
   :subscribed-channels '(allgemein)
   :full-and-display-names t)

  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)
  (evil-define-key 'normal slack-mode-map
    ",c" 'slack-buffer-kill
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message)
  (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel))

(after! circe
  (set-irc-server! "chat.freenode.net"
                   `(:tls t
                     :port 6697
                     :nick "dakling"
                     :channels ("#emacs" "#guix"))))

(use-package! alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))


(use-package! elfeed
  :commands (eww elfeed elfeed-update)
  :config
  (setq
   elfeed-search-filter "+youtube"
   elfeed-feeds
   '(("https://www.zeitsprung.fm/feed/ogg/" podcast zeitsprung)
     ("https://kickermeetsdazn.podigee.io/feed/mp3/" podcast kmd fussball)
     ("https://audioboom.com/channels/2399216.rss" podcast nstaaf)
     ("http://fokus-fussball.de/feed/mp3/" podcast collina erben fussball)
     ("https://liebling-bosman.podigee.io/feed/mp3" podcast liebling bosman fussball)
     ("https://tribuenengespraech.podigee.io/feed/vorbis" podcast rasenfunk tribünengespräch fussball)
     ("https://feeds.feedburner.com/hacks-on-tap" podcast hacks on tap politics)
     ("https://lexfridman.com/feed/podcast" podcast lex friedman)
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
     ("http://www.reddit.com/r/baduk/.rss" go baduk reddit)
     ("http://tv.dfb.de/rss.php" dfb futsal fussball)
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
   :n "f" #'ace-link-eww)
  (map!
   :localleader
   :map eww-mode-map
   :n "v" #'my/youtube-watch
   :n "d" #'my/youtube-dl))

(use-package! ytdious
  :config
  (setq ytdious-invidious-api-url
        "https://invidious.tube"
        ;; "https://invidious.zee.li"
        ;; "https://invidious.tinfoil-hat.net"
        )
  (map!
   :map ytdious-mode-map
   :n "s" #'ytdious-search
   :n "S" #'ytdious-search-recent
   :n "c" #'ytdious-view-channel-at-point
   :n "RET" #'ytdious-play
   :n "o" #'ytdious-play
   :n "O" #'ytdious-play-continious
   :n "Q" #'ytdious-stop-continious))

;; (use-package! ytel-show
;;   :after ytel
;;   :bind (:map ytel-mode-map ("RET" . ytel-show)))


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
  (pinentry-start)
  :config
  (setenv "GPG_AGENT_INFO" nil)
  ;; (setq epg-pinentry-mode 'ask)
  (setq epg-pinentry-mode 'loopback))

(use-package! stumpwm-mode
  :when (system-name= "klingenberg-laptop" "klingenberg-tablet" "klingenberg-pc" "helensInfinitybook")
  :config
  (defun my/stumpwm-connect ()
    (interactive)
    (sly-connect "localhost" 4005))
  (defun my/activate-stump-mode ()
    (when (equalp
           (buffer-file-name)
           "/home/klingenberg/.dotfiles/stumpwm.lisp")
      (stumpwm-mode 1)))
  (add-hook 'lisp-mode-hook #'my/activate-stump-mode)
  (map! :localleader :map stumpwm-mode-map
        ;; "ef" #'stumpwm-eval-defun
        ;; "ee" #'stumpwm-eval-last-sexp
        "'" #'my/stumpwm-connect))

;; (use-package! eaf
;;   :defer t
;;   :config
;;   ;; (setq eaf-enable-debug t) ; should only be used when eaf is wigging out
;;   (eaf-setq eaf-browser-dark-mode "true") ; dark mode is overrated
;;   (setq eaf-browser-default-search-engine "duckduckgo")
;;   (eaf-setq eaf-browse-blank-page-url "https://duckduckgo.com"))

;; (use-package! eaf-evil ;; evil bindings in my browser
;;   :after eaf
;;   :config
;;   (setq eaf-evil-leader-keymap doom-leader-map)
;;   (setq eaf-evil-leader-key "SPC"))

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

(use-package! system-packages
  :config
  (setq my/pacmanfile-file "~/.dotfiles/pacmanfile.txt")
  (defun my/pacmanfile-visit ()
    (interactive)
    (find-file my/pacmanfile-file))
  (defun my/pacmanfile-sync ()
    (interactive)
    (async-shell-command "pacmanfile sync"))
  (defun my/pacman-update ()
    (interactive)
    (async-shell-command "yay -Syu"))
  (add-to-list 'system-packages-supported-package-managers
               '(yay .
                        ((default-sudo . nil)
                         (install . "yay -S")
                         (search . "yay -Ss")
                         (uninstall . "yay -Rs")
                         (update . "yay -Syu")
                         (clean-cache . "yay -Sc")
                         (log . "cat /var/log/pacman.log")
                         (get-info . "yay -Qi")
                         (get-info-remote . "yay -Si")
                         (list-files-provided-by . "yay -Ql")
                         (verify-all-packages . "yay -Qkk")
                         (verify-all-dependencies . "yay -Dk")
                         (remove-orphaned . "yay -Rns $(pacman -Qtdq)")
                         (list-installed-packages . "yay -Qe")
                         (list-installed-packages-all . "yay -Q")
                         (list-dependencies-of . "yay -Qi")
                         (noconfirm . "--noconfirm"))))
  (setq system-packages-use-sudo nil)
  (setq system-packages-package-manager 'yay))

(use-package! helm-system-packages
  :config
  (map!
   :map helm-map
   "M-j" #'helm-next-line
   "M-k" #'helm-previous-line
   "M-h" #'helm-find-files-up-one-level
   "M-l" #'helm-execute-persistent-action
   "M-w" #'helm-select-action
   "M-H" #'left-char
   "M-L" #'right-char
   "M-TAB" #'helm-toggle-visible-mark-forward)
  (defun my//add-package-to-pacfile (&optional install?)
    (let ((package (helm-get-selection)))
      (find-file my/pacmanfile-file)
      (goto-line 0)
      ;; check if package is already listed
      (if (search-forward-regexp (concat "^" package "$") nil t)
          (progn
            (message "package already installed"))
        (progn
          (write-region (format "%s\n" package) nil my/pacmanfile-file 'append)
          (save-buffer)
          (when install? (my/pacmanfile-sync))))
      (kill-buffer (current-buffer))))
  (defun my/add-package-to-pacfile-and-install ()
    (interactive)
    (my//add-package-to-pacfile t))
  (defun my/add-package-to-pacfile-no-install ()
    (interactive)
    (my//add-package-to-pacfile nil))
  (map!
   :map (helm-system-packages-pacman-map)
   "M-i" #'my/add-package-to-pacfile-and-install
   "M-A" #'my/add-package-to-pacfile-no-install
   "M-I" #'helm-system-packages-toggle-explicit
   "M-N" #'helm-system-packages-toggle-uninstalled
   "M-D" #'helm-system-packages-toggle-dependencies
   "M-O" #'helm-system-packages-toggle-orphans
   "M-L" #'helm-system-packages-toggle-locals
   "M-G" #'helm-system-packages-toggle-groups
   "C-]" #'helm-system-packages-toggle-descriptions))

(use-package! shelldon)

(use-package! igo-org
  :after org
  :config
  ;; (igo-org-setup)
  (autoload 'igo-sgf-mode "igo-sgf-mode")
  (add-to-list 'auto-mode-alist '("\\.sgf$" . igo-sgf-mode)))

;; (use-package! fira-code-mode
;;   :custom (fira-code-mode-disabled-ligatures (list "[]" "#{" "#(" "#_" "#_(" "x"))
;;   :config (global-fira-code-mode -1))

;; (plist-put! +ligatures-extra-symbols
;;   ;; org
;;   :name          "»"
;;   :src_block     "»"
;;   :src_block_end "«"
;;   :quote         "“"
;;   :quote_end     "”"
;;   ;; Functional
;;   :lambda        "λ"
;;   :def           "ƒ"
;;   :composition   "∘"
;;   :map           "↦"
;;   ;; Types
;;   :null          "∅"
;;   :true          "✓"
;;   :false         "✗"
;;   :int           "ℤ"
;;   :float         "ℝ"
;;   :str           "σ"
;;   :bool          "±"
;;   :list          "ƛ"
;;   ;; Flow
;;   :not           "￢"
;;   :in            "∈"
;;   :not-in        "∉"
;;   :and           "∧"
;;   :or            "∨"
;;   :for           "∀"
;;   :some          "∃"
;;   :return        "⟼"
;;   :yield         "⟻"
;;   ;; Other
;;   :union         "⋃"
;;   :intersect     "∩"
;;   :diff          "∖"
;;   :tuple         "⨂"
;;   :pipe          "" ;; FIXME: find a non-private char
;;   :dot           "•")

;; (use-package! rigpa

;;   :after (evil parsec symex)

;;   :config
;;   (setq rigpa-mode t)

;;   ;; custom config
;;   (setq rigpa-show-menus t)

;;   ;; navigating meta modes
;;   (map!
;;    :no "s-m s-m" 'rigpa-flashback-to-last-tower
;;     :n "C-<escape>" 'my-enter-tower-mode
;;     :n "M-<escape>" 'my-enter-mode-mode
;;     :n "s-<escape>" 'my-enter-mode-mode
;;     :n "M-<return>"
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-selected-level)
;;                      (let ((ground (rigpa--get-ground-buffer)))
;;                        (my-exit-mode-mode)
;;                        (switch-to-buffer ground)))
;;     :n "s-<return>"
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-selected-level)
;;                      (let ((ground (rigpa--get-ground-buffer)))
;;                        (my-exit-mode-mode)
;;                        (switch-to-buffer ground)))
;;     :n "C-<return>"
;;                    (lambda ()
;;                      (interactive)
;;                      (my-exit-tower-mode)
;;                      (my-enter-mode-mode))

;;    ;; indexed entry to various modes
;;     :n "s-n" 'evil-normal-state
;;     :n "s-y"        ; symex mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "symex"))
;;     "s-w"        ; window mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "window"))
;;     "s-v"        ; view mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "view"))
;;     "s-x"        ; char mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "char"))
;;     "s-a"        ; activity mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "activity"))
;;     "s-z"        ; text mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "text"))
;;     "s-g"        ; history mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "history"))
;;     "s-i"        ; system mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "system"))
;;     "s-b"        ; buffer mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "buffer"))
;;     "s-f"        ; file mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "file"))
;;     "s-t"        ; tab mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "tab"))
;;     "s-l"        ; line mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "line"))
;;    "s-e"        ; application mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "application"))
;;    "s-r"        ; word mode
;;                    (lambda ()
;;                      (interactive)
;;                      (rigpa-enter-mode "word"))))

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
