;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq! user-full-name "Dario Klingenberg"
       user-mail-address "dario@ellamind.com")

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

(setq! doom-theme 'doom-palenight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil ', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq! display-line-numbers-type 'absolute)


(setq doom-localleader-key "-")

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
(setq! evil-respect-visual-line-mode t)
(setq! evil-search-module 'evil-search)
(setq!
 isearch-lax-whitespace t
 isearch-regexp-lax-whitespace t
 search-whitespace-regexp (purecopy "[ \t\r\n]+")) ; TODO: only do this is some modes?

(setq ns-alternate-modifier 'meta)       ; Left Option = Meta
(setq ns-right-alternate-modifier 'none) ; Right Option = Option key


(setq! evil-collection-setup-minibuffer t)
(setq! evil-ex-substitute-global t)

(setq! +evil-want-o/O-to-continue-comments nil)
(setq! display-time-24hr-format t
       display-time-default-load-average nil )

(setq! langtool-default-language "en-GB")
(setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
;; jinx: faster spell-checking (replaces flyspell once installed via doom sync)
;; After `doom sync`, you can disable the spell module in init.el and uncomment
;; the global keybindings below to fully switch over.
(use-package! jinx
  :hook (text-mode . jinx-mode)
  :config
  (setq jinx-languages "en_GB"))

(setq! flycheck-checker-error-threshold 10000)


(display-time-mode 1)

(setq! initial-major-mode 'lisp-interaction-mode
      doom-scratch-initial-major-mode 'lisp-interaction-mode)

(setq! +lookup-provider-url-alist
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
        ("Github" "https://github.com/search?ref=simplesearch&q=%s")
        ("Youtube" "https://youtube.com/results?aq=f&oq=&search_query=%s")
        ("Doom Emacs issues" "https://github.com/doomemacs/doomemacs/issues?q=is%%3Aissue+%s")))

(setq! browse-url-browser-function
       (if (eq system-type 'gnu/linux) 'browse-url-firefox 'browse-url-default-browser))
(setq-default abbrev-mode t)


(setq! abbrev-file-name "~/Dropbox/Dario/abbrev.el")

(cond
 ((system-name= "klingenberg-laptop" "klingenberg-pc" "helensInfinitybook")
  (add-load-path! "/usr/share/emacs/site-lisp/")))


(use-package! helm
  :defer t
  :config
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-buffer-not-found))
  (map!
   :map helm-map
   "M-j" #'helm-next-line
   "M-k" #'helm-previous-line
   "M-h" #'helm-find-files-up-one-level
   "M-l" #'helm-execute-persistent-action
   ;; "M-w" #'helm-select-action
   "M-H" #'left-char
   "M-L" #'right-char
   "M-TAB" #'helm-toggle-visible-mark-forward
   :map helm-find-files-map
   "M-l" #'helm-ff-RET
   "M-k" #'helm-previous-line ; needed here again to override default function
   "C-l" nil
   "M-y" #'helm-ff-run-copy-file
   "M-r" #'helm-ff-run-rename-file
   "M-s" #'helm-ff-run-find-file-as-root
   "M--" #'helm-ff-run-marked-files-in-dired
   "M-o" #'helm-ff-run-switch-other-window
   "M-O" #'helm-ff-run-switch-other-frame
   "M-RET" #'helm-ff-run-open-file-with-default-tool
   :map helm-buffer-map
   "M-l" #'helm-maybe-exit-minibuffer
   "M-d" #'helm-buffer-run-kill-persistent
   :map doom-leader-buffer-map
   "b" #'helm-mini)
  (setq! helm-move-to-line-cycle-in-source nil)
  (setq! helm-truncate-lines nil)
  (setq! helm-buffer-max-length nil)
  (setq! helm-buffers-truncate-lines nil))

(use-package! ultra-scroll
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(after! dirvish
  (map! :map dirvish-mode-map
        :ng "f" #'dirvish-narrow
        :ng "gf" #'dirvish-file-info-menu))

(use-package! dired-x
 :hook (dired-mode . dired-omit-mode)
 :config
 (setq dired-omit-verbose nil
       dired-omit-files
       (concat dired-omit-files
               "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$")))

(after! undo-fu
  (setq! undo-fu-allow-undo-in-region t))

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)
  (after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (map! :map (helm-map minibuffer-mode-map minibuffer-local-map minibuffer-local-ns-map minibuffer-local-isearch-map minibuffer-local-completion-map evil-command-line-map)
          "<escape>" #'evil-normal-state) ; why did this become necessary?
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)))

(use-package! projectile
  :defer t
  :config
  (setq projectile-per-project-compilation-buffer t))


(after! sly
  (setq! inferior-lisp-program (cond
                                ((system-name= "klingenberg-tablet")  "~/.local/bin/.run-sbcl.sh")
                                (t "/usr/bin/sbcl --load /home/klingenberg/quicklisp.lisp")))
  (map! :map sly-mrepl-mode-map
        :ni "M-k" #'sly-mrepl-previous-prompt
        :ni "M-j" #'sly-mrepl-next-prompt)
  (map! :localleader
        :map sly-mrepl-mode-map
        :n "g" #'helm-comint-prompts-all))

(after! evil-snipe (evil-snipe-mode -1))

(setq! magit-repository-directories '(("~/" . 1)))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(map!
 :after evil
 :map general-override-mode-map
 :v "s" #'evil-surround-region)


(after! org
  (setq! org-id-link-to-org-use-id t)
  (setq! org-file-apps
        (remove (assoc "\\.pdf\\'" org-file-apps)
                org-file-apps))
  (setq! org-todo-keywords (list "TODO" "PROG" "|" "DONE" "BLOC" "KILL"))
  (setq! org-todo-keyword-faces
        '(("DONE" . font-lock-comment-face)))
  (setq! org-capture-templates
        `(("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox") "* TODO %?
%i
%a" :prepend t) ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox") "* %u %?
%i
%a" :prepend t) ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file) "* %U %?
%i
%a" :prepend t) ("p" "Templates for projects") ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?
%i
%a" :prepend t) ("pn" "Project-local notes" entry (file+headline +org-capture-project-notes-file "Inbox") "* %U %?
%i
%a" :prepend t) ("pc" "Project-local changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?
%i
%a" :prepend t) ("o" "Centralized templates for projects") ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?
 %i
 %a" :heading "Tasks" :prepend nil ) ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?
 %i
 %a" :heading "Notes" :prepend t) ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?
 %i
 %a" :heading "Changelog" :prepend t))))

(use-package! org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "◈" "◇" "▸")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-block-name t
        org-modern-keyword t
        org-modern-todo t
        org-modern-priority t
        org-modern-tag t))

(add-hook! 'org-mode-hook (variable-pitch-mode 1))

(map!
 :localleader
 :after org
 :map org-mode-map
 "-" nil
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

(map!
 :after org
 :map evil-org-mode-map
 :nvi "M-k" nil
 :nvi "M-j" nil
 :nvi "M-l" nil
 :nvi "C-M-h" #'org-metaleft
 :nvi "C-M-k" #'org-metaup
 :nvi "C-M-j" #'org-metadown
 :nvi "C-M-l" #'org-metaright)

(use-package! org-super-links
  :after org
  :config
  (map! :localleader
        :map org-mode-map
        ("ss" #'org-super-links-link)
        ("ls" #'org-super-links-store-link)
        ("lS" #'org-super-links-insert-link)))

(setq! smerge-command-prefix "#")

(set-popup-rules!
  '(("^\\*Async Shell Command\\*" :slot -1 :size 20)))
;;; Defining some useful functions

(when (eq system-type 'gnu/linux)
  (defun shutdown ()
    (interactive)
    (run-hook-with-args-until-failure 'kill-emacs-query-functions)
    (cond
     ((system-name= "klingenberg-laptop" "klingenberg-tablet") (async-shell-command "sudo shutdown"))
     (t (shell-command "shutdown now"))))

  (defun reboot ()
    (interactive)
    (run-hook-with-args-until-failure 'kill-emacs-query-functions)
    (async-shell-command "sudo reboot")))

(defun my/open-in-external-app ()
  (interactive)
  (let ((process-connection-type nil ))
    (helm-find-file-extern (buffer-file-name))
    ;; (counsel-find-file-extern (buffer-file-name))
    ))

(when (eq system-type 'gnu/linux)
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
        (shell-command "xinput --map-to-output $(xinput list --id-only \"HDX HDX DIGITIZER Pen (0)\") eDP-1")))))

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
     (alert mesg :title "Reminder")))
  (message "Made alert for %s at %s" mesg time))

(after! bash-completion
  (setq! bash-completion-nospace t))

(defun my/close-buffer ()
  (interactive)
  (if (< 1 (length (window-list)))
      (evil-window-delete)
    (qtile/window-close)))

(defun my/run-command-ssh (server &rest cmds)
  "Run COMMAND on SERVER, assumes that you set it up properly"
  (async-shell-command (concat "ssh " server " '" (mapconcat 'identity cmds "; ")"'")))

(defun qtile/move-focus (direction)
  (with-current-buffer "*scratch*"
      (shell-command-to-string (format "qtile cmd-obj -o layout -f %s" direction))))

(defun qtile/emacs-window-right ()
  (interactive)
  (condition-case nil
      (evil-window-right 1)
    (error (qtile/move-focus "right"))))

(defun qtile/emacs-window-left ()
  (interactive)
  (condition-case nil
      (evil-window-left 1)
    (error (qtile/move-focus "left"))))

(defun qtile/emacs-window-up ()
  (interactive)
  (condition-case nil
      (evil-window-up 1)
    (error (qtile/move-focus "up"))))

(defun qtile/emacs-window-down ()
  (interactive)
  ;; (evil-window-down 1)
  (condition-case nil
      (evil-window-down 1)
    (error (qtile/move-focus "down"))))

(defun qtile/window-close ()
  (with-current-buffer "*scratch*"
      (shell-command-to-string "qtile cmd-obj -o window -f kill")))

(defun my/create-super-bindings ()
  "Create super-key bindings for window management and quick access."
  (map!
   :n
   "s-w" '(other-window :which-key "other window")
   "s-M-l" 'enlarge-window-horizontally
   "s-M-h" 'shrink-window-horizontally
   "s-M-j" 'enlarge-window
   "s-M-k" 'shrink-window
   "s-c" 'my/close-buffer
   "s-q" 'my/get-rid-of-mouse
   "s-m" 'delete-other-windows
   "s-<f1>" '+eshell/here
   "C-s-<f1>" '+eshell/toggle
   "s-<f2>" '(lambda () (interactive)
             (funcall browse-url-browser-function "" "-new-tab"))
   "s-<f3>" 'deer
   "s-<f4>" '(lambda () (interactive)
             (mu4e))
   "s-x" 'helm-M-x
   "s-f" 'helm-find-files
   "s-p" 'helm-projectile
   "s-g" 'helm-system-packages
   "s-b" 'helm-mini
   "s-P" '+pass/copy-secret))

(map!
   :g "C-s-l" #'evil-window-right
   :g "C-s-h" #'evil-window-left
   :g "C-s-j" #'evil-window-down
   :g "C-s-k" #'evil-window-up)

(my/create-super-bindings)

(after! mu4e
  (setq! mu4e-context-policy 'always-ask
         mu4e-compose-context-policy 'always-ask)

  (set-email-account! "gmail"
                      ;; Under each account, set the account-specific variables you want.
                      '(;; (mu4e-sent-messages-behavior . sent)
                        (mu4e-sent-messages-behavior . delete)
                        ;; (mu4e-compose-signature-auto-include nil )
                        (message-send-mail-function . smtpmail-send-it)
                        (mu4e-sent-folder . "/gmail/sent")
                        (mu4e-drafts-folder . "/gmail/Drafts")
                        (user-mail-address . "dario.klingenberg@gmail.com")
                        (smtpmail-smtp-user . "dario.klingenberg@gmail.com")
                        (smtpmail-smtp-server . "smtp.gmail.com")
                        (smtpmail-smtp-service . 465)
                        (smtpmail-stream-type . starttls)
                        (user-full-name . "Dario Klingenberg")
                        (mu4e-compose-signature . nil )
                        ;; (org-msg-signature nil )
                        )
                      t)
  (set-email-account! "web"
                      '((mu4e-sent-messages-behavior . sent)
                        ;; (mu4e-compose-signature-auto-include nil )
                        (mu4e-sent-folder . "/web/Sent Items")
                        (mu4e-drafts-folder . "/web/Drafts")
                        ;; (smtpmail-smtp-server . "smtp.web.de")
                        (smtpmail-smtp-user . "dario.klingenberg@web.de")
                        (user-mail-address . "dario.klingenberg@web.de")
                        ;; (smtpmail-smtp-service . 587)
                        (smtpmail-stream-type . starttls)
                        (user-full-name . "dario")
                        (mu4e-compose-signature . nil )
                        ;; (org-msg-signature nil )
                        )))

(use-package! mu4e-alert
  :after-call mu4e-index-updated-hook
  :config
  (setq! mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND NOT maildir:/Web/INBOX/")
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-set-default-style (if (eq system-type 'gnu/linux) 'libnotify 'osx-notifier))
  (alert-add-rule
   :category "mu4e-alert"
   :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode)))
   :continue t))

(after! mu4e
  (defalias 'mu4e~start 'mu4e--start)
  (defalias 'mu4e~main-view 'mu4e--main-view)
  (setq! shr-use-colors nil)
  (setq! message-subject-re-regexp
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
  (add-hook! 'mu4e-compose-mode-hook
    (auto-fill-mode 1))
  (setq! mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version))
  (setq! mu4e-update-interval 120)
  (setq! mu4e-compose-signature-auto-include t)
  (setq! mu4e-enable-notifications t)
  (customize-set-variable 'mu4e-headers-leave-behavior 'apply)
  (setq! mu4e-view-use-gnus t)
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  (require 'org-agenda)
  (setq! gnus-icalendar-org-capture-file "~/org/notes.org")
  (setq! gnus-icalendar-org-capture-headline '("Inbox"))
  (gnus-icalendar-org-setup))

;; keybindings

(map!
 "C-g" #'keyboard-quit
 "M-i" nil
 :n "gb" #'pop-tag-mark
 "M-p" #'helm-show-kill-ring
 :n "M-y" #'helm-show-kill-ring
 ;; :n "s" #'avy-goto-char-timer
 ;; :n "S" #'avy-goto-char-timer
 )

(map! :leader
      "SPC" #'execute-extended-command
      "++" #'+popup/toggle
      "+ RET" #'+popup/other
      "lm" #'bookmark-set
      "er" #'eval-expression
      "w w" #'evil-switch-to-windows-last-buffer
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
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-j") nil )
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-k") nil )
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-h") nil )
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-l") nil )
(after!
  lispy
  (lispy-set-key-theme '(lispy c-digits))
  (setq! lispy-colon-p nil )) ;; disable single-key bindings

(after!
  lispyville
  (setq! lispyville-key-theme '(operators
                               c-w
                               additional
                               prettify
                               additional-insert
                               (escape insert)
                               slurp/barf-cp))
  (map!
   :map lispyville-mode-map
   :ni "C-H" #'lispy-left
   :ni "C-L" #'lispyville-next-closing
   :n "C-J" #'lispy-down
   :n "C-K" #'lispy-up
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
   :map (lispy-mode-map lispy-mode-map-lispy)
   "M-RET" nil
   "M-<return>" nil
   "[" nil
   "]" nil )

  (map!
   :localleader
   :map (emacs-lisp-mode-map lisp-mode-map scheme-mode-map)
   "el" #'lispy-eval
   "1" #'lispy-describe-inline
   "2" #'lispy-arglist-inline
   "x" #'lispy-x))

(map!
 :localleader
 :map julia-mode-map
 "ee" #'julia-repl-send-region-or-line
 "eb" #'julia-repl-send-buffer)

(defun my/python-shell-send-main ()
   (interactive)
   (with-temp-buffer
     (insert-file-contents "./main.py")
     ;; (python-shell-send-string (concat (buffer-string) "\nmain()"))
     (python-shell-send-string (buffer-string))))

 (defun my/python-shell-send-buffer ()
   (interactive)
   (if (file-exists-p "./main.py")
       (my/python-shell-send-main)
     (python-shell-send-buffer)))

 (map!
  :after python
  :localleader
  :map (python-mode-map python-ts-mode-map)
  "ef" #'python-shell-send-defun
  "ee" #'python-shell-send-statement
  "eb" #'my/python-shell-send-buffer
  "eB" #'python-shell-send-buffer
  "em" #'my/python-shell-send-main)

(defun my/haskell-load-and-run ()
  "Loads and runs the current Haskell file main function."
  (interactive)
  (haskell-process-load-file)
  (haskell-interactive-mode-run-expr "main"))

(defun my/haskell-insert-type ()
  (interactive)
  (let ((current-prefix-arg '-)) ; simulate pressing C-u
    (haskell-process-do-type)))

(map!
 :localleader
 :map haskell-mode-map
 "t" #'my/haskell-insert-type
 "eB" #'haskell-process-load-file
 "eb" #'my/haskell-load-and-run)

;; doc-view mode
(after! doc-view
 (map!
  :map 'doc-view-mode-map
  :n "j" #'doc-view-next-page
  :n "k" #'doc-view-previous-page
  :n "<down>" #'doc-view-next-page
  :n "<up>" #'doc-view-previous-page))

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
(setq! +latex-viewers '(pdf-tools))
(setq my/latex-macro-directory
      (cond
       ((system-name= "klingenberg-laptop" "klingenberg-tablet") "~/Documents/conferences/latex_macros/")
       ((system-name= "klingenberg-pc") "~/Documents/conferences/latex_macros/")))
(setq! reftex-default-bibliography (list (concat my/latex-macro-directory "bibliography.bib")))
(setq my/latex-bibliography-file (concat my/latex-macro-directory "bibliography.bib"))
(setq my/latex-macro-file (concat my/latex-macro-directory "dakling.sty"))

(require 'doi-utils)
(use-package! doi-utils
  :after (latex bibtex LaTeX TeX))

(map!
 :localleader
 :map bibtex-mode-map
 :n "d" #'biblio-doi-insert-bibtex)


(use-package! latex
  :defer t
  :config
  (add-to-list 'TeX-command-list '("LaTeXMk (cont.)" "latexmk %(latexmk-out) %(file-line-error) %(output-dir) -halt-on-error -pvc %`%(extraopts) %S%(mode)%' %t" TeX-run-format nil
                (LaTeX-mode docTeX-mode)
                :help "Run LaTeXMk continuously"))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq! reftex-label-alist '(AMSTeX))
  (setq! reftex-ref-style-alist
         '(("Cleveref" "cleveref"
            (("\\cref" 99)
             ("\\Cref" 67)
             ("\\labelcref" 108)))
           ("Default" t
            (("\\ref" 13)
             ("\\Ref" 82)
             ("\\footref" 110)
             ("\\pageref" 112)))))
  (remove-hook! '(TeX-mode-hook LaTeX-mode-hook latex-mode-hook tex-mode-hook) #'visual-line-mode)
  (add-hook! '(plain-TeX-mode-hook tex-mode-hook latex-mode-hook) #'LaTeX-mode)
  (add-hook! '(TeX-mode-hook LaTeX-mode-hook latex-mode-hook tex-mode-hook)
             :append
    (visual-line-mode -1)
    (variable-pitch-mode 1)
    (auto-fill-mode 1)
    (setq-local TeX-electric-math (cons "\\(" "")) ; gets closed automatically apparently
    (add-hook! '(before-save-hook) #'reftex-parse-all)
    ;; (setq-local TeX-electric-math (cons "\\(" "\\)"))
    ))

(use-package! cdlatex
  :defer t
  :config
  (setq cdlatex-math-symbol-prefix ?#)
  (map! :map cdlatex-mode-map
        "#" #'cdlatex-math-symbol)
  (map! :map org-cdlatex-mode-map
        "#" #'cdlatex-math-symbol))

(use-package! font-latex
  :defer t)

(defun my/reftex-fix-cleveref-ref ()
  (interactive)
  (replace-regexp "}.?\\l?a?b?e?l?cref{" "," nil (point-at-bol) (point-at-eol))
  (end-of-line))

(map!
 :map (TeX-mode-map LaTeX-mode-map)
 "C-c e" #'LaTeX-environment
 "C-c s" #'LaTeX-section
 "C-c m" #'TeX-insert-macro
 "C-c l" (lambda () (interactive) (progn (reftex-cleveref-labelcref) (my/reftex-fix-cleveref-ref)))
 "C-c r" (lambda () (interactive) (progn (reftex-cleveref-cref) (my/reftex-fix-cleveref-ref)))
 "C-c z" #'reftex-citation
 "C-c C-l" (lambda () (interactive) (progn (reftex-cleveref-labelcref) (my/reftex-fix-cleveref-ref)))
 "C-c C-r" (lambda () (interactive) (progn (reftex-cleveref-cref) (my/reftex-fix-cleveref-ref)))
 "C-c C-z" #'reftex-citation)
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
 "rl" #'reftex-cleveref-labelcref
 "rc" #'reftex-citation
 "og" (lambda () (interactive) (find-file my/latex-macro-file))
 "ob" (lambda () (interactive) (find-file my/latex-bibliography-file)))
(map!
 :map (reftex-select-shared-map)
 :n "U" #'reftex-parse-one
 :n "gr" #'reftex-parse-one)

(setq reftex-cite-format
      '((?t . "\\citet[]{%l}")
        (?p . "\\citep[]{%l}")))

(defun my/latexdiff ()
  "Create a pdf showing the differences between some old revision and a new git revision of the current file."
  (interactive)
  (labels ((first-word-of (str)
                          (car (split-string str))))
    (let*
        ((commit-hashes (magit-git-lines "log" "--format=oneline" "--format=%H %s %cn, %cr, %cD"))
         (old-revision (first-word-of (completing-read "old revision: " (append (list "HEAD^ (previous version)" "HEAD (current version)") commit-hashes))))
         (new-revision (first-word-of (completing-read "new revision: " (append (list "HEAD (current version)" "HEAD^ (previous version)") commit-hashes))))
         (file (buffer-file-name)))
      (async-shell-command (format "latexdiff-vc -r %s -r %s %s --git --pdf" old-revision new-revision file)))))

(use-package! evil-tex
  :after-call LaTeX-mode-hook
  :config
  (add-hook 'LaTeX-mode-hook #'evil-tex-mode))

;; Keybindings
(map! :map doom-leader-open-map
      "c" #'claude-code-ide-menu
      "lL" #'(lambda () (interactive) (let ((current-prefix-arg t)) (call-interactively #'gptel)))
      "lf" #'gptel-add-file
      "ld" (cmd! (gptel-add-file default-directory))
      "lp" (cmd! (gptel-add-file (projectile-project-root))))
(map! :map gptel-mode-map
        "RET" #'gptel-send              ;; TODO check if this is good
        "C-c C-c" #'gptel-send
        "C-RET" #'evil-ret)
(map! :map gptel-mode-map
        :localleader
        "d" (cmd! (gptel-add-file default-directory))
        "P" (cmd! (gptel-add-file (projectile-project-root)))
        "RET" #'gptel-send
        "C-RET" #'gptel-send-newline
        "i" #'gptel-add-buffer
        "f" #'gptel-add-file
        "r" #'gptel-context-remove-all
        "t" #'gptel-select-prefix
        "s" #'gptel-switch-model
        "o" #'gptel-open-log
        "n" #'gptel-next-message
        "p" #'gptel-previous-message
        "k" #'gptel-delete-current
        "c" #'gptel-new-chat
        "q" #'kill-buffer)

(use-package! claude-code-ide
  :bind
  :config
  ;; (claude-code-ide-emacs-tools-setup)   ; Optionally enable Emacs MCP tools
  (setq! my/claude-text-snippets-list
         '("Please use the AskUserQuestion tool to ask for clarification on anything that is unclear."
           "Be very concise. Sacrifice grammar for the sake of concision."
           "Please check if a linear issue for the current task exists, and, if not, create on/e before starting to work on this. Ask questions if you need information about some of the task details."
           "Please create a TODO list to track progress."
           "Always use the explore subagents if you need more context."
           "Please start five parallel explore subagents to explore solutions."
           "Please give me five different solution prototypes."))
  (defun my/claude-snippet-menu ()
    (interactive)
    (let ((snippet (completing-read "Snippet to insert:"
                                    my/claude-text-snippets-list)))
      (send-string (current-buffer) (concat " " snippet " "))))
  (map! :map vterm-mode-map
        "C-c C-r"  #'my/claude-snippet-menu))

(use-package! prompt-compose
  :after claude-code-ide
  :init
  (map! :map doom-leader-open-map
        "P" #'prompt-compose)
  (map! :map vterm-mode-map
        "s-P" #'prompt-compose)
  :config
  (prompt-compose-setup-default-backends))


(use-package! goose
  :commands (goose goose-transient)
  :config
  (setq goose-program-name "goose")  ; Ensure goose CLI is in PATH
  ;; Skip the session name prompt — auto-generate timestamp labels
  (advice-add #'goose-start-session :override
              (defun my/goose-start-session (&optional name)
                "Start a Goose session without prompting for a name."
                (interactive)
                (let ((label (goose--session-label name))
                      (args  (goose--build-args name)))
                  (goose--run-session label args))))
  ;; Run goose with exec so the process exits when goose exits
  (define-advice goose--run-session (:around (fn label args) exec-goose)
    (let ((goose-program-name (concat "exec " goose-program-name)))
      (funcall fn label args)))
  ;; Kill the buffer when the goose process exits (e.g. /exit)
  (add-hook 'goose-mode-hook
            (lambda () (setq-local vterm-kill-buffer-on-exit t)))
  :hook
  (goose-mode . (lambda () (display-line-numbers-mode -1))))

(map! :leader
      "l g" #'goose-transient)  ; Open goose with SPC l g

(use-package! minimax-agent
  :config
  ;; Configuration options
  (setq minimax-agent-api-key (password-store-get "minimax-api-key")))


(use-package! dap-mode
  :defer t
  :config
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (require 'dap-python))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(add-hook! '(yaml-mode-hook)
 :append
 (visual-line-mode -1))




(use-package! vterm
  :defer t
  :config
  (map!
   :map vterm-mode-map
   :ni "M-p" #'vterm-send-C-p
   :ni "M-n" #'vterm-send-C-n
   :ni "M-k" #'vterm-previous-prompt
   :ni "M-j" #'vterm-next-prompt))

(use-package! pdf-tools
  :defer t
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-auto-slice-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
  (evil-collection-init 'pdf)
  (add-to-list 'desktop-locals-to-save 'pdf-view-register-alist)
  ;; (setq! pdf-view-midnight-colors '("WhiteSmoke" . "gray16"))
  (progn
   (map!
    :map pdf-view-mode-map
    :n "J" #'pdf-view-next-page
    :n "K" #'pdf-view-previous-page
    :n "zm" #'pdf-view-themed-minor-mode
    "-" nil
    "SPC" nil )
   (map!
    :localleader
    :map pdf-view-mode-map
    "at" #'pdf-annot-add-text-annotation
    "ah" #'pdf-annot-add-highlight-markup-annotation
    "ao" #'pdf-annot-add-strikeout-markup-annotation
    "aD" #'pdf-annot-delete
    "al" #'pdf-annot-list-annotations
    "d" #'org-ref-pdf-to-bibtex)))

(use-package! pulseaudio-control
  :defer t
  :when (system-name= "klingenberg-tablet")
  :custom
  (pulseaudio-control-volume-step "5%")
  :config
  (setq! pulseaudio-control--volume-maximum '(("percent" . 110)
                                             ("decibels" . 2.5)
                                             ("raw" . 72000))))





(use-package! alert
  :commands (alert)
  :init
  (setq! alert-default-style (if (eq system-type 'gnu/linux) 'libnotify 'osx-notifier)))

(use-package! elfeed
  :commands (eww elfeed elfeed-update)
  :init (setq! elfeed-search-title-max-width 150)
  :config
  (setq!
   elfeed-search-filter "@12-months-ago AI"
   elfeed-feeds
   '(("https://www.anthropic.com/news/rss.xml" anthropic AI)
     ("https://openai.com/blog/rss/" OpenAI AI)
     ("https://deepmind.google/discover/blog/feed/" google deepmind AI)
     ("https://blogs.microsoft.com/ai/feed/" microsoft ai)
     ("https://aiweekly.co/rss" aiweekly AI)
     ("https://venturebeat.com/feed/" venturebeat AI)
     ("https://techcrunch.com/feed/" techcrunch AI)
     ("http://arxiv.org/rss/cs.LG" arxiv ML AI)
     ("http://arxiv.org/rss/cs.AI" arxiv AI)
     ("http://news.mit.edu/rss/topic/artificial-intelligence2" MIT AI)
     ("https://www.technologyreview.com/feed/" AI)
     ("https://bair.berkeley.edu/blog/feed.xml" AI)
     ("http://proceedings.mlr.press//feed.xml" AI)
     ("https://distill.pub/rss.xml" AI)
     ("https://www.wired.com/feed/tag/ai/latest/rss" AI)
     ("https://machinelearningmastery.com/blog/feed/" AI)
     ("https://jalammar.github.io/feed.xml" AI)
     ("https://blogs.nvidia.com/blog/category/ai/feed/" AI)
     ("https://aws.amazon.com/blogs/machine-learning/feed/" AI)
     ("http://feeds.feedburner.com/FeaturedBlogPosts-DataScienceCentral?format=xml" AI)
     ("https://www.sciencedaily.com/rss/computers_math/artificial_intelligence.xml" AI)
     ("https://www.therundown.ai/feed" AI)
     ("https://www.inference.vc/rss" AI)
     ("https://www.fast.ai/atom.xml" AI)
     ("http://davidstutz.de/feed" AI)
     ("https://danieltakeshi.github.io/feed.xml" AI)
     ("https://mlinproduction.com/feed" AI)
     ("https://aiweirdness.com/rss" AI)
     ("https://becominghuman.ai/feed" AI)))


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

 
  (defun elfeed-open-item-generic (entry)
    (cond
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
        :n "u" #'elfeed-update
        :n "o" #'elfeed-open-generic
        :n "e" #'elfeed-eww-open
        :n "b" #'elfeed-firefox-open))


(after! eww
  (map!
   :map eww-mode-map
   :n "M-h" #'eww-back-url
   :n "M-l" #'eww-forward-url
   :n "M-y" #'eww-copy-page-url
   :n "f" #'ace-link-eww)
)



(use-package! pinentry
  :defer t
  ;; :init
  ;; (pinentry-start)
  :config
  (pinentry-start)
  (setenv "GPG_AGENT_INFO" nil )
  ;; (setq! epg-pinentry-mode 'ask)
  (setq! epg-pinentry-mode 'loopback))


(use-package! system-packages
  :defer t
  :config
  (setq! my/pacmanfile-file "~/.dotfiles/pacmanfile.txt")
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
                        ((default-sudo . nil )
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
  (setq! system-packages-use-sudo nil )
  (setq! system-packages-package-manager 'yay))

(use-package! helm-system-packages
  :defer t
  :config
  (defun my//add-package-to-pacfile (&optional install?)
    (let ((package (helm-get-selection)))
      (find-file my/pacmanfile-file)
      (goto-line 0)
      ;; check if package is already listed
      (if (search-forward-regexp (concat "^" package "$") nil  t)
          (progn
            (message "package already installed"))
        (progn
          (write-region (format "%s\n" package) nil  my/pacmanfile-file 'append)
          (save-buffer)
          (when install? (my/pacmanfile-sync))))
      (kill-buffer (current-buffer))))
  (defun my/add-package-to-pacfile-and-install ()
    (interactive)
    (my//add-package-to-pacfile t))
  (defun my/add-package-to-pacfile-no-install ()
    (interactive)
    (my//add-package-to-pacfile nil ))
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

(after! shelldon
; below is the configuration I use, take what you want...
      ; tell bash this shell is interactive (Linux only; -ic can cause issues on macOS)
      (when (eq system-type 'gnu/linux)
        (setq shell-command-switch "-ic"))
      ; recursive minibuffers for nested autocompletion from minibuffer commands,
      ; to e.g. interactively select from the kill-ring
      (setq enable-recursive-minibuffers t)
      ; comint output may contain SGR control sequences that may be translated into
      ; text properties if emacs has something equivalent. This requires special
      ; processing.
      (add-hook 'shelldon-mode-hook 'ansi-color-for-comint-mode-on)
      (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
      (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
      ; Tell Emacs how to display shelldon’s output buffers
      (add-to-list 'display-buffer-alist
                   '("*shelldon:"
                     (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window display-buffer-pop-up-window)
                     (side . right)
                     (slot . 0)
                     (window-width . 80))))

(map!
 :leader
 :g "d" #'shelldon
 :g "C-d" #'shelldon-cd
 :g "D" #'shelldon-loop)

(use-package! igo-org
  :after org
  :config
  (igo-org-setup)
  (autoload 'igo-sgf-mode "igo-sgf-mode")
  (add-to-list 'auto-mode-alist '("\\.sgf$" . igo-sgf-mode)))

(use-package! beacon
  :defer t
  :config
  (beacon-mode 1))

(use-package! systemd
  :defer t)

(when (eq system-type 'gnu/linux)
  (defun aur-checker ()
    (run-at-time
     "15 minutes"
     (* 3600 2)
     (lambda ()
       (let ((aur-failures
              (with-temp-buffer
                (insert-file-contents "~/.cache/aur-failures.log")
                (string-to-number (buffer-string)))))
         (when (< 0 aur-failures)
           (my/make-alert nil  (format "%s of my AUR PKGBUILDS failed" aur-failures)))))))

  (aur-checker))

;; load my custom scripts
(load "~/Dropbox/Helen+Dario/washing-machine-timer.el" t t)
(load "~/Dropbox/Helen+Dario/einkaufsliste/interactiveEnterLisp.el" t t)

;; Redirect Customize to a separate file
(setq custom-file (expand-file-name "custom.el" doom-user-dir))
(load custom-file 'noerror)
