;; -*- lexical-binding: t -*-

;;; package  --- Summary
;; my emacs config
;;; Commentary:
;; I use evil-mode everywhere, and the config is based on use-package and general
;;; Code:


(setq lexical-binding t)
;;; speed up startup using Ambrevar's suggestions: (reset later by loading gcmh)
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.6)

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar-reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist
                file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
(add-hook 'after-init-hook #'ambrevar-reset-file-name-handler-alist)

;;; 
(setq user-emacs-directory "~/.config/emacs/")
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; todo change for 27
;; (setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
;; todo change for 27
;; (package-initialize)

;; Bootstrap quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; not needed if using quelpa
;; ;; Bootstrap `use-package'
;; (unless (package-installed-p 'use-package) ; unless it is already installed
;;   (package-refresh-contents) ; updage packages archive
;;   (package-install 'use-package)) ; and install the most recent version of use-package

;; (require 'use-package)

(setq use-package-always-ensure t)

;; defaults suggested by blog and extended by me
(setq frame-inhibit-implied-resize t) ; reddit suggestion by hlissner
;; (setq initial-major-mode 'fundamental-mode) ; s.a.
(setq delete-old-versions -1)		; delete excess backup versions silently
(setq version-control t)		; use version control
(setq vc-make-backup-files t)		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))) ; which directory to put backups file
(setq vc-follow-symlinks t)				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms `((".*"  ,(concat user-emacs-directory "auto-save-list/") t))) ;transform backups file name
(setq inhibit-startup-screen t)	; inhibit startup screen
(setq ring-bell-function 'ignore)	; silent bell when you make a mistake
(set-language-environment "UTF-8")
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
;; (setq default-fill-column 80)		; toggle wrapping text at the 80th character
;; (setq default-major-mode 'text-mode)
(blink-cursor-mode -1)
(setq revert-without-query '("*pdf")) ; automatically revert pdf-files
(add-to-list 'default-frame-alist
             '(font . "Source Code Pro"))
(add-hook 'focus-out-hook (lambda () (when buffer-file-name (save-buffer))))
(recentf-mode 1)
(setq delete-by-moving-to-trash t)
(setq-default indent-tabs-mode nil)

(display-time-mode)
(setq display-time-24hr-format t
      display-time-default-load-average nil)

(setq
 initial-scratch-message
 "(print \"Welcome\") \n \n(async-shell-command \"yay --sudoloop -Syu\") \n \n(shell-command-to-string \"acpi -b\") \n")
                                        ; print a default message in the empty scratch buffer opened at startup

(defalias 'yes-or-no-p 'y-or-n-p) ;reduce typing effort

(customize-set-variable 'compilation-scroll-output t)

(electric-pair-mode 1) ;close brackets

(electric-indent-mode)

(show-paren-mode 1)

(push '(?< . ?>) electric-pair-pairs)   ;add angle brackets to electric pairs (autoclose brackets)

;; howard abrams yt video
(defadvice transpose-words
    (before my/transpose-words)
  (when (looking-at "$")
    (backward-word 1)))

(ad-activate 'transpose-words)

;; useful functions
(defun system-name= (&rest names)
  (cl-some
   (lambda (name)
     (string-equal name (system-name)))
   names))

(defun shutdown ()
  (interactive)
  (cond
   ((system-name= "klingenberg-laptop") (async-shell-command "sudo shutdown"))
   (t (shell-command "shutdown now"))))

(defun reboot ()
  (interactive)
  (async-shell-command "sudo reboot now"))

(defvar browser 
  (cond
   ;; ((system-name= "klingenberg-tablet") "next")
   ((system-name= "klingenberg-laptop") "epiphany")
   (t "firefox")))

(defun find-config-file ()
  "Open Emacs configuration file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun load-config-file ()
  "Load Emacs configuration file."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(defun find-dotfile-dir ()
  "Open dotfile directory."
  (interactive)
  (find-file "~/.dotfiles/dotfiles/"))

(defun find-todo ()
  "Open dotfile directory."
  (interactive)
  (find-file "~/Documents/TODO.org")
  (calendar))

(defun my/get-rid-of-mouse ()
  "Move the mouse to the bottom right corner of the screen"
  (interactive)
  (shell-command "xdotool mousemove 100000 100000")) ; extremely high numbers to ensure the cursor goes to the bottom right regardless of display size

(defun my--convert-to-pdf (filename)
  (shell-command (concat "unoconv " filename)))

(defun my/dired-convert-to-pdf ()
  (interactive)
  (mapc #'my--convert-to-pdf (dired-get-marked-files))
  (ranger-refresh))

(defun my/brightness+ ()
  (interactive)
  (shell-command "xbacklight -inc 10"))

(defun my/brightness- ()
  (interactive)
  (shell-command "xbacklight -dec 10"))

(defun my/fix-touchscreen ()
  (when (system-name= "klingenberg-tablet")
    (shell-command "xinput --map-to-output $(xinput list --id-only \"ELAN Touchscreen\") eDP1")))

(defun my/open-url (url)
  (start-process-shell-command
   "" nil (concat browser
                  url)))

(defun my/close-buffer ()
  (interactive)
  (kill-this-buffer)
  (when (< 1 (length (window-list)))
    (evil-window-delete)))

(defun my/add-to-path (path)
  "Add path to PATH."
  (setenv "PATH" (concat
                  path
                  ":"
                  (getenv "PATH")))
  (add-to-list 'exec-path path))

(defun my/remove-hook-interactively ()
  "Remove hook interactively."
  (interactive)
  (let* ((hook (intern (completing-read "hook: "
                                        obarray
                                        #'(lambda (symbol) (string-suffix-p "hook" (format "%s" symbol))))))
         (fun (intern (completing-read "fun: " (eval hook)))))
    (remove-hook hook fun)))

;; (defmacro ! (&rest args)
;;   "convenient way to execute shell commands from scratch buffer"
;;   `(shell-command (mapcar #'write-to-string ,args)))

(defun fdy-mount (source target)
  "Mount a directory from fdy windows remote server."
  (async-shell-command (concat
                        "sudo /usr/bin/mount //dc1/"
                        source
                        " "
                        target
                        " -t cifs -o username=klingenberg,noexec,uid=klingenberg")))

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

(defun ambrevar-toggle-window-split ()
  "Switch between vertical and horizontal split.
It only works for frames with exactly two windows.
\(Credits go to ambrevar and his awesome blog\)"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer )
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun my/indent-buffer ()
  "Indent the entire buffer using evil-indent."
  (interactive)
  (save-excursion
    (evil-indent (point-min) (point-max))))

;; packages with configuration
(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package general
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  :config
  (general-evil-setup t)
  (general-auto-unbind-keys)

  (general-create-definer my/leader-def
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "s-SPC"
    :states '(motion normal emacs))

  (general-create-definer my/local-leader-def
    :keymaps 'override
    :prefix "-"
    :states '(motion normal))

  (general-create-definer my/local-insert-leader-def
    :keymaps 'override
    :prefix "C-c"
    :states '(motion normal emacs))

  (general-nmap "Y" "y$")

  (general-define-key "ESC" 'keyboard-quit :which-key "abort command")
  (general-define-key "TAB" 'company-complete :which-key "trigger completion")

  (general-define-key
   :keymaps 'override
   :states 'normal
   "gb" '(pop-tag-mark :which-key "go back"))

  (general-define-key
   :keymaps 'override
   :states 'insert
   "M-t" '(transpose-words :which-key "transpose-words"))

  ;; many spacemacs bindings go here
  (my/leader-def
    "SPC" '(helm-M-x :which-key "M-x")
    "a" '(:ignore t :which-key "applications")
    "ad" '(deer :which-key "call deer")
    "ab" '(eww :which-key "open browser")
    "am" '(gnus :which-key "open unread mail")
    "ap" '(helm-system-packages :which-key "package management")
    "ao" '(sx-search :which-key "search stackoverflow")
    "ar" '(md4rd :which-key "reddit")
    "ag" '(go-play :which-key "play the game of go")
    "ae" '(elfeed :which-key "open elfeed")
    "at" '(shell :which-key "open shell")
    "aS" '(eshell :which-key "open existing eshell")
    "as" '((lambda () (interactive) (eshell 'N)) :which-key "open new eshell")
    "g"  '(:ignore t :which-key "git")
    "/"  '(helm-occur t :which-key "helm-occur")
    "cc" '(org-capture :which-key "org capture")
    "f" '(:ignore t :which-key "file")
    "fs" '(save-buffer :which-key "save file")
    "fS" '(write-file :which-key "save file as")
    "ff" '(helm-find-files :which-key "find file")
    "fed" '(find-config-file :which-key "find config file")
    "fer" '(load-config-file :which-key "load config file")
    "feD" '(find-dotfile-dir :which-key "find dotfile directory")
    "ft"  '(find-todo :which-key "find todo file")
    "fz"  '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "find scratch buffer")
    "fp" '(helm-locate :which-key "helm-locate")
    "fg" '(helm-do-grep-ag :which-key "helm-ag")
    "b" '(:ignore t :which-key "buffer")
    "bb" '(helm-mini :which-key "switch buffer")
    "be" '(helm-exwm :which-key "switch to exwm buffer")
    "bd" '(kill-this-buffer :which-key "kill buffer")
    "m"  '(imenu t :which-key "imenu")
    "w"  '(:ignore t :which-key "window management")
    "w TAB"  '(evil-switch-to-windows-last-buffer :which-key switch to last buffer)
    ;; "w2"  'spacemacs/layout-double-columns
    ;; "w3"  'spacemacs/layout-triple-columns
    ;; "wb"  'spacemacs/switch-to-minibuffer-window
    "wd"  'evil-window-delete
    "wH"  'evil-window-move-far-left
    "wh"  'evil-window-left
    "wJ"  'evil-window-move-very-bottom
    "wj"  'evil-window-down
    "wK"  'evil-window-move-very-top
    "wk"  'evil-window-up
    "wL"  'evil-window-move-far-right
    "wl"  'evil-window-right
    "wm"  'delete-other-windows
    "ws"  'split-window-below
    "wS"  'split-window-below-and-focus
    "w-"  'split-window-below
    "wU"  'winner-redo
    "wu"  'winner-undo
    "wv"  'split-window-right
    "wV"  'split-window-right-and-focus
    "ww"  'other-window
    "w="  'balance-windows
    "r"   '(:ignore t :which-key "recent-files")
    "rr"  'helm-recentf
    "w+"  '(ambrevar-toggle-window-split :which-key "toggle window split")
    "e"  '(:ignore t :which-key "eval elisp")
    "ee"  'eval-last-sexp
    "ef"  'eval-defun
    "ep"  'eval-print-last-sexp
    "er"  'eval-expression
    "i"   '(:ignore :which-key "internet")
    "id"  '((lambda () (interactive) (my/open-url "https://www.dazn.com")) :which-key "dazn")
    "ig"  '((lambda () (interactive) (my/open-url "https://www.dragongoserver.net/status.php")) :which-key "dgs")
    "iy"  '((lambda () (interactive) (my/open-url "https://www.youtube.com/")) :which-key "youtube")
    "ss"  'shutdown
    "sr"  'reboot
    "sl"  (lambda () (interactive) (shell-command "/usr/bin/slock"))))

(use-package mini-modeline
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

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config (evil-mode 1))

(use-package evil-collection
  :after (evil helm) 
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "s" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :config
  (setq evil-snipe-scope 'visible)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  (evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
  (evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S))

(use-package evil-easymotion
  :config
  (evilem-default-keybindings "z"))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :init (evil-commentary-mode))

(use-package evil-args
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  (general-define-key
   :keymaps 'override
   :states 'normal
   "gl" 'evil-forward-arg
   "gh" 'evil-backward-arg
   "gK" 'evil-jump-out-args))

(use-package evil-iedit-state
  :config
  (general-define-key
   :keymaps 'override
   :states 'normal
   "C-s" 'iedit-mode))

(use-package evil-mc
  :diminish evil-mc-mode
  :config
  (global-evil-mc-mode 1))

;; (use-package evil-owl
;;   :diminish evil-owl-mode
;;   :config
;;   (evil-owl-mode))

(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package expand-region
  :custom
  expand-region-contract-fast-key "X"
  :config
  (general-define-key
   :keymaps 'override
   :states 'visual
   "x" 'er/expand-region))

(use-package vdiff
  :config
  (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map))

(use-package vdiff-magit
  :config
  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit)
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

;; (use-package windower
;;   :config
;;   (global-set-key (kbd "s-M-h") 'windower-move-border-left)
;;   (global-set-key (kbd "s-M-j") 'windower-move-border-below)
;;   (global-set-key (kbd "s-M-k") 'windower-move-border-above)
;;   (global-set-key (kbd "s-M-l") 'windower-move-border-right)

;;   (global-set-key (kbd "s-H") 'windower-swap-left)
;;   (global-set-key (kbd "s-J") 'windower-swap-below)
;;   (global-set-key (kbd "s-K") 'windower-swap-above)
;;   (global-set-key (kbd "s-L") 'windower-swap-right))

(use-package which-key
  :defer t
  :init (which-key-mode)
  :diminish which-key-mode)

(my/leader-def
  "l" '(:ignore :which-key "bookmarks")
  "lm" '(bookmark-set :which-key "set bookmark")
  "ll" '(bookmark-jump :which-key "jump to bookmark"))

;; (use-package bufler
;;   :config
;;   (require 'helm-bufler)
;;   (bufler-mode))

;;appearance
;; (use-package zenburn-theme
;;   :config
;;   (load-theme 'zenburn t))
;; (use-package cyberpunk-theme :ensure t)

;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-dark+ t))

;; (use-package modus-operandi-theme
;;   :config
;;   (load-theme 'modus-operandi t))

(use-package modus-vivendi-theme
  :config
  (load-theme 'modus-vivendi t))

;; (use-package eziam-dusk-theme  
;;   :ensure eziam-theme)  

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package auto-dim-other-buffers
  :config
  (setq auto-dim-other-buffers-face nil)
  (auto-dim-other-buffers-mode 1))

;; eshell
(defun my/eshell-delete-line ()
  (interactive)
  (eshell-bol)
  (kill-line))

(defun my/eshell-change-line ()
  (interactive)
  (my/eshell-delete-line)
  (evil-insert 1))

(setq eshell-cmpl-ignore-case t)

(use-package eshell-prompt-extras
  :config
  (setq eshell-highlight-prompt t
        eshell-prompt-function 'epe-theme-lambda))

(use-package fish-completion)

(use-package bash-completion)

(use-package pcomplete-extension)

(use-package pcmpl-args)

(use-package pcmpl-git)

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;; ranger
(use-package ranger
  :commands ranger
  :config
  (general-define-key
   :keymaps 'ranger-normal-mode-map
   "cp" '(my/dired-convert-to-pdf :which-key "convert to pdf")
   "gr" '(ranger-refresh :which-key "refresh"))
  (setq ranger-cleanup-eagerly t)
  (ranger-override-dired-mode t))

;; (use-package ivy :ensure t
;;   :diminish (ivy-mode . "") ; does not display ivy in the modeline
;;   :init (ivy-mode 1)        ; enable ivy globally at startup
;;   :config
;;   (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
;;   (setq ivy-height 20)               ; set height of the ivy window
;;   (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
;;   )

(use-package pulseaudio-control
  :custom
  (pulseaudio-control-volume-step "5%")
  :config
  (setq pulseaudio-control--volume-maximum '(("percent" . 110)
                                             ("decibels" . 2.5)
                                             ("raw" . 72000))))

(use-package helm
  :diminish helm-mode
  :defer t
  :after helm-exwm
  :config
  (general-define-key
   :keymaps 'helm-find-files-map
   "M-H" 'left-char
   "M-L" 'right-char
   "M-y" 'helm-ff-run-copy-file
   "M-r" 'helm-ff-run-rename-file
   "M-s" 'helm-ff-run-find-file-as-root
   "M-o" 'helm-ff-run-switch-other-window
   "M-O" 'helm-ff-run-switch-other-frame
   "M-RET" 'helm-ff-run-open-file-with-default-tool)
  (general-define-key
   :keymaps 'helm-buffer-map
   "M-d" 'helm-buffer-run-kill-persistent)
  (setq completion-styles `(basic partial-completion emacs22 initials
                                  ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching           t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-file-cache-fuzzy-match           t)
  (setq helm-imenu-fuzzy-match                t)
  (setq helm-mode-fuzzy-match                 t)
  (setq helm-locate-fuzzy-match               t) 
  (setq helm-quick-update                     t)
  (setq helm-recentf-fuzzy-match              t)
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf))
  (helm-mode 1))

(use-package helm-exwm)

(use-package helm-dictionary)

(use-package helm-tramp)

(use-package helm-unicode)

(use-package helm-flycheck)

(use-package company
  :diminish company-mode
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  (setq company-dabbrev-downcase nil)
  (setq read-file-name-completion-ignore-case t)
  (global-company-mode 1))

;; abbrev mode
(setq abbrev-file-name             ;; tell emacs where to read abbrev
      (concat user-emacs-directory "abbrev-snippets.el"))    ;; definitions from...
(setq save-abbrevs 'silently)
(setq-default abbrev-mode nil)

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package projectile
  :diminish projectile-mode
  :defer t
  :config
  (my/leader-def
    :states 'normal
    "p" 'projectile-command-map)
  (projectile-mode 1))

(use-package helm-projectile
  :after projectile
  :config
  (helm-projectile-on))

(use-package projectile-ripgrep)

(use-package magit
  :commands magit-status
  :general (my/leader-def
             "gs" '(magit-status :which-key "git status")))

(use-package evil-magit)

(use-package vc-msg
  :general (my/leader-def
             "gb" '(vc-msg-show :which-key "git blame")))

(defun doc-view-setup ()
  ;; open docx as text
  (define-derived-mode
    pandoc-view-mode
    markdown-mode
    "pandoc-view-mode"
    "View pandoc processing of docx file using markdown mode."
    (erase-buffer)
    (let* ((pandoc (executable-find "pandoc")))
      (insert (shell-command-to-string
               (concat pandoc " --wrap=none " (shell-quote-argument (buffer-file-name)) " -t markdown"))))
    (not-modified)
    (read-only-mode t))
  ;; (add-to-list 'auto-mode-alist '("\\.docx\\'" . pandoc-view-mode))
  (my/local-leader-def
    :keymaps 'doc-view-mode-map
    "p" 'pandoc-view-mode ; TODO create toggle function
    "d" 'doc-view-mode)
  (general-define-key
   :states 'normal
   :keymaps 'doc-view-mode-map
   "j" 'doc-view-next-page
   "k" 'doc-view-previous-page
   "<down>" 'doc-view-next-page
   "<up>" 'doc-view-previous-page))

(doc-view-setup)

(unless (system-name= "localhost" "lina")
  (use-package pdf-tools
    :defer t
    :init
    (pdf-tools-install)
    :magic ("%PDF" . pdf-view-mode)
    :config
    (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
    (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
    (setq-default pdf-view-display-size 'fit-page)
    (setq pdf-view-continuous nil)
    (evil-collection-init 'pdf)
    (setq pdf-view-midnight-colors '("WhiteSmoke" . "gray16"))
    :general
    (general-define-key
     :states '(motion normal)
     :keymaps 'pdf-view-mode-map
     ;; evil-style bindings
     ;; "SPC"  nil ;TODO where to put this globally?
     "-"  nil ;TODO where to put this globally?
     "j"  '(pdf-view-scroll-up-or-next-page :which-key "scroll down")
     "k"  '(pdf-view-scroll-down-or-previous-page :which-key "scroll up")
     "<down>" 'pdf-view-scroll-up-or-next-page
     "<up>" 'pdf-view-scroll-down-or-previous-page
     ;; "j"  '(pdf-view-next-line-or-next-page :which-key "scroll down")
     ;; "k"  '(pdf-view-previous-line-or-previous-page :which-key "scroll up")
     "L"  '(image-forward-hscroll :which-key "scroll right")
     "H"  '(image-backward-hscroll :which-key "scroll left")
     "l"  '(pdf-view-next-page :which-key "page down")
     "h"  '(pdf-view-previous-page :which-key "page up")
     "u"  '(pdf-view-scroll-down-or-previous-page :which-key "scroll down")
     "d"  '(pdf-view-scroll-up-or-next-page :which-key "scroll up")
     "/"  '(isearch-forward :which-key search forward)
     "?"  '(isearch-backward :which-key search backward)
     "0"  '(image-bol :which-key "go left")
     "$"  '(image-eol :which-key "go right"))
    (my/local-leader-def
      ;; :states 'normal
      :keymaps 'pdf-view-mode-map
      ;; Scale/Fit
      ;; "f"  nil
      "fw"  '(pdf-view-fit-width-to-window :which-key "fit width")
      "fh"  '(pdf-view-fit-height-to-window :which-key "fit heigth")
      "fp"  '(pdf-view-fit-page-to-window :which-key "fit page")
      "a"  '(:ignore :which-key "annotations")
      "at"  '(pdf-annot-add-text-annotation :which-key "text")
      "ah"  '(pdf-annot-add-highlight-markup-annotation :which-key "highlight")
      "ao"  '(pdf-annot-add-strikeout-markup-annotation :which-key "strikeout")
      "aD"  '(pdf-annot-delete :which-key "delete")
      "m"  '(pdf-view-set-slice-using-mouse :which-key "slice using mouse")
      "b"  '(pdf-view-set-slice-from-bounding-box :which-key "slice from bounding box")
      "R"  '(pdf-view-reset-slice :which-key "reset slice")
      "zr" '(pdf-view-scale-reset :which-key "zoom reset"))))

;; prettify stuff
;; (set-fontset-font "fontset-default" '(#x1d4d0 . #x1d4e2) "Symbola")
(global-prettify-symbols-mode +1)
(defvar global-prettify-symbols-alist '(("lambda" . 955)
                                        ("*" . 215) 
                                        ("/" . 247) 
                                        ("<=" . ?≤)
                                        (">=" . ?≥)))
(defun my/setup-pretty-symbols (list)
  (mapc #'(lambda (pair) (push pair prettify-symbols-alist))
        (append
         global-prettify-symbols-alist
         list)))

(defun my/lisp-setup-pretty-symbols ()
  (my/setup-pretty-symbols
   '(("defun" . 8518))))

(add-hook 'lisp-mode-hook #'my/lisp-setup-pretty-symbols)

(defun my/csharp-setup-pretty-symbols ()
  (my/setup-pretty-symbols
   '(("!=" . 8800) 
     ("==" . #xff1d) 
     ("=" . 8592) 
     ("int" . #x2124)
     ("double" . #x211d)
     ("bool" . 8492)
     ("string" . #x3c3)
     ("String" . #x3c3)
     ("void" . #x2205)
     ("new" . #x2605)
     ("this" . #x3c4)
     ("List" . #x2112)
     ;; ("[]" . #x2a02)
     ;; ("Dictionary" . #x1d507)
     ("public" . 8511)
     ("protected" . 8508)
     ("not" .      #x2757)
     ("for" .      #x2200)
     ("foreach" . #x2200)
     ("in" . #x2208)
     ("var" .  #x3bd)
     ("delegate" . 955)
     ("return" . #x27fb)
     ("get" . #x2934)
     ("set" . #x2935)
     ("null" . #x2205)
     ("true" . 10003)
     ("false" . 10007))))

;; (add-hook 'csharp-mode-hook #'my/csharp-setup-pretty-symbols)

(use-package pretty-mode
  :config
  (pretty-activate-groups
   '(:sub-and-superscripts :greek :arithmetic-nary :equality :ordering :ordering-double :ordering-triple :arrows :arrows-twoheaded :punctuation :logic :sets)))

;;exwm
(if (not (system-name= "lina"))
    (progn
      (use-package exwm 
        :init
        (server-start)
        :config
        (evil-set-initial-state 'exwm-mode 'emacs)
        (setq mouse-autoselect-window nil
              focus-follows-mouse nil)
        (exwm-enable))

      (use-package exwm-input
        :after exwm-randr
        :ensure nil
        :demand t
        :config
        (define-key exwm-mode-map (kbd "C-c") nil)
        (setq exwm-input-global-keys
              `(([?\s-r] . exwm-reset)
                ([?\s-e] . exwm-input-release-keyboard)
                ([?\s-F] . exwm-layout-set-fullscreen)
                ([?\s-a] . exwm-workspace-switch)
                ([?\s-A] . exwm-workspace-move-window)
                ,@(mapcar (lambda (i)
                            `(,(kbd (format "s-%d" i)) .
                              (lambda () (interactive)
                                (exwm-workspace-switch-create ,i))))
                          (number-sequence 0 9))
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
      
      (use-package exwm-systemtray
        :ensure nil
        :after exwm
        :demand t
        :config (exwm-systemtray-enable))

      (use-package exwm-randr
        :ensure nil
        :after exwm
        :demand t
        :preface
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

        :hook (exwm-randr-screen-change . my/exwm-xrandr)
        :init
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
        (progn
          (exwm-randr-enable)))

      (use-package exwm-workspace
        :ensure nil
        :after exwm
        :demand t
        :init
        (progn
          (setq exwm-workspace-number 10)
          (setq exwm-workspace-show-all-buffers t)
          (setq exwm-layout-show-all-buffers t)))

      ;; (require 'exwmx-xfce)
      ;; (exwmx-xfce-enable)
      ))

(defun my/create-super-bindings ()
  "Create bindings starting with super for use outside exwm."
  (general-define-key
   :keymaps 'override
   :states '(insert emacs hybrid normal visual motion operator replace)
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

(my/create-super-bindings)

(use-package flycheck
  :diminish flycheck-mode
  :defer t
  :init (global-flycheck-mode))

;;;programming languages
;; lisp
;; (use-package slime
;;   :defer t
;;   :config
;;   ;;(setq inferior-lisp-program "/usr/bin/sbcl --load /home/klingenberg/quicklisp.lisp")
;;   (sbcl-cvs ("/home/klingenberg/sbcl-cvs/src/runtime/sbcl"
;; 	     "--core" "/home/klingenberg/.sbcl.core")
;; 	    :env ("SBCL_HOME=/home/klingenberg/"))
;;   ;;:init (setenv 'SBCL-HOME " ") ;;TODO
;;   :general (my/local-leader-def
;; 	     :keymaps 'lisp-mode-map
;; 	     "'" '(slime :which-key "start slime")
;; 	     "e" '(:ignore :which-key "slime eval")
;; 	     "ef" '(slime-eval-function :which-key "eval function")
;; 	     "ee" '(slime-eval-last-expression :which-key "eval last expression")
;; 	     "eb" '(slime-eval-buffer :which-key "eval buffer")))

(use-package lsp-mode
  :hook ((
          ;; csharp-mode
          tex-mode latex-mode) . lsp)
  :commands lsp
  :config
  (general-define-key
   :states 'normal
   :keymaps 'lsp-mode-map
   "gd" '(lsp-find-definition :which-key "go to definition")
   "<f12>" '(lsp-find-definition :which-key "go to definition for Florian")
   "gr" '(lsp-rename :which-key "rename"))
  (my/local-leader-def
    :keymaps 'lsp-mode-map
    "rt" '(lsp-ui-imenu :which-key "imenu")
    ;; "t" '(omnisharp-current-type-information :which-key "current type information")
    ;; "T" '(omnisharp-current-type-documentation :which-key "current type documentation")
    ;; "gr" '(omnisharp-run-code-action-refactoring :which-key "refactor")
    "fi" '(lsp-find-implementation :which-key "find implementations")
    ;; "fu" '(omnisharp-find-usages :which-key "find usages")
    ;; "fI" '(omnisharp-fix-code-issue-at-point :which-key "fix code issue at point")
    ;; "fU" '(omnisharp-fix-usings :which-key "fix usings")
    ;; "rt" '((lambda () (interactive) (my/run-tests (my/csharp-find-current-project))) :which-key "run tests")
    ;; "ro" '(run-csharp-repl-other-frame :which-key "start repl")
    ;; "rr" '(csharp-repl-send-region :which-key "csharp-send-region-to-repl")
    ))

;; optionally
(use-package lsp-ui 
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

;; optionally if you want to use debugger
(use-package dap-mode)

;; (use-package dap-csharp
;;   :ensure t) ; to load the dap adapter for your language

;; (use-package eglot
;;   :ensure t
;;   :commands (eglot eglot-ensure)
;;   :hook ((csharp-mode . eglot-ensure))
;;   :config
;;   (progn
;;     (add-to-list 'eglot-server-programs
;;                  `(csharp-mode . ("~/.omnisharp/omnisharp/omnisharp/OmniSharp.exe" "-lsp")))))

(use-package lispy
  :diminish lispy-mode
  :defer t
  :config
  (lispy-set-key-theme '(lispy c-digits)) ;; disable single-key bindings
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  (add-hook 'lisp-mode-hook #'lispy-mode)
  (add-hook 'scheme-mode-hook #'lispy-mode)
  ;; (add-hook 'lispy-mode-hook (lambda () (add-hook 'before-save-hook #'my/indent-buffer nil t)))
  ;; (add-hook 'lispy-mode-hook #'rainbow-delimiters-mode-enable)
  ) 

(use-package lispyville
  :diminish lispyville-mode
  :after lispy
  :config
  (add-hook 'lispy-mode-hook #'lispyville-mode)
  (add-hook 'csharp-mode-hook #'lispyville-mode) ; because why not :)
  (setq lispy-use-sly t)
  ;; copied and slightly adapted from ambrevar's config
  (lispyville-set-key-theme '(operators
                              c-w
                              additional
                              prettify
                              additional-insert
                              (escape insert)
                              slurp/barf-cp))
  ;; Bindings that must only be set in lisp languages
  (general-define-key
   :states '(override motion normal visual)
   :keymaps '(emacs-lisp-mode-map lisp-mode-map scheme-mode-map)
   "gd" #'lispy-goto-symbol)
  ;; Bindings that can safely be set in other languages
  (general-define-key
   :states '(override motion normal visual)
   :keymaps 'lispyville-mode-map
   "M-h" #'lispy-left
   ;; (kbd "M-h") #'lispyville-previous-opening
   "M-l" #'lispyville-next-opening
   "M-j" #'lispy-down
   "M-k" #'lispy-up
   "M-J" #'lispyville-drag-forward
   "M-K" #'lispyville-drag-backward
   "M-H" #'lispyville-<
   "M-L" #'lispyville->
   "C-M-h" #'lispy-move-left
   "C-M-l" #'lispy-move-right
   "M-r" #'lispy-raise-sexp
   "M-d" #'lispyville-wrap-round
   ;; (kbd "M-8") #'lispyville-wrap-brackets
   ;; (kbd "M-7") #'lispyville-wrap-braces
   ;; (kbd "M-9") #'lispyville-wrap-brackets
   ;; (kbd "M-0") #'lispyville-wrap-braces
   "C-<return>" #'lispy-split
   ;; (kbd "M-<backspace>") 'lispyville-delete-backward-word
   ;; (kbd "/") #'lispy-occur
   "gc" #'lispyville-comment-or-uncomment
   ;; "gcc" #'lispyville-comment-or-uncomment-line ;; maybe not needed
   ;; TODO: lispy-eval-and-replace
   "=" #'lispyville-prettify)
  (my/local-leader-def
    :keymaps 'lispyville-mode-map
    "el" #'lispy-eval
    "d" #'lispy-describe-inline
    "a" #'lispy-arglist-inline
    "x" #'lispy-x)
  (general-define-key
   :states 'insert
   :keymaps 'lispyville-mode-map
   (kbd "<backspace>") 'lispy-delete-backward
   (kbd "M-<backspace>") 'lispyville-delete-backward-word
   ;; ";" 'lispy-comment
   ;; ":" 'lispy-colon ; The colon is not always used to delimit keys.
   "'" 'lispy-tick
   "`" 'lispy-backtick
   "\"" 'lispy-quotes
   "(" 'lispy-parens
   ")" 'lispy-right-nostring))

;; (use-package evil-smartparens
;;   :config
;;   ;; (add-hook 'emacs-lisp-mode-hook (lambda () (evil-smartparens-mode 1)))
;;   ;; (add-hook 'lisp-mode-hook (lambda () (evil-smartparens-mode 1)))
;;   (add-hook 'csharp-mode-hook (lambda () (evil-smartparens-mode 1))))

;; (use-package evil-lispy
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook (lambda () (evil-smartparens-mode 1)))
;;   (add-hook 'lisp-mode-hook (lambda () (evil-smartparens-mode 1))))

(use-package sly
  :defer t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl --load /home/klingenberg/quicklisp.lisp")
  :general (my/local-leader-def
             :keymaps 'lisp-mode-map
             "'" '(sly :which-key "start repl")
             "e" '(:ignore :which-key "eval")
             "ef" '(sly-compile-defun :which-key "eval function")
             "ee" '(sly-compile-last-expression :which-key "eval last expression")
             "eb" '(sly-compile-buffer :which-key "eval buffer")))



;; (use-package sly-quicklisp
;;   :ensure t)
(use-package geiser
  :defer t
  :config
  ;; (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode-enable)
  ;; (add-hook 'scheme-mode-hook (lambda () (add-hook 'before-save-hook #'my/indent-buffer nil t)))
  (when (system-name= "klingenberg-laptop")
    (with-eval-after-load 'geiser-guile
      (add-to-list 'geiser-guile-load-path "~/guix-packages/guix/"))
    (with-eval-after-load 'yasnippet
      (add-to-list 'yas-snippet-dirs "~/guix-packages/guix/etc/snippets")))
  :general (my/local-leader-def
             :keymaps 'scheme-mode-map
             "'" '(geiser :which-key "start reps")
             "e" '(:ignore :which-key "eval")
             "ef" '(geiser-eval-definition :which-key "eval definition")
             "ee" '(geiser-eval-last-sexp :which-key "eval last expression")
             "eb" '(geiser-eval-buffer :which-key "eval buffer")))

(use-package eval-sexp-fu
  :config
  (setq eval-sexp-fu-flash-face
        '((((class color)) (:background "black" :foreground "gray" :bold t))
          (t (:inverse-video nil)))))

(use-package piper
  :quelpa (piper :fetcher gitlab :repo "howardabrams/emacs-piper"))

;; (use-package maplev
;;   :ensure nil
;;   :load-path "~/emacs-packages/maplev/lisp/"
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.mpl\\'" . maplev-mode)))


;;org
(use-package org
  :diminish org-indent-mode
  :diminish org-beamer-mode
  :ensure org-plus-contrib
  :defer t
  :config
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook '(lambda () (org-indent-mode 1)))
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-to-list 'org-export-backends 'beamer)
  (add-to-list 'org-export-backends 'md)
  (setq org-confirm-babel-evaluate nil)
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)))
  (setq org-babel-lisp-eval-fn 'sly-eval)
  (setq org-todo-keywords
        '((sequence "TODO" "WAIT-FOR" "REVISIT" "DELEGATED" "DONE"))
        org-fontify-done-headline t)
  (setq org-default-notes-file "~/Documents/TODO.org")
  (setq org-agenda-files (list org-default-notes-file))
  (setq org-capture-templates
        '(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %i%? \n:PROPERTIES: \n:CREATED: %U \n:END: \n " :prepend t)
          ("l" "todo with link" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %A \n:PROPERTIES: \n:CREATED: %U \n:END: \n" :prepend t)
          ("c" "current buffer" entry (file+headline org-default-notes-file "Important Dates")
           "%(shell-command-to-string \"cat %f\") \n" :prepend t)
          ;; ("c" "current buffer" entry (file+headline org-default-notes-file "Important Dates")
          ;;  "%[ %f ]" :prepend t)
          ("p" "Process" entry (file+headline org-default-notes-file "Tasks")
           "* TODO [#A] Process mail from %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n:PROPERTIES:\n:CREATED: %U\n:END:\n %a" :immediate-finish t :prepend t)))
  :general
  (my/local-leader-def
    :keymaps 'org-mode-map
    "e" '(org-export-dispatch :which-key "export")
    "a" '((lambda () (interactive)
            (let ((current-prefix-arg '-)) ; simulate pressing C-u
              (call-interactively 'org-export-dispatch))) :which-key "repeat last export")
    "s" '(org-edit-special :which-key "edit source code")
    "t" 'org-todo
    "n" 'org-agenda
    "l" '(:ignore :which-key "links")
    "ll" '(org-insert-link :which-key "insert link")
    "lf" '((lambda () (interactive)
             (let ((current-prefix-arg '(4))) ; simulate pressing C-u
               (call-interactively 'org-insert-link))) :which-key "insert link to file"))
  (general-define-key
   :states '(motion normal)
   :keymaps 'org-mode-map
   "RET" '(org-open-at-point :which-key "open link")))

(use-package evil-org
  :diminish evil-org-mode
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook '(lambda () (org-bullets-mode 1))))

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook '(lambda () (org-superstar-mode 1))))

(use-package org-ref
  :defer t
  :init
  (setq org-latex-pdf-process (list "latexmk -shell-escape -f -pdf %f"))
  :config
  (setq org-ref-ivy-cite t)
  (setq org-ref-default-bibliography '("~/HESSENBOX-DA/bibliography/bibliography.bib"))
  (setq bibtex-completion-library-path "~/HESSENBOX-DA/bibliography/bibtex-pdfs")
  :general
  (my/local-leader-def
    :keymaps 'org-mode-map
    "r" '(:ignore :which-key "references")
    "rc" '(org-ref-helm-insert-cite-link :which-key "insert citation")
    "rr" '(org-ref-insert-ref-link :which-key "insert reference")))

(use-package org-re-reveal)

(use-package csv-mode
  :config
  (add-hook 'csv-mode-hook 'csv-align-mode))

(use-package ggtags)

(use-package wgrep)

(use-package jenkins
  :defer t
  :config
  (setq jenkins-api-token "115e5da14d9018ef2d51d040fefc48eeb4")
  (setq jenkins-url "http://130.83.248.141:8080/")
  (setq jenkins-username "klingenberg")
  :general
  (general-define-key
   :states 'normal
   :keymaps 'jenkins-mode-map
   "RET" 'jenkins-enter-job)
  (my/local-leader-def
    :keymaps 'jenkins-job-view-mode-map
    "b" '(jenkins--call-build-job-from-main-screen :which-key "build")
    "v" '(jenkins-visit-jenkins-web-page :which-key "view")
    "o" '(jenkins--show-console-output-from-job-screen :which-key "view")))

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


(defun my/setup-csharp-and-bosss ()
  "Setup stuff specific to bosss and csharp."
  (use-package csharp-repl
    :ensure nil
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
  (add-hook 'csharp-mode-hook #'company-mode)
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
                (t (iter (concat dir "/../" )))))) ; if there is no .csproj file, look one directory higher
      (iter (file-name-directory (buffer-file-name)))))

  
  (my/local-leader-def
    :keymaps 'csharp-mode-map
    "b" '(:ignore :which-key "build")
    "bd" '((lambda () (interactive) (compile (concat "msbuild /p:Configuration=Debug " (my/csharp-find-current-project)))) :which-key "build debug")
    "br" '((lambda () (interactive) (compile (concat "msbuild /p:Configuration=Release " bosss-master-solution))) :which-key "build release")
    "be" '((lambda () (interactive) (compile (concat "msbuild /p:Configuration=Debug " bosss-master-solution))) :which-key "build everything")
    "bb" '(recompile :which-key "recompile")
    "=" '(my/indent-buffer-without-bosss-header :which-key "indent buffer")
    "et" '((lambda () (interactive) (my/run-tests (my/csharp-find-current-project))) :which-key "run tests")
    "eo" '(run-csharp-repl-other-frame :which-key "start repl")
    "er" '(csharp-repl-send-region :which-key "csharp-send-region-to-repl"))

  ;; bosss
  (use-package bosss
    :ensure nil
    :load-path "~/Documents/programming/elisp/emacs-bosss/"
    :init
    (add-to-list 'auto-mode-alist '("\\.bws\\'" . bosss-mode))
    (setq bosss-pad-path "/home/klingenberg/BoSSS-experimental/public/src/L4-application/BoSSSpad/bin/Release/BoSSSpad.exe")
    (setq bosss-path-reference (mapcar #'(lambda (proj) (concat "/home/klingenberg/BoSSS-experimental/internal/src/private-kli/" proj))
                                       '("RANSCommon/bin/Release/RANS_Solver.dll"
                                         "KOmegaModelSolver/bin/Release/KOmegaSolver.exe"
                                         "KOmegaStatSymmModelSolver/bin/Release/KOmegaSSSolver.exe")))
    :config
    (my/local-leader-def
      :keymaps 'bosss-mode-map
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

  (use-package omnisharp
    :diminish omnisharp-mode
    :config
    (add-hook 'csharp-mode-hook #'omnisharp-mode)
    ;; (add-hook 'csharp-mode-hook
    ;;           (lambda ()
    ;;             (push \\='(\"<=\" . ?≤) prettify-symbols-alist)))
    (general-define-key
     :states 'normal
     :keymaps 'csharp-mode-map
     "gd" '(omnisharp-go-to-definition :which-key "go to definition")
     "<f12>" '(omnisharp-go-to-definition :which-key "go to definition for Florian")
     "gr" '(omnisharp-rename :which-key "rename"))
    (my/local-leader-def
      :keymaps 'csharp-mode-map
      "t" '(omnisharp-current-type-information :which-key "current type information")
      "T" '(omnisharp-current-type-documentation :which-key "current type documentation")
      "a" '(my/csharp-toggle-list-and-array :which-key "current type documentation")
      "gr" '(omnisharp-run-code-action-refactoring :which-key "refactor")
      "n" '('ignore :which-key "navigate")
      "nf" '('ignore :which-key "function")
      "nfk" '(csharp-move-back-to-beginning-of-defun :which-key "navigate to beginning of function")
      "nfj" '(csharp-move-fwd-to-end-of-defun :which-key "navigate to end of function")
      "nc" '('ignore :which-key "class")
      "nck" '(csharp-move-back-to-beginning-of-class :which-key "navigate to beginning of class")
      "ncj" '(csharp-move-fwd-to-end-of-class :which-key "navigate to end of class")
      "nb" '('ignore :which-key "block")
      "nbk" '(csharp-move-back-to-beginning-of-block :which-key "navigate to beginning of block")
      "fi" '(omnisharp-find-implementations :which-key "find implementations")
      "fu" '(omnisharp-find-usages :which-key "find usages")
      "fI" '(omnisharp-fix-code-issue-at-point :which-key "fix code issue at point")
      "fU" '(omnisharp-fix-usings :which-key "fix usings"))))

(add-hook 'csharp-mode-hook #'my/setup-csharp-and-bosss)

(use-package fsharp-mode
  :defer t
  :general
  (my/local-leader-def
    :keymaps 'fsharp-mode-map
    "ef" '(fsharp-eval-phrase :which-key "eval current phrase")))

;;latex (auctex)
(use-package tex
  :ensure auctex
  :init
  (setq
   ;; TeX-command-default 'LaTeX
   TeX-view-program-selection '((output-pdf "PDF Tools"))
   TeX-source-correlate-start-server t
   TeX-auto-save t
   TeX-parse-self t
   TeX-syntactic-comment t
   ;; Synctex support
   TeX-source-correlate-start-server nil
   ;; Don't insert line-break at inline math
   LaTeX-math-abbrev-prefix "#"
   LaTeX-fill-break-at-separators nil)
  :config
  (TeX-interactive-mode -1)
  (TeX-source-correlate-mode -1)
  (setq TeX-electric-math '("\\(" . "\\)"))
  (setq TeX-electric-sub-and-superscript t)
  (setq TeX-save-query nil)
  (reftex-mode 1)
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (progn
                (push '(?d . ("\\left\( " . " \\right\)")) evil-surround-pairs-alist)
                (push '(?\$ . ("\\\(" . "\\\)")) evil-surround-pairs-alist))))
  ;; (general-define-key
  ;;  :states '(motion normal)
  ;;  :keymaps 'LaTeX-mode-map
  ;;  "-"  nil)
  ;; (add-to-list 'company-backends 'company-auctex t)
  (add-to-list 'company-backends 'company-math t)
  (my/local-leader-def
    :keymaps '(TeX-mode-map LaTeX-mode-map)
    "-"   'TeX-recenter-output-buffer         
    "."   'LaTeX-mark-environment
    "*"   'LaTeX-mark-section
    "a"   'TeX-command-run-all                
    "b"   'TeX-command-master
    "e"   'TeX-next-error
    "k"   'TeX-kill-job                       
    "l"   'TeX-recenter-output-buffer         
    "m"   'TeX-insert-macro                   
    "v"   'TeX-view                           
    "c" '(:ignore :which-key "change")
    "cs" '(:ignore :which-key "change environment")
    "cse" '((lambda() (interactive) (LaTeX-environment 1)) :which-key "change current environment")
    "yae" '((lambda() (interactive)
              (progn
                (LaTeX-mark-environment)
                (kill-ring-save 0 0 t))) :which-key "yank current environment")
    "dae" '((lambda() (interactive)
              (progn 
                (LaTeX-mark-environment)
                (kill-region 0 0 t))) :which-key "delete current environment")
    ;; TeX-doc is a very slow function
    "hd"  'TeX-doc
    "xb"  'latex/font-bold
    "xc"  'latex/font-code
    "xe"  'latex/font-emphasis
    "xi"  'latex/font-italic
    "xr"  'latex/font-clear
    "xo"  'latex/font-oblique
    "xfc" 'latex/font-small-caps
    "xff" 'latex/font-sans-serif
    "xfr" 'latex/font-serif
    "r"   '(:ignore :which-key "reftex")
    "rt" '(reftex-toc :which-key "table of contents")
    "rr"   '(reftex-cleveref-cref :which-key "cref")
    "rc"   '(reftex-citation :which-key "cite")
    "ol" '(lambda() (interactive) (find-file "definLocal.tex"))
    "og" '(lambda() (interactive) (find-file (getenv "LatexGlobalConfig")))
    "ob" '(lambda() (interactive) (find-file "bibliography.bib")))
  (my/local-insert-leader-def
    :keymaps '(TeX-mode-map LaTeX-mode-map)
    "r"   '(:ignore :which-key "reftex")
    "rt" '(reftex-toc :which-key "table of contents")
    "rr"   '(reftex-cleveref-cref :which-key "cref")
    "rc"   '(reftex-citation :which-key "cite")))

(use-package latex-extra
  :hook (LaTeX-mode . latex-extra-mode)
  :config
  (my/local-leader-def
    :keymaps 'LaTeX-mode-map
    "j" 'latex/forward-environment
    "k" 'latex/backward-environment))

;; (use-package auctex-latexmk
;;   :defer t
;;   :init
;;   (progn
;;     (auctex-latexmk-setup)
;;     (setq auctex-latexmk-inherit-TeX-PDF-mode t)))

(use-package company-reftex
  :defer t
  :config
  (add-to-list 'company-backends 'company-reftex-labels t)
  (add-to-list 'company-backends 'company-reftex-citations t))

(use-package latex-pretty-symbols)

;; browser
(use-package helm-eww
  :config
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
  (my/local-leader-def
    :keymaps 'eww-mode-map
    "o" 'helm-eww
    "d" '(my/youtube-dl :which-key "Download and open youtube video"))
  (general-define-key
   :states '(override motion normal)
   :keymaps 'eww-mode-map
   "M-h" 'eww-back-url
   "M-l" 'eww-forward-url
   "f" 'ace-link-eww))

(use-package async-await)
;; adapted from snippet by oremacs
(defun my/youtube-dl ()
  (interactive)
  (let* ((url (plist-get eww-data :url))
         (str (replace-regexp-in-string
               "https://www.youtube.com/watch\\?v="
               ""
               url))
         (download-dir "~/Videos/"))
    (set-process-sentinel
     (start-process-shell-command
      "youtube-download"
      "*youtube-download*"
      "youtube-dl"
      (concat "-o " download-dir "%\\(title\\)s%\\(id\\)s")
      str)
     (lambda (_ _)
       (helm-open-file-with-default-tool (car (directory-files
                                               "~/Videos/"
                                               ;; download-dir
                                               t str)))))))

(use-package ace-link)

(use-package system-packages
  :config
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

(use-package helm-system-packages)

;; mail - gnus
(gnus-demon-add-handler 'gnus-demon-scan-news 5 nil)
(gnus-demon-init)

(use-package nnreddit
  :config
  (setq elpy-rpc-python-command "python3"))

;; Don't ask auto-save stuff when opening
(setq gnus-always-read-dribble-file t)
                                        ; No primary server:
(setq gnus-select-method '(nnnil ""))

                                        ; Get local email, and store it in nnml; connect via IMAP to imap.gmail.com...:
(setq gnus-secondary-select-methods '((nnml "")
                                      (nnimap "mail.tu-darmstadt.de")
                                      (nnimap "imap.gmail.com")
                                      (nnimap "imap.web.de")
                                      (nnimap "mail.gsc.ce.tu-darmstadt.de")
                                      ;;(nnrss "https://www.zeitsprung.fm/feed/mp3/")
                                      ;; (nnreddit "")
                                      ))

                                        ; Archive outgoing email in Sent folder on imap.mcom.com:
;; (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
;;       gnus-message-archive-group "[Gmail]/Sent Mail")

                                        ; Mark gcc'ed (archived) as read:
(setq gnus-gcc-mark-as-read t)

                                        ; Send email via Gmail:
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-default-smtp-server "smtp.gmail.com")


;; Available SMTP accounts.
(defvar smtp-accounts
  '((ssl "klingenberg@fdy.tu-darmstadt.de" "smtp.tu-darmstadt.de" 465 "km88econ" nil)
    (ssl "dario.klingenberg@gmail.com" "smtp.gmail.com" 465  "dario.klingenberg" nil)
    (starttls "dario.klingenberg@web.de" "smtp.web.de" 587 "dario.klingenberg" nil)
    (ssl "klingenberg@gsc.tu-darmstadt.de" "smtp.gsc.ce.tu-darmstadt.de" 465 "klingenberg" nil)))

;; Let Gnus change the "From:" line by looking at current group we are in.
(setq gnus-posting-styles
      '(((mail-header-from "*fdy*") (signature-file "~/.config/emacs/fdy-signature"))
        ("fdy" (address "klingenberg@fdy.tu-darmstadt.de") (signature-file "~/.config/emacs/fdy-signature"))
        ("gsc" (address "klingenberg@gsc.tu-darmstadt.de") (signature-file "~/.config/emacs/gsc-signature"))
        ("gmail" (address "dario.klingenberg@gmail.com") (signature-file nil))
        ("web" (address "dario.klingenberg@web.de") (signature-file nil))))

;; Default smtpmail.el configurations.
(require 'cl)
(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      user-full-name "Dario Klingenberg"
      user-mail-address "klingenberg@fdy.tu-darmstadt.de"
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(defun set-smtp (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-auth-supported (list mech)
        smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'."
           server port user))

(defun set-smtp-ssl (server port user password  &optional key cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-stream-type 'ssl
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-starttls-credentials (list (list server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
   server port user))

(defun set-smtp-starttls (server port user password  &optional key cert)
  "Set related SMTP and Starttls variables for supplied parameters."
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-stream-type 'starttls
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-starttls-credentials (list (list server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
   server port user))

(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (auth-mech address . auth-spec) in smtp-accounts
          when (string-match address from)
          do (cond
              ((memq auth-mech '(cram-md5 plain login))
               (return (apply 'set-smtp (cons auth-mech auth-spec))))
              ((eql auth-mech 'ssl)
               (return (apply 'set-smtp-ssl auth-spec)))
              ((eql auth-mech 'starttls)
               (return (apply 'set-smtp-starttls auth-spec)))
              (t (error "Unrecognized SMTP auth. mechanism: `%s'" auth-mech)))
          finally (error "Cannot infer SMTP information"))))

(defadvice smtpmail-via-smtp
    (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer))
  "Call `change-smtp' before every `smtpmail-via-smtp'."
  (with-current-buffer smtpmail-text-buffer (change-smtp)))

(setq message-dont-reply-to-names "klingenberg")

(ad-activate 'smtpmail-via-smtp)
;; (ad-deactivate 'smtpmail-via-smtp)

                                        ; Use topics per default:
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
                                        ; Show more MIME-stuff:
(setq gnus-mime-display-multipart-related-as-mixed t)
                                        ; Smileys:
(setq smiley-style 'medium)
                                        ; Don't get the first article automatically:
(setq gnus-auto-select-first nil)
                                        ; Don't show that annoying arrow:
(setq gnus-summary-display-arrow nil)

;; (setq nnimap-split-inbox "INBOX") ;; (1)
;; (setq nnimap-split-predicate "UNDELETED") ;; (2)
;; (setq nnimap-split-rule
;;        '(
;;          ("INBOX.web" "^To:.*web.de")
;;          ("INBOX.gmail" "^To:.*gmail.com")    
;;          ("INBOX.fdy" "^To:.*fdy.tu-darmstadt.de")    
;;          ("INBOX.gsc" "^To:.*gsc.tu-darmstadt.de")   
;;         )) 

(setq gnus-topic-topology '(("Gnus" visible)
                            (("important" visible)
                             (("work" visible)))
                            (("misc" visible))))

;; TODO - test
(mapcar #'(lambda (topic)
            (gnus-topic-set-parameters
             topic
             '((gnus-use-adaptive-scoring nil)
               (gnus-use-scoring nil)
               (visible . t)
               (display . all)
               (modeline-notify . t))))
        '("important" "work"))

                                        ; Email splitting rules:
(setq nnmail-split-fancy
      '(|
        (any "klingenberg@fdy.tu-darmstadt.de" "work")
        (any "klingenberg@gsc.tu-darmstadt.de" "work")
        (any "klingenberg@gmail.tu-darmstadt.de" "important")
        "INBOX"))
                                        ; Use fancy splitting:
(setq nnmail-split-methods 'nnmail-split-fancy)

(general-define-key
 :keymaps '(gnus-group-mode-map gnus-topic-mode-map)
 :states '(override normal)
 "s" '(gnus-group-make-nnir-group :which-key "search")
 "o" '(gnus-group-list-all-groups :which-key "all groups")
 "D" '(gnus-group-delete-group :which-key "delete groups")
 "M-G" '(gnus-group-get-new-news :which-key "refresh all groups"))

(general-define-key
 :keymaps '(gnus-summary-mode-map gnus-article-mode-map)
 :states '(override normal)
 "r" '(gnus-summary-reply-with-original :which-key "reply")
 "R" '(gnus-summary-wide-reply-with-original :which-key "reply to all"))

(use-package gnus-desktop-notify
  :config
  (gnus-desktop-notify-mode)
  (gnus-demon-add-scanmail))

(require 'gnus-icalendar)
(gnus-icalendar-setup)
(setq gnus-icalendar-org-capture-file "~/Documents/TODO.org")
(setq gnus-icalendar-org-capture-headline '("IMPORTANT DATES")) ;;make sure to create Calendar heading first
(gnus-icalendar-org-setup)


(use-package rainbow-delimiters)

(use-package dmenu)

(use-package google-translate-smooth-ui
  :ensure google-translate
  :config
  (setq google-translate-enable-ido-completion t)
  (setq google-translate-translation-directions-alist '(("en" . "de") ("de" . "en")))
  (my/leader-def
    "t" '(google-translate-smooth-translate :which-key "translate")))

(use-package excorporate
  :config
  (general-define-key
   :keymaps 'calendar-mode-map
   :states 'normal
   "e" '((lambda ()
           (interactive)
           (exco-calendar-show-day)
           (switch-to-buffer "diary-excorporate-transient"))
         :which-key "excorporate show day"))
  (setq excorporate-configuration (cons "klingenberg@fdy.tu-darmstadt.de" "https://mail.tu-darmstadt.de/ews/exchange.asmx")))

(use-package emms
  :defer t
  :config
  (emms-standard)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music"))

(use-package sx
  :defer t
  :config
  (general-define-key
   :keymaps 'sx-question-list-mode-map
   :states 'normal
   "RET" 'sx-display))

(use-package mediawiki
  :defer t)

(use-package debbugs
  :defer t)

(use-package md4rd
  :defer t
  :config
  (setq md4rd-subs-active 
        '(linux
          emacs
          lisp
          baduk)))

;; (use-package elfeed
;;   :defer t
;;   :config
;;   (setq elfeed-feeds
;;         '("https://www.zeitsprung.fm/feed/ogg/"
;;           "https://kickermeetsdazn.podigee.io/feed/mp3")))

(use-package telega
  :config
  (telega-notifications-mode 1))

(use-package pinentry
  :init
  (pinentry-start))

(use-package diminish
  :config
  (mapcar #'diminish '(reftex-mode
                       auto-revert-mode
                       undo-tree-mode
                       eldoc-mode
                       pdf-view-midnight-minor-mode
                       subword-mode
                       flyspell-mode
                       defining-kbd-macro)))
(use-package eaf
  :ensure nil
  :load-path ("~/.config/emacs/site-lisp/emacs-application-framework")
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  ;; (eaf-bind-key scroll_up "k" eaf-browser-keybinding)
  ;; (eaf-bind-key scroll_down "j" eaf-browser-keybinding)
  (setq eaf-browser-default-search-engine "duckduckgo")
  (eaf-setq eaf-browse-blank-page-url "https://duckduckgo.com")
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;; (setq browse-url-browser-function 'eaf-open-browser)
  ;; (defalias 'browse-web #'eaf-open-browser)
  (eaf-setq eaf-browser-dark-mode "true")
  (eaf-setq eaf-mindmap-dark-mode "true"))

;; load my custom scripts
(load "~/Dropbox/Helen/washing-machine-timer.el" t t)
(load "~/Dropbox/Helen/einkaufsliste/interactiveEnterLisp.el" t t)

;;; emacs ends here
