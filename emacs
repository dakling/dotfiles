;;; package  --- Summary
;; my emacs config
;;; Commentary:
;; I use evil-mode everywhere, and the config is based on use-package and general
;;; Code:

;;; speed up startup using Ambrevar's suggestions:
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook #'ambrevar/reset-gc-cons-threshold)

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist
                file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
(add-hook 'after-init-hook #'ambrevar/reset-file-name-handler-alist)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;; 
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; TODO change for 27
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("reduce ide" . "http://reduce-algebra.sourceforge.net/reduce-ide/packages/")))
;; TODO change for 27
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)

(setq use-package-always-ensure t)

;; defaults suggested by blog and extended by me
(setq delete-old-versions -1)		; delete excess backup versions silently
(setq version-control t)		; use version control
(setq vc-make-backup-files t)		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ; which directory to put backups file
(setq vc-follow-symlinks t)				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))) ;transform backups file name
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
 "(print \"Welcome\") \n \n(async-shell-command \"yay --sudoloop -Syu\") \n \n(shell-command-to-string \"acpi -b\") \n \n(find-file \"~/BoSSS-experimental/internal/src/private-kli/RANS_Solver/RANS_Main.cs\")") ; print a default message in the empty scratch buffer opened at startup

(defalias 'yes-or-no-p 'y-or-n-p) ;reduce typing effort

(electric-pair-mode 1) ;close brackets

(electric-indent-mode)

(show-paren-mode 1)

(push '(?< . ?>) electric-pair-pairs)   ;add angle brackets to electric pairs (autoclose brackets)

;; useful functions
(defun system-name= (&rest names)
  (cl-some
   (lambda (name)
     (string-equal name (system-name)))
   names))

(defun shutdown ()
  (interactive)
  (cond
   ((system-name= "klingenberg-tablet" "klingenberg-laptop") (async-shell-command "sudo shutdown"))
   (t (shell-command "shutdown now"))))

(defun reboot ()
  (interactive)
  (cond
   ((system-name= "klingenberg-tablet") (async-shell-command "sudo reboot"))
   (t (shell-command "reboot now"))))

(defvar browser 
  (cond
   ;; ((system-name= "klingenberg-tablet") "next")
   ((system-name= "klingenberg-laptop") "epiphany")
   (t "firefox")))

(defun find-config-file ()
  "Open Emacs configuration file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun load-config-file ()
  "Load Emacs configuration file."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun find-dotfile-dir ()
  "Open dotfile directory."
  (interactive)
  (find-file "~/.dotfiles/dotfiles/"))

(defun find-todo ()
  "Open dotfile directory."
  (interactive)
  (find-file "~/Documents/TODO.org")
  (calendar))

(defun my-get-rid-of-mouse ()
  "Move the mouse to the bottom right corner of the screen"
  (interactive)
  (shell-command "xdotool mousemove 100000 100000")) ; extremely high numbers to ensure the cursor goes to the bottom right regardless of display size

(defun my--convert-to-pdf (filename)
  (shell-command (concat "unoconv " filename)))

(defun my-dired-convert-to-pdf ()
  (interactive)
  (mapc #'my--convert-to-pdf (dired-get-marked-files))
  (ranger-refresh))

(defun my-brightness+ ()
  (interactive)
  (shell-command "xbacklight -inc 10"))

(defun my-brightness- ()
  (interactive)
  (shell-command "xbacklight -dec 10"))

(defun my-open-url (url)
  (start-process-shell-command
   "" nil (concat browser
                  url)))

(defun my-close-buffer ()
  (interactive)
  (kill-this-buffer)
  (when (< 1 (length (window-list)))
    (evil-window-delete)))

(defun my-add-to-path (path)
  "Add path to PATH."
  (setenv "PATH" (concat
                  path
                  ":"
                  (getenv "PATH")))
  (add-to-list 'exec-path path))

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
               ((string= location "lehre") '("lehre" "~/lehre")))))

(defun ambrevar/toggle-window-split ()
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

(defun my-indent-buffer ()
  "Indent the entire buffer using evil-indent."
  (interactive)
  (save-excursion
    (evil-indent (point-min) (point-max))))

;; packages with configuration
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

  (general-create-definer my-leader-def
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "s-SPC"
    :states '(motion normal emacs))

  (general-create-definer my-local-leader-def
    :keymaps 'override
    :prefix "-"
    :states '(motion normal))

  (general-create-definer my-local-insert-leader-def
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

  ;; many spacemacs bindings go here
  (my-leader-def
    "SPC" '(helm-M-x :which-key "M-x")
    "a" '(:ignore t :which-key "applications")
    "ad" '(deer :which-key "call deer")
    "ab" '(eww :which-key "open browser")
    "am" '(mu4e-alert-view-unread-mails :which-key "open unread mail")
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
    "w+"  '(ambrevar/toggle-window-split :which-key "toggle window split")
    "e"  '(:ignore t :which-key "eval elisp")
    "ee"  'eval-last-sexp
    "ef"  'eval-defun
    "ep"  'eval-print-last-sexp
    "er"  'eval-expression
    "i"   '(:ignore :which-key "internet")
    "id"  '((lambda () (interactive) (my-open-url "https://www.dazn.com")) :which-key "dazn")
    "ig"  '((lambda () (interactive) (my-open-url "https://www.dragongoserver.net/status.php")) :which-key "dgs")
    "iy"  '((lambda () (interactive) (my-open-url "https://www.youtube.com/")) :which-key "youtube")
    "ss"  'shutdown
    "sr"  'reboot
    "sl"  (lambda () (interactive) (shell-command "/usr/bin/slock"))))


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
   "C-s" 'iedit))

(use-package evil-mc
  :diminish evil-mc-mode
  :config
  (global-evil-mc-mode 1))

(use-package evil-owl
  :diminish evil-owl-mode
  :config
  (evil-owl-mode))

(use-package evil-exchange
  :config
  
  (evil-exchange-install))

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

;;appearance
;; (use-package zenburn-theme :ensure t)
;; (use-package cyberpunk-theme :ensure t)
(use-package doom-themes
  :config
  (load-theme 'doom-dark+ t))

;; (use-package eziam-dusk-theme  
;;   :ensure eziam-theme)  

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package feebleline
  :config
  (defun my-feebleline-time ()
    "Show time as string."
    (format-time-string "%k:%M" (current-time)))
  (defun my-feebleline-exwm-workspace ()
    "Exwm workspaces as string."
    (format (concat "<%s> " (unless (null (my-exwm-get-other-workspace)) "[%s] "))
            exwm-workspace-current-index
            (my-exwm-get-other-workspace)))
  (setq feebleline-msg-functions
        '((my-feebleline-exwm-workspace)
          (my-feebleline-time)
          (feebleline-line-number         :post "" :fmt "%5s")
          (feebleline-column-number       :pre ":" :fmt "%-2s")
          (feebleline-file-directory      :face feebleline-dir-face :post "")
          (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
          (feebleline-file-modified-star  :face font-lock-warning-face :post "")
          (feebleline-git-branch          :face feebleline-git-face :pre " : ")
          (feebleline-project-name        :align right)))
  (feebleline-mode 1))

(tool-bar-mode -1)
(menu-bar-mode -1)
(menu-bar-no-scroll-bar)

;; eshell
(defun my-eshell-delete-line ()
  (interactive)
  (eshell-bol)
  (kill-line))

(defun my-eshell-change-line ()
  (interactive)
  (my-eshell-delete-line)
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
   "cp" '(my-dired-convert-to-pdf :which-key "convert to pdf")
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

(unless (system-name= "klingenberg-tablet")
  (use-package pulseaudio-control))

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
   "M-O" 'helm-ff-run-switch-other-frame)
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

(use-package helm-system-packages)

(use-package helm-exwm)

(use-package helm-eww)

(use-package helm-dictionary)

(use-package helm-tramp)

(use-package helm-unicode)

(use-package helm-flycheck)

(use-package company
  :diminish company-mode
  :config
  (setq company-dabbrev-downcase nil)
  (setq read-file-name-completion-ignore-case t)
  (global-company-mode 1))

;; abbrev mode
(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/HESSENBOX-DA/programming/abbrev-snippets.el")    ;; definitions from...
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
  (my-leader-def
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
  :general (my-leader-def
             "gs" '(magit-status :which-key "git status")))

(use-package evil-magit)

(use-package vc-msg
  :general (my-leader-def
             "gb" '(vc-msg-show :which-key "git blame")))

(use-package ediff)

(use-package doc-view
  :config
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
  (my-local-leader-def
    "p" 'pandoc-view-mode ; TODO create toggle function
    "d" 'doc-view-mode)
  (general-define-key
   :states 'normal
   :keymaps 'doc-view-mode-map
   "j" 'doc-view-next-page
   "k" 'doc-view-previous-page
   "<down>" 'doc-view-next-page
   "<up>" 'doc-view-previous-page))

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
    (my-local-leader-def
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


;;exwm
(if (not (system-name= "lina"))
    (progn
      (use-package exwm 
        :init
        (server-start)
        :config
        (defun my-autostart ()
          (when (system-name= "klingenberg-tablet")
            (my-add-to-path
             "/home/klingenberg/.nix-profile/bin/")
            (start-process "/home/klingenberg/.autostart.sh" "*/home/klingenberg/.autostart.sh*" "/home/klingenberg/.autostart.sh")
            ;; (start-process "gnome-session" "*gnome-session*" "gnome-session")
            )
          (start-process "synchting" "*synchting*" "syncthing" "-no-browser"))
        (evil-set-initial-state 'exwm-mode 'emacs)
        (setq mouse-autoselect-window nil
              focus-follows-mouse nil)
        (exwm-enable)
        (my-autostart))

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
                ([?\s-o] . my-exwm-switch-to-other-workspace)
                ([?\s-O] . my-exwm-move-window-to-other-workspace)
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
                ([?\s-c] . my-close-buffer)
                ([?\s-q] . my-get-rid-of-mouse)
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
                            #'my-brightness+)
        (exwm-input-set-key (kbd "<XF86MonBrightnessDown>")
                            #'my-brightness-)
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
        (defun my-exwm-get-other-workspace ()
          (cond ((not (= 2 (length (seq-filter #'identity (mapcar #'exwm-workspace--active-p exwm-workspace--list))))) nil) ;currently only works for two monitors
                ((= exwm-workspace-current-index
                    (cl-position t (mapcar #'exwm-workspace--active-p exwm-workspace--list) :from-end t))
                 (cl-position t (mapcar #'exwm-workspace--active-p exwm-workspace--list) :from-end nil))
                ((= exwm-workspace-current-index
                    (cl-position t (mapcar #'exwm-workspace--active-p exwm-workspace--list) :from-end nil))
                 (cl-position t (mapcar #'exwm-workspace--active-p exwm-workspace--list) :from-end t))))
        (defun my-exwm-switch-to-other-workspace () (interactive)
               (exwm-workspace-switch (my-exwm-get-other-workspace)))
        (defun my-exwm-move-window-to-other-workspace () (interactive)
               (exwm-workspace-move-window (my-exwm-get-other-workspace)))
        (cond
         ((system-name= "klingenberg-tablet") (progn (set 'monitor1 "eDP-1")
                                                     (set 'monitor2 "HDMI-2")
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
                     " --auto"))))
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
      )
  (progn
    (defun my-create-super-bindings ()
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
       "s-v" 'split-window-right
       "s-s" 'split-window-below
       "s-c" 'my-close-buffer
       "s-q" 'my-get-rid-of-mouse
       "s-m" 'delete-other-windows
       "s-<f1>" '(lambda () (interactive) (eshell 'N))
       "C-s-<f1>" 'eshell
       "s-<f2>" '(lambda () (interactive)
                   (start-process "" nil browser))
       "s-<f3>" 'deer
       "s-<f4>" '(lambda () (interactive)
                   (mu4e))
       "s-<f12>" '(lambda () (interactive)
                    (start-process "" nil "/usr/bin/slock"))))

    (my-create-super-bindings)))

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
;;   :general (my-local-leader-def
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
  (my-local-leader-def
    :keymaps 'lsp-mode-map
    "rt" '(lsp-ui-imenu :which-key "imenu")
    ;; "t" '(omnisharp-current-type-information :which-key "current type information")
    ;; "T" '(omnisharp-current-type-documentation :which-key "current type documentation")
    ;; "gr" '(omnisharp-run-code-action-refactoring :which-key "refactor")
    "fi" '(lsp-find-implementation :which-key "find implementations")
    ;; "fu" '(omnisharp-find-usages :which-key "find usages")
    ;; "fI" '(omnisharp-fix-code-issue-at-point :which-key "fix code issue at point")
    ;; "fU" '(omnisharp-fix-usings :which-key "fix usings")
    ;; "rt" '((lambda () (interactive) (my-run-tests my-bosss-project)) :which-key "run tests")
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

;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;; (add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))

;; (use-package lispy
;;   :diminish lispy-mode
;;   :defer t
;;   :config
;;   (add-hook 'lispy-mode-hook (lambda () (add-hook 'before-save-hook #'my-indent-buffer nil t)))
;;   ;; (add-hook 'lispy-mode-hook #'rainbow-delimiters-mode-enable)
;;   )

;; (use-package lispyville
;;   :diminish lispyville-mode
;;   :after lispy
;;   :init
;;   (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
;;   :config
;;   (lispyville-set-key-theme '(operators c-w additional)))

(use-package evil-smartparens
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (evil-smartparens-mode 1)))
  (add-hook 'lisp-mode-hook (lambda () (evil-smartparens-mode 1)))
  (add-hook 'csharp-mode-hook (lambda () (evil-smartparens-mode 1))))

(use-package sly
  :defer t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl --load /home/klingenberg/quicklisp.lisp")
  :general (my-local-leader-def
             :keymaps 'lisp-mode-map
             "'" '(sly :which-key "start reps")
             "e" '(:ignore :which-key "eval")
             "ef" '(sly-eval-defun :which-key "eval function")
             "ee" '(sly-eval-last-expression :which-key "eval last expression")
             "eb" '(sly-eval-buffer :which-key "eval buffer")))

;; (use-package sly-quicklisp
;;   :ensure t)
(use-package geiser
  :defer t
  :config
  ;; (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode-enable)
  (add-hook 'scheme-mode-hook (lambda () (add-hook 'before-save-hook #'my-indent-buffer nil t)))
  (when (system-name= "klingenberg-tablet")
    (with-eval-after-load 'geiser-guile
      (add-to-list 'geiser-guile-load-path "~/guix-packages/guix/"))
    (with-eval-after-load 'yasnippet
      (add-to-list 'yas-snippet-dirs "~/guix-packages/guix/etc/snippets")))
  :general (my-local-leader-def
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
  ;; in case drastic measures are required:
  ;; (setq org-latex-pdf-process
  ;; 	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;; 	  "bibtex %b"
  ;; 	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;; 	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-export-backends 'beamer)
  (add-to-list 'org-export-backends 'md)
  (setq org-confirm-babel-evaluate nil)
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)))
  (setq org-babel-lisp-eval-fn 'sly-eval)
  (setq org-default-notes-file "~/Documents/TODO.org")
  (setq org-agenda-contributing-files (list org-default-notes-file))
  (setq org-capture-templates
        '(("t" "todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %i%? \n:PROPERTIES: \n:CREATED: %U \n:END: \n ")
          ("l" "todo with link" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %i%? \n:PROPERTIES: \n:CREATED: %U \n:END: \n %a\n")
          ("p" "Process" entry (file+headline org-default-notes-file "Tasks")
           "* TODO [#A] Process mail from %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n:PROPERTIES:\n:CREATED: %U\n:END:\n %a" :immediate-finish t :prepend t)))
  :general
  (my-local-leader-def
    :keymaps 'org-mode-map
    "e" '(org-export-dispatch :which-key "export")
    "a" '((lambda () (interactive)
            (let ((current-prefix-arg '-)) ; simulate pressing C-u
              (call-interactively 'org-export-dispatch))) :which-key "repeat last export")
    "s" '(org-edit-special :which-key "edit source code")
    "t" 'org-todo
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

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook '(lambda () (org-bullets-mode 1))))

(use-package org-ref
  :defer t
  :init
  (setq org-latex-pdf-process (list "latexmk -shell-escape -f -pdf %f"))
  :config
  (setq org-ref-ivy-cite t)
  (setq org-ref-default-bibliography '("~/HESSENBOX-DA/bibliography/bibliography.bib"))
  (setq bibtex-completion-library-path "~/HESSENBOX-DA/bibliography/bibtex-pdfs")
  :general
  (my-local-leader-def
    :keymaps 'org-mode-map
    "r" '(:ignore :which-key "references")
    "rc" '(org-ref-helm-insert-cite-link :which-key "insert citation")
    "rr" '(org-ref-insert-ref-link :which-key "insert reference")))

(use-package org-re-reveal)

(use-package csv-mode
  :config
  (add-hook 'csv-mode-hook 'csv-align-mode))

(use-package ggtags)

;;reduce
(use-package reduce-ide
  :defer t
  :general (my-local-leader-def
             :states 'normal
             :keymaps 'reduce-mode-map
             "e" '(:ignore :which-key "eval")
             "ee" '(reduce-eval-last-statement :which-key "eval last statement")
             "eb" '(reduce-run-buffer :which-key "run buffer"))
  )
;; maple
;; (use-package maplev)

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
  (my-local-leader-def
    :keymaps 'jenkins-job-view-mode-map
    "b" '(jenkins--call-build-job-from-main-screen :which-key "build")
    "v" '(jenkins-visit-jenkins-web-page :which-key "view")
    "o" '(jenkins--show-console-output-from-job-screen :which-key "view")))

;;c#
(defun my-setup-csharp-and-bosss ()
  "Setup stuff specific to bosss and csharp."
  (use-package csharp-repl
    :ensure nil
    :load-path "~/Documents/programming/elisp/emacs-csharp-repl/")
  
  (defun my-bosss-file-p ()
    (or
     (file-in-directory-p (buffer-file-name) "~/BoSSS/")
     (file-in-directory-p (buffer-file-name) "~/BoSSS-experimental/internal/src/private-kli/")))

  (defun my-add-header ()
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
        (when (my-bosss-file-p)
          (unless (search-forward (substring header-text 93) nil t) ;; check if header already exists, start a bit later to ignore year)
            (princ header-text (current-buffer)))))))

  (defun my-indent-buffer-without-bosss-header ()
    "Indent file, but ignore header"
    (interactive)
    (save-excursion
      (goto-line 16)
      (let ((beg (point)))
        (evil-indent beg (point-max)))))

  (defun my-run-bosss-control-file (solver control-file &optional debug)
    "Run SOLVER with CONTROL-FILE, optionally using sbd to DEBUB"
    (async-shell-command
     (if debug
         (concat "sdb \"args -c " control-file "\" \"run " solver "\"")
       (concat "mono " solver " -c " control-file))))

  (defun my-run-tests (path-to-assembly)
    "Implement tests manually as default functions do not work"
    (interactive)
    (async-shell-command (concat "nunit3-console " path-to-assembly)))

  (add-hook 'csharp-mode-hook #'subword-mode)
  (add-hook 'csharp-mode-hook #'company-mode)
  ;; (add-hook 'csharp-mode-hook #'rainbow-delimiters-mode-enable)
  (add-hook 'csharp-mode-hook (lambda ()
                                (push '(?< . ("< " . " >")) evil-surround-pairs-alist)))
  (add-hook 'csharp-mode-hook #'my-add-header)
  (add-hook 'csharp-mode-hook (lambda () 
                                (add-hook 'before-save-hook #'my-indent-buffer-without-bosss-header nil t)))

  (setq bosss-master-solution "/home/klingenberg/BoSSS-experimental/internal/src/Master.sln")
  (setq my-bosss-project "/home/klingenberg/BoSSS-experimental/internal/src/private-kli/RANS_Solver/RANS.csproj")

  (my-local-leader-def
    :keymaps 'csharp-mode-map
    "b" '(:ignore :which-key "build")
    "bd" '((lambda () (interactive) (compile (concat "msbuild /p:Configuration=Debug " my-bosss-project))) :which-key "build debug")
    "br" '((lambda () (interactive) (compile (concat "msbuild /p:Configuration=Release " my-bosss-project))) :which-key "build release")
    "be" '((lambda () (interactive) (compile (concat "msbuild /p:Configuration=Debug " bosss-master-solution))) :which-key "build everything")
    "bb" '(recompile :which-key "recompile")
    "et" '((lambda () (interactive) (my-run-tests my-bosss-project)) :which-key "run tests")
    "eo" '(run-csharp-repl-other-frame :which-key "start repl")
    "er" '(csharp-repl-send-region :which-key "csharp-send-region-to-repl"))

  ;; bosss
  (use-package bosss
    :ensure nil
    :load-path "~/Documents/programming/elisp/emacs-bosss/"
    :defer t
    :init
    (add-to-list 'auto-mode-alist '("\\.bws\\'" . bosss-mode))
    (setq bosss-pad-path "/home/klingenberg/BoSSS-experimental/public/src/L4-application/BoSSSpad/bin/Debug/BoSSSpad.exe")
    (setq bosss-path-reference "/home/klingenberg/BoSSS-experimental/internal/src/private-kli/RANS_Solver/bin/Debug/RANS_Solver.exe")
    :config
    (my-local-leader-def
      :keymaps 'bosss-mode-map
      "j" '(bosss-next-field :which-key "next field")
      "k" '(bosss-previous-field :which-key "previous field")
      "ro" '(run-bosss-repl-other-window :which-key "start repl in other window")
      "rn" '(bosss-bosss-repl-run-bosss-pad :which-key "run bossspad")
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
    (general-define-key
     :states 'normal
     :keymaps 'csharp-mode-map
     "gd" '(omnisharp-go-to-definition :which-key "go to definition")
     "<f12>" '(omnisharp-go-to-definition :which-key "go to definition for Florian")
     "gr" '(omnisharp-rename :which-key "rename"))
    (my-local-leader-def
      :keymaps 'csharp-mode-map
      "t" '(omnisharp-current-type-information :which-key "current type information")
      "T" '(omnisharp-current-type-documentation :which-key "current type documentation")
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
      "fU" '(omnisharp-fix-usings :which-key "fix usings")))
  )

(add-hook 'csharp-mode-hook 'my-setup-csharp-and-bosss)

(use-package fsharp-mode
  :defer t
  :general
  (my-local-leader-def
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
  (my-local-leader-def
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
  (my-local-insert-leader-def
    :keymaps '(TeX-mode-map LaTeX-mode-map)
    "r"   '(:ignore :which-key "reftex")
    "rt" '(reftex-toc :which-key "table of contents")
    "rr"   '(reftex-cleveref-cref :which-key "cref")
    "rc"   '(reftex-citation :which-key "cite")))

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

;; browser
(use-package eww
  :config
  (defun my-eww-open-league-table ()
    "Do an internet search for soccer league table."
    (interactive)
    (let* ((country-search-string-table
            '(("germany" "german bundesliga tabelle")
              ("spain" "la liga tabelle")
              ("italy" "seria a tabelle")
              ("france" "ligue 1 tabelle")
              ("england" "premier league tabelle")))
           (country (completing-read "which country? " (mapcar #'car country-search-string-table))))
      (eww (cadr (assoc country country-search-string-table)))))
  (my-local-leader-def
    :keymaps 'eww-mode-map
    "o" 'helm-eww)
  (general-define-key
   :states '(override motion normal)
   :keymaps 'eww-mode-map
   "M-h" 'eww-back-url
   "M-l" 'eww-forward-url
   "f" 'ace-link-eww))

(use-package ace-link)

;; mail

(defun my-mu4e-setup ()

  (use-package helm-mu)
  
  ;; (use-package mu4e-conversation
  ;;   :ensure t)

  (require 'mu4e)
  (setenv "GPG_AGENT_INFO" nil)
  (setq mu4e-confirm-quit nil)
  ;; (global-mu4e-conversation-mode)
  (defun my-mu4e-set-account ()
    "Set the account for composing a message."
    (let* ((account
            (if mu4e-compose-parent-message
                (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                  (string-match "/\\(.*?\\)/" maildir)
                  (match-string 1 maildir))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'(lambda (var) (car var))
                                                  my-mu4e-account-alist "/"))
                               (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                               nil t nil nil (caar my-mu4e-account-alist))))
           (account-vars (cdr (assoc account my-mu4e-account-alist))))
      (if account-vars
          (mapc #'(lambda (var)
                    (set (car var) (cadr var)))
                account-vars)
        (error "No email account found"))))

  ;; ask for account when composing mail
  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
  (setq mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e")
  (setq mu4e-maildir "~/Mail")
  (setq mu4e-trash-folder "/Trash")
  (setq mu4e-refile-folder "/Archive")
  (setq mu4e-get-mail-command "offlineimap -o")
  (setq mu4e-update-interval 120)
  (setq mu4e-hide-index-messages t) ; do not show minibuffer messages after updates
  (setq mu4e-index-update-error-warning nil)
  (setq mu4e-compose-signature-auto-include t)
  (setq mu4e-view-show-images t)
  (setq mu4e-enable-notifications t)
  (setq send-mail-function 'smtpmail-send-it)
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq smtpmail-stream-type 'ssl)
  (setq mu4e-view-show-addresses t)
  (setq my-mu4e-account-alist
        '(("FDY"
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
           (mu4e-sent-folder "/FDY/Sent Items")
           (mu4e-drafts-folder "/FDY/Drafts")
           (smtpmail-smtp-server "smtp.tu-darmstadt.de")
           (smtpmail-smtp-service 465)
           (smtpmail-stream-type ssl)
           (user-mail-address "klingenberg@fdy.tu-darmstadt.de")
           (user-full-name "Dario Klingenberg"))
          ("GSC"
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
           (mu4e-sent-folder "/GSC/Sent Items")
           (mu4e-drafts-folder "/GSC/Drafts")
           (smtpmail-smtp-server "smtp.gsc.ce.tu-darmstadt.de")
           (smtpmail-smtp-service 465)
           (smtpmail-stream-type ssl)
           (user-mail-address "klingenberg@gsc.tu-darmstadt.de")
           (user-full-name "Dario Klingenberg"))
          ("Gmail"
           ;; Under each account, set the account-specific variables you want.
           (mu4e-sent-messages-behavior delete)
           (mu4e-compose-signature-auto-include nil)
           (mu4e-sent-folder "/Gmail/sent")
           (mu4e-drafts-folder "/Gmail/drafts")
           (user-mail-address "dario.klingenberg@gmail.com")
           (smtpmail-smtp-server "smtp.gmail.com")
           (smtpmail-smtp-service 465)
           (smtpmail-stream-type ssl)
           (user-full-name "Dario Klingenberg"))
          ("Web"
           (mu4e-sent-messages-behavior sent)
           (mu4e-compose-signature-auto-include nil)
           (mu4e-sent-folder "/Web/Sent Items")
           (mu4e-drafts-folder "/Web/Drafts")
           (smtpmail-smtp-server "smtp.web.de")
           (smtpmail-smtp-service 587)
           (smtpmail-stream-type starttls)
           (user-mail-address "dario.klingenberg@web.de")
           (user-full-name "dario"))))
  (run-at-time t mu4e-update-interval #'(lambda () (mu4e-update-mail-and-index t))) ; this should not be needed, but it is

  (use-package evil-mu4e
    :config
    (evil-define-key 'evilified mu4e-main-mode-map (kbd "j") 'evil-next-line)
    (evil-define-key 'evilified mu4e-main-mode-map (kbd "s") 'helm-mu)
    (bind-keys :map mu4e-main-mode-map
               ("c" . mu4e-compose-new))
    :general
    (general-define-key
     :states '(motion normal)
     :keymaps 'mu4e-view-mode-map
     "RET" '(mu4e~view-browse-url-from-binding :which-key "follow link")))

  ;; taken from reddit
  (use-package mu4e-alert
    :config
    (mu4e-alert-enable-mode-line-display)
    (mu4e-alert-enable-notifications)
    (mu4e-alert-set-default-style 'libnotify)
    (alert-add-rule
     :category "mu4e-alert"
     :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode)))
     :continue t)))

(unless (or (system-name= "localhost") (system-name= "lina"))
  (my-mu4e-setup))

(use-package rainbow-delimiters)

(use-package dmenu)

;; (use-package auto-dim-other-buffers
;;   :ensure t
;;   :config (auto-dim-other-buffers-mode t))

(use-package google-translate
  :defer t)

(use-package excorporate
  :defer t
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

(use-package elfeed
  :defer t
  :config
  (setq elfeed-feeds
        '("https://www.zeitsprung.fm/feed/ogg/")))

(use-package pinentry
  :init
  (pinentry-start))

(use-package go
  :ensure nil
  :load-path "~/Documents/programming/elisp/el-go/")

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
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (eaf-bind-key scroll_up "j" eaf-browser-keybinding)
  (eaf-bind-key scroll_down "j" eaf-browser-keybinding)
  (setq eaf-browser-default-search-engine 'duckduckgo)
  (eaf-setq eaf-browse-blank-page-url "https://duckduckgo.com")
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (setq browse-url-browser-function 'eaf-open-browser)
  (defalias 'browse-web #'eaf-open-browser))

;; (use-package powerline
;;   :config
;;   ;; (setq powerline-default-separator nil)
;;   (defun my-powerline-theme ()
;;     "Setup my mode-line."
;;     ;; (interactive)
;;     (setq-default mode-line-format
;;                   '("%e"
;;                     (:eval
;;                      (let* ((active (powerline-selected-window-active))
;;                             (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
;;                             (mode-line (if active 'mode-line 'mode-line-inactive))
;;                             (face0 (if active 'powerline-active0 'powerline-inactive0))
;;                             (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                             (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                             (separator-left (intern (format "powerline-%s-%s"
;;                                                             (powerline-current-separator)
;;                                                             (car powerline-default-separator-dir))))
;;                             (separator-right (intern (format "powerline-%s-%s"
;;                                                              (powerline-current-separator)
;;                                                              (cdr powerline-default-separator-dir))))
;;                             (lhs (list (powerline-raw "%*" face0 'l)
;;                                        (powerline-raw (format (concat "<%s> "
;;                                                                       (unless (null (my-exwm-get-other-workspace)) "[%s] "))
;;                                                               exwm-workspace-current-index
;;                                                               (my-exwm-get-other-workspace))
;;                                                       face0
;;                                                       'l)
;;                                        (when powerline-display-buffer-size
;;                                          (powerline-buffer-size face0 'l))
;;                                        (when powerline-display-mule-info
;;                                          (powerline-raw mode-line-mule-info face0 'l))
;;                                        (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
;;                                        (when (and (boundp 'which-func-mode) which-func-mode)
;;                                          (powerline-raw which-func-format face0 'l))
;;                                        (powerline-raw " " face0)
;;                                        (funcall separator-left face0 face1)
;;                                        (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
;;                                          (powerline-raw erc-modified-channels-object face1 'l))
;;                                        (powerline-major-mode face1 'l)
;;                                        (powerline-process face1)
;;                                        (powerline-minor-modes face1 'l)
;;                                        (powerline-narrow face1 'l)
;;                                        (powerline-raw " " face1)
;;                                        (funcall separator-left face1 face2)
;;                                        (powerline-vc face2 'r)
;;                                        (when (bound-and-true-p nyan-mode)
;;                                          (powerline-raw (list (nyan-create)) face2 'l))))
;;                             (rhs (list (powerline-raw global-mode-string face2 'r)
;;                                        (funcall separator-right face2 face1)
;;                                        (unless window-system
;;                                          (powerline-raw (char-to-string #xe0a1) face1 'l))
;;                                        (powerline-raw "%4l" face1 'l)
;;                                        (powerline-raw ":" face1 'l)
;;                                        (powerline-raw "%3c" face1 'r)
;;                                        (funcall separator-right face1 face0)
;;                                        (powerline-raw " " face0)
;;                                        (powerline-raw "%6p" face0 'r)
;;                                        (when powerline-display-hud
;;                                          (powerline-hud face0 face2))
;;                                        (powerline-fill face0 0)
;;                                        )))
;;                        (concat (powerline-render lhs)
;;                                (powerline-fill face2 (powerline-width rhs))
;;                                (powerline-render rhs)))))))
;;   (my-powerline-theme)
;;   ;; (powerline-default-theme)
;;   )
;;; emacs ends here
