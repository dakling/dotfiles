;;; auto-generated stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bc75dfb513af404a26260b3420d1f3e4131df752c19ab2984a7c85def9a2917e" default)))
 '(evil-snipe-mode t)
 '(evil-snipe-override-mode t)
 '(global-evil-surround-mode 1)
 '(gud-tooltip-mode t)
 '(org-agenda-files (quote ("~/Documents/TODO.org")))
 '(package-selected-packages
   (quote
    (projectile-ripgrep dmenu projectile helm-firefox helm-company helm-unicode helm-tramp helm-ext helm-dictionary helm-eww helm-mu helm-exwm podcaster lispy helm-system-packages mu4e-conversation excorporate md4rd sx emms yasnippet-snippets google-translate fsharp-mode wgrep guix pdf-tools magit yasnippet company ivy mu4e-alert evil-mu4e smooth-scrolling doom-themes ggtags zenburn-theme which-key use-package smart-mode-line-atom-one-dark-theme sly ranger rainbow-delimiters ox-reveal org-ref org-re-reveal org-plus-contrib org-bullets omnisharp general geiser exwm evil-surround evil-snipe evil-org evil-magit evil-commentary evil-collection eval-sexp-fu eshell-prompt-extras counsel company-reftex auctex ace-link)))
 '(scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; my emacs config
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("reduce ide" . "http://reduce-algebra.sourceforge.net/reduce-ide/packages/")))
(package-initialize) 

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)

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
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq default-major-mode 'text-mode)
(blink-cursor-mode -1)
(setq revert-without-query '("*pdf")) ; automatically revert pdf-files
(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro"))
(add-hook 'focus-out-hook (lambda () (when buffer-file-name (save-buffer))))
(recentf-mode 1)
(setq delete-by-moving-to-trash t)
(setq-default indent-tabs-mode nil)

(setq
 initial-scratch-message
 "(print \"Welcome\")

(async-shell-command \"yay --sudoloop -Syu\")

(shell-command-to-string \"acpi -b\")
") ; print a default message in the empty scratch buffer opened at startup

;; add my packages
(add-to-list 'load-path "~/.emacs.d/dev/")

(defalias 'yes-or-no-p 'y-or-n-p) ;reduce typing effort
(electric-pair-mode 1) ;close brackets

;; useful functions
(defun system-name= (&rest names)
  (cl-some
    (lambda (name)
      (string-equal name (system-name)))
    names))

(defun shutdown ()
  (interactive)
  (cond
   ((system-name= "klingenberg-tablet") (async-shell-command "sudo shutdown"))
   (t (shell-command "shutdown now"))))

(defun reboot ()
  (interactive)
  (cond
   ((system-name= "klingenberg-tablet") (async-shell-command "sudo reboot"))
   (t (shell-command "reboot now"))))

(defvar browser 
  (cond
   ((system-name= "klingenberg-tablet") "next")
    (t "firefox")))

(defun find-config-file ()
  "open emacs configuration file"
  (interactive)
  (find-file "~/.emacs"))

(defun load-config-file ()
  "load emacs configuration file"
  (interactive)
  (load-file "~/.emacs"))

(defun find-dotfile-dir ()
  "open dotfile directory"
  (interactive)
  (find-file "~/.dotfiles/dotfiles/"))

(defun find-todo ()
  "open dotfile directory"
  (interactive)
  (find-file "~/Documents/TODO.org")
  (calendar))

(defun my-get-rid-of-mouse ()
  (interactive)
  (shell-command "xdotool mousemove 0 0"))

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

;; (defmacro ! (&rest args)
;;   "convenient way to execute shell commands from scratch buffer"
;;   `(shell-command (mapcar #'write-to-string ,args)))

(defun fdy-mount (source target)
  "mount a directory from fdy windows remote server"
  (async-shell-command (concat
			"sudo /usr/bin/mount //dc1/"
			source
			" "
			target
			" -t cifs -o username=klingenberg,noexec,uid=klingenberg")))

(defun qmount (location)
  "shortcuts for mounting frequent locations"
  (interactive)
  (apply #'fdy-mount
	 (cond ((string= location "lectures") '("misc/fdy-lectures.git" "~/git/mnt/fdy-lectures.git"))
	       ((string= location "klausuren") '("lehre/TM1/Klausuren.git" "~/git/mnt/Klausuren.git"))
	       ((string= location "publications") '("misc/fdy-publications.git" "~/git/mnt/fdy-publications.git"))
	       ((string= location "misc") '("misc" "~/misc"))
	       ((string= location "scratch") '("scratch" "~/scratch"))
	       ((string= location "lehre") '("lehre" "~/lehre")))))

(defun ambrevar/toggle-window-split ()
  "Switch between vertical and horizontal split.
It only works for frames with exactly two windows.
(Credits go to ambrevar and his awesome blog)"
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


;; packages with configuration
(use-package general :ensure t
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
    "ab" '(helm-eww :which-key "open browser")
    "am" '(mu4e :which-key "open mail")
    "ap" '(helm-system-packages :which-key "package management")
    "ao" '(sx-search :which-key "search stackoverflow")
    "ar" '(md4rd :which-key "reddit")
    "at" '(ansi-term :which-key "open ansi-term")
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
    "w"  '(:ignore t :which-key "window management")
    "w TAB"  '(lambda () (interactive) (ivy--switch-buffer-action (buffer-name (other-buffer (current-buffer)))))
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
    "i"   '(:ignore :which-key "internet")
    "id"  '((lambda () (interactive) (my-open-url "https://www.dazn.com")) :which-key "dazn")
    "ig"  '((lambda () (interactive) (my-open-url "https://www.dragongoserver.net/status.php")) :which-key "dgs")
    "iy"  '((lambda () (interactive) (my-open-url "https://www.youtube.com/")) :which-key "youtube")
    "ss"  'shutdown
    "sr"  'reboot
    "sl"  (lambda () (interactive) (shell-command "/usr/bin/slock"))))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after (evil helm) 
  :ensure t
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
 (evil-define-key 'operator global-map "s" 'evil-surround-edit)
    (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
    (evil-define-key 'visual global-map "s" 'evil-surround-region)
    (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

(use-package evil-snipe
  :ensure t
  :config
  (setq evil-snipe-scope 'visible)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  (evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
  (evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S))

(use-package evil-commentary
  :ensure t
  :init (evil-commentary-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode)

;;appearance
;; (use-package zenburn-theme :ensure t)
;; ;; (use-package cyberpunk-theme :ensure t)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-vibrant t))

(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;   (setq doom-modeline-height 25)
;;   )

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))

(use-package smart-mode-line
  :after smart-mode-line-atom-one-dark-theme
  :ensure t
  :config
  (setq sml/theme 'atom-one-dark)
  (setq mode-line-format
	'("%e"
	  (:eval (propertize
		  (format (concat "<%s> "
				  (unless (null (my-exwm-get-other-workspace)) "[%s] "))
			  exwm-workspace-current-index
			  (my-exwm-get-other-workspace))
		  'face 'sml/numbers-separator))
	  ;; (:eval (if (exwm-workspace--active-p exwm-workspace--current)
	  ;; 	     (format "%s " exwm-workspace-current-index)
	  ;; 	     (format "%s " (my-exwm-get-other-workspace)))) ;; TODO this is always true, determine the correct variable
	  sml/pos-id-separator
	  mode-line-mule-info
	  mode-line-client
	  mode-line-modified
	  mode-line-remote
	  mode-line-frame-identification
	  mode-line-buffer-identification
	  sml/pos-id-separator
	  mode-line-front-space
	  mode-line-position
	  evil-mode-line-tag
	  (vc-mode vc-mode)
	  sml/pre-modes-separator
	  mode-line-modes
	  mode-line-misc-info
	  mode-line-end-spaces))
  (sml/setup)
  (set-face-background 'mode-line-inactive "light")) 
(tool-bar-mode -1)
(menu-bar-mode -1)
(menu-bar-no-scroll-bar)

;; eshell
(setq eshell-cmpl-ignore-case t)

(use-package eshell-prompt-extras
  :ensure t
  :config
  (setq eshell-highlight-prompt t
	eshell-prompt-function 'epe-theme-lambda))

(use-package ranger :ensure t
  :commands (ranger)
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

(use-package helm
  :after helm-exwm
  :ensure t
  :config
  (general-define-key
   :keymaps 'helm-find-files-map
   "M-H" 'left-char
   "M-L" 'right-char
   "M-y" 'helm-ff-run-copy-file
   "M-r" 'helm-ff-run-rename-file
   "M-s" 'helm-ff-run-find-file-as-root
   "M-o" 'helm-ff-run-switch-other-frame
   "M-O" 'helm-ff-run-switch-other-window)
  (general-define-key
   :keymaps 'helm-buffer-map
   "M-d" 'helm-buffer-run-kill-persistent)
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

(use-package helm-system-packages
  :ensure t)

(use-package helm-exwm
  :ensure t)

(use-package helm-eww
  :ensure t)

(use-package helm-dictionary
  :ensure t)

(use-package helm-tramp
  :ensure t)

(use-package helm-unicode
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-dabbrev-downcase nil)
  (setq read-file-name-completion-ignore-case t)
  (global-company-mode 1))

;; abbrev mode
(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/HESSENBOX-DA/programming/abbrev-snippets.el")    ;; definitions from...
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(use-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode 1)
    (add-to-list 'company-backends 'company-yasnippet t)
    ;; Add yasnippet support for all company backends
    ;; https://github.com/syl20bnr/spacemacs/pull/179
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")
    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	  backend
	(append (if (consp backend) backend (list backend))
		'(:with company-yasnippet))))
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))))

(use-package yasnippet-snippets
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (my-leader-def
    :states 'normal
    "p" 'projectile-command-map)
  (projectile-mode 1))

(use-package projectile-ripgrep
  :ensure t)

(use-package magit
  :ensure t
  :general (my-leader-def
	     "gs" '(magit-status :which-key "git status")))

(use-package evil-magit :ensure t)

(use-package ediff :ensure t)

(unless (system-name= "localhost" "lina")
  (use-package pdf-tools
    :ensure t
    :init
    (pdf-tools-install)
    :magic ("%PDF" . pdf-view-mode)
    :config
    (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
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
      "m"  '(pdf-view-set-slice-using-mouse :which-key "slice using mouse")
      "b"  '(pdf-view-set-slice-from-bounding-box :which-key "slice from bounding box")
      "R"  '(pdf-view-reset-slice :which-key "reset slice")
      "zr" '(pdf-view-scale-reset :which-key "zoom reset"))))

;;exwm
(unless (system-name= "lina")
  (use-package exwm 
    :ensure t
    :init
    (server-start)
    :config
    (exwm-enable))

  (use-package exwm-config
    :after exwm
    :demand t
    :config
    (evil-set-initial-state 'exwm-mode 'emacs)
    (display-time-mode)
    (setq mouse-autoselect-window nil
	  focus-follows-mouse nil))

  (use-package exwm-input
    :after exwm-randr
    :demand t
    :config
    (define-key exwm-mode-map (kbd "C-c") nil)
    (setq exwm-input-global-keys
	  `(([?\s-r] . exwm-reset)
	    ([?\s-e] . exwm-input-release-keyboard)
	    ([?\s-w] . exwm-workspace-switch)
	    ([?\s-W] . exwm-workspace-move-window)
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
	    ([?\s-d] . dmenu)
	    ([?\s-l] . evil-window-right)
	    ([?\s-h] . evil-window-left)
	    ([?\s-j] . evil-window-down)
	    ([?\s-k] . evil-window-up)
	    ([?\s-c] . kill-this-buffer)
	    ([?\s-q] . my-get-rid-of-mouse)
	    ([?\s-o] . my-exwm-switch-to-other-workspace)
	    ([?\s-O] . my-exwm-move-window-to-other-workspace)
	    ([?\s-m] . delete-other-windows)
	    ([s-f1] . (lambda () (interactive) (eshell 'N)))
	    ([C-s-f1] . eshell)
	    ([s-f2] . (lambda () (interactive)
			(start-process "" nil browser)))
	    ([s-f3] . deer)
	    ([s-f4] . (lambda () (interactive)
			(mu4e)))
	    ([s-f12] . (lambda () (interactive)
			 (start-process "" nil "/usr/bin/slock")))))
    (push ?\s-\  exwm-input-prefix-keys)
    ;; (push ?\M-m  exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "<XF86MonBrightnessUp>")
			#'my-brightness+)
    (exwm-input-set-key (kbd "<XF86MonBrightnessDown>")
			#'my-brightness-)
    (exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
			(lambda () (interactive) (start-process-shell-command "" nil "pactl set-sink-volume @DEFAULT_SINK@ -5%")))
    (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
			(lambda () (interactive) (start-process-shell-command "" nil "pactl set-sink-volume @DEFAULT_SINK@ +5%")))
    (exwm-input-set-key (kbd "<XF86AudioMute>")
			(lambda () (interactive) (start-process-shell-command "" nil "pactl set-sink-mute @DEFAULT_SINK@ toggle"))))

  (use-package exwm-systemtray
    :after exwm
    :demand t
    :config (exwm-systemtray-enable))

  (use-package exwm-randr
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
     ((system-name= "klingenberg-tablet") (progn (set 'monitor1 "eDP1")
						(set 'monitor2 "HDMI2")
						(set 'placement "below")))
     ((system-name= "klingenbergLaptop") (progn (set 'monitor1 "LVDS1")
						(set 'monitor2 "VGA1")
						(set 'placement "below")))
     (t (progn (set 'monitor1 "VGA-1")
	       (set 'monitor2 "HDMI-1")
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
    :after exwm
    :demand t
    :init
    (progn
      (setq exwm-workspace-number 10)
      (setq exwm-workspace-show-all-buffers t)
      (setq exwm-layout-show-all-buffers t))))
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

(use-package lispy
  :ensure t)

(use-package sly
  :ensure t
  :config
  (add-hook 'sly-db-mode 'evil-insert-state) ;TODO
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
  :ensure t
  :general (my-local-leader-def
	     :keymaps 'scheme-mode-map
	     "'" '(geiser :which-key "start reps")
	     "e" '(:ignore :which-key "eval")
	     "ef" '(geiser-eval-definition :which-key "eval definition")
	     "ee" '(geiser-eval-last-sexp :which-key "eval last expression")
	     "eb" '(geiser-eval-buffer :which-key "eval buffer")))

(use-package eval-sexp-fu
  :ensure t
  :config
  (setq eval-sexp-fu-flash-face
	'((((class color)) (:background "black" :foreground "gray" :bold t))
	  (t (:inverse-video nil)))))
;;org
(use-package org
  :ensure org-plus-contrib
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
  :general
    (my-local-leader-def
      :keymaps 'org-mode-map
      "e" '(org-export-dispatch :which-key "export")
      "a" '((lambda () (interactive)
      		    (let ((current-prefix-arg '-)) ; simulate pressing C-u
      		      (call-interactively 'org-export-dispatch))) :which-key "repeat last export")
      "s" '(org-edit-special :which-key "edit source code")
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
  :ensure t
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
  :ensure t
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

(use-package org-bullets
  :ensure t
  :after org
  :config
  (org-bullets-mode 1))

(use-package org-re-reveal
  :ensure t)

(use-package ggtags
  :ensure t)

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

(use-package wgrep
  :ensure t)


;;c#
(require 'csharp-repl)
(use-package omnisharp
  ;; :after company
  :ensure t
  :hook
  (csharp-mode-hook omnisharp-mode)
  (csharp-mode-hook flycheck-mode)
  ;; (csharp-mode-hook company-mode)
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  :general
  (general-define-key
   :states 'normal
   :keymaps 'csharp-mode-map ; TODO figure out why this does not work with omnisharp-mode-map
   "gd" '(omnisharp-go-to-definition :which-key "go to definition")
   "gr" '(omnisharp-rename :which-key "rename"))
  (my-local-leader-def
    :keymaps 'csharp-mode-map ; TODO figure out why this does not work with omnisharp-mode-map
    "b" '(:ignore :which-key "build")
    "bd" '((lambda () (interactive) (compile "msbuild /p:Configuration=Debug")) :which-key "build debug")
    "br" '((lambda () (interactive) (compile "msbuild /p:Configuration=Release")) :which-key "build release")
    "ro" '(run-csharp-repl-other-frame :which-key "start repl")
    "rr" '(csharp-repl-send-region :which-key "csharp-send-region-to-repl")))

;; bosss
(defun bosss-init ()
  (setq bosss-path "/home/klingenberg/BoSSS-experimental/")
  (setq bosss-path-reference "/home/klingenberg/BoSSS-experimental/internal/src/private-kli/RANS_Solver/bin/Debug/RANS_Solver.exe")
  (require 'bosss-repl)
  (require 'bosss)
  (add-to-list 'auto-mode-alist '("\\.bws\\'" . bosss-mode))
  (add-hook 'bosss-mode-hook 'bosss-config))

(defun bosss-config ()
  (my-local-leader-def
    :keymaps 'bosss-mode-map
    "ro" '(run-bosss-repl-other-frame :which-key "start repl")
    "rr" '(bosss-repl-send-current-field :which-key "send region to repl")
    "rm" '(bosss-repl-send-region :which-key "send region to repl")
    "in" '(bosss-create-new-field :which-key "create new input field")))

(bosss-init)

(use-package fsharp-mode
  :ensure t
  :general
  (my-local-leader-def
    :keymaps 'fsharp-mode-map
    "ef" '(fsharp-eval-phrase :which-key "eval current phrase")
    ))

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
  :general
  (my-local-leader-def
    :keymaps 'LaTeX-mode-map
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
    "ob" '(lambda() (interactive) (find-file "bibliography.bib"))))

;; (use-package auctex-latexmk
;;   :ensure t
;;   :defer t
;;   :init
;;   (progn
;;     (auctex-latexmk-setup)
;;     (setq auctex-latexmk-inherit-TeX-PDF-mode t)))

(use-package company-reftex
  :ensure t
  :config
  (add-to-list 'company-backends 'company-reftex-labels t)
  (add-to-list 'company-backends 'company-reftex-citations t))

;; browser
(use-package eww
  :ensure t
  ;; :general
  ;; (general-define-key
  ;;  "f" 'ace-link)
  )

(use-package ace-link
  :ensure t)

;; mail

(defun my-mu4e-setup ()

  (use-package helm-mu
    :ensure t)

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
  (setq mu4e-update-interval 300)
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
Telefon: +9 6151 16-26207
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

  (use-package evil-mu4e
    :ensure t
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
    :ensure t
    :config
    (mu4e-alert-enable-mode-line-display)
    ;; (setq alert-default-style 'libnotify) ; not sure why this is needed
    (mu4e-alert-set-default-style 'notifications)
    (setq mu4e-alert-interesting-mail-query
	  (concat "(maildir:<fu> AND date:today..now"
		  " OR maildir:<bar> AND date:today..now"
		  " AND flag:unread"))
    (alert-add-rule
     :category "mu4e-alert"
     :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode)))
     :continue t)

    ;; display stuff on modeline as well as notify
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)))

(unless (or (system-name= "localhost") (system-name= "lina"))
  (my-mu4e-setup))

(use-package rainbow-delimiters
  :ensure t
  :init (rainbow-delimiters-mode t))

(when (or (system-name= "klingenberg-tablet") (system-name= "klingenbergLaptop"))
  (use-package guix :ensure t))

(use-package dmenu
  :ensure t)

;; (use-package auto-dim-other-buffers
;;   :ensure t
;;   :config (auto-dim-other-buffers-mode t))

(use-package google-translate
  :ensure t)

(use-package excorporate
  :ensure t
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
  :ensure t)

(use-package sx
  :ensure t
  :config
  (general-define-key
   :keymaps 'sx-question-list-mode-map
   :states 'normal
   "RET" 'sx-display))

(use-package md4rd
  :ensure t
  :config
  (mapc
   (lambda (elem) (add-to-list 'md4rd-subs-active elem))
   '(linux
     baduk)))

(use-package podcaster
  :ensure t
  :config
  (setq podcaster-feeds-urls "https://www.zeitsprung.fm"))

(show-paren-mode 1)

;;; TODO
;; - scrolling (?)
;; - mail: notifications
;; - eshell: expand
;; related to https://lists.gnu.org/archive/html/bug-gnu-emacs/2012-11/msg00878.html
;; - el-go

