
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("reduce ide" . "http://reduce-algebra.sourceforge.net/reduce-ide/packages/")))
(package-initialize) ; guess what this one does ?

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package) ; guess what this one does too ?

;; defaults suggested by blog and extended by me
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome") ; print a default message in the empty scratch buffer opened at startup
(defalias 'yes-or-no-p 'y-or-n-p) ;reduce typing effort
(electric-pair-mode 1) ;close brackets

;; useful functions
(defun find-config-file ()
  "open emacs configuration file"
  (interactive)
  (find-file "~/.dotfiles/dotfiles/emacs"))
(defun load-config-file ()
  "load emacs configuration file"
  (interactive)
  (load-file "~/.dotfiles/dotfiles/emacs"))
(defun find-dotfile-dir ()
  "open dotfile directory"
  (interactive)
  (find-file "~/.dotfiles/dotfiles/"))

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

  (general-create-definer my-leader-def
    :keymaps 'override
    :prefix "SPC"
    :states 'normal)

  (general-create-definer my-local-leader-def
    :keymaps 'override
    :prefix "-"
    :states 'normal)

  ;; many spacemacs bindings go here
  (my-leader-def
   "ar" '(ranger :which-key "call ranger")
   "ad" '(deer :which-key "call deer")
   "g"  '(:ignore t :which-key "git")
   "f" '(:ignore :which-key "file")
   "fs" '(save-buffer :which-key "save file")
   "ff" '(counsel-find-file :which-key "find file")
   "fed" '(find-config-file :which-key "find config file")
   ;; "fed" '((lambda () (interactive) (find-file "~/.emacs.d")) :which-key "find config file") ;less nice (I think) alternative
   "fer" '(load-config-file :which-key "load config file")
   "feD" '(find-dotfile-dir :which-key "find dotfile directory")
   ;; "w" '(:ignore :which-key "window")
   ;; "wo" '(other-window :which-key "other window")
   "SPC" '(counsel-M-x :which-key "M-x")
   "fp" '(counsel-locate :which-key "counsel-locate")
   "fg" '(counsel-ag :which-key "counsel-ag")
   ))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  :general (general-define-key
	    :states '(visual)
	    "s" 'evil-surround-region))

(use-package evil-commentary
  :ensure t
  :init (evil-commentary-mode))
 
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode)

;;appearance
(use-package zenburn-theme :ensure t)
;; (use-package cyberpunk-theme :ensure t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(menu-bar-no-scroll-bar)

(use-package ranger :ensure t
  :commands (ranger)
  :config
  (setq ranger-cleanup-eagerly t))

(use-package ivy :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  )

(use-package counsel :ensure t)

(use-package magit
  :ensure t
  :general (my-leader-def
	     "gs" '(magit-status :which-key "git status"))
  )

(use-package evil-magit :ensure t)

(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq pdf-view-continuous nil)
  (evil-collection-init 'pdf)
  :general
  (general-define-key
    :states 'normal
    :keymaps 'pdf-view-mode-map
    ;; evil-style bindings
    ;; "SPC"  nil ;TODO where to put this globally?
    "-"  nil ;TODO where to put this globally?
    "j"  '(pdf-view-next-line-or-next-page :which-key "scroll down")
    "k"  '(pdf-view-previous-line-or-previous-page :which-key "scroll up")
    "L"  '(image-forward-hscroll :which-key "scroll right")
    "H"  '(image-backward-hscroll :which-key "scroll left")
    "l"  '(pdf-view-next-page :which-key "page down")
    "h"  '(pdf-view-previous-page :which-key "page up")
    "u"  '(pdf-view-scroll-down-or-previous-page :which-key "scroll down")
    "d"  '(pdf-view-scroll-up-or-next-page :which-key "scroll up")
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
    "b"  '(pdf-view-set-slice-from-bounding-box :which-key "sclice from bounding box")
    "R"  '(pdf-view-reset-slice :which-key "reset slice")
    "zr" '(pdf-view-scale-reset :which-key "zoom reset"))
  )

;;;programming languages
;; lisp
(use-package slime
  :defer t
  :config (setq inferior-lisp-program "/usr/bin/sbcl")
  ;;:init (setenv 'SBCL-HOME " ") ;;TODO
  :general (my-local-leader-def
	     :keymaps 'lisp-mode-map
	     "'" '(slime :which-key "start slime")
	     "e" '(:ignore :which-key "slime eval")
	     "ef" '(slime-eval-function :which-key "eval function")
	     "ee" '(slime-eval-last-expression :which-key "eval last expression")
	     "eb" '(slime-eval-buffer :which-key "eval buffer"))
  )

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme pdf-tools reduce-ide evil-commentary evil-surround slime evil-magit magit counsel zeno-theme zeno evil ranger which-key general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
