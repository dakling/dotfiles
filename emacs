;;; auto-generated stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bc75dfb513af404a26260b3420d1f3e4131df752c19ab2984a7c85def9a2917e" default)))
 '(package-selected-packages
   (quote
    (eval-sexp-fu rainbow-delimiters multi-eshell auctex-latexmk em-smart eshell-prompt-extras exwm-randr auctex evil-mu4e mu4e company exwm smart-mode-line-atom-one-dark-theme zenburn-theme pdf-tools reduce-ide evil-commentary evil-surround slime evil-magit magit counsel zeno-theme zeno evil ranger which-key general use-package)))
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
  (general-auto-unbind-keys)

  (general-create-definer my-leader-def
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "s-SPC"
    :states '(normal emacs))

  (general-create-definer my-local-leader-def
    :keymaps 'override
    :prefix "-"
    :states 'normal)

  (general-nmap "Y" "y$")

  (general-define-key "ESC" 'keyboard-quit :which-key "abort command")

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
    ;; "wo" '(other-window :which-key "other window")
    "SPC" '(counsel-M-x :which-key "M-x")
    "fp" '(counsel-locate :which-key "counsel-locate")
    "fg" '(counsel-ag :which-key "counsel-ag")
    "b" '(:ignore :which-key "buffer")
    "bb" '(counsel-switch-buffer :which-key "switch buffer")
    "bk" '(kill-this-buffer :which-key "kill buffer") ; TODO kill current buffer immediately
    ;; "w TAB"  'spacemacs/alternate-window
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
    "wo"  'other-frame
    "ws"  'split-window-below
    "wS"  'split-window-below-and-focus
    "w-"  'split-window-below
    "wU"  'winner-redo
    "wu"  'winner-undo
    "wv"  'split-window-right
    "wV"  'split-window-right-and-focus
    "ww"  'other-window
    "w="  'balance-windows
    ;; "w+"  'spacemacs/window-layout-toggle
    "ee"  'eval-last-sexp
    "ef"  'eval-defun
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
(use-package smart-mode-line
  :config
  (setq sml/theme 'atom-one-dark)
  (setq mode-line-format
	'("%e"
	  (:eval (propertize
	  	  (format "<%s> [%s]"
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
  ) 
(tool-bar-mode -1)
(menu-bar-mode -1)
(menu-bar-no-scroll-bar)

;; eshell
(use-package eshell
  :config
  (setq shell-protect-eshell-prompt nil)
  (setq eshell-cmpl-ignore-case t))

(use-package eshell-prompt-extras
  :ensure t
  :config
  (setq eshell-highlight-prompt nil
	eshell-prompt-function 'epe-theme-lambda))

(use-package ranger :ensure t
  :commands (ranger)
  :config
  (setq ranger-cleanup-eagerly t)
  (ranger-override-dired-mode t))

(use-package ivy :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  )

(use-package counsel :ensure t)

(use-package company
  :ensure t
  :config (company-mode 1))

(use-package magit
  :ensure t
  :general (my-leader-def
	     "gs" '(magit-status :which-key "git status")))

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
    "zr" '(pdf-view-scale-reset :which-key "zoom reset")))

;;exwm
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
  (setq mouse-autoselect-window t
	focus-follows-mouse t))

(use-package exwm-input
  :after exwm-randr
  :demand t
  :config
  (setq exwm-input-global-keys
	`(([?\s-r] . exwm-reset)
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
	  ([?\s-d] . counsel-linux-app)
	  ([?\s-l] . evil-window-right)
	  ([?\s-h] . evil-window-left)
	  ([?\s-j] . evil-window-down)
	  ([?\s-k] . evil-window-up)
	  ([?\s-c] . kill-this-buffer)
	  ([?\s-o] . my-exwm-switch-to-other-workspace)
	  ([?\s-O] . my-exwm-move-window-to-other-workspace)
	  ([?\s-m] . delete-other-windows)
	  ([s-f1] . (lambda () (interactive) (eshell 'N)))
	  ([s-f2] . (lambda () (interactive)
		      (start-process "" nil "qutebrowser")))
	  ([s-f3] . deer)
	  ([s-f4] . (lambda () (interactive)
		      (start-process "" nil "thunderbird")))
	  ([s-f12] . (lambda () (interactive)
		       (start-process "" nil "/usr/bin/slock")))))
  (push ?\s-\  exwm-input-prefix-keys)
  ;; (push ?\M-m  exwm-input-prefix-keys)
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
  (progn
    (if (string-equal "klingenbergTablet" (getenv "HOSTNAME"))
	(progn (set 'monitor1 "eDP1")
	       (set 'monitor2 "HDMI2"))
      (progn (set 'monitor1 "VGA-1")
	     (set 'monitor2 "HDMI-1")))
    ;; (set 'monitor1 "VGA-1")
    ;; (set 'monitor2 "HDMI-1")
    (defun my/exwm-xrandr ()
      "Configure screen with xrandr."
      (start-process-shell-command
       "xrandr" nil
       (if (string-equal "klingenbergTablet" (getenv "HOSTNAME"))
	   "xrandr --output VGA-1 --primary --left-of HDMI-1 --auto"
	 "xrandr --output eDP1 --primary --below-of HDMI1 --auto"))))
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
  (setq exwm-workspace-number 10)
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t))

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

(use-package eval-sexp-fu
  :ensure t
  :config
  (setq eval-sexp-fu-flash-face
    '((((class color)) (:background "white" :foreground "black" :bold t))
      (t (:inverse-video t)))))

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

;;latex (auctex)
(use-package tex
  :ensure auctex
  :init
  (progn
    (setq TeX-command-default 'LatexMK
	  TeX-view-program-selection '((output-pdf "PDF Tools"))
	  TeX-source-correlate-start-server t
	  TeX-auto-save t
	  TeX-parse-self t
	  TeX-syntactic-comment t
	  ;; Synctex support
	  TeX-source-correlate-start-server nil
	  TeX-interactive-mode 1
	  ;; Don't insert line-break at inline math
	  LaTeX-fill-break-at-separators nil))
  :hook
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  (LaTeX-mode-hook . LaTeX-math-mode)
  (LaTeX-mode-hook . TeX-source-correlate-mode)
  (LaTeX-mode-hook . TeX-PDF-mode)
  :config
  (progn
    ;; Key bindings for plain TeX
    :general
    (my-local-leader-def
      "\\"  'TeX-insert-macro                            ;; C-c C-m
      "-"   'TeX-recenter-output-buffer                  ;; C-c C-l
      "%"   'TeX-comment-or-uncomment-paragraph          ;; C-c %
      ";"   'TeX-comment-or-uncomment-region             ;; C-c ; or C-c :
      ;; TeX-command-run-all runs compile and open the viewer
      "a"   'TeX-command-run-all                         ;; C-c C-a
      "b"   'TeX-command-master
      "k"   'TeX-kill-job                                ;; C-c C-k
      "l"   'TeX-recenter-output-buffer                  ;; C-c C-l
      "m"   'TeX-insert-macro                            ;; C-c C-m
      "v"   'TeX-view                                    ;; C-c C-v
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
      "xfr" 'latex/font-serif)))

(use-package auctex-latexmk
  :ensure t
  :defer t
  :init
  (progn
    (auctex-latexmk-setup)
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)))

;; mail
(use-package evil-mu4e
  :ensure t
  :init
  (setq mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e")
  (setq mu4e-maildir "~/Mail"
	mu4e-trash-folder "/Trash"
	mu4e-refile-folder "/Archive"
	mu4e-get-mail-command "offlineimap -o"
	mu4e-update-interval 600
	mu4e-compose-signature-auto-include t
	mu4e-view-show-images t
	mu4e-enable-notifications t
	message-send-mail-function 'smtpmail-send-it
	smtpmail-stream-type 'starttls
	mu4e-view-show-addresses t)
  (setq mu4e-account-alist
	'(("Gmail"
	   ;; Under each account, set the account-specific variables you want.
	   (mu4e-sent-messages-behavior delete)
	   (mu4e-compose-signature-auto-include nil)
	   (mu4e-sent-folder "/Gmail/sent")
	   (mu4e-drafts-folder "/Gmail/drafts")
	   (user-mail-address "dario.klingenberg@gmail.com")
	   (smtpmail-smtp-server "smtp.gmail.com")
	   (smtpmail-smtp-service 465)
	   (user-full-name "Dario Klingenberg"))
	  ("Web"
	   (mu4e-sent-messages-behavior sent)
	   (mu4e-compose-signature-auto-include nil)
	   (mu4e-sent-folder "/Web/Sent Items")
	   (mu4e-drafts-folder "/Web/Drafts")
	   (smtpmail-smtp-server "smtp.web.de")
	   (smtpmail-smtp-service 587)
	   (user-mail-address "dario.klingenberg@web.de")
	   (user-full-name "dario"))
	  ("FDY"
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
	   (user-mail-address "klingenberg@gsc.tu-darmstadt.de")
	   (user-full-name "Dario Klingenberg"))
	  ))
  ;; (mu4e/mail-account-reset)
  :config
    (evil-define-key 'evilified mu4e-main-mode-map (kbd "j") 'evil-next-line)
    (bind-keys :map mu4e-main-mode-map
	       ;; ("j" . evil-next-line)
	       ("c" . mu4e-compose-new)))


(use-package rainbow-delimiters
  :ensure t
  :init (rainbow-delimiters-mode t))

(show-paren-mode 1)

;;; TODO
;; - font
;; - scrolling (?)
;; - autocomplete
;; - snippets
;; - buffer management
;; - mail
;; - exwm
;; - exwm SPC w (window management)
;; - exwm multiple monitors
;; - exwm host-specific settings
;; - latex
;; - eshell
