;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     python
     markdown
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     (auto-completion :variables
        auto-completion-enable-snippets-in-popup t
        auto-completion-tab-key-behavior 'cycle
        auto-completion-ret-key-behavior 'complete
        ;; spacemacs-default-company-backends '(company-files company-capf)
        auto-completion-enable-sort-by-usage t)
      (haskell :variables haskell-completion-backend 'intero)
     better-defaults
     emacs-lisp
     evil-snipe
     git
     ;; markdown
     org
     (shell :variables
            shell-default-shell 'eshell
            shell-default-position 'full
            shell-enable-smart-eshell t)
     spell-checking
     ;; syntax-checking
     latex
     (ranger :variables ranger-override-dired t)
     (pdf-tools :variables
                pdf-view-continuous t
                pdf-view-incompatible-modes linum-mode)
     haskell
     common-lisp
     scheme
     ;; racket
     ;; julia ;only in development branch of spacemacs right now
     csharp
     (mu4e :variables mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e")
   )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      eww-lnum
                                      ;; exwm
                                      md4rd
                                      yasnippet-snippets
                                      helm-system-packages
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    exec-path-from-shell
                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 10)
                                (bookmarks . 10)
                                (todos . 10))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         monokai
                         spacemacs-dark
                         ;; spacemacs-light
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 11
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   ;; dotspacemacs-leader-key "s-SPC"
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "s-y"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key "#"
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-mode-line-theme 'all-the-icons
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative nil
                                :disabled-for-modes dired-mode
                                                    doc-view-mode
                                                    markdown-mode
                                                    org-mode
                                                    pdf-view-mode
                                                    text-mode
                                :size-limit-kb 1000)
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  ;; for obvious reasons get rid of fd as escape sequence
  (setq-default evil-escape-key-sequence nil)
  ;; exwm
 ;;  (display-time-mode)
 ;;  (server-start)
 ;;  (setq mouse-autoselect-window t
 ;;        focus-follows-mouse t)
 ;;  (require 'exwm)
 ;;  (require 'exwm-config)
 ;;  (setq exwm-workspace-number 10)
 ;;  (setq exwm-workspace-show-all-buffers t)
 ;;  (setq exwm-layout-show-all-buffers t)
 ;;  (require 'exwm-randr)
 ;;  (set 'monitor1 "eDP1")
 ;;  (set 'monitor2 "HDMI2")
 ;;  (setq exwm-randr-workspace-output-plist
 ;;          '(0 monitor1
 ;;            2 monitor1
 ;;            4 monitor1
 ;;            6 monitor1
 ;;            8 monitor1
 ;;            1 monitor2
 ;;            3 monitor2
 ;;            5 monitor2
 ;;            7 monitor2
 ;;            9 monitor2))
 ;;  (add-hook 'exwm-randr-screen-change-hook
 ;;            (lambda ()
 ;;              (start-process-shell-command
 ;;               "xrandr" nil "xrandr --ouput HDMI2 --output eDP1 --auto")))
 ;;  (exwm-randr-enable)
 ;;  (require 'exwm-systemtray)
 ;;  (exwm-systemtray-enable)
 ;;  (evil-set-initial-state 'exwm-mode 'emacs)
 ;;  (setq exwm-input-global-keys
 ;;      `(([?\s-r] . exwm-reset)
 ;;        ([?\s-w] . exwm-workspace-switch)
 ;;        ,@(mapcar (lambda (i)
 ;;                    `(,(kbd (format "s-%d" i)) .
 ;;                      (lambda ()
 ;;                        (interactive)
 ;;                        (exwm-workspace-switch-create ,i))))
 ;;                  (number-sequence 0 9))
 ;; ;; Bind "s-&" to launch applications ('M-&' also works if the output
 ;;        ;; buffer does not bother you).
 ;;        ([?\s-d] . (lambda (command)
 ;;  	                 (interactive (list (read-shell-command "$ ")))
 ;;  	                 (start-process-shell-command command nil command)))
 ;;        ([s-f12] . (lambda ()
 ;;  	                 (interactive)
 ;;  	                 (start-process "" nil "/usr/bin/slock")))
 ;;        ([s-f2] . (lambda ()
 ;;                    (interactive)
 ;;                    (start-process "" nil "qutebrowser")))
 ;;        ([s-f1] . (lambda ()
 ;;  	                  eshell))
 ;;        ))
 ;;  ;; (push ?\s-\  exwm-input-prefix-keys)
 ;;  (push ?\M-m  exwm-input-prefix-keys)
 ;;  (exwm-enable)
  ;; for tiling wms set true
  (setq pop-up-frames nil)
  ;; (set-frame-parameter nil 'fullscreen 'fullboth)
  (setq eshell-cmpl-ignore-case t)
  (setq ivy-ignore-buffers '("\\` " "\\`\\*"))
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  ;; (setq-default TeX-master "../main.tex") ; Master file is always called main in the directory above
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode "o m" (lambda() (interactive)(find-file TeX-master)))
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode "o g" (lambda() (interactive)(find-file (getenv "LatexGlobalConfig"))))
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode "o l" (lambda() (interactive)(find-file "definLocal.tex")))
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode "o b" (lambda() (interactive)(find-file "bibliography.bib")))
  (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode "-" 'pdf-view-shrink)
  (setq powerline-default-separator 'bar)
  ;; (add-hook 'pdf-view-mode (lambda ()(evil-define-key 'normal key-translation-map (kbd "l") (kbd "d"))))
  ;; (add-hook 'pdf-view-mode ((lambda ()(global-unset-key "l"))))
  ;; (add-hook 'pdf-view-mode ((lambda ()(define-key evil-normal-state-map "l" 'pdf-view-scroll-down-or-previous-page))))
  (eval-after-load "eww"
    '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
            (define-key eww-mode-map "F" 'eww-lnum-universal)))
  (add-hook 'after-save-hook
            (lambda ()
                     (when (string= major-mode 'latex-mode)
                       (latex/build))))
;;; Set up some common mu4e variables
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
Telefon: +49 6151 16-26207
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
  (mu4e/mail-account-reset)
  (with-eval-after-load 'mu4e-alert
    ;; Enable Desktop notifications
    (mu4e-alert-set-default-style 'notifications)) ; For linux
  (with-eval-after-load 'mu4e
    (evil-define-key 'evilified mu4e-main-mode-map (kbd "j") 'evil-next-line)
    (bind-keys :map mu4e-main-mode-map
               ;; ("j" . evil-next-line)
               ("c" . mu4e-compose-new)))
;; ;;; Mail directory shortcuts
;;   (setq mu4e-maildir-shortcuts
;;         '(("/gmail/INBOX" . ?g)
;;           ("/web/INBOX" . ?c)))

;; ;;; Bookmarks
;;   (setq mu4e-bookmarks
;;         `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
;;           ("date:today..now" "Today's messages" ?t)
;;           ("date:7d..now" "Last 7 days" ?w)
;;           ("mime:image/*" "Messages with images" ?p)
;;           (,(mapconcat 'identity
;;                        (mapcar
;;                         (lambda (maildir)
;;                           (concat "maildir:" (car maildir)))
;;                         mu4e-maildir-shortcuts) " OR ")
;;            "All inboxes" ?i)))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-electric-math (quote ("\\(" . "")))
 '(TeX-electric-sub-and-superscript t)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open"))))
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(electric-pair-mode t)
 '(evil-want-Y-yank-to-eol t)
 '(find-directory-functions (quote (cvs-dired-noselect dired-noselect)))
 '(inhibit-startup-screen nil)
 '(package-selected-packages
   (quote
    (md4rd helm-pydoc helm-system-packages yasnippet-snippets nix-mode helm-nixos-options company-nixos-options nixos-options geiser exwm-x switch-window mmm-mode markdown-toc markdown-mode gh-md slime-company slime helm-hoogle helm-gitignore common-lisp-snippets omnisharp shut-up csharp-mode smeargle orgit magit-gitflow gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit ghub treepy graphql with-editor org-mime intero flycheck hlint-refactor hindent haskell-snippets company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode go gnugo xpm ascii-art-to-unicode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional cython-mode company-anaconda anaconda-mode pythonic eww-lnum mu4e-maildirs-extension mu4e-alert ht exwm xelb engine-mode wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help w3m pdf-tools tablist evil-snipe ample-zenburn-theme ranger unfill org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-download mwim htmlize helm-company helm-c-yasnippet gnuplot fuzzy flyspell-correct-helm flyspell-correct company-statistics company-auctex company auto-yasnippet yasnippet auto-dictionary auctex-latexmk auctex ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(preview-auto-cache-preamble nil)
 '(ranger-show-hidden nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.tu-darmstadt.de")
 '(smtpmail-smtp-service 587)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C")))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-electric-math (quote ("\\(" . "")))
 '(TeX-electric-sub-and-superscript t)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open"))))
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(electric-pair-mode t)
 '(evil-want-Y-yank-to-eol t)
 '(find-directory-functions (quote (cvs-dired-noselect dired-noselect)))
 '(inhibit-startup-screen nil)
 '(package-selected-packages
   (quote
    (lsp-julia lsp-mode julia-repl julia-mode mmm-mode markdown-toc markdown-mode gh-md slime-company slime helm-hoogle helm-gitignore common-lisp-snippets omnisharp shut-up csharp-mode smeargle orgit magit-gitflow gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit ghub treepy graphql with-editor org-mime intero flycheck hlint-refactor hindent haskell-snippets company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode go gnugo xpm ascii-art-to-unicode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional cython-mode company-anaconda anaconda-mode pythonic eww-lnum mu4e-maildirs-extension mu4e-alert ht exwm xelb engine-mode wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help w3m pdf-tools tablist evil-snipe ample-zenburn-theme ranger unfill org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-download mwim htmlize helm-company helm-c-yasnippet gnuplot fuzzy flyspell-correct-helm flyspell-correct company-statistics company-auctex company auto-yasnippet yasnippet auto-dictionary auctex-latexmk auctex ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(preview-auto-cache-preamble nil)
 '(ranger-show-hidden nil)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C")))))
)
