;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq! user-full-name "Helen Klingenberg"
       user-mail-address "helen.grohme@gmail.com")

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
;; (setq! doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; (setq! doom-font (font-spec :family "Fira Code")
;;       doom-variable-pitch-font (font-spec :family "Fira Code"))
;; (setq! doom-font (font-spec :family "DejaVu Sans Mono"))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq! doom-theme 'doom-solarized-dark)

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

(setq! evil-collection-setup-minibuffer t)
(setq! evil-ex-substitute-global t)

(setq! +evil-want-o/O-to-continue-comments nil )
(setq! display-time-24hr-format t
       display-time-default-load-average nil )

(setq! ispell-dictionary "en_GB")
(setq! ispell-alternate-dictionary nil)

(setq! flycheck-checker-error-threshold 10000)
;; (setq! ispell-alternate-dictionary "de_DE")


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
        ("Csharp (.Net)" "https://docs.microsoft.com/en-us/search/?terms=%s&scope=.NET")
        ("Github" "https://github.com/search?ref=simplesearch&q=%s")
        ("Youtube" "https://youtube.com/results?aq=f&oq=&search_query=%s")
        ("Doom Emacs issues" "https://github.com/hlissner/doom-emacs/issues?q=is%%3Aissue+%s")))

(setq! browse-url-browser-function 'browse-url-firefox)
(setq-default abbrev-mode t)


(cond
 ((system-name= "klingenberg-laptop" "klingenberg-pc" "helensInfinitybook")
  (add-load-path! "/usr/share/emacs/site-lisp/")
  (add-load-path! "/usr/share/stumpwm/contrib/util/swm-emacs/"))
 ((system-name= "klingenberg-pi")
  (add-load-path! "/run/current-system/sw/share/emacs/site-lisp/mu4e"))
 ((system-name= "klingenberg-tablet")
  (add-load-path! "/run/current-system/profile/share/emacs/site-lisp/")
  (add-load-path! "~/.guix-profile/share/emacs/site-lisp/")))

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
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)))

(use-package! projectile
  :config
  (setq projectile-per-project-compilation-buffer t))

(after! org-pomodoro
  (defun my/disable-notifications ()
    (mu4e-alert-disable-mode-line-display)
    (mu4e-alert-disable-notifications)
    (shell-command "dunstctl set-paused true"))
  (defun my/enable-notifications ()
    (mu4e-alert-enable-mode-line-display)
    (mu4e-alert-enable-notifications)
    (shell-command "dunstctl set-paused false"))
  (defun my/org-pomodoro-text-time ()
    "Display remaining pomodoro time in i3 status bar. Credit to dakra on reddit."
    (if (org-pomodoro-active-p)
        (cond ((eq org-pomodoro-state :pomodoro) (format "pomodoro: %s" (org-pomodoro-format-seconds)))
              ((eq org-pomodoro-state :short-break) (format "break: %s" (org-pomodoro-format-seconds)))
              ((eq org-pomodoro-state :long-break) (format "break: %s" (org-pomodoro-format-seconds))))
      ""))
  :config
  (setq org-pomodoro-manual-break t)
  (add-hook! 'org-pomodoro-started-hook #'my/disable-notifications)
  (add-hook! 'org-pomodoro-started-hook #'org-todo)
  (add-hook! 'org-pomodoro-overtime-hook #'my/enable-notifications))
  (add-hook! 'org-pomodoro-finished-hook #'my/enable-notifications)


(customize-set-variable 'compilation-scroll-output t)

(after! evil-snipe (evil-snipe-mode -1))

(customize-set-variable 'avy-all-windows t)

(customize-set-variable 'avy-single-candidate-jump t)

(setq! magit-repository-directories '(("~/" . 1)))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)


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
%a" :prepend t) ("b" "BoSSS calculation" entry (file+headline "~/Documents-work/bosss/calculation-log.org" ,(with-temp-buffer
                                                                                                              (org-insert-time-stamp (current-time)))) "** RUNNING %T %(my/bosss-worksheet-get-project-name \"%f\")
- %(org-link-make-string (concat (bosss-get-most-recent-deploy-directory) \"/stdout.txt\")  \"stdout.txt\")
- PID: %(bosss-get-most-recent-pid)
- previous calculation:
- %?
%i" :prepend nil ) ("p" "Templates for projects") ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?
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

;; (setq! org-roam-capture-templates
;;       '(("d" "default" plain #'org-roam-capture--get-point "%? \n %i \n %a"
;;          :file-name "%<%Y%m%d%H%M%S>-${slug}"
;;          :head "#+TITLE: ${title}\n"
;;          :unnarrowed t)))

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
  '(("^\\*bosss\\*" :slot -1 :size 20 :select nil ) ; popup bosss process buffer
    ("^\\*Async Shell Command\\*" :slot -1 :size 20)))
;;; Defining some useful functions



(defun my/make-alert (time mesg)
  (run-at-time
   time
   nil
   (lambda ()
     (alert mesg :title "Reminder")))
  (message "Made alert for %s at %s" mesg time))

(after! bash-completion
  (setq! bash-completion-nospace t))


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
;; TODO why doesnt the map! macro work?
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-j") nil )
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-k") nil )
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-h") nil )
(evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "M-l") nil )
;; (map! :map outline-minor-mode-map ;TODO check if this messes up other situtations
;;       :n "M-j" nil
;;       :n "M-k" nil
;;       :n "M-h" nil
;;       :n "M-l" nil )

 ;; disable single-key bindings

(defun my/python-shell-send-main ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents "./main.py")
    (python-shell-send-string (concat (buffer-string) "\nmain()"))))

(defun my/python-shell-send-buffer ()
  (interactive)
  (if (file-exists-p "./main.py")
      (my/python-shell-send-main)
      (python-shell-send-buffer)))

(map!
 :localleader
 :map python-mode-map
 "ef" #'python-shell-send-defun
 "ee" #'python-shell-send-statement
 "eB" #'python-shell-send-buffer
 "eb" #'my/python-shell-send-buffer
 "em" #'my/python-shell-send-main)

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
;; (setq! +latex-viewers '(zathura))
(setq my/latex-macro-directory
      (cond
       ((system-name= "klingenberg-laptop" "klingenberg-tablet") "~/Documents/conferences/latex_macros/")
       ((system-name= "klingenberg-pc") "~/Documents/conferences/latex_macros/")))
(setq! reftex-default-bibliography (concat my/latex-macro-directory "bibliography.bib"))
(setq my/latex-bibliography-file (concat my/latex-macro-directory "bibliography.bib"))
(setq my/latex-macro-file (concat my/latex-macro-directory "dakling.sty"))

(map!
 :localleader
 :map bibtex-mode-map
 :n "d" (lambda (doi)
          (interactive
           (list (read-string
                  "DOI: "
                  ;; now set initial input
                  (doi-utils-maybe-doi-from-region-or-current-kill))))
          (doi-utils-add-bibtex-entry-from-doi
           doi
           (buffer-file-name))))

;; (add-hook! '(TeX-mode-hook LaTeX-mode-hook) (visual-line-mode -1))

(use-package! latex
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
  (add-hook! '(TeX-mode-hook LaTeX-mode-hook latex-mode-hook tex-mode-hook)
             :append
    (visual-line-mode -1)
    (auto-fill-mode 1)
    (setq-local TeX-electric-math (cons "\\(" "")) ; gets closed automatically apparently
    (add-hook! '(before-save-hook) #'reftex-parse-all)
    ;; (setq-local TeX-electric-math (cons "\\(" "\\)"))
    ))

(use-package! lsp-ltex
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp)))          ; or lsp-deferred
  :init
  (setq lsp-ltex-version "15.2.0")
  :config
  (setq lsp-ltex-enabled t)
  ;; (setq lsp-ltex-language "en-US")
  (setq lsp-ltex-language "en-GB"))

(use-package! cdlatex
  :config
  (setq cdlatex-math-symbol-prefix ?#)
  (map! :map cdlatex-mode-map
        "#" #'cdlatex-math-symbol)
  (map! :map org-cdlatex-mode-map
        "#" #'cdlatex-math-symbol))

(use-package! font-latex)

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

;;c#
(use-package! lsp
  :config
  (setq lsp-auto-guess-root t))

(use-package! gud
  :config
  (defhydra gud-hydra (:color pink :hint nil :foreign-keys run)
    "
^Stepping^          ^Switch^                 ^Breakpoints^         ^Debug^
^^^^^^^^---------------------------------------------------------------------------------
_n_: Next           _su_: Up stack frame     _bb_: Toggle          _dd_: Debug
_i_: Step in        _sd_: Down stack frame   _bd_: Delete          _ds_: Debug restart
_c_: Continue
_r_: Restart frame
_Q_: Disconnect     "

    ("n" gud-next)
    ("i" gud-step)
    ("c" gud-cont)
    ("r" gud-refresh)
    ("su" gud-up)
    ("sd" gud-down)
    ("bb" gud-break)
    ("bd" gud-remove)
    ("dd" gud-run)
    ("ds" gud-refresh)
    ("q" nil "quit" :color blue)
    ("Q" gud-finish :color red))
(defun gud-hydra ()
  "Run `gud-hydra/body'."
  (interactive)
  (gud-hydra/body)))

(use-package! dap-mode
  :config
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (require 'dap-python)
  (require 'dap-netcore))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(after! lsp
  ;; (setq! lsp-file-watch-threshold 30000)
  (setq! lsp-file-watch-threshold nil))


(add-hook! '(yaml-mode-hook)
 :append
 (visual-line-mode -1))

(map! :map (company-mode-map company-active-map)
      "RET" nil
      "<return>" nil
      "<left>" nil
      ;; :i "TAB" #'+company/complete
      ;; :i "<right>" #'company-complete-selection
      :i "M-RET" #'company-complete-selection
      :i "M-l" #'company-complete-selection
      :i "M-j" #'company-select-next-or-abort
      :i "M-k" #'company-select-previous-or-abort
      :i "C-l" #'company-complete-selection
      :i "C-j" #'company-select-next-or-abort
      :i "C-k" #'company-select-previous-or-abort)

(map!
 :after evil-snipe
 :v "s" #'evil-surround-region)

(map!
 :map evil-snipe-mode-map
 :after evil-snipe
 :n "s" #'evil-avy-goto-char-timer)

(use-package! helm
  :diminish helm-mode
  :config
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-buffer-not-found))
  (map!
   :map doom-leader-buffer-map
   "b" #'helm-mini)
  (map!
   :map helm-map
   "M-j" #'helm-next-line
   "M-k" #'helm-previous-line
   "M-h" #'helm-find-files-up-one-level
   "M-l" #'helm-execute-persistent-action
   ;; "M-l" #'helm-maybe-exit-minibuffer
   "M-w" #'helm-select-action
   "M-H" #'left-char
   "M-L" #'right-char
   "M-TAB" #'helm-toggle-visible-mark-forward)
  (map!
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
   "M-RET" #'helm-ff-run-open-file-with-default-tool)
  (map!
   :map helm-buffer-map
   "M-l" #'helm-maybe-exit-minibuffer
   "M-d" #'helm-buffer-run-kill-persistent)
  (setq! helm-move-to-line-cycle-in-source nil)
  (setq! helm-truncate-lines nil)
  (setq! helm-buffer-max-length nil)
  (setq! helm-buffers-truncate-lines nil))


(use-package! pdf-tools
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
 '(vc-annotate-background-mode nil )
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
