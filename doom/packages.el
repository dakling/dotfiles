;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
;
;; Beginning of my packages

;; exwm
;; (package! xelb)
;; (package! exwm)
;; (package! dmenu)
;; (package! pulseaudio-control)

(package! mu4e-alert)

(package! smooth-scrolling)

(package! telega
  :pin "9187e6e3d903474645f3e64806bc62ef687ec205")

(package! slack)

(package! alert)

(package! pinentry)

(package! elfeed)
(disable-packages! elfeed-goodies)

(package! diminish)

(package! async-await)

(package! evil-tex :recipe (:host github :repo "itai33/evil-tex"))

(package! wgrep)

(package! ytdious)

;; (package! ytel)

;; (package! ytel-show)

; (package! md4rd)

(package! emms)

(package! ace-link)

(package! org-super-links :recipe (:host github :repo "toshism/org-super-links"))

(package! org-ref)

;; (package! omnisharp :recipe (:no-native-compile t))

;; (package! rigpa
;;   :recipe (:host github
;;            :repo "countvajhula/rigpa"))

(unpin! sly)

;; (unpin! org-mode)

(package! gnu-apl-mode :recipe (:host github :repo "lokedhs/gnu-apl-mode"))

(package! el-igo :recipe (:host github :repo "misohena/el-igo"))

(package! csharp-repl :recipe (:host github :repo "dakling/emacs-csharp-repl"))

(package! bosss :recipe (:host github :repo "dakling/emacs-bosss"))

(disable-packages! org-msg)

;; (package! solaire-mode :disable t)

(package! nov)

(package! system-packages)
(package! helm-system-packages)

(package! shelldon
  :recipe (:host github
           :repo "Overdr0ne/shelldon"
           :branch "master"))

(package! disable-mouse)
                                        ; ;; installed by guix
                                        ; (package! guix)

;; (package! eaf :recipe
;;   (:host github
;;    :repo "manateelazycat/emacs-application-framework"
;;    :files ("*.el" "*.py" "core" "app")
;;    :no-byte-compile t))
