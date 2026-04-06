;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a repo
;; (package! some-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you encounter issues with a package in the package archive, you can
;; disable the built-in version and use a fork instead:
;; (package! builtin-package
;;   :recipe (:host github :repo "myfork/builtin-package"))

;; Nix support
(package! nix-mode
  :recipe (:host github :repo "NixOS/nix-mode"))

;; Additional packages
(package! direnv)
(package! fish-mode)
(package! systemd)

;; Org-mode extras
(package! org-roam)
(package! org-bullets)

;; Development
(package! dockerfile-mode)
(package! yaml-mode)
(package! json-mode)
(package! toml-mode)

;; Julia
(package! julia-mode)
(package! julia-repl)

;; Haskell
(package! haskell-mode)

;; Python
(package! python-black)
(package! py-isort)

;; Go
(package! go-mode)
(package! go-guru)

;; Rust
(package! rustic)