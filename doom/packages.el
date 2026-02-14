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

(unpin! evil-collection)
;; (package! evil-collection
;;   :recipe (:repo "meliache/evil-collection" :branch "mu4e-development"))

(package! mu4e-alert)

(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll"))


(package! alert)

(package! pinentry)

(package! elfeed)
(disable-packages! elfeed-goodies)
(disable-packages! ts-fold)


(package! ediff-chunk-select
  :recipe (:host github :repo "dakling/ediff-chunk-select"))

(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"))

(package! claude-code-ide-mcp-tools
  :recipe (:host github :repo "Kaylebor/claude-code-ide-mcp-tools"))

(package! claude-code-ide-extras
  :recipe (:host github :repo "acmorrow/claude-code-ide-extras"))

(package! mcp
  :recipe (:host github :repo "lizqwerscott/mcp.el"))

(package! goose
  :recipe (:host github :repo "aq2bq/goose.el"))

(package! codex-cli)

(package! prompt-compose
  :recipe (:host github :repo "dakling/prompt-compose"))

(package! emacs-claude-bridge
  :recipe (:host github :repo "dakling/emacs-claude-bridge"))

(package! ai-workflows
  :recipe (:local-repo "/Users/darioklingenberg/code/emacs-packages/ai-workflows"
           :files ("ai-workflows.el")))

(package! async-await)

(package! evil-tex :recipe (:host github :repo "itai33/evil-tex"))

(package! wgrep)


(package! ace-link)

(package! org-modern)

(package! org-super-links :recipe (:host github :repo "toshism/org-super-links"))

(package! org-ref)

(package! lsp-ltex)

(unpin! sly)

(package! el-igo :recipe (:host github :repo "misohena/el-igo"))

(disable-packages! org-msg)

(disable-packages! writegood-mode)

(package! nov)

(package! system-packages)
(package! helm-system-packages)

(package! shelldon
  :recipe (:host github
           :repo "Overdr0ne/shelldon"
           :branch "master"))

(package! jinx)

(package! beacon)

(package! rotate)

(package! string-inflection)

(package! systemd)

(unpin! haskell-mode)
