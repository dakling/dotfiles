(tool-bar-mode -1)
(menu-bar-mode -1)
(menu-bar-no-scroll-bar)

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up

(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
;; TODO change for 27
(package-initialize)
