(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil-leader elisp-slime-nav evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))


(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)

(use-package elisp-slime-nav)
(require 'elisp-slime-nav)
(defun my-lisp-hook ()
  (elisp-slime-nav-mode)
  (turn-on-eldoc-mode)
  )

(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)


(use-package evil)
(require 'evil)
(evil-mode 1)

(use-package evil-leader
      :commands (evil-leader-mode)
      :ensure evil-leader
      :demand evil-leader
      :init
      (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader ",")
        ;; bindings from earlier
        )
      )

(evil-leader/set-leader " ")
(evil-leader/set-key "fs" 'save-buffer)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-s") 'save-buffer)

(require 'dired-x)
(defun my-dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternate-file"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)
    ))
(evil-define-key 'normal dired-mode-map "h" 'my-dired-up-directory)
(evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
(evil-define-key 'normal dired-mode-map "o" 'dired-sort-toggle-or-edit)
(evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
(evil-define-key 'normal dired-mode-map "m" 'dired-mark)
(evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
(evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
(evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
(evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
(evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
(evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)
(put 'dired-find-alternate-file 'disabled nil)
