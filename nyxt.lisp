(in-package :nyxt-user)
;; (in-package :nyxt)

(load "~/.sbclrc")

;; TODO find path that is always valid
(load "/home/klingenberg/quicklisp/dists/quicklisp/software/sly-20210411-git/slynk/slynk.asd")

(defvar *my-keymap* (make-keymap "my-map"))
(define-key *my-keymap*
  "space space" 'execute-command)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme (keymap:make-scheme
                   scheme:vi-normal *my-keymap*))))

(defvar *my-insert-keymap* (make-keymap "my-insert-map"))
(define-key *my-insert-keymap*
  "M-j" 'nyxt/prompt-buffer-mode:select-next
  "M-k" 'nyxt/prompt-buffer-mode:select-previous
  "M-l" 'nyxt/prompt-buffer-mode:return-selection)

(define-mode my-insert-mode ()
  "Dummy mode for the custom key bindings in `*my-insert-keymap*'."
  ((keymap-scheme (keymap:make-scheme
                   scheme:vi-insert *my-insert-keymap*))))

;; (defun old-reddit-handler (request-data)
;;   (let ((url (url request-data)))
;;     (setf (url request-data)
;;           (if (search "reddit.com" (quri:uri-host url))
;;               (progn
;;                 (setf (quri:uri-host url) "old.reddit.com")
;;                 (log:info "Switching to old Reddit: ~s" (object-display url))
;;                 url)
;;               url)))
;;   request-data)

(define-configuration browser ((remote-execution-p :t)))

(define-configuration (buffer web-buffer)
    ((default-modes (append '(my-mode vi-normal-mode
                              blocker-mode ;; noscript-mode
                              force-https-mode
                              reduce-tracking-mode
                              dark-mode)
                            %slot-default%))
     ;; (request-resource-hook
     ;;  (hooks:add-hook %slot-default% (make-handler-resource #'old-reddit-handler)))
     ))

(define-configuration (prompt-buffer)
  ((default-modes (append '(my-insert-mode vi-insert-mode) %slot-default%))))

;; (push #'password:make-password-store-interface password:interface-list)

;; (ql:quickload :slynk)
(load-after-system :slynk (nyxt-init-file "my-slynk.lisp"))

;; (load-after-system :slynk "/home/klingenberg/.dotfiles/my-slynk.lisp")
