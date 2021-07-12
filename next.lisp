(in-package :nyxt)

(defvar *my-keymap* (make-keymap "my-map"))
(define-key *my-keymap*
  "space space" 'execute-command)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme (keymap:make-scheme
                   ;; scheme:cua *my-keymap*
                   ;; scheme:emacs *my-keymap*
                   scheme:vi-normal *my-keymap*))))

(defun old-reddit-handler (request-data)
  (let ((url (url request-data)))
    (setf (url request-data)
          (if (search "reddit.com" (quri:uri-host url))
              (progn
                (setf (quri:uri-host url) "old.reddit.com")
                (log:info "Switching to old Reddit: ~s" (object-display url))
                url)
              url)))
  request-data)

(define-configuration (buffer web-buffer)
    ((default-modes (append '(my-mode vi-normal-mode) %slot-default))
     ;; (request-resource-hook
     ;;  (add-hook %slot-default (make-handler-resource #'old-reddit-handler)))
     ))

(push #'password:make-password-store-interface password:interface-list)
