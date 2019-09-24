(in-package :next)

(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

;; Use development platform port.
(setf (get-default 'port 'path)
      (format nil "~a/common-lisp/next/ports/gtk-webkit/next-gtk-webkit"
	      (uiop:getenv "HOME")))

;; Donwload youtube videos
(define-command youtube-dl-current-page ()
  "Download a Youtube video in the currently open buffer."
  (with-result (url (buffer-get-url))
    (uiop:run-program
     (list "youtube-dl" url))))

;; (define-key *global-map* (key ". d") 'youtube-dl-current-page)
