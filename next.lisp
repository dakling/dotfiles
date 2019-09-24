(in-package :next)

(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

;; Use development platform port.
;; (setf (get-default 'port 'path)
;;       (format nil "~a/common-lisp/next/ports/gtk-webkit/next-gtk-webkit"
;; 	      (uiop:getenv "HOME")))

(setf (get-default 'remote-interface 'open-external-link-in-new-window-p) t)

(define-key :scheme :vi-normal
  "h" 'history-backwards
  "l" 'history-forwards)


(setf (get-default 'buffer 'box-style)
      (cl-css:inline-css
       '(:background "#C38A22"
         :color "black"
         :border "1px #C38A22 solid"
         :font-weight "bold"
         :padding "1px 3px 0px 3px"
         :padding "1px 3px 0px 3px"
         :position "absolute"
         :text-align "center"
         :text-shadow "0 3px 7px 0px rgba(0,0,0,0.3)")))

;; Donwload youtube videos
(define-command youtube-dl-current-page ()
  "Download a Youtube video in the currently open buffer."
  (with-result (url (buffer-get-url))
    (uiop:run-program
     (list "youtube-dl" url))))

;; (define-key *global-map* (key ". d") 'youtube-dl-current-page)
