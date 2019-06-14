(in-package :next)

(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

;; Use development platform port.
(setf (get-default 'port 'path)
      (format nil "~a/common-lisp/next/ports/gtk-webkit/next-gtk-webkit"
              (uiop:getenv "HOME")))
