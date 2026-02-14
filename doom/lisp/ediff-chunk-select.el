;;; ediff-chunk-select.el --- Per-hunk accept/reject for ediff  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Dario Klingenberg
;; Keywords: tools, convenience

;;; Commentary:

;; Layers per-hunk accept/reject on top of standard ediff sessions.
;; Each hunk can be independently accepted (use proposed change from B)
;; or rejected (keep original from A).  The result is assembled from
;; the selected combination of hunks.

;;; Code:

(require 'cl-lib)
(require 'ediff)

;;; Faces

(defface ediff-chunk-select-pending-A
  '((t :background "#4a4000" :extend t))
  "Face for pending hunks in buffer A (undecided)."
  :group 'ediff-chunk-select)

(defface ediff-chunk-select-pending-B
  '((t :background "#4a4000" :extend t))
  "Face for pending hunks in buffer B (undecided)."
  :group 'ediff-chunk-select)

(defface ediff-chunk-select-accepted-A
  '((t :background "#1a3a1a" :extend t))
  "Face for accepted hunks in buffer A (will be replaced)."
  :group 'ediff-chunk-select)

(defface ediff-chunk-select-accepted-B
  '((t :background "#1a3a1a" :extend t))
  "Face for accepted hunks in buffer B (chosen change)."
  :group 'ediff-chunk-select)

(defface ediff-chunk-select-rejected-A
  '((t :background "#2a1a1a" :extend t))
  "Face for rejected hunks in buffer A (kept original)."
  :group 'ediff-chunk-select)

(defface ediff-chunk-select-rejected-B
  '((t :background "#2a1a1a" :strikethrough t :extend t))
  "Face for rejected hunks in buffer B (discarded)."
  :group 'ediff-chunk-select)

;;; Buffer-local state (in ediff control buffer)

(defvar-local ediff-chunk-select--hunk-states nil
  "Vector of `pending', `accepted', or `rejected' per diff index.")

(defvar-local ediff-chunk-select--overlays-A nil
  "Vector of overlay objects in buffer A.")

(defvar-local ediff-chunk-select--overlays-B nil
  "Vector of overlay objects in buffer B.")

(defvar-local ediff-chunk-select--undo-stack nil
  "List of (INDEX . OLD-STATE) for undo.")

(defvar-local ediff-chunk-select--completion-callback nil
  "Called with (ACCEPTED-P CONTENT HUNK-SUMMARY) on finish.
HUNK-SUMMARY is an alist with keys `total', `accepted', `rejected',
and `all-accepted'.")

(defvar-local ediff-chunk-select--active nil
  "Non-nil when chunk-select is active in this ediff session.")

;;; Setup hook variable (set before ediff-buffers, consumed in startup hook)

(defvar ediff-chunk-select--pending-callback nil
  "Callback for the next ediff session.  Consumed by the startup hook.")

;;; Overlay management

(defun ediff-chunk-select--face-for (state buffer-label)
  "Return the face for STATE (`pending', `accepted', `rejected') in BUFFER-LABEL (`A' or `B')."
  (pcase (cons state buffer-label)
    ('(pending . A)  'ediff-chunk-select-pending-A)
    ('(pending . B)  'ediff-chunk-select-pending-B)
    ('(accepted . A) 'ediff-chunk-select-accepted-A)
    ('(accepted . B) 'ediff-chunk-select-accepted-B)
    ('(rejected . A) 'ediff-chunk-select-rejected-A)
    ('(rejected . B) 'ediff-chunk-select-rejected-B)))

(defun ediff-chunk-select--diff-region (n buf-label)
  "Return (BEG . END) for diff N in buffer BUF-LABEL (`A' or `B').
Must be called from the ediff control buffer.  `ediff-get-diff-posn'
returns markers valid in the target buffer, so no buffer switch is needed."
  (cons (ediff-get-diff-posn buf-label 'beg n)
        (ediff-get-diff-posn buf-label 'end n)))

(defun ediff-chunk-select--create-overlays ()
  "Create overlays for all diffs.  Must be called from the control buffer."
  (let ((n ediff-number-of-differences))
    (setq ediff-chunk-select--overlays-A (make-vector n nil))
    (setq ediff-chunk-select--overlays-B (make-vector n nil))
    (dotimes (i n)
      (let ((region-a (ediff-chunk-select--diff-region i 'A))
            (region-b (ediff-chunk-select--diff-region i 'B)))
        (let ((ov-a (make-overlay (car region-a) (cdr region-a) ediff-buffer-A)))
          (overlay-put ov-a 'priority 100)
          (overlay-put ov-a 'face (ediff-chunk-select--face-for 'pending 'A))
          (overlay-put ov-a 'ediff-chunk-select t)
          (aset ediff-chunk-select--overlays-A i ov-a))
        (let ((ov-b (make-overlay (car region-b) (cdr region-b) ediff-buffer-B)))
          (overlay-put ov-b 'priority 100)
          (overlay-put ov-b 'face (ediff-chunk-select--face-for 'pending 'B))
          (overlay-put ov-b 'ediff-chunk-select t)
          (aset ediff-chunk-select--overlays-B i ov-b))))))

(defun ediff-chunk-select--update-overlay (n state)
  "Update overlays for diff N to reflect STATE."
  (when-let ((ov-a (aref ediff-chunk-select--overlays-A n)))
    (overlay-put ov-a 'face (ediff-chunk-select--face-for state 'A)))
  (when-let ((ov-b (aref ediff-chunk-select--overlays-B n)))
    (overlay-put ov-b 'face (ediff-chunk-select--face-for state 'B))))

(defun ediff-chunk-select--delete-all-overlays ()
  "Remove all chunk-select overlays."
  (when ediff-chunk-select--overlays-A
    (cl-loop for ov across ediff-chunk-select--overlays-A
             when ov do (delete-overlay ov)))
  (when ediff-chunk-select--overlays-B
    (cl-loop for ov across ediff-chunk-select--overlays-B
             when ov do (delete-overlay ov))))

;;; Header / mode line

(defun ediff-chunk-select--status-string ()
  "Return a string like [Chunks: 3a 2r 5p] for the header."
  (if (null ediff-chunk-select--hunk-states)
      ""
    (let ((accepted 0) (rejected 0) (pending 0))
      (cl-loop for s across ediff-chunk-select--hunk-states
               do (pcase s
                    ('accepted (cl-incf accepted))
                    ('rejected (cl-incf rejected))
                    ('pending  (cl-incf pending))))
      (format "[Chunks: %da %dr %dp]" accepted rejected pending))))

(defun ediff-chunk-select--update-header ()
  "Update the ediff control buffer header with chunk status."
  (when (and ediff-chunk-select--active
             (boundp 'ediff-control-buffer)
             (buffer-live-p ediff-control-buffer))
    (with-current-buffer ediff-control-buffer
      (setq header-line-format
            (concat " " (ediff-chunk-select--status-string)
                    "  a:accept  x:reject  u:undo  A:accept-all  X:reject-all  RET:finish")))))

;;; Next pending navigation

(defun ediff-chunk-select--next-pending (&optional from-index)
  "Return the index of the next pending hunk after FROM-INDEX, or nil."
  (let ((start (1+ (or from-index ediff-current-difference)))
        (n (length ediff-chunk-select--hunk-states)))
    (cl-loop for i from start below n
             when (eq (aref ediff-chunk-select--hunk-states i) 'pending)
             return i)))

(defun ediff-chunk-select--auto-advance ()
  "Move to the next pending hunk if there is one."
  (when-let ((next (ediff-chunk-select--next-pending)))
    (ediff-jump-to-difference (1+ next))))  ; ediff uses 1-based

;;; Interactive commands

(defun ediff-chunk-select--set-hunk-state (state)
  "Set current hunk to STATE and auto-advance."
  (unless ediff-chunk-select--active
    (user-error "Chunk-select is not active"))
  (when (< ediff-current-difference 0)
    (user-error "No current difference"))
  (let ((idx ediff-current-difference)
        (old-state (aref ediff-chunk-select--hunk-states ediff-current-difference)))
    (push (cons idx old-state) ediff-chunk-select--undo-stack)
    (aset ediff-chunk-select--hunk-states idx state)
    (ediff-chunk-select--update-overlay idx state)
    (ediff-chunk-select--update-header)
    (ediff-chunk-select--auto-advance)))

(defun ediff-chunk-select-accept ()
  "Accept the current hunk (use version from buffer B)."
  (interactive)
  (ediff-chunk-select--set-hunk-state 'accepted))

(defun ediff-chunk-select-reject ()
  "Reject the current hunk (keep version from buffer A)."
  (interactive)
  (ediff-chunk-select--set-hunk-state 'rejected))

(defun ediff-chunk-select-undo ()
  "Undo the last accept/reject action."
  (interactive)
  (unless ediff-chunk-select--active
    (user-error "Chunk-select is not active"))
  (unless ediff-chunk-select--undo-stack
    (user-error "Nothing to undo"))
  (let* ((entry (pop ediff-chunk-select--undo-stack))
         (idx (car entry))
         (old-state (cdr entry)))
    (aset ediff-chunk-select--hunk-states idx old-state)
    (ediff-chunk-select--update-overlay idx old-state)
    (ediff-chunk-select--update-header)
    (ediff-jump-to-difference (1+ idx))))

(defun ediff-chunk-select-accept-all ()
  "Accept all remaining pending hunks."
  (interactive)
  (unless ediff-chunk-select--active
    (user-error "Chunk-select is not active"))
  (dotimes (i (length ediff-chunk-select--hunk-states))
    (when (eq (aref ediff-chunk-select--hunk-states i) 'pending)
      (push (cons i 'pending) ediff-chunk-select--undo-stack)
      (aset ediff-chunk-select--hunk-states i 'accepted)
      (ediff-chunk-select--update-overlay i 'accepted)))
  (ediff-chunk-select--update-header))

(defun ediff-chunk-select-reject-all ()
  "Reject all remaining pending hunks."
  (interactive)
  (unless ediff-chunk-select--active
    (user-error "Chunk-select is not active"))
  (dotimes (i (length ediff-chunk-select--hunk-states))
    (when (eq (aref ediff-chunk-select--hunk-states i) 'pending)
      (push (cons i 'pending) ediff-chunk-select--undo-stack)
      (aset ediff-chunk-select--hunk-states i 'rejected)
      (ediff-chunk-select--update-overlay i 'rejected)))
  (ediff-chunk-select--update-header))

(defun ediff-chunk-select-next ()
  "Move to the next difference (any state)."
  (interactive)
  (ediff-next-difference))

(defun ediff-chunk-select-prev ()
  "Move to the previous difference (any state)."
  (interactive)
  (ediff-previous-difference))

;;; Result assembly

(defun ediff-chunk-select--build-result ()
  "Build the result string by combining hunks from buffers A and B.
Accepted hunks take content from B, rejected/pending from A.
Inter-hunk regions always come from A.

Uses overlay positions rather than `ediff-get-diff-posn' so this works
even after `ediff-really-quit' has cleared ediff's internal diff vectors
\(which happens before quit hooks run)."
  (let ((n (length ediff-chunk-select--hunk-states))
        (parts nil)
        (prev-end-a nil))
    (with-current-buffer ediff-buffer-A
      (setq prev-end-a (point-min)))
    (dotimes (i n)
      (let* ((ov-a (aref ediff-chunk-select--overlays-A i))
             (ov-b (aref ediff-chunk-select--overlays-B i))
             (state (aref ediff-chunk-select--hunk-states i))
             (beg-a (overlay-start ov-a))
             (end-a (overlay-end ov-a))
             (beg-b (overlay-start ov-b))
             (end-b (overlay-end ov-b)))
        ;; Inter-hunk text from buffer A
        (push (with-current-buffer ediff-buffer-A
                (buffer-substring-no-properties prev-end-a beg-a))
              parts)
        ;; Hunk content: from B if accepted, from A if rejected/pending
        (push (if (eq state 'accepted)
                  (with-current-buffer ediff-buffer-B
                    (buffer-substring-no-properties beg-b end-b))
                (with-current-buffer ediff-buffer-A
                  (buffer-substring-no-properties beg-a end-a)))
              parts)
        (setq prev-end-a end-a)))
    ;; Remaining text after the last diff from buffer A
    (push (with-current-buffer ediff-buffer-A
            (buffer-substring-no-properties prev-end-a (point-max)))
          parts)
    (apply #'concat (nreverse parts))))

;;; Finish / finalize

(defun ediff-chunk-select-finish (&optional skip-quit)
  "Finalize the chunk-select session.
If hunks are still pending, prompt the user; pending defaults to rejected.
When SKIP-QUIT is non-nil, don't call `ediff-really-quit' (used when
called from the quit hook to avoid re-entrancy)."
  (interactive)
  (unless ediff-chunk-select--active
    (user-error "Chunk-select is not active"))
  (let ((pending-count (cl-count 'pending ediff-chunk-select--hunk-states)))
    (when (and (> pending-count 0)
               (not (y-or-n-p
                     (format "%d hunk(s) still pending (will keep original). Finish? "
                             pending-count))))
      (user-error "Aborted"))
    ;; Treat remaining pending as rejected
    (dotimes (i (length ediff-chunk-select--hunk-states))
      (when (eq (aref ediff-chunk-select--hunk-states i) 'pending)
        (aset ediff-chunk-select--hunk-states i 'rejected)))
    (let* ((result (ediff-chunk-select--build-result))
           (total (length ediff-chunk-select--hunk-states))
           (accepted-count (cl-count 'accepted ediff-chunk-select--hunk-states))
           (rejected-count (cl-count 'rejected ediff-chunk-select--hunk-states))
           (any-accepted (> accepted-count 0))
           (all-accepted (= rejected-count 0))
           (hunk-summary `((total . ,total)
                           (accepted . ,accepted-count)
                           (rejected . ,rejected-count)
                           (all-accepted . ,all-accepted)))
           (callback ediff-chunk-select--completion-callback))
      (ediff-chunk-select--delete-all-overlays)
      (setq ediff-chunk-select--active nil)
      ;; Quit ediff cleanly (skip when already inside ediff-really-quit)
      (unless skip-quit
        (ediff-really-quit nil))
      ;; Call the completion callback
      (when callback
        (funcall callback (if any-accepted t nil) result hunk-summary)))))

;;; Keymap setup

(defun ediff-chunk-select--setup-keymap ()
  "Install chunk-select keybindings in the ediff control buffer.
Called from `ediff-keymap-setup-hook'."
  (when ediff-chunk-select--active
    (let ((map ediff-mode-map))
      (define-key map "a" #'ediff-chunk-select-accept)
      (define-key map "x" #'ediff-chunk-select-reject)
      (define-key map "u" #'ediff-chunk-select-undo)
      (define-key map "A" #'ediff-chunk-select-accept-all)
      (define-key map "X" #'ediff-chunk-select-reject-all)
      (define-key map "]c" #'ediff-chunk-select-next)
      (define-key map "[c" #'ediff-chunk-select-prev)
      (define-key map (kbd "RET") #'ediff-chunk-select-finish)
      (define-key map "Q" #'ediff-chunk-select-finish))
    ;; Normalize evil keymaps if evil-collection is active
    (when (fboundp 'evil-normalize-keymaps)
      (evil-normalize-keymaps))))

;;; Session initialization (startup hook)

(defun ediff-chunk-select--startup-hook ()
  "Initialize chunk-select state when an ediff session starts.
Consumes `ediff-chunk-select--pending-callback'."
  (when ediff-chunk-select--pending-callback
    (let ((callback ediff-chunk-select--pending-callback))
      (setq ediff-chunk-select--pending-callback nil)
      (when ediff-control-buffer
        (with-current-buffer ediff-control-buffer
          (setq ediff-chunk-select--active t)
          (setq ediff-chunk-select--completion-callback callback)
          (setq ediff-chunk-select--hunk-states
                (make-vector ediff-number-of-differences 'pending))
          (setq ediff-chunk-select--undo-stack nil)
          ;; Make buffers read-only during review
          (when (buffer-live-p ediff-buffer-A)
            (with-current-buffer ediff-buffer-A
              (setq buffer-read-only t)))
          (when (buffer-live-p ediff-buffer-B)
            (with-current-buffer ediff-buffer-B
              (setq buffer-read-only t)))
          ;; Create overlays and setup keybindings
          (ediff-chunk-select--create-overlays)
          (ediff-chunk-select--setup-keymap)
          (ediff-chunk-select--update-header)
          ;; Override quit to go through our finish flow
          ;; Pass skip-quit=t since ediff-really-quit is already running
          (setq-local ediff-quit-hook
                      (list (lambda ()
                              (when ediff-chunk-select--active
                                (ediff-chunk-select-finish t))))))))))

;;; Public API

;;;###autoload
(defun ediff-chunk-select-enable-for-session (&optional callback)
  "Enable chunk-select for the next ediff session.
CALLBACK is called with (ACCEPTED-P NEW-CONTENTS) when the user finishes.
Call this before `ediff-buffers'."
  (setq ediff-chunk-select--pending-callback callback))

;;;###autoload
(defun ediff-chunk-select-buffers (buffer-a buffer-b)
  "Start an ediff session with per-hunk chunk-select between BUFFER-A and BUFFER-B.
Interactively, prompts for two buffers.  When finished, the result
is placed in a new buffer called *chunk-select-result*."
  (interactive
   (list (read-buffer "Buffer A (original): " (current-buffer) t)
         (read-buffer "Buffer B (proposed): " (other-buffer) t)))
  (ediff-chunk-select-enable-for-session
   (lambda (accepted-p content &optional _hunk-summary)
     (if (not accepted-p)
         (message "All hunks rejected — no changes.")
       (let ((buf (get-buffer-create "*chunk-select-result*")))
         (with-current-buffer buf
           (erase-buffer)
           (insert content))
         (switch-to-buffer buf)
         (message "Result assembled with accepted hunks.")))))
  (ediff-buffers (get-buffer buffer-a) (get-buffer buffer-b)))

;;;###autoload
(defun ediff-chunk-select-files (file-a file-b)
  "Start an ediff session with per-hunk chunk-select between FILE-A and FILE-B.
Interactively, prompts for two files."
  (interactive "fFile A (original): \nfFile B (proposed): ")
  (ediff-chunk-select-enable-for-session
   (lambda (accepted-p content &optional _hunk-summary)
     (if (not accepted-p)
         (message "All hunks rejected — no changes.")
       (let ((buf (get-buffer-create "*chunk-select-result*")))
         (with-current-buffer buf
           (erase-buffer)
           (insert content))
         (switch-to-buffer buf)
         (message "Result assembled with accepted hunks.")))))
  (ediff-files file-a file-b))

;; Install hooks
(add-hook 'ediff-startup-hook #'ediff-chunk-select--startup-hook)
(add-hook 'ediff-keymap-setup-hook #'ediff-chunk-select--setup-keymap)

(provide 'ediff-chunk-select)
;;; ediff-chunk-select.el ends here
