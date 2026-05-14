;;; gh-copilot-chat --- gh-copilot-chat-spinner.el --- copilot chat spinner -*- lexical-binding: t; -*-

;; Copyright (C) 2024  gh-copilot-chat maintainers

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;; Code:

(require 'gh-copilot-chat-instance)
(require 'gh-copilot-chat-frontend)

(defcustom gh-copilot-chat-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames used for the spinner animation during streaming."
  :type '(repeat string)
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-spinner-interval 0.1
  "Interval in seconds between spinner frame updates."
  :type 'number
  :group 'gh-copilot-chat)

(defface gh-copilot-chat-spinner-face '((t :inherit font-lock-keyword-face))
  "Face used for the spinner during streaming."
  :group 'gh-copilot-chat)


(defun gh-copilot-chat--get-spinner-buffers (instance)
  "Get Spinner buffer from the active frontend.
Argument INSTANCE is the copilot chat instance to get the buffer for."
  (condition-case err
      (let ((get-buffer-fn
             (gh-copilot-chat-frontend-get-spinner-buffers-fn
              (gh-copilot-chat--get-frontend))))
        (when (and get-buffer-fn
                   (buffer-live-p (gh-copilot-chat-chat-buffer instance)))
          (funcall get-buffer-fn instance)))
    (error
     (when gh-copilot-chat-debug
       (message "Error getting spinner buffer: %S" err))
     nil)))

(defun gh-copilot-chat--spinner-start (instance)
  "Start the spinner animation in the Copilot Chat buffer.
Argument INSTANCE is the copilot chat instance to use."
  (when (gh-copilot-chat-spinner-timer instance)
    (cancel-timer (gh-copilot-chat-spinner-timer instance)))

  (setf
   (gh-copilot-chat-spinner-index instance) 0
   (gh-copilot-chat-spinner-status instance) "Thinking"
   (gh-copilot-chat-spinner-timer instance)
   (run-with-timer
    0 gh-copilot-chat-spinner-interval #'gh-copilot-chat--spinner-update
    instance)))

(defun gh-copilot-chat--spinner-update (instance)
  "Update the spinner animation in the Copilot Chat buffer.
Argument INSTANCE is the copilot chat instance to use."
  (let ((buffers (gh-copilot-chat--get-spinner-buffers instance)))
    (dolist (buffer buffers)
      (when (and buffer (buffer-live-p buffer))
        (let ((frame
               (nth
                (gh-copilot-chat-spinner-index instance)
                gh-copilot-chat-spinner-frames))
              (status-text
               (if (gh-copilot-chat-spinner-status instance)
                   (concat (gh-copilot-chat-spinner-status instance) " ")
                 "")))
          (with-current-buffer buffer
            (save-excursion
              ;; Remove existing spinner overlay if any
              (remove-overlays
               (point-min) (point-max) 'gh-copilot-chat-spinner t)
              ;; Create new spinner overlay at the end of buffer
              (goto-char (point-max))
              (let ((ov (make-overlay (point) (point))))
                (overlay-put ov 'gh-copilot-chat-spinner t)
                (overlay-put
                 ov 'after-string
                 (propertize (concat status-text frame)
                             'face
                             'gh-copilot-chat-spinner-face))))))

        ;; Update spinner index
        (setf (gh-copilot-chat-spinner-index instance)
              (% (1+ (gh-copilot-chat-spinner-index instance))
                 (length gh-copilot-chat-spinner-frames)))))))

(defun gh-copilot-chat--spinner-stop (instance)
  "Stop the spinner animation.
Argument INSTANCE is the copilot chat instance to use."
  (when (gh-copilot-chat-spinner-timer instance)
    (cancel-timer (gh-copilot-chat-spinner-timer instance))
    (setf (gh-copilot-chat-spinner-timer instance) nil))

  ;; Remove spinner overlay - with robust error handling
  (condition-case err
      (let ((buffers (gh-copilot-chat--get-spinner-buffers instance)))
        (dolist (buffer buffers)
          (when (and buffer (buffer-live-p buffer))
            (with-current-buffer buffer
              (remove-overlays
               (point-min) (point-max) 'gh-copilot-chat-spinner t)))))
    (error
     (when gh-copilot-chat-debug
       (message "Error stopping spinner: %S" err)))))

(defun gh-copilot-chat--spinner-set-status (instance status)
  "Set the status message to display with the spinner.
Argument INSTANCE is the copilot chat instance to use.
Argument STATUS is the status message to display."
  (setf (gh-copilot-chat-spinner-status instance) status)
  (when (gh-copilot-chat-spinner-timer instance)
    (gh-copilot-chat--spinner-update instance)))

(provide 'gh-copilot-chat-spinner)
;;; gh-copilot-chat-spinner.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
