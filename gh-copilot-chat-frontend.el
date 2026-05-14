;;; gh-copilot-chat --- gh-copilot-chat-frontend.el --- define copilot frontend interface -*- lexical-binding: t; -*-

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

(require 'cl-lib)

(defvar gh-copilot-chat-frontend)

(cl-defstruct
 gh-copilot-chat-frontend
 id
 init-fn
 clean-fn
 instance-init-fn
 instance-clean-fn
 save-fn
 load-fn
 format-fn
 format-code-fn
 format-buffer-fn
 create-req-fn
 send-to-buffer-fn
 copy-fn
 yank-fn
 write-fn
 get-buffer-fn
 insert-prompt-fn
 pop-prompt-fn
 goto-input-fn
 get-spinner-buffers-fn)

(defvar gh-copilot-chat--frontend-list '()
  "Copilot-chat frontends and functions list.
Each element must be a `gh-copilot-chat-frontend' struct instance.
Elements are added in the module that defines each front end.")

(defvar gh-copilot-chat--frontend-init-p nil
  "Flag to indicate if the frontend has been initialized.")

(cl-declaim
 (type (list-of gh-copilot-chat-frontend) gh-copilot-chat--frontend-list))

(defun gh-copilot-chat--get-frontend ()
  "Get frontend from custom."
  (cl-find
   gh-copilot-chat-frontend
   gh-copilot-chat--frontend-list
   :key #'gh-copilot-chat-frontend-id
   :test #'eq))

(defun gh-copilot-chat--get-buffer (instance)
  "Get Copilot Chat buffer from the active frontend.
Argument INSTANCE is the copilot chat instance to get the buffer for."
  (let ((get-buffer-fn
         (gh-copilot-chat-frontend-get-buffer-fn
          (gh-copilot-chat--get-frontend))))
    (when get-buffer-fn
      (funcall get-buffer-fn instance))))

(provide 'gh-copilot-chat-frontend)
;;; gh-copilot-chat-frontend.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
