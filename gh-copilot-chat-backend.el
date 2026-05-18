;;; gh-copilot-chat --- gh-copilot-chat-backend.el --- define copilot backend interface -*- lexical-binding: t; -*-

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

;; Forward declaration of custom variables
(defvar gh-copilot-chat-backend)

;; Struct
(cl-defstruct
 gh-copilot-chat-backend
 "Struct for Copilot chat backend."
 id
 init-fn
 clean-fn
 login-fn
 renew-token-fn
 ask-fn
 cancel-fn
 quotas-fn)

(cl-declaim
 (type (list-of gh-copilot-chat-backend) gh-copilot-chat--backend-list))

(defvar gh-copilot-chat--backend-list '()
  "Copilot-chat backends and functions list.
Each element must be a `gh-copilot-chat-backend' struct instance.
Elements are added in the module that defines each backend.")

(defun gh-copilot-chat--get-backend ()
  "Get backend from custom."
  (cl-find
   gh-copilot-chat-backend
   gh-copilot-chat--backend-list
   :key #'gh-copilot-chat-backend-id
   :test #'eq))


(provide 'gh-copilot-chat-backend)
;;; gh-copilot-chat-backend.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
