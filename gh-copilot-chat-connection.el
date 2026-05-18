;;; gh-copilot-chat --- gh-copilot-chat-connection.el --- copilot chat connection -*- lexical-binding: t; -*-

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

(cl-defstruct
 (gh-copilot-chat-connection
  (:constructor gh-copilot-chat-connection--make) (:copier nil))
 "Struct for Copilot connection information."
 (ready nil :type boolean)
 (github-token nil :type (or null string))
 (token nil)
 (sessionid nil :type (or null string))
 (machineid nil :type (or null string))
 (models nil :type list)
 (last-models-fetch-time 0 :type number))

(defvar gh-copilot-chat--connection (gh-copilot-chat-connection--make)
  "Connection information for Copilot chat.")

(cl-declaim (type gh-copilot-chat-connection gh-copilot-chat--connection))

(provide 'gh-copilot-chat-connection)
;;; gh-copilot-chat-connection.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
