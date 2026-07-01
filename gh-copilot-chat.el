;;; gh-copilot-chat.el --- Copilot chat interface -*- lexical-binding: t -*-

;; Copyright (C) 2024  gh-copilot-chat maintainers

;; Author: cedric.chepied <cedric.chepied@gmail.com>
;; Version: 4.0.0
;; URL: https://github.com/chep/gh-copilot-chat.el
;; Package-Requires: ((emacs "30.1") (aio "1.0") (request "0.3.2") (transient "0.8.3") (polymode "0.2.2") (org "9.4.6") (markdown-mode "2.6") (shell-maker "0.76.2") (mcp "0.1.0"))
;; Keywords: convenience, tools


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

;; Add the ability to chat with github copilot

;;; Code:

;; All modules are loaded at the top level except those that are loaded lazily or for development.
;; Lazy-loaded stuff is `gh-copilot-chat-markdown', `gh-copilot-chat-org', `gh-copilot-chat-shell-maker'.
(require 'gh-copilot-chat-body)
(require 'gh-copilot-chat-command)
(require 'gh-copilot-chat-common)
(require 'gh-copilot-chat-connection)
(require 'gh-copilot-chat-copilot)
(require 'gh-copilot-chat-frontend)
(require 'gh-copilot-chat-backend)
(require 'gh-copilot-chat-git)
(require 'gh-copilot-chat-instance)
(require 'gh-copilot-chat-model)
(require 'gh-copilot-chat-prompts)
(require 'gh-copilot-chat-prompt-mode)
(require 'gh-copilot-chat-spinner)
(require 'gh-copilot-chat-transient)

(defcustom gh-copilot-chat-frontend 'org
  "Frontend to use with `gh-copilot-chat'.  Can be org, markdown or shell-maker."
  :type
  '(choice
    (const :tag "org-mode" org)
    (const :tag "markdown" markdown)
    (const :tag "shell-maker" shell-maker))
  :set
  (lambda (symbol value)
    (set-default-toplevel-value symbol value)
    (pcase value
      (`org (require 'gh-copilot-chat-org))
      (`markdown (require 'gh-copilot-chat-markdown))
      (`shell-maker (require 'gh-copilot-chat-shell-maker))))
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-backend 'curl
  "Copilot chat backend.  Can be `curl` or a custom on."
  :type '(choice (const :tag "curl" curl) (const :tag "request" request))
  :set
  (lambda (symbol value)
    (set-default-toplevel-value symbol value)
    (pcase value
      (`curl (require 'gh-copilot-chat-curl))
      (`claude (require 'gh-copilot-chat-claude))))
  :group 'gh-copilot-chat)

(provide 'gh-copilot-chat)
;;; gh-copilot-chat.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
