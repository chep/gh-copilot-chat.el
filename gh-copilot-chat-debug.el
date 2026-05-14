;;; gh-copilot-chat --- gh-copilot-chat-debug.el --- copilot chat for debug -*- lexical-binding: t; -*-

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

(defcustom gh-copilot-chat-debug nil
  "When non-nil, show debug information for API requests."
  :type 'boolean
  :group 'gh-copilot-chat)

(defun gh-copilot-chat--debug (category format-string &rest args)
  "Print debug message when `gh-copilot-chat-debug' is enabled.

CATEGORY is a symbol indicating the message category.
For example, `'commit', `'model', `'auth'.
FORMAT-STRING is the format string passed to `message'.
ARGS are the arguments to be formatted according to FORMAT-STRING.

The message is prefixed with '[gh-copilot-chat:CATEGORY]' for easy identification.
No message is printed if `gh-copilot-chat-debug' is nil."
  (when gh-copilot-chat-debug
    (unless (symbolp category)
      (signal 'wrong-type-argument (list 'symbolp category)))
    (unless (stringp format-string)
      (signal 'wrong-type-argument (list 'stringp format-string)))
    (let ((formatted-msg
           (condition-case err
               (apply #'format format-string args)
             (error
              (message "Error formatting debug message: %S" err)
              (format "Error formatting message with args: %S" args)))))
      (message "[gh-copilot-chat:%s] %s" category formatted-msg))))

(provide 'gh-copilot-chat-debug)
;;; gh-copilot-chat-debug.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
