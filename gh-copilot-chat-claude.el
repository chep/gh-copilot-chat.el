;;; gh-copilot-chat --- gh-copilot-chat-claude.el --- copilot chat claude backend -*- lexical-binding: t; -*-

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
;; This is claude backend for gh-copilot-chat code

;;; Code:

(defcustom gh-copilot-chat-claude-program "claude"
  "Claude program to use."
  :type 'string
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-claude-allowed-tools ""
  "Claude allowed tools, see --allowedTools."
  :type 'string
  :group 'gh-copilot-chat)

;; structures
(cl-defstruct
 gh-copilot-chat-claude
 "Private data for Copilot chat claude backend."
 (process nil :type (or null process))
 (session-id nil :type (or null string))
 (current-data nil :type (or null string)))

;; functions
(defun gh-copilot-chat--claude-extract-segment (segment)
  "Extract data from an json string, returning one of:
- `empty` if the segment has no data
- `partial`: if the segment seems to be incomplete, i.e. more data in a
  future response
- otherwise, the entire JSON content (data: {...})
Argument SEGMENT is data segment to parse."
  (cond
   ;; empty
   ((string-empty-p segment)
    'empty)
   ((string-prefix-p "event:" segment)
    'event)
   ((string-prefix-p "data: " segment)
    (let ((data (substring segment 6)))
      (condition-case _err
          (json-parse-string data :object-type 'alist :false-object :json-false)
        ;; failure => the segment was probably truncated and we need more data from a future
        ;; response
        (json-parse-error
         'partial)
        (json-end-of-file
         'partial))))
   (t
    (condition-case _err
        (json-parse-string segment
                           :object-type 'alist
                           :false-object
                           :json-false)
      (error
       'partial)))))

(defun gh-copilot-chat--claude-manage-data
    (instance claude-struct callback out-of-context data)
  "Manage DATA from claude stream-json output.
Argument INSTANCE is the copilot chat instance to use.
Argument CLAUDE-STRUCT is the copilot chat claude data.
Argument CALLBACK is the function to call with analysed data.
Argument OUT-OF-CONTEXT is a boolean to indicate
if the prompt is out of context."
  (let ((type (alist-get 'type data)))
    (cond
     ;; Save session ID from init message for future --resume
     ((string= type "system")
      (when (string= (alist-get 'subtype data) "init")
        (setf (gh-copilot-chat-claude-session-id claude-struct)
              (alist-get 'session_id data))))

     ;; Extract text chunks from assistant messages
     ((string= type "assistant")
      (gh-copilot-chat--spinner-set-status instance "Generating")
      (let* ((message (alist-get 'message data))
             (content (alist-get 'content message)))
        (seq-do
         (lambda (item)
           (when (string= (alist-get 'type item) "text")
             (funcall callback instance (alist-get 'text item))))
         content)))

     ;; End of response: send magic to signal completion
     ((string= type "result")
      (setf (gh-copilot-chat-claude-session-id claude-struct)
            (alist-get 'session_id data))
      (when (string= (alist-get 'subtype data) "error")
        (funcall callback instance (alist-get 'result data)))
      (funcall callback instance gh-copilot-chat--magic)))))

(defun gh-copilot-chat--claude-analyze-answer
    (instance data callback out-of-context)
  "Argument INSTANCE is the copilot chat instance to use.
Argument DATA is the string returned by the claude process.
Argument CALLBACK is the function to call with analysed data.
Argument OUT-OF-CONTEXT is a boolean to indicate
if the prompt is out of context."
  (let* ((claude (gh-copilot-chat--backend instance))
         (current-data (gh-copilot-chat-claude-current-data claude))
         (full-data
          (if current-data
              (concat current-data data)
            data))
         (lines (split-string full-data "\n")))
    (setf (gh-copilot-chat-claude-current-data claude) nil)
    ;; All lines except the last are complete JSON objects; the last may be partial
    (dolist (line (butlast lines))
      (let ((extracted (gh-copilot-chat--claude-extract-segment line)))
        (when (and extracted (not (memq extracted '(empty event partial))))
          (gh-copilot-chat--claude-manage-data
           instance claude callback out-of-context extracted))))
    ;; Save the last line if non-empty (it may be a partial JSON object)
    (let ((last-line (car (last lines))))
      (when (and last-line (not (string-empty-p last-line)))
        (setf (gh-copilot-chat-claude-current-data claude) last-line)))))

;; functions
(defun gh-copilot-chat--claude-ask (instance prompt callback out-of-context)
  "Ask a question to Copilot using claude backend.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the prompt to send to copilot.  It can be a string or a list
of json objects.
Argument CALLBACK is the function to call with copilot answer as argument.
Argument OUT-OF-CONTEXT is a boolean to indicate
if the prompt is out of context."
  ;; Start the spinner animation only for instances with chat buffers
  (when (buffer-live-p (gh-copilot-chat-chat-buffer instance))
    (gh-copilot-chat--spinner-start instance))

  ;; start claude process
  (let* ((copilot-instruction-content
          (and gh-copilot-chat-use-copilot-instruction-files
               (gh-copilot-chat--read-copilot-instructions-file)))
         (formatted-copilot-instructions
          (and copilot-instruction-content
               (gh-copilot-chat--format-copilot-instructions
                copilot-instruction-content)))
         (git-commit-instruction-content
          (and gh-copilot-chat-use-git-commit-instruction-files
               (gh-copilot-chat--read-git-commit-instructions-file)))
         (instructions
          (if formatted-copilot-instructions
              formatted-copilot-instructions
            (if (and git-commit-instruction-content
                     (eq (gh-copilot-chat-type instance) 'commit))
                git-commit-instruction-content
              gh-copilot-chat-prompt)))
         (command
          (append
           (list
            gh-copilot-chat-claude-program
            "--print"
            "--verbose"
            "--output-format=stream-json")
           (unless (string-empty-p gh-copilot-chat-claude-allowed-tools)
             (list
              "--allowedTools"
              (concat "\"" gh-copilot-chat-claude-allowed-tools "\"")))
           (when (and (not out-of-context)
                      (gh-copilot-chat-claude-session-id
                       (gh-copilot-chat--backend instance)))
             (list
              "--resume"
              (gh-copilot-chat-claude-session-id
               (gh-copilot-chat--backend instance))))
           (list (concat "\"" prompt "\""))
           (when instructions
             (list "--system-prompt" (concat "\"" instructions "\""))))))
    (setf (gh-copilot-chat-claude-process (gh-copilot-chat--backend instance))
          (make-process
           :name "gh-copilot-chat-claude"
           :buffer nil
           :filter
           (lambda (proc string)
             (gh-copilot-chat--debug
              'curl "gh-copilot-chat--claude-ask: %s" string)
             (gh-copilot-chat--claude-analyze-answer
              instance string callback out-of-context))
           :sentinel
           (lambda (proc _exit)
             (when (/= (process-exit-status proc) 0)
               (let ((error-msg
                      (format "Claude interrupted: %d"
                              (process-exit-status proc))))
                 (funcall callback instance error-msg)
                 (funcall callback instance gh-copilot-chat--magic)))
             (setf (gh-copilot-chat-claude-process
                    (gh-copilot-chat--backend instance))
                   nil)
             (gh-copilot-chat--spinner-stop instance))
           :stderr (get-buffer-create "*gh-copilot-chat-claude-stderr*")
           :command command))))


(defun gh-copilot-chat--claude-init (instance)
  "Initialize Copilot chat claude backend for INSTANCE."
  (setf (gh-copilot-chat--backend instance) (make-gh-copilot-chat-claude)))

(defun gh-copilot-chat--claude-cancel (instance)
  "Cancel Copilot chat claude backend for INSTANCE."
  (when (process-live-p
         (gh-copilot-chat-claude-process (gh-copilot-chat--backend instance)))
    (delete-process
     (gh-copilot-chat-claude-process (gh-copilot-chat--backend instance))))
  (gh-copilot-chat--spinner-stop instance))

;; Top-level execute code.
(cl-pushnew
 (make-gh-copilot-chat-backend
  :id 'claude
  :init-fn #'gh-copilot-chat--claude-init
  :clean-fn nil
  :login-fn nil
  :renew-token-fn nil
  :ask-fn #'gh-copilot-chat--claude-ask
  :cancel-fn #'gh-copilot-chat--claude-cancel
  :quotas-fn nil)
 gh-copilot-chat--backend-list
 :test #'equal)

(provide 'gh-copilot-chat-claude)
;;; gh-copilot-chat-claude.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
