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

(require 'gh-copilot-chat-body)
(require 'gh-copilot-chat-common)
(require 'gh-copilot-chat-spinner)
(require 'gh-copilot-chat-backend)
(require 'gh-copilot-chat-mcp)

(defvar mcp-hub-servers)

(defcustom gh-copilot-chat-claude-program "claude"
  "Claude program to use."
  :type 'string
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-claude-allowed-tools "Edit Write Read"
  "Claude allowed tools, see --allowedTools."
  :type 'string
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-claude-use-ide-tools t
  "When non-nil, expose claude-code-ide emacs-tools via MCP if the package is loaded."
  :type 'boolean
  :group 'gh-copilot-chat)

(defconst gh-copilot-chat-claude--builtin-tools
  '("Bash"
    "Edit"
    "Write"
    "Read"
    "MultiEdit"
    "WebSearch"
    "WebFetch"
    "TodoRead"
    "TodoWrite"
    "NotebookRead"
    "NotebookEdit"
    "LS"
    "Glob"
    "Grep"
    "Task")
  "Built-in Claude Code tools (fixed list).")

;; structures
(cl-defstruct
 gh-copilot-chat-claude
 "Private data for Copilot chat claude backend."
 (process nil :type (or null process))
 (session-id nil :type (or null string))
 (current-data nil :type (or null string))
 (allowed-tools "" :type string))

;; functions

(defun gh-copilot-chat-claude--tool-candidates (instance)
  "Build tool completion candidates for INSTANCE.
Combines built-in tools, MCP server wildcards, and IDE tools."
  (append
   gh-copilot-chat-claude--builtin-tools
   (when (bound-and-true-p mcp-hub-servers)
     (mapcar
      (lambda (server-name) (format "mcp__%s__*" server-name))
      (gh-copilot-chat-mcp-servers instance)))
   (when (fboundp 'claude-code-ide-mcp-server-get-tool-names)
     (claude-code-ide-mcp-server-get-tool-names "mcp__emacs-tools__"))))

(defun gh-copilot-chat--claude-ide-setup (session-id)
  "Start claude-code-ide emacs-tools MCP server and register SESSION-ID.
Returns the mcpServers alist entry (with SESSION-ID in the URL) on success,
nil if the package is not loaded or the server fails to start."
  (when (and gh-copilot-chat-claude-use-ide-tools
             (featurep 'claude-code-ide-emacs-tools)
             (fboundp 'claude-code-ide-emacs-tools-setup)
             (fboundp 'claude-code-ide-mcp-server-ensure-server)
             (fboundp 'claude-code-ide-mcp-server-get-config)
             (fboundp 'claude-code-ide-mcp-server-session-started))
    (claude-code-ide-emacs-tools-setup)
    (when (claude-code-ide-mcp-server-ensure-server)
      (let* ((project-dir
              (or (when (fboundp 'project-root)
                    (when-let* ((proj (project-current)))
                      (project-root proj)))
                  default-directory))
             (buffer (current-buffer)))
        (claude-code-ide-mcp-server-session-started
         session-id project-dir buffer)
        (alist-get
         'mcpServers (claude-code-ide-mcp-server-get-config session-id))))))

(defun gh-copilot-chat--claude-mcp-config-arg (instance &optional ide-servers)
  "Return --mcp-config JSON string for INSTANCE's MCP servers, or nil.
IDE-SERVERS is an optional mcpServers alist from claude-code-ide to merge in."
  (let* ((hub-servers
          (when (and (bound-and-true-p mcp-hub-servers)
                     (gh-copilot-chat-mcp-servers instance))
            (delq
             nil
             (mapcar
              (lambda (server-name)
                (when-let* ((config (cdr (assoc server-name mcp-hub-servers))))
                  (let ((command (plist-get config :command))
                        (args (plist-get config :args))
                        (url (plist-get config :url)))
                    (cons
                     (intern server-name)
                     (if url
                         `((url . ,url))
                       `((command . ,command) (args . ,(vconcat args))))))))
              (gh-copilot-chat-mcp-servers instance)))))
         (all-servers (append hub-servers ide-servers)))
    (when all-servers
      (json-encode `((mcpServers . ,all-servers))))))

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

  ;; Record prompt in history for navigation (M-p / M-n).
  ;; The Claude backend relies on --resume for context, so only
  ;; user prompts are stored here (not assistant answers).
  (unless out-of-context
    (push
     (list :role "user" :content prompt) (gh-copilot-chat-history instance)))

  ;; start claude process
  (let* ((backend (gh-copilot-chat--backend instance))
         (copilot-instruction-content
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
         (ide-session-id
          (when (and gh-copilot-chat-claude-use-ide-tools
                     (featurep 'claude-code-ide-emacs-tools))
            (format "gh-copilot-chat-%s"
                    (format-time-string "%Y%m%d-%H%M%S%3N"))))
         (ide-servers
          (when ide-session-id
            (gh-copilot-chat--claude-ide-setup ide-session-id)))
         (mcp-config
          (gh-copilot-chat--claude-mcp-config-arg instance ide-servers))
         (all-allowed-tools
          (let* ((allowed (gh-copilot-chat-claude-allowed-tools backend))
                 (parts
                  (delq
                   nil
                   (list
                    (unless (string-empty-p
                             gh-copilot-chat-claude-allowed-tools)
                      gh-copilot-chat-claude-allowed-tools)
                    (unless (string-empty-p allowed)
                      allowed)
                    (when (and ide-servers
                               (fboundp
                                'claude-code-ide-mcp-server-get-tool-names))
                      (mapconcat #'identity
                                 (claude-code-ide-mcp-server-get-tool-names
                                  "mcp__emacs-tools__")
                                 " "))))))
            (when parts
              (string-join parts " "))))
         (command
          (append
           (list
            gh-copilot-chat-claude-program
            "--print"
            "--verbose"
            "--output-format=stream-json")
           (when all-allowed-tools
             (list "--allowedTools" all-allowed-tools))
           (when mcp-config
             (list "--mcp-config" mcp-config))
           (when (and (not out-of-context)
                      (gh-copilot-chat-claude-session-id
                       (gh-copilot-chat--backend instance)))
             (list
              "--resume"
              (gh-copilot-chat-claude-session-id
               (gh-copilot-chat--backend instance))))
           (when instructions
             (list "--system-prompt" instructions))
           (list prompt))))
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
             (when (and ide-servers
                        ide-session-id
                        (fboundp 'claude-code-ide-mcp-server-session-ended))
               (claude-code-ide-mcp-server-session-ended ide-session-id))
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

;;;###autoload
(defun gh-copilot-chat-claude-set-allowed-tools ()
  "Interactively set extra allowed tools for the current Claude session.
The tools selected here are added on top of the always-allowed
\"Edit Write Read\" and the global `gh-copilot-chat-claude-allowed-tools'."
  (interactive)
  (let* ((instance (gh-copilot-chat--current-instance))
         (backend (gh-copilot-chat--backend instance)))
    (unless (gh-copilot-chat-claude-p backend)
      (user-error "Current backend is not Claude"))
    (let* ((current (gh-copilot-chat-claude-allowed-tools backend))
           (current-list (split-string current " " t))
           (selected
            (completing-read-multiple
             (format "Allowed tools [global: %s] [current: %s]: "
                     gh-copilot-chat-claude-allowed-tools
                     (if current-list
                         (string-join current-list ", ")
                       "none"))
             (gh-copilot-chat-claude--tool-candidates instance) nil
             nil ;; require-match = nil
             (string-join current-list ","))))
      (setf (gh-copilot-chat-claude-allowed-tools backend)
            (string-join selected " "))
      (message "Session allowed tools: %s"
               (if selected
                   (string-join selected " ")
                 "(none)")))))


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
