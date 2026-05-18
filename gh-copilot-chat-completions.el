;;; gh-copilot-chat --- gh-copilot-chat-completions.el --- copilot chat completions api implementation -*- lexical-binding: t; -*-

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
;; This is the completions api implementation for the backend

;;; Code:
(require 'gh-copilot-chat-backend)
(require 'gh-copilot-chat-mcp)
(require 'gh-copilot-chat-instance)
(require 'gh-copilot-chat-body)
(require 'gh-copilot-chat-spinner)


(cl-defstruct
 gh-copilot-chat-completions
 "Private data for Copilot chat /completions endpoint."
 (current-data nil :type (or null string))
 (answer nil :type (or null string))
 (functions nil :type list))


(defun gh-copilot-chat--completions-extract-segment (segment)
  "Extract data from an individual line-delimited SEGMENT, returning one of:
- `empty` if the segment has no data
- `partial`: if the segment seems to be incomplete, i.e. more data in a
  future response
- `done`: if this segment indicates completion (data: [DONE])
- otherwise, the entire JSON content (data: {...})
Argument SEGMENT is data segment to parse."
  (cond
   ;; empty
   ((string-empty-p segment)
    'empty)
   ;; seems to have a valid prefix
   ((string-prefix-p "data: " segment)
    (let ((data (substring segment 6)))
      (if (string= data "[DONE]")
          ;; the magic done marker
          'done
        ;; not the done marker, so must be "done: {...json...}"
        (condition-case _err
            (json-parse-string data
                               :object-type 'alist
                               :false-object
                               :json-false)
          ;; failure => the segment was probably truncated and we need more data from a future
          ;; response
          (json-parse-error
           'partial)
          (json-end-of-file
           'partial)))))
   ;; otherwise, try parsing the segment as a non-prefixed json (such as in
   ;; error responses) When even this fails, then we have a partial response
   ;; that was probably truncated (e.g. "dat", or "data:") => need more data
   ;; from a future response
   (t
    (condition-case _err
        (json-parse-string segment
                           :object-type 'alist
                           :false-object
                           :json-false)
      (error
       'partial)))))


(defun gh-copilot-chat--completions-call-functions (instance functions callback)
  "Call the FUNCTIONS and manage the result.
INSTANCE is the copilot chat instance."
  (let ((results nil))
    (dolist (function functions)
      (let*
          ((connection
            (gh-copilot-chat--mcp-find-connection instance function))
           (name (gh-copilot-chat-function-name function))
           (arguments (gh-copilot-chat-function-arguments function))
           (choice
            (if (gh-copilot-chat-allow-all instance)
                "all"
              (read-answer
               (format
                "Copilot Chat wants to call the tool '%s' with arguments: %s. Allow?"
                name
                (if (string-empty-p arguments)
                    "none"
                  arguments))
               '(("yes" ?y "allow this call")
                 ("no" ?n "deny this call")
                 ("all" ?! "allow all remaining calls"))))))
        (when (string= choice "all")
          (setf (gh-copilot-chat-allow-all instance) t))
        (if (or (string= choice "yes") (string= choice "all"))
            (mcp-async-call-tool
             connection name
             (if (and arguments (not (string-empty-p arguments)))
                 (json-parse-string arguments
                                    :object-type 'alist
                                    :false-object
                                    :json-false)
               nil)
             (lambda (result)
               (push `(:role
                       "tool"
                       :tool_call_id ,(gh-copilot-chat-function-id function)
                       :name ,name
                       :content
                       ,(plist-get (aref (plist-get result :content) 0) :text))
                     results)
               (gh-copilot-chat--send-function-result-if-needed
                instance callback results functions))
             (lambda (_ msg)
               (message "Error calling function %s: %s" name msg)
               (push `(:role
                       "tool"
                       :tool_call_id ,(gh-copilot-chat-function-id function)
                       :content ,msg)
                     results)
               (gh-copilot-chat--send-function-result-if-needed
                instance callback results functions)))
          (push `(:role
                  "tool"
                  :tool_call_id ,(gh-copilot-chat-function-id function)
                  :content ,(format "User denied the tool call for '%s'." name))
                results)
          (gh-copilot-chat--send-function-result-if-needed
           instance callback results functions))))))

(defun gh-copilot-chat--completions-analyze
    (instance completions string callback no-history)
  "Analyse curl response when using /chat/completions endpoint.
Argument INSTANCE is the copilot chat instance to use.
Argument COMPLETIONS is the completions request data.
Argument STRING is the data returned by curl.
Argument CALLBACK is the function to call with analysed data.
Argument NO-HISTORY is a boolean to indicate
if the response should be added to history."
  ;; The API conceptually sends us big blob of line-deliminated information, e.g.
  ;;
  ;;     data: {"choices":[{...,"delta":{"content":"great"}}],...}
  ;;
  ;;     data: {"choices":[{...,"delta":{"content":"work"}}],...}
  ;;
  ;;     data: [DONE]
  ;;
  ;; We receive this piecewise, with this function called with `string' as any substring, completely
  ;; ignoring the lines and other rules of the protocol. Thus, this function processes line-by-line
  ;; but needs to be careful to handle partial input any point. We do this by saving a left-over
  ;; line that failed processing to `current-data' and reading it on the next call.
  ;;
  ;; For instance, this function could be called with three segments like:
  ;;
  ;; 1. "data: {...}\n\ndat" (break in the middle of a "data: " prefix)
  ;; 2. "a: {...}\n\ndata: [D" (break in the middle of some data content)
  ;; 3. "ONE]\n\n"
  ;;
  ;; Those calls will proceed like this:
  ;;
  ;; 1. With segment 1, successfully process the first line (`callback' is called with argument "great"), skip
  ;;    the next empty line, and then fail to process the trailing "dat"; "dat" is saved to
  ;;    `current-data'.
  ;;
  ;; 2. With segment 2, the value of `current-data' is first prepended to `string', and
  ;;    processing continues with "data: {...}\n\ndata: [D". Thus, `callback' is called with "work",
  ;;    the next line skipped, and then "data: [D" saved to `current-data'.
  ;;
  ;; 3. With segment 3, `current-data' is prepended to `string', resulting in a value of
  ;;    "data: [DONE]\n\n". Thus, `callback' is called with the value of `gh-copilot-chat--magic', and
  ;;    the two trailing empty lines are skipped.
  (when (gh-copilot-chat-completions-current-data completions)
    (setq string
          (concat
           (gh-copilot-chat-completions-current-data completions) string))
    (setf (gh-copilot-chat-completions-current-data completions) nil))

  (let ((segments (split-string string "\n")))
    (dolist (segment segments)
      (let ((extracted (gh-copilot-chat--completions-extract-segment segment)))
        (cond
         ;; No data at all, just skip:
         ((eq extracted 'empty)
          nil)
         ;; Data looks truncated, save it for the next segment:
         ((eq extracted 'partial)
          (setf (gh-copilot-chat-completions-current-data completions) segment))
         ;; Final segment, all done:
         ((eq extracted 'done)
          (let ((answer (gh-copilot-chat-completions-answer completions))
                (functions (gh-copilot-chat-completions-functions completions)))

            ;; History
            (unless no-history
              (let ((new-hist
                     `(:content
                       ,(if answer
                            answer
                          "")
                       :role "assistant")))
                (when functions
                  (setq
                   new-hist
                   (append
                    new-hist
                    `(:tool_calls
                      ,(vconcat
                        (mapcar
                         (lambda (function)
                           `(:type
                             "function"
                             :id ,(gh-copilot-chat-function-id function)
                             :function
                             (:name
                              ,(gh-copilot-chat-function-name function)
                              :arguments
                              ,(gh-copilot-chat-function-arguments function))))
                         functions))))))
                (when (or (not (string-empty-p answer)) functions)
                  (push new-hist (gh-copilot-chat-history instance)))))

            ;; manage tool
            (if functions
                ;; We have tools to call
                (gh-copilot-chat--completions-call-functions
                 instance functions callback)
              ;; Else, we are not using tools,
              ;; so just we can send magic and clean.
              (gh-copilot-chat--spinner-stop instance)
              (funcall callback instance gh-copilot-chat--magic))
            (setf
             (gh-copilot-chat-completions-functions completions) nil
             (gh-copilot-chat-completions-answer completions) nil)))

         ;; Otherwise, JSON parsed successfully
         (extracted
          (cond
           ;; extract .choices[0].delta.content and pass that along:
           ((alist-get 'choices extracted)
            (let* ((choices (alist-get 'choices extracted))
                   (delta
                    (and (> (length choices) 0)
                         (alist-get 'delta (aref choices 0))))
                   (token (and delta (alist-get 'content delta))))
              (when token
                (if (eq token :null)
                    (let ((tool_calls (alist-get 'tool_calls delta)))
                      (when (and tool_calls (not (eq tool_calls :null)))
                        (setf (gh-copilot-chat-completions-functions
                               completions)
                              (gh-copilot-chat--append-vector-to-functions
                               tool_calls
                               (gh-copilot-chat-completions-functions
                                completions)))))
                  (when (not (gh-copilot-chat-completions-answer completions))
                    (gh-copilot-chat--spinner-set-status instance "Generating"))
                  (funcall callback instance token)
                  (setf (gh-copilot-chat-completions-answer completions)
                        (concat
                         (gh-copilot-chat-completions-answer completions)
                         token))))))

           ;; display .error.message in the chat.
           ((alist-get 'error extracted)
            (gh-copilot-chat--spinner-stop instance)
            (let* ((err-response (alist-get 'error extracted))
                   (err-message (alist-get 'message err-response))
                   (answer (format "Error: %s" err-message)))
              (message answer)
              (funcall callback instance answer)
              (funcall callback instance gh-copilot-chat--magic)
              ;; Add an empty response to the chat history to avoid confusing
              ;; the assistant with its own error messages...
              (setf
               (gh-copilot-chat-history instance)
               (cons
                `(:content "" :role "assistant")
                (gh-copilot-chat-history instance))
               (gh-copilot-chat-completions-answer completions) nil
               (gh-copilot-chat-completions-functions completions) nil)))
           ;; Fallback -- nag developers about possibly unhandled payloads
           (t
            (warn "Unhandled message from copilot: %S" extracted)))))))))

(defun gh-copilot-chat--completions-analyze-nonstream
    (instance completions proc string callback no-history)
  "Analyse curl response non stream version.
o1 differs from the other models in the format of the reply.
Argument INSTANCE is the copilot chat instance to use.
Argument COMPLETIONS is the completions request data.
Argument PROC is curl process.
Argument STRING is the data returned by curl.
Argument CALLBACK is the function to call with analysed data.
Argument NO-HISTORY is a boolean to indicate
 if the response should be added to history."
  (when (gh-copilot-chat-completions-current-data completions)
    (setq string
          (concat
           (gh-copilot-chat-completions-current-data completions) string))
    (setf (gh-copilot-chat-completions-current-data completions) nil))

  (condition-case err
      (let* ((extracted
              (json-parse-string string
                                 :object-type 'alist
                                 :false-object
                                 :json-false))
             (choices (alist-get 'choices extracted))
             (message
              (and (> (length choices) 0)
                   (alist-get 'message (aref choices 0))))
             (token (and message (alist-get 'content message))))
        (when (and token (not (eq token :null)))
          (gh-copilot-chat--spinner-stop instance)
          (funcall callback instance token)
          (funcall callback instance gh-copilot-chat--magic)
          (setf (gh-copilot-chat-completions-answer completions)
                (concat (gh-copilot-chat-completions-answer completions) token))
          (unless no-history
            (setf (gh-copilot-chat-history instance)
                  (cons
                   `(:content
                     ,(gh-copilot-chat-completions-answer completions)
                     :role "assistant")
                   (gh-copilot-chat-history instance))))
          (setf
           (gh-copilot-chat-completions-answer completions) nil
           (gh-copilot-chat-completions-functions completions) nil
           (gh-copilot-chat-completions-current-data completions) nil)))
    ;; o1 often returns `rate limit exceeded` because of its severe rate limitation,
    ;; so the message in case of an error should be easy to understand.
    (error
     ;; When JSON parsing fails, but the process has not terminated and may be
     ;; in the middle of a sentence, do not make an error, set the string and
     ;; wait for the next call.
     ;; I'm not sure if asynchronous control is working properly.
     (progn
       (setf (gh-copilot-chat-completions-current-data completions) string)
       (unless (process-live-p proc)
         (gh-copilot-chat--spinner-stop instance)
         (setf (gh-copilot-chat-completions-current-data completions) nil)
         (funcall callback
                  instance
                  (format "GitHub Copilot error: %S\nResponse is %S"
                          err
                          (string-trim string))))))))

(defun gh-copilot-chat--completions-create-req (instance prompt no-context)
  "Create a request for Copilot.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT Copilot prompt to send (string or list of json objects)
Argument NO-CONTEXT tells `gh-copilot-chat' to not send history and buffers.
The create req function is called first and will return new prompt."
  (let* ((create-req-fn
          (gh-copilot-chat-frontend-create-req-fn
           (gh-copilot-chat--get-frontend)))
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
         (messages (list))
         (tools (gh-copilot-chat--get-tools instance nil)))
    ;; Apply create-req-fn if available
    (when create-req-fn
      (setq prompt (funcall create-req-fn prompt no-context)))

    ;; reset vision support
    (setf (gh-copilot-chat-uses-vision instance) nil)

    ;; user prompt
    (if (stringp prompt)
        (push `(:content ,prompt :role "user") messages)
      (setq messages prompt))


    ;; Add context if needed
    (unless no-context
      ;; Clean buffer list once and add buffer contents
      (setf (gh-copilot-chat-buffers instance)
            (cl-remove-if-not
             #'buffer-live-p (gh-copilot-chat-buffers instance)))
      (dolist (buffer (gh-copilot-chat-buffers instance))
        (setq messages
              (gh-copilot-chat--add-buffer-to-req buffer instance messages)))

      ;; history
      (dolist (elt (gh-copilot-chat-history instance))
        (cond
         ((plist-member elt :content)
          (push elt messages))
         ((plist-member elt :tool_calls)
          (push elt messages))
         ((plist-member elt :type)
          (push `(:role
                  "tool"
                  :tool_call_id ,(plist-get elt :call_id)
                  :content ,(plist-get elt :output))
                messages))
         (t
          (push elt messages)))))


    ;; system.
    ;; Add custom instruction as a separate message if available.
    ;; Prefer Global < Project.
    (when (and formatted-copilot-instructions)
      (push
       `(:content ,formatted-copilot-instructions :role "system") messages))

    (when (and git-commit-instruction-content
               (eq (gh-copilot-chat-type instance) 'commit))
      (push
       `(:content ,git-commit-instruction-content :role "system") messages))

    (push `(:content ,gh-copilot-chat-prompt :role "system") messages)

    (json-serialize (if (gh-copilot-chat--instance-support-streaming instance)
                        `(:messages
                          ,(vconcat messages)
                          :top_p 1
                          :model ,(gh-copilot-chat-model instance)
                          :stream t
                          :n 1
                          :intent t
                          :temperature 0.1
                          :tools ,(vconcat tools)
                          :parallel_tool_calls t)
                      `(:messages
                        ,(vconcat messages)
                        :model ,(gh-copilot-chat-model instance)
                        :stream
                        :json-false))
                    :false-object
                    :json-false)))


(provide 'gh-copilot-chat-completions)
;;; gh-copilot-chat-completions.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
