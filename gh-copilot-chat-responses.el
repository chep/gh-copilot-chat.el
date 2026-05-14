;;; gh-copilot-chat --- gh-copilot-chat-responses.el --- copilot chat responses api implementation -*- lexical-binding: t; -*-

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
;; This is the responses api implementation for the backend

;;; Code:
(require 'gh-copilot-chat-backend)
(require 'gh-copilot-chat-mcp)
(require 'gh-copilot-chat-instance)
(require 'gh-copilot-chat-body)
(require 'gh-copilot-chat-spinner)

;; structures
(cl-defstruct
 gh-copilot-chat-responses
 "Private data for Copilot chat /responses endpoint."
 (current-data nil :type (or null string))
 (event nil :type (or null symbol)))

;; functions
(defun gh-copilot-chat--responses-extract-segment (segment)
  "Extract data from an individual line-delimited SEGMENT, returning one of:
- `empty` if the segment has no data
- `partial`: if the segment seems to be incomplete, i.e. more data in a
  future response
- `event`: if the segment is the event description
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

(defun gh-copilot-chat--responses-manage-data
    (instance responses callback no-history data)
  "Manage DATA extracted from /responses endpoint.
Argument INSTANCE is the copilot chat instance to use.
Argument RESPONSES is the copilot chat responses data.
Argument CALLBACK is the function to call with analysed data.
Argument NO-HISTORY is a boolean to indicate
if the response should be added to history."
  (let* ((event (gh-copilot-chat-responses-event responses)))
    (cond
     ((string= event "response.created")
      (setf responses (make-gh-copilot-chat-responses))
      (gh-copilot-chat--spinner-set-status instance "Generating"))
     ((string= event "response.in_progress"))
     ((string= event "response.output_item.added")
      ;; extract `content` from the item element and display it if needed
      (let* ((item (alist-get 'item data))
             (type (and item (alist-get 'type item)))
             (content (and (string= type "message") (alist-get 'content item))))
        (when (stringp content)
          (funcall callback instance content))))
     ((string= event "response.content_item.done"))
     ((string= event "response.content_part.added"))
     ((string= event "response.content_part.done"))
     ((string= event "response.output_text.delta")
      (let ((text (alist-get 'delta data)))
        (when (stringp text)
          (funcall callback instance text))))
     ((string= event "response.refusal.delta"))
     ((string= event "response.refusal.done"))
     ((string= event "response.function_call_arguments.delta"))
     ((string= event "response.function_call_arguments.done"))
     ((string= event "response.completed")
      (let* ((response (alist-get 'response data))
             (output (and response (alist-get 'output response)))
             (functions nil)
             (new-hist nil))

        (when (arrayp output)
          (mapc
           (lambda (elt)
             (cond
              ;; find function call and add to functions
              ((string= (alist-get 'type elt) "function_call")
               (push (make-gh-copilot-chat-function
                      :name (alist-get 'name elt)
                      :arguments (alist-get 'arguments elt)
                      :id (alist-get 'call_id elt)
                      :index -1)
                     functions))
              ;; find message and add to history
              ((and (not no-history) (string= (alist-get 'type elt) "message"))
               (let* ((content (alist-get 'content elt))
                      (answer (and content (alist-get 'text (aref content 0)))))
                 (setq new-hist
                       `(:content
                         ,(if answer
                              answer
                            "")
                         :role "assistant"))))))
           output))

        ;; add functions to history
        (unless no-history
          (when functions
            (setq new-hist
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
                        (reverse functions)))))))
          (push new-hist (gh-copilot-chat-history instance)))

        ;; manage tool
        (if functions
            ;; We have tools to call
            (gh-copilot-chat--responses-call-functions
             instance (reverse functions) callback)
          ;; Else, we are not using tools,
          ;; so just we can send magic and clean.
          (gh-copilot-chat--spinner-stop instance)
          (funcall callback instance gh-copilot-chat--magic))))
     ((string= event "response.incomplete"))
     (t))))


(defun gh-copilot-chat--responses-analyze
    (instance responses string callback no-history)
  "Analyse curl response when using /responses endpoint.
Argument INSTANCE is the copilot chat instance to use.
Argument RESPONSES is the copilot chat responses data.
Argument STRING is the data returned by curl.
Argument CALLBACK is the function to call with analysed data.
Argument NO-HISTORY is a boolean to indicate
if the response should be added to history."
  (let ((current-data (gh-copilot-chat-responses-current-data responses)))
    (when current-data
      (setq string (concat current-data string))
      (setf (gh-copilot-chat-responses-current-data responses) nil)))

  (let ((segments (split-string string "\n")))
    (dolist (segment segments)
      ;;(funcall callback instance (concat "Brut :\n" segment "\nFin brut\n\n"))
      (let ((extracted (gh-copilot-chat--responses-extract-segment segment)))
        (cond
         ;; No data at all, just skip:
         ((eq extracted 'empty)
          nil)
         ;; Data looks truncated, save it for the next segment:
         ((eq extracted 'partial)
          (setf (gh-copilot-chat-responses-current-data responses) segment))
         ;; new event
         ((eq extracted 'event)
          (setf (gh-copilot-chat-responses-event responses)
                (substring segment 7)))
         ;; Otherwise, JSON parsed successfully
         (extracted
          (gh-copilot-chat--responses-manage-data
           instance responses callback no-history extracted)))))))

(defun gh-copilot-chat--responses-create-req (instance prompt no-context)
  "Create a request for Copilot using responses api.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT Copilot prompt to send (string or list of json objects)
Argument NO-CONTEXT tells `gh-copilot-chat' to not send history and buffers."
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
         (messages nil)
         (tools (gh-copilot-chat--get-tools instance t)))

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
         ((plist-member elt :tool_calls)
          (mapc
           (lambda (call)
             (let* ((id (plist-get call :id))
                    (function (plist-get call :function))
                    (name (plist-get function :name))
                    (arguments (plist-get function :arguments)))
               (push `(:type
                       "function_call"
                       :call_id ,id
                       :name ,name
                       :arguments ,arguments)
                     messages)))
           (plist-get elt :tool_calls)))
         ((plist-member elt :content)
          (let ((role (plist-get elt :role)))
            (if (string= role "tool")
                (push `(:type
                        "function_call_output"
                        :call_id ,(plist-get elt :tool_call_id)
                        :output ,(plist-get elt :content))
                      messages)
              (push elt messages))))
         ((plist-member elt :type)
          (push elt messages)))))

    (let ((instructions
           (if formatted-copilot-instructions
               formatted-copilot-instructions
             (if (and git-commit-instruction-content
                      (eq (gh-copilot-chat-type instance) 'commit))
                 git-commit-instruction-content
               gh-copilot-chat-prompt))))

      ;; Create the appropriate payload based on model type
      (json-serialize `(:model
                        ,(gh-copilot-chat-model instance)
                        :background
                        :json-false
                        :input ,(vconcat messages)
                        :instructions ,instructions
                        :stream t
                        :tools ,(vconcat tools))
                      :false-object
                      :json-false))))


(defun gh-copilot-chat--responses-call-functions (instance functions callback)
  "Call the FUNCTIONS and manage the result.
INSTANCE is the copilot chat instance."
  (let ((results nil))
    (dolist (function functions)
      (let* ((connection
              (gh-copilot-chat--mcp-find-connection instance function))
             (name (gh-copilot-chat-function-name function))
             (arguments (gh-copilot-chat-function-arguments function)))
        (mcp-async-call-tool
         connection name
         (if (and arguments (not (string-empty-p arguments)))
             (progn
               (json-parse-string arguments
                                  :object-type 'alist
                                  :false-object
                                  :json-false))
           nil)
         (lambda (result)
           (push `(:type
                   "function_call_output"
                   :call_id ,(gh-copilot-chat-function-id function)
                   :output
                   ,(plist-get (aref (plist-get result :content) 0) :text))
                 results)
           (gh-copilot-chat--send-function-result-if-needed
            instance callback results functions))
         (lambda (_ msg)
           (message "Error calling function %s: %s" name msg)
           (push `(:type
                   "function_call_output"
                   :call_id ,(gh-copilot-chat-function-id function)
                   :output ,msg)
                 results)
           (gh-copilot-chat--send-function-result-if-needed
            instance callback results functions)))))))

(provide 'gh-copilot-chat-responses)
;;; gh-copilot-chat-responses.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; checkdoc-verb-check-experimental-flag: nil
;; End:
