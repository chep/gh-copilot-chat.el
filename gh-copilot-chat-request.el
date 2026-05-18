;;; gh-copilot-chat --- gh-copilot-chat-request.el --- copilot chat request legacy functions -*- lexical-binding: t; -*-

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

;; Some legacy functions not implemented with curl

;;; Code:

(require 'request)
(require 'gh-copilot-chat-model)
(require 'gh-copilot-chat-instance)
(require 'gh-copilot-chat-common)

(defun gh-copilot-chat--get-headers ()
  "Get headers for Copilot API requests."
  `(("openai-intent" . "conversation-panel")
    ("content-type" . "application/json")
    ("accept" . "application/json")
    ("user-agent" . "CopilotChat.nvim/2.0.0")
    ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
    ("authorization" .
     ,(concat
       "Bearer "
       (alist-get
        'token (gh-copilot-chat-connection-token gh-copilot-chat--connection))))
    ("x-request-id" . ,(gh-copilot-chat--uuid))
    ("vscode-sessionid"
     .
     ,(gh-copilot-chat-connection-sessionid gh-copilot-chat--connection))
    ("vscode-machineid"
     .
     ,(gh-copilot-chat-connection-machineid gh-copilot-chat--connection))
    ("copilot-integration-id" . "vscode-chat")
    ("openai-organization" . "github-copilot")
    ("editor-version" . "Neovim/0.10.0")))
(cl-defun
 gh-copilot-chat--request-models-cb (&key response &key data &allow-other-keys)
 "Handle models response from Copilot API.
Argument DATA is the parsed JSON response.
Argument RESPONSE is request-response object."
 (unless (= (request-response-status-code response) 200)
   (error "Failed to fetch models: %s" (request-response-status-code response)))

 (let* ((models-vector (alist-get 'data data))
        (models (append models-vector nil)) ; Convert vector to list
        (chat-models nil))
   ;; Filter for chat models and extract capabilities
   (dolist (model models)
     (when (and (alist-get 'capabilities model)
                (equal
                 (alist-get 'type (alist-get 'capabilities model)) "chat"))
       (push model chat-models)))

   (when gh-copilot-chat-debug
     (message "Fetched %d models" (length chat-models))
     (message "Models: %s" chat-models))

   ;; Store models in instance and return them
   (let ((sorted-models (nreverse chat-models)))
     (setf (gh-copilot-chat-connection-models gh-copilot-chat--connection)
           sorted-models)

     ;; Cache models to disk
     (gh-copilot-chat--save-models-to-cache sorted-models)

     ;; Enable policies for models if needed
     (dolist (model sorted-models)
       (when (and (alist-get 'policy model)
                  (equal
                   (alist-get 'state (alist-get 'policy model)) "unconfigured"))
         (gh-copilot-chat--request-enable-model-policy (alist-get 'id model))))

     ;; Return the models list for immediate use
     sorted-models)))

(defun gh-copilot-chat--request-enable-model-policy (model-id)
  "Enable policy for MODEL-ID."
  (let ((url (format "https://api.githubcopilot.com/models/%s/policy" model-id))
        (headers (gh-copilot-chat--get-headers))
        (data
         (json-serialize '((state . "enabled")) :false-object :json-false)))
    (when gh-copilot-chat-debug
      (message "Enabling policy for model %s" model-id))
    (request
     url
     :type "POST"
     :headers headers
     :data data
     :parser
     (apply-partially 'json-parse-buffer
                      :object-type 'alist
                      :false-object
                      :json-false))))

(defun gh-copilot-chat--request-models (&optional quiet)
  "Fetch available models from Copilot API.
Optional argument QUIET suppresses user messages when non-nil."
  (let ((url "https://api.githubcopilot.com/models")
        (headers (gh-copilot-chat--get-headers)))
    (when gh-copilot-chat-debug
      (message "Fetching models from %s" url))
    (unless quiet
      (message "Fetching available Copilot models..."))
    (request
     url
     :type "GET"
     :headers headers
     :parser
     (apply-partially 'json-parse-buffer
                      :object-type 'alist
                      :false-object
                      :json-false)
     :sync t ; Use synchronous request when called directly
     :complete #'gh-copilot-chat--request-models-cb)))

(defun gh-copilot-chat--request-models-async (&optional quiet)
  "Fetch available models from Copilot API asynchronously.
Optional argument QUIET suppresses user messages when non-nil."
  (let ((url "https://api.githubcopilot.com/models")
        (headers (gh-copilot-chat--get-headers)))
    (when gh-copilot-chat-debug
      (message "Fetching models asynchronously from %s" url))
    (unless quiet
      (message "Fetching available Copilot models in background..."))
    (request
     url
     :type "GET"
     :headers headers
     :parser
     (apply-partially 'json-parse-buffer
                      :object-type 'alist
                      :false-object
                      :json-false)
     :sync nil ; Use asynchronous request for background fetching
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        ;; Process models data
        (let* ((models-vector (alist-get 'data data))
               (models (append models-vector nil)) ; Convert vector to list
               (chat-models nil))
          ;; Filter for chat models
          (dolist (model models)
            (when (and (alist-get 'capabilities model)
                       (equal
                        (alist-get 'type (alist-get 'capabilities model))
                        "chat"))
              (push model chat-models)))

          (when gh-copilot-chat-debug
            (message "Successfully fetched %d models asynchronously"
                     (length chat-models)))

          ;; Store models in instance and cache them
          (let ((sorted-models (nreverse chat-models)))
            (setf (gh-copilot-chat-connection-models
                   gh-copilot-chat--connection)
                  sorted-models)
            (gh-copilot-chat--save-models-to-cache sorted-models)

            ;; Enable policies for models if needed
            (dolist (model sorted-models)
              (when (and (alist-get 'policy model)
                         (equal
                          (alist-get 'state (alist-get 'policy model))
                          "unconfigured"))
                (gh-copilot-chat--request-enable-model-policy
                 (alist-get 'id model))))))))
     :error
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
        (when gh-copilot-chat-debug
          (message "Error fetching models asynchronously: %S"
                   error-thrown)))))))

(provide 'gh-copilot-chat-request)
;;; gh-copilot-chat-request.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
