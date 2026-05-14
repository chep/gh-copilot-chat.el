;;; gh-copilot-chat --- gh-copilot-chat-mcp.el --- mcp servers management -*- lexical-binding: t; -*-

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

(require 'json)
(require 'mcp-hub)
(require 'gh-copilot-chat-instance)

(declare-function gh-copilot-chat--ask "gh-copilot-chat-copilot")
(declare-function gh-copilot-chat-prompt-cb "gh-copilot-chat-prompt-mode")


(cl-defstruct
 gh-copilot-chat-function
 "Structure to hold function information."
 (name "" :type string)
 (arguments "" :type string)
 (id "" :type string)
 (index -1 :type integer))


(defun gh-copilot-chat--update-function (function args)
  "Update FUNCTION by appending ARGS to its arguments."
  (setf (gh-copilot-chat-function-arguments function)
        (concat (gh-copilot-chat-function-arguments function) args)))

(defun gh-copilot-chat--append-vector-to-functions (vector functions)
  "Create or update FUNCTIONS elements using tool call data from VECTOR."
  (let ((name "")
        (args "")
        (id "")
        (index -1))

    (dolist (item (elt vector 0))
      (cond
       ((eq (car item) 'function)
        (setq
         name (alist-get 'name item)
         args (alist-get 'arguments item)))
       ((eq (car item) 'id)
        (setq id (cdr item)))
       ((eq (car item) 'index)
        (setq index (cdr item)))))

    ;; search for function with index
    (let ((func
           (seq-find
            (lambda (f)
              (= (gh-copilot-chat-function-index f) index))
            functions)))
      (if func
          (gh-copilot-chat--update-function func args)
        (let ((new-function
               (make-gh-copilot-chat-function
                :name name
                :arguments args
                :id id
                :index index)))
          (setq functions (append functions (list new-function))))))
    functions))

(defun gh-copilot-chat--mcp-find-connection (instance function)
  "Find the MCP connection for the given FUNCTION in INSTANCE."
  (catch 'break
    (dolist (server (gh-copilot-chat-mcp-servers instance))
      (let ((connection (gethash server mcp-server-connections)))
        (when connection
          (let ((tools (mcp--tools connection)))
            (when (seq-some
                   (lambda (item)
                     (string=
                      (plist-get item :name)
                      (gh-copilot-chat-function-name function)))
                   tools)
              (throw 'break connection))))))
    nil))

(defun gh-copilot-chat--send-function-result-if-needed
    (instance callback results functions)
  "Send the FUNCTIONS results if all calls are completed.
INSTANCE is the copilot chat instance.
CALLBACK is `gh-copilot-chat--ask' callback.
RESULTS is the list of results collected.
ARGLIST is the list of arguments that were processed."
  (when (= (length results) (length functions))
    (gh-copilot-chat--ask instance results callback)))

(defun gh-copilot-chat--activate-mcp-servers (instance)
  "Start the MCP server connections for INSTANCE."
  (let ((servers (gh-copilot-chat-mcp-servers instance)))
    (dolist (server-name servers)
      (dolist (server mcp-hub-servers)
        (when (string= server-name (car server))
          (unless (mcp--server-running-p server)
            (apply #'mcp-connect-server
                   (append
                    (list (car server)) (cdr server)
                    (list
                     :initial-callback (lambda (_))
                     :tools-callback
                     (lambda (server _)
                       (message
                        (format "MCP server %s started"
                                (slot-value server 'name))))
                     :prompts-callback nil
                     :resources-callback nil
                     :resources-templates-callback nil
                     :error-callback
                     (lambda (_ err)
                       (error (concat "MCP server start error :" err))))))))))))

(defun gh-copilot-chat--get-tools (instance responses-api)
  "Return the list of tools from the MCP servers managed in INSTANCE.
If RESPONSES-API is t, use openAI responses format."
  (let ((all-tools nil))
    (dolist (server (gh-copilot-chat-mcp-servers instance))
      (let ((connection (gethash server mcp-server-connections)))
        (when connection
          (let ((tools (mcp--tools connection)))
            (mapc
             (lambda (tool)
               (let* ((name (plist-get tool :name))
                      (desc (plist-get tool :description))
                      (type (plist-get (plist-get tool :inputSchema) :type))
                      (properties
                       (plist-get (plist-get tool :inputSchema) :properties)))
                 (if responses-api
                     (push `(:type
                             "function"
                             :name ,(or name "")
                             :description ,(or desc "")
                             :parameters
                             (:type
                              ,(or type "object")
                              :properties ,(or properties (list))))
                           all-tools)
                   (push `(:type
                           "function"
                           :function
                           (:name
                            ,(or name "")
                            :description ,(or desc "")
                            :parameters
                            (:type
                             ,(or type "object")
                             :properties ,(or properties (list)))))
                         all-tools))))
             tools)))))
    all-tools))

(provide 'gh-copilot-chat-mcp)
;;; gh-copilot-chat-mcp.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; checkdoc-verb-check-experimental-flag: nil
;; End:
