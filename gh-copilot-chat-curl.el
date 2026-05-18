;;; gh-copilot-chat --- gh-copilot-chat-curl.el --- copilot chat curl backend -*- lexical-binding: t; -*-

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
;; This is curl backend for gh-copilot-chat code

;;; Code:

(require 'gh-copilot-chat-body)
(require 'gh-copilot-chat-common)
(require 'gh-copilot-chat-connection)
(require 'gh-copilot-chat-spinner)
(require 'gh-copilot-chat-backend)
(require 'gh-copilot-chat-mcp)
(require 'gh-copilot-chat-responses)
(require 'gh-copilot-chat-completions)

;; customs
(defcustom gh-copilot-chat-curl-program "curl"
  "Curl program to use if `gh-copilot-chat-use-curl' is set."
  :type 'string
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-curl-proxy nil
  "Curl will use this proxy if defined.
The proxy string can be specified with a protocol:// prefix.  No protocol
specified or http:// it is treated as an HTTP proxy.  Use socks4://,
socks4a://, socks5:// or socks5h:// to request a specific SOCKS version
to be used.

Unix domain sockets are supported for socks proxy.  Set localhost for the
host part.  e.g. socks5h://localhost/path/to/socket.sock

HTTPS proxy support works set with the https:// protocol prefix for
OpenSSL and GnuTLS.  It also works for BearSSL, mbedTLS, rustls,
Schannel, Secure Transport and wolfSSL (added in 7.87.0).

Unrecognized and unsupported proxy protocols cause an error.  Ancient
curl versions ignored unknown schemes and used http:// instead.

If the port number is not specified in the proxy string, it is assumed
to be 1080.

This option overrides existing environment variables that set the proxy
to use.  If there is an environment variable setting a proxy, you can set
proxy to \"\" to override it.

User and password that might be provided in the proxy string are URL
decoded by curl. This allows you to pass in special characters such as @
by using %40 or pass in a colon with %3a."
  :type 'string
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-curl-proxy-insecure nil
  "Insecure flag for `gh-copilot-chat' proxy with curl backend.
Every secure connection curl makes is verified to be secure before the
transfer takes place.  This option makes curl skip the verification step
with a proxy and proceed without checking."
  :type 'boolean
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-curl-proxy-user-pass nil
  "User password for `gh-copilot-chat' proxy with curl backend.
Specify the username and password <user:password> to use for proxy
authentication."
  :type 'boolean
  :group 'gh-copilot-chat)

;; structures
(cl-defstruct
 gh-copilot-chat-curl
 "Private data for Copilot chat curl backend."
 (file nil :type (or null file))
 (process nil :type (or null process))
 (responses (make-gh-copilot-chat-responses) :type gh-copilot-chat-responses)
 (completions
  (make-gh-copilot-chat-completions)
  :type gh-copilot-chat-completions))


;; functions
(defun gh-copilot-chat--curl-call-process (address method data &rest args)
  "Call curl synchronously.
Argument ADDRESS is the URL to call.
Argument METHOD is the HTTP method to use.
Argument DATA is the data to send.
Arguments ARGS are additional arguments to pass to curl."
  (let ((curl-args
         (append
          (list
           address
           "-s"
           "-X"
           (if (eq method 'post)
               "POST"
             "GET")
           "-A"
           "user-agent: CopilotChat.nvim/2.0.0"
           "-H"
           "content-type: application/json"
           "-H"
           "accept: application/json"
           "-H"
           "editor-plugin-version: CopilotChat.nvim/2.0.0"
           "-H"
           "editor-version: Neovim/0.10.0")
          (when data
            (list "-d" data))
          (when gh-copilot-chat-curl-proxy
            (list "-x" gh-copilot-chat-curl-proxy))
          (when gh-copilot-chat-curl-proxy-insecure
            (list "--proxy-insecure"))
          (when gh-copilot-chat-curl-proxy-user-pass
            (list "-U" gh-copilot-chat-curl-proxy-user-pass))
          args)))
    (let ((result
           (apply #'call-process
                  gh-copilot-chat-curl-program
                  nil
                  t
                  nil
                  curl-args)))
      (when (/= result 0)
        (error (format "curl returned non-zero result: %d" result))))))

(defun gh-copilot-chat--curl-make-process
    (instance address method data filter vision callback &rest args)
  "Call curl asynchronously for INSTANCE.
Argument ADDRESS is the URL to call.
Argument METHOD is the HTTP method to use.
Argument DATA is the data to send.
Argument FILTER is the function called to parse data.
If VISION is t, add vision header.
Argument CALLBACK is the function to call with analysed data.
Optional argument ARGS are additional arguments to pass to curl."
  (let ((command
         (append
          (list
           gh-copilot-chat-curl-program
           address
           "-s"
           "-X"
           (if (eq method 'post)
               "POST"
             "GET")
           "-A"
           "user-agent: CopilotChat.nvim/2.0.0"
           "-H"
           "content-type: application/json"
           "-H"
           "accept: application/json"
           "-H"
           "editor-plugin-version: CopilotChat.nvim/2.0.0"
           "-H"
           "editor-version: Neovim/0.10.0"
           "-H"
           "copilot-integration-id: vscode-chat")
          (when vision
            (list "-H" "Copilot-Vision-Request: true"))
          (when data
            (list "-d" data))
          (when gh-copilot-chat-curl-proxy
            (list "-x" gh-copilot-chat-curl-proxy))
          (when gh-copilot-chat-curl-proxy-insecure
            (list "--proxy-insecure"))
          (when gh-copilot-chat-curl-proxy-user-pass
            (list "-U" gh-copilot-chat-curl-proxy-user-pass))
          args)))
    (setf (gh-copilot-chat-curl-process (gh-copilot-chat--backend instance))
          (make-process
           :name "gh-copilot-chat-curl"
           :buffer nil
           :filter filter
           :sentinel
           (lambda (proc _exit)
             (when (/= (process-exit-status proc) 0)
               (let ((error-msg
                      (format "Curl interrupted: %d"
                              (process-exit-status proc))))
                 (funcall callback instance error-msg)
                 (funcall callback instance gh-copilot-chat--magic)))
             (setf (gh-copilot-chat-curl-process
                    (gh-copilot-chat--backend instance))
                   nil)
             (gh-copilot-chat--spinner-stop instance))
           :stderr (get-buffer-create "*gh-copilot-chat-curl-stderr*")
           :command command))))

(defun gh-copilot-chat--curl-parse-github-token ()
  "Curl github token request parsing."
  (goto-char (point-min))
  (let* ((json-data (json-parse-buffer :false-object :json-false))
         (token (gethash "access_token" json-data)))
    (setf (gh-copilot-chat-connection-github-token gh-copilot-chat--connection)
          token)
    (gh-copilot-chat--write-cached-token token)))

(defun gh-copilot-chat--curl-parse-login ()
  "Curl login request parsing."
  (goto-char (point-min))
  (let* ((json-data (json-parse-buffer :false-object :json-false))
         (device-code (gethash "device_code" json-data))
         (user-code (gethash "user_code" json-data))
         (verification-uri (gethash "verification_uri" json-data)))
    (gui-set-selection 'CLIPBOARD user-code)
    (message
     (format
      "Your one-time code %s is copied. \
Press ENTER to open GitHub in your browser. \
If your browser does not open automatically, browse to %s."
      user-code verification-uri))
    (read-from-minibuffer
     (format
      "Your one-time code %s is copied. \
Press ENTER to open GitHub in your browser. \
If your browser does not open automatically, browse to %s."
      user-code verification-uri))
    (browse-url verification-uri)
    (read-from-minibuffer "Press ENTER after authorizing.")
    (with-temp-buffer
      (gh-copilot-chat--curl-call-process
       "https://github.com/login/oauth/access_token" 'post
       (format
        "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"device_code\":\"%s\",\"grant_type\":\"urn:ietf:params:oauth:grant-type:device_code\"}"
        device-code))
      (gh-copilot-chat--curl-parse-github-token))))


(defun gh-copilot-chat--curl-login ()
  "Manage github login."
  (with-temp-buffer
    (gh-copilot-chat--curl-call-process
     "https://github.com/login/device/code"
     'post
     "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"scope\":\"read:user\"}")
    (gh-copilot-chat--curl-parse-login)))


(defun gh-copilot-chat--curl-parse-renew-token ()
  "Curl renew token request parsing."
  (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (let ((json-data
         (json-parse-buffer
          :object-type 'alist ;need alist to be compatible with
          ;gh-copilot-chat-token format
          :false-object
          :json-false))
        (cache-dir
         (file-name-directory (expand-file-name gh-copilot-chat-token-cache))))
    (setf (gh-copilot-chat-connection-token gh-copilot-chat--connection)
          json-data)
    ;; save token in gh-copilot-chat-token-cache file after creating
    ;; folders if needed
    (when (not (file-directory-p cache-dir))
      (make-directory cache-dir t))
    (with-temp-file gh-copilot-chat-token-cache
      (insert (json-serialize json-data :false-object :json-false)))))


(defun gh-copilot-chat--curl-renew-token ()
  "Renew session token."
  (with-temp-buffer
    (gh-copilot-chat--curl-call-process
     "https://api.github.com/copilot_internal/v2/token" 'get nil
     "-H"
     (format
      "authorization: token %s"
      (gh-copilot-chat-connection-github-token gh-copilot-chat--connection)))
    (gh-copilot-chat--curl-parse-renew-token)))


(defun gh-copilot-chat--curl-analyze-answer
    (instance string callback no-history)
  "Analyse curl response.
Argument INSTANCE is the copilot chat instance to use.
Argument STRING is the data returned by curl.
Argument CALLBACK is the function to call with analysed data.
Argument NO-HISTORY is a boolean to indicate
if the response should be added to history."
  (if (gh-copilot-chat--instance-support-responses-endpoint instance)
      (gh-copilot-chat--responses-analyze
       instance
       (gh-copilot-chat-curl-responses (gh-copilot-chat--backend instance))
       string
       callback
       no-history)
    (gh-copilot-chat--completions-analyze
     instance
     (gh-copilot-chat-curl-completions (gh-copilot-chat--backend instance))
     string
     callback
     no-history)))

(defun gh-copilot-chat--curl-ask (instance prompt callback out-of-context)
  "Ask a question to Copilot using curl backend.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the prompt to send to copilot.  It can be a string or a list
of json objects.
Argument CALLBACK is the function to call with copilot answer as argument.
Argument OUT-OF-CONTEXT is a boolean to indicate
if the prompt is out of context."
  (setf
   (gh-copilot-chat-curl-responses (gh-copilot-chat--backend instance)) (make-gh-copilot-chat-responses)
   (gh-copilot-chat-curl-completions (gh-copilot-chat--backend instance)) (make-gh-copilot-chat-completions))

  ;; Start the spinner animation only for instances with chat buffers
  (when (buffer-live-p (gh-copilot-chat-chat-buffer instance))
    (gh-copilot-chat--spinner-start instance))

  (let ((file (gh-copilot-chat-curl-file (gh-copilot-chat--backend instance))))
    (when (and file (file-exists-p file))
      (delete-file file)))
  (setf (gh-copilot-chat-curl-file (gh-copilot-chat--backend instance))
        (make-temp-file "gh-copilot-chat"))
  (let ((coding-system-for-write 'raw-text))
    (with-temp-file (gh-copilot-chat-curl-file
                     (gh-copilot-chat--backend instance))
      (insert
       (if (gh-copilot-chat--instance-support-responses-endpoint instance)
           (gh-copilot-chat--responses-create-req
            instance prompt out-of-context)
         (gh-copilot-chat--completions-create-req
          instance prompt out-of-context)))))

  (unless out-of-context
    (let* ((history (gh-copilot-chat-history instance))
           (new-history
            (if (stringp prompt)
                ;; classic prompt
                (cons `(:content ,prompt :role "user") history)
              ;; tool answer
              (append prompt history))))
      (setf (gh-copilot-chat-history instance) new-history)))

  (gh-copilot-chat--curl-make-process
   instance
   (if (gh-copilot-chat--instance-support-responses-endpoint instance)
       "https://api.githubcopilot.com/responses"
     "https://api.githubcopilot.com/chat/completions")
   'post
   (concat "@" (gh-copilot-chat-curl-file (gh-copilot-chat--backend instance)))
   (lambda (proc string)
     (gh-copilot-chat--debug 'curl "gh-copilot-chat--curl-ask: %s" string)
     (if (not
          (string= string "quota exceeded\n"))
         (if (gh-copilot-chat--instance-support-streaming instance)
             (gh-copilot-chat--curl-analyze-answer
              instance string callback out-of-context)
           (gh-copilot-chat--completions-analyze-nonstream
            instance
            (gh-copilot-chat-curl-completions
             (gh-copilot-chat--backend instance))
            proc string callback out-of-context))
       (gh-copilot-chat--spinner-stop instance)
       (funcall callback instance "Quota exceeded.")))
   (gh-copilot-chat-uses-vision instance)
   callback
   "-H"
   "openai-intent: conversation-panel"
   "-H"
   (concat
    "authorization: Bearer "
    (alist-get
     'token (gh-copilot-chat-connection-token gh-copilot-chat--connection)))
   "-H"
   (concat "x-request-id: " (gh-copilot-chat--uuid))
   "-H"
   (concat
    "vscode-sessionid: "
    (gh-copilot-chat-connection-sessionid gh-copilot-chat--connection))
   "-H"
   (concat
    "vscode-machineid: "
    (gh-copilot-chat-connection-machineid gh-copilot-chat--connection))))

(defun gh-copilot-chat--curl-cancel (instance)
  "Cancel the current request for INSTANCE."
  (gh-copilot-chat--spinner-stop instance)
  (let ((proc
         (gh-copilot-chat-curl-process (gh-copilot-chat--backend instance))))
    (when (process-live-p proc)
      (delete-process proc))))

(defun gh-copilot-chat--curl-quotas ()
  "Get the current GitHub Copilot quotas."
  (with-temp-buffer
    (let* ((curl-args
            (append
             (list
              "https://api.github.com/rate_limit" "-s" "-X" "GET" "-H"
              (concat
               "authorization: Bearer "
               (gh-copilot-chat-connection-github-token
                gh-copilot-chat--connection))
              "-H" "Accept: application/vnd.github+json")))
           (result
            (apply #'call-process
                   gh-copilot-chat-curl-program
                   nil
                   t
                   nil
                   curl-args)))
      (when (/= result 0)
        (error (format "curl returned non-zero result: %d" result))))
    (goto-char (point-min))
    (let* ((json-data
            (json-parse-buffer :object-type 'alist :false-object :json-false))
           (resources (alist-get 'resources json-data))
           (result '()))
      (dolist (resource resources)
        (let* ((name
                (capitalize
                 (replace-regexp-in-string
                  "_" " "
                  (symbol-name (car resource)))))
               (data (cdr resource))
               (limit (alist-get 'limit data))
               (used (alist-get 'used data))
               (remaining (alist-get 'remaining data))
               (reset (alist-get 'reset data)))
          (push (list name limit used remaining reset) result)))
      (nreverse result))))

(defun gh-copilot-chat--curl-init (instance)
  "Initialize Copilot chat curl backend for INSTANCE."
  (setf (gh-copilot-chat--backend instance) (make-gh-copilot-chat-curl)))


;; Top-level execute code.
(cl-pushnew
 (make-gh-copilot-chat-backend
  :id 'curl
  :init-fn #'gh-copilot-chat--curl-init
  :clean-fn nil
  :login-fn #'gh-copilot-chat--curl-login
  :renew-token-fn #'gh-copilot-chat--curl-renew-token
  :ask-fn #'gh-copilot-chat--curl-ask
  :cancel-fn #'gh-copilot-chat--curl-cancel
  :quotas-fn #'gh-copilot-chat--curl-quotas)
 gh-copilot-chat--backend-list
 :test #'equal)

(provide 'gh-copilot-chat-curl)
;;; gh-copilot-chat-curl.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
