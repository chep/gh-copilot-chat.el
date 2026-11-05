;;; gh-copilot-chat-mcp-proxy.el --- Expose mcp.el connections over HTTP -*- lexical-binding: t; -*-

(require 'web-server)
(require 'jsonrpc)
(require 'mcp)

(defvar gh-copilot-chat-mcp-proxy--server nil)
(defvar gh-copilot-chat-mcp-proxy--token nil
  "Secret partagé avec Claude, demandé dans l'en-tête Authorization.")

(defcustom gh-copilot-chat-mcp-proxy-timeout 120
  "Délai maximal (en secondes) pour une requête relayée."
  :type 'integer :group 'gh-copilot-chat)

(defun gh-copilot-chat-mcp-proxy--header (headers name)
  "Valeur de l'en-tête NAME dans HEADERS, sans tenir compte de la casse."
  (cdr (seq-find (lambda (h) (string-equal-ignore-case
                              (string-remove-prefix ":" (format "%s" (car h)))
                              name))
                 headers)))

(defun gh-copilot-chat-mcp-proxy--reply (process status &optional body)
  "Envoie BODY (une plist, ou nil) avec STATUS sur PROCESS, puis ferme la connexion."
  (when (process-live-p process)
    ;; jsonrpc décode `false' en :json-false ; json-serialize attend :false par défaut.
    (let ((json (and body (encode-coding-string
                           (json-serialize body :false-object :json-false)
                           'utf-8))))
      (ws-response-header process status
                          (cons "Content-Type" "application/json")
                          (cons "Content-Length"
                                (number-to-string (if json (length json) 0)))
                          (cons "Connection" "close"))
      (when json (process-send-string process json))
      (delete-process process))))

(defun gh-copilot-chat-mcp-proxy--connection (server-name)
  (when-let* ((conn (gethash server-name mcp-server-connections)))
    (and (jsonrpc-running-p conn) conn)))

(defun gh-copilot-chat-mcp-proxy--handle (request)
  (with-slots (process headers body) request
    (let* ((path (cdr (assoc :POST headers)))
           (server-name (and path (string-match "\\`/mcp/\\([^/?]+\\)" path)
                             (url-unhex-string (match-string 1 path))))
           ;; Pas « Authorization: Bearer » : web-server.el ne gère que Basic/Digest
           ;; et renvoie une erreur 500 « un-support protocol » pour le reste.
           (auth (gh-copilot-chat-mcp-proxy--header headers "x-mcp-proxy-token")))
      (cond
       ;; Seuls POST et un jeton valide sont acceptés
       ((not path) (gh-copilot-chat-mcp-proxy--reply process 405))
       ((not (equal auth gh-copilot-chat-mcp-proxy--token))
        (gh-copilot-chat-mcp-proxy--reply process 401))
       (t
        (let* ((msg (json-parse-string body :object-type 'plist :array-type 'array
                                       :null-object nil :false-object :json-false))
               (id (plist-get msg :id))
               (method (plist-get msg :method))
               (params (plist-get msg :params))
               (conn (and server-name (gh-copilot-chat-mcp-proxy--connection server-name))))
          (cond
           ;; Notifications (initialized, cancelled…) : mcp.el a déjà fait l'initialisation
           ((null id) (gh-copilot-chat-mcp-proxy--reply process 202))
           ((null conn)
            (gh-copilot-chat-mcp-proxy--reply
             process 200 `(:jsonrpc "2.0" :id ,id
                           :error (:code -32000 :message ,(format "MCP server %s not running in Emacs" server-name)))))
           ;; initialize : on répond avec ce que mcp.el a déjà négocié
           ((equal method "initialize")
            (gh-copilot-chat-mcp-proxy--reply
             process 200
             `(:jsonrpc "2.0" :id ,id
               :result (:protocolVersion ,(or (plist-get params :protocolVersion) "2025-06-18")
                        :capabilities ,(or (oref conn -capabilities) '(:tools (:listChanged :json-false)))
                        :serverInfo ,(or (oref conn -serverinfo) `(:name ,server-name :version "proxy"))))))
           ;; Tout le reste (tools/list, tools/call, ping…) est relayé tel quel
           (t
            (jsonrpc-async-request
             conn (intern (concat ":" method)) params
             :timeout gh-copilot-chat-mcp-proxy-timeout
             :success-fn (lambda (result)
                           (gh-copilot-chat-mcp-proxy--reply
                            process 200 `(:jsonrpc "2.0" :id ,id :result ,result)))
             :error-fn (lambda (err)
                         (gh-copilot-chat-mcp-proxy--reply
                          process 200 `(:jsonrpc "2.0" :id ,id
                                        :error (:code ,(or (plist-get err :code) -32603)
                                                :message ,(or (plist-get err :message) "error")))))
             :timeout-fn (lambda ()
                           (gh-copilot-chat-mcp-proxy--reply
                            process 200 `(:jsonrpc "2.0" :id ,id
                                          :error (:code -32001 :message "timeout")))))
            ;; La réponse est envoyée plus tard, de manière asynchrone
            :keep-alive))))))))

(defun gh-copilot-chat-mcp-proxy-ensure ()
  "Démarre le proxy si besoin et renvoie son port."
  (unless (and gh-copilot-chat-mcp-proxy--server
               (process-live-p (oref gh-copilot-chat-mcp-proxy--server process)))
    (setq gh-copilot-chat-mcp-proxy--token
          (secure-hash 'sha256 (format "%s%s%s" (random t) (emacs-pid) (float-time))))
    (setq gh-copilot-chat-mcp-proxy--server
          (ws-start '(((:POST . "^/mcp/") . gh-copilot-chat-mcp-proxy--handle)
                      ((lambda (_) t) . (lambda (r) (gh-copilot-chat-mcp-proxy--reply (oref r process) 405))))
                    0 nil :host "127.0.0.1")))
  (process-contact (oref gh-copilot-chat-mcp-proxy--server process) :service))

(defun gh-copilot-chat-mcp-proxy-config (server-name)
  "Entrée mcpServers (alist) pointant vers le proxy, ou nil si SERVER-NAME n'est pas connecté."
  (when (gh-copilot-chat-mcp-proxy--connection server-name)
    (let ((port (gh-copilot-chat-mcp-proxy-ensure)))
      `((type . "http")
        (url . ,(format "http://127.0.0.1:%d/mcp/%s" port (url-hexify-string server-name)))
        (headers . ((X-MCP-Proxy-Token . ,gh-copilot-chat-mcp-proxy--token)))))))

(provide 'gh-copilot-chat-mcp-proxy)
