;;; gh-copilot-chat --- gh-copilot-chat-transient.el  --- copilot chat transient functions -*- lexical-binding: t; -*-

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

(require 'transient)

(require 'gh-copilot-chat-command)
(require 'gh-copilot-chat-mcp)

;;;###autoload (autoload 'gh-copilot-chat-transient "gh-copilot-chat" nil t)
(transient-define-prefix
 gh-copilot-chat-transient () "Copilot chat command menu."
 [["Commands"
   ("d" "Display chat" gh-copilot-chat-display)
   ("h" "Hide chat" gh-copilot-chat-hide)
   ("x" "Reset" gh-copilot-chat-reset)
   ("g" "Go to buffer" gh-copilot-chat-switch-to-buffer)
   ("q" "Quit" transient-quit-one)]
  ["Instance"
   ("M" "Set model" gh-copilot-chat-set-model)
   ("C" "Set commit model" gh-copilot-chat-set-commit-model)
   ("S" "Save chat" gh-copilot-chat-save)
   ("L" "Load chat" gh-copilot-chat-load)
   ("k" "Kill instance" gh-copilot-chat-kill-instance)]
  ["Actions"
   ("p" "Custom prompt" gh-copilot-chat-custom-prompt-selection)
   ("i" "Ask and insert" gh-copilot-chat-ask-and-insert)
   ("m" "Insert commit message" gh-copilot-chat-insert-commit-message)]
  ["Data"
   ("y" "Yank last code block" gh-copilot-chat-yank)
   ("s" "Send code to buffer" gh-copilot-chat-send-to-buffer)]
  ["Tools"
   ("b" "Buffers" gh-copilot-chat-transient-buffers)
   ("c" "Code helpers" gh-copilot-chat-transient-code)]])

;;;###autoload (autoload 'gh-copilot-chat-transient-buffers "gh-copilot-chat" nil t)
(transient-define-prefix
 gh-copilot-chat-transient-buffers () "Copilot chat buffers menu."
 [["Buffers" ("a" "Add buffers" gh-copilot-chat-add-buffers)
   ("A"
    "Add all buffers in current frame"
    gh-copilot-chat-add-buffers-in-current-window)
   ("d" "Delete buffers" gh-copilot-chat-del-buffers)
   ("D" "Delete all buffers" gh-copilot-chat-list-clear-buffers)
   ("f"
    "Add files under current directory"
    gh-copilot-chat-add-files-under-dir)
   ("l" "Display buffer list" gh-copilot-chat-list)
   ("c"
    "Clear buffers"
    gh-copilot-chat-list-clear-buffers)
   ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'gh-copilot-chat-transient-code "gh-copilot-chat" nil t)
(transient-define-prefix
 gh-copilot-chat-transient-code () "Copilot chat code helpers menu."
 [["Code helpers"
   ("e" "Explain" gh-copilot-chat-explain)
   ("E" "Explain symbol" gh-copilot-chat-explain-symbol-at-line)
   ("r" "Review" gh-copilot-chat-review)
   ("d" "Doc" gh-copilot-chat-doc)
   ("f" "Fix" gh-copilot-chat-fix)
   ("o" "Optimize" gh-copilot-chat-optimize)
   ("t" "Test" gh-copilot-chat-test)
   ("F" "Explain function" gh-copilot-chat-explain-defun)
   ("c" "Custom prompt function" gh-copilot-chat-custom-prompt-function)
   ("R" "Review whole buffer" gh-copilot-chat-review-whole-buffer)
   ("q" "Quit" transient-quit-one)]])


(defun gh-copilot-chat--index-to-key (index)
  "Convert INDEX to a string key for transient suffixes."
  (cond
   ((< index 10)
    (format "%d" index))
   ((< index 36)
    (char-to-string (+ ?a (- index 10))))
   ((< index 62)
    (char-to-string (+ ?A (- index 10))))
   (t
    (error "Index %d is out of range for transient suffixes" index))))

(defun gh-copilot-chat--mcp-generate-server-suffixes ()
  "Generate dynamic switches for servers."
  (let ((suffixes '())
        (index 0)
        (instance (gh-copilot-chat--current-instance)))
    ;; Add each server as switch
    (dolist (server (mapcar 'car mcp-hub-servers))
      (push (list
             (gh-copilot-chat--index-to-key index)
             (format "Add %s" server)
             (format "%s" server)
             :init-value
             (lambda (obj)
               (when (member
                      (slot-value obj 'argument)
                      (gh-copilot-chat-mcp-servers instance))
                 (setf (slot-value obj 'value) (slot-value obj 'argument)))))
            suffixes)
      (setq index (1+ index)))

    (push (list (gh-copilot-chat--index-to-key index) "Add All" "ALL") suffixes)
    (push (list (gh-copilot-chat--index-to-key (1+ index)) "Clear All" "CLEAR")
          suffixes)

    (nreverse suffixes)))

(defun gh-copilot-chat--mcp-handle-selection (servers)
  "Handle selected SERVERS from arguments."
  (interactive (list (transient-args 'gh-copilot-chat-mcp-servers-transient)))
  (let ((instance (gh-copilot-chat--current-instance)))
    (cond
     ((member "ALL" servers)
      (setf (gh-copilot-chat-mcp-servers instance)
            (mapcar 'car mcp-hub-servers)))
     ((member "CLEAR" servers)
      (setf (gh-copilot-chat-mcp-servers instance) nil))
     (t
      (setf (gh-copilot-chat-mcp-servers instance) servers)))
    (gh-copilot-chat--activate-mcp-servers instance)))

;;;###autoload (autoload 'gh-copilot-chat-mcp-servers-transient "gh-copilot-chat" nil t)
(transient-define-prefix
 gh-copilot-chat-mcp-servers-transient () "Copilot chat MCP servers menu."
 ["MCP servers:"
  :class transient-column
  :setup-children
  (lambda (_)
    (transient-parse-suffixes
     transient--prefix (gh-copilot-chat--mcp-generate-server-suffixes)))]
 [["Actions" ("RET"
    "Validate"
    gh-copilot-chat--mcp-handle-selection)
   ("q" "Cancel" transient-quit-one)]])

;;;###autoload (autoload 'gh-copilot-chat-set-mcp-servers "gh-copilot-chat" nil t)
(defalias
  'gh-copilot-chat-set-mcp-servers 'gh-copilot-chat-mcp-servers-transient)

(provide 'gh-copilot-chat-transient)
;;; gh-copilot-chat-transient.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; checkdoc-verb-check-experimental-flag: nil
;; End:
