;;; gh-copilot-chat --- gh-copilot-chat-command.el --- copilot chat command -*- lexical-binding: t; -*-

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

(require 'polymode)

(require 'gh-copilot-chat-copilot)
(require 'gh-copilot-chat-git)
(require 'gh-copilot-chat-model)
(require 'gh-copilot-chat-prompt-mode)
(require 'gh-copilot-chat-request)

;; customs
(defcustom gh-copilot-chat-list-added-buffers-only nil
  "If non-nil, only show buffers that have been added to the Copilot chat list."
  :type 'boolean
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-list-show-path t
  "If t, show path of files in the Copilot chat list.
If nil, show only file names."
  :type 'boolean
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-list-show-relative-path t
  "If t, show relative path of buffers in the Copilot chat list.
If nil, show absolute path."
  :type 'boolean
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-default-save-dir
  (concat user-emacs-directory "gh-copilot-chat")
  "Default directory to save chats."
  :type 'string
  :group 'gh-copilot-chat)

;; Faces
(defface gh-copilot-chat-list-selected-buffer-face
  '((t :inherit font-lock-keyword-face))
  "Face used for selected buffers in `gh-copilot-chat' buffer list."
  :group 'gh-copilot-chat)
(defface gh-copilot-chat-list-default-face '((t :inherit default))
  "Face used for unselected buffers in `gh-copilot-chat' buffer list."
  :group 'gh-copilot-chat)

;; Variables
(defvar gh-copilot-chat-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'gh-copilot-chat-list-add-or-remove-buffer)
    (define-key map (kbd "SPC") 'gh-copilot-chat-list-add-or-remove-buffer)
    (define-key map (kbd "C-c C-c") 'gh-copilot-chat-list-clear-buffers)
    (define-key
     map (kbd "g")
     (lambda ()
       (interactive)
       (gh-copilot-chat-list-refresh (gh-copilot-chat--current-instance))))
    (define-key
     map (kbd "q")
     (lambda ()
       (interactive)
       (bury-buffer)
       (delete-window)))
    map)
  "Keymap for `gh-copilot-chat-list-mode'.")

;; Functions
(define-derived-mode
 gh-copilot-chat-list-mode
 special-mode
 "Copilot Chat List"
 "Major mode for listing and managing buffers in Copilot chat."
 (setq buffer-read-only t))

(defun gh-copilot-chat-prompt-send ()
  "Send the prompt content to Copilot.
Retrieves the current prompt, displays it in the chat buffer, and sends it
to Copilot for processing."
  (interactive)
  (let ((instance (gh-copilot-chat--current-instance)))
    (when instance
      (gh-copilot-chat--display instance)
      (let ((prompt (gh-copilot-chat--pop-current-prompt instance)))
        (gh-copilot-chat--write-buffer
         instance
         (gh-copilot-chat--format-data instance prompt 'prompt) nil)
        (with-current-buffer (gh-copilot-chat--get-buffer instance)
          (recenter-top-bottom))
        (setf (gh-copilot-chat-prompt-history-position instance) nil)
        (gh-copilot-chat--ask instance prompt 'gh-copilot-chat-prompt-cb)))))

;;;###autoload (autoload 'gh-copilot-chat-ask-and-insert "gh-copilot-chat" nil t)
(defun gh-copilot-chat-ask-and-insert ()
  "Send to Copilot a custom prompt and insert answer in current buffer at point."
  (interactive)
  (let* ((instance (gh-copilot-chat--current-instance))
         (prompt (read-from-minibuffer "Copilot prompt: "))
         (current-buf (current-buffer)))
    (gh-copilot-chat--ask
     instance prompt
     (lambda (instance content)
       (gh-copilot-chat-prompt-cb instance content current-buf)))))

(defun gh-copilot-chat--ask-region (prompt beg end)
  "Send to Copilot a prompt followed by the current selected code.
Argument PROMPT is the prompt to send to Copilot.
BEG and END are the beginning and end positions of the region to explain."
  (let ((instance (gh-copilot-chat--current-instance))
        (code (buffer-substring-no-properties beg end)))
    (gh-copilot-chat--insert-and-send-prompt
     instance
     (concat
      (cdr (assoc prompt (gh-copilot-chat--prompts)))
      (gh-copilot-chat--format-code code)))))

;;;###autoload (autoload 'gh-copilot-chat-explain "gh-copilot-chat" nil t)
(defun gh-copilot-chat-explain (beg end)
  "Ask Copilot to explain the current selected code.
BEG and END are the beginning and end positions of the region to explain."
  (interactive "r")
  (gh-copilot-chat--ask-region 'explain beg end))

;;;###autoload (autoload 'gh-copilot-chat-review "gh-copilot-chat" nil t)
(defun gh-copilot-chat-review (beg end)
  "Ask Copilot to review the current selected code.
BEG and END are the beginning and end positions of the region to review."
  (interactive "r")
  (gh-copilot-chat--ask-region 'review beg end))

;;;###autoload (autoload 'gh-copilot-chat-doc "gh-copilot-chat" nil t)
(defun gh-copilot-chat-doc (beg end)
  "Ask Copilot to write documentation for the current selected code.
BEG and END are the beginning and end positions of the region to document."
  (interactive "r")
  (gh-copilot-chat--ask-region 'doc beg end))

;;;###autoload (autoload 'gh-copilot-chat-fix "gh-copilot-chat" nil t)
(defun gh-copilot-chat-fix (beg end)
  "Ask Copilot to fix the current selected code.
BEG and END are the beginning and end positions of the region to fix."
  (interactive "r")
  (gh-copilot-chat--ask-region 'fix beg end))

;;;###autoload (autoload 'gh-copilot-chat-optimize "gh-copilot-chat" nil t)
(defun gh-copilot-chat-optimize (beg end)
  "Ask Copilot to optimize the current selected code.
BEG and END are the beginning and end positions of the region to optimize."
  (interactive "r")
  (gh-copilot-chat--ask-region 'optimize beg end))

;;;###autoload (autoload 'gh-copilot-chat-test "gh-copilot-chat" nil t)
(defun gh-copilot-chat-test (beg end)
  "Ask Copilot to generate test for the current selected code.
BEG and END are the beginning and end positions of the region to test."
  (interactive "r")
  (gh-copilot-chat--ask-region 'test beg end))

(defun gh-copilot-chat--insert-prompt (instance prompt)
  "Insert PROMPT in the Copilot Chat prompt region.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the text to insert in the prompt region."
  (let ((prompt-fn
         (gh-copilot-chat-frontend-insert-prompt-fn
          (gh-copilot-chat--get-frontend))))
    (when prompt-fn
      (funcall prompt-fn instance prompt))))

(defun gh-copilot-chat--insert-and-send-prompt (instance prompt)
  "Helper function to prepare buffer and send PROMPT to Copilot.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the text to send to Copilot."
  (gh-copilot-chat--insert-prompt instance prompt)
  (gh-copilot-chat-prompt-send))

(defun gh-copilot-chat--get-language ()
  "Get the current language of the buffer.
Derives language name from the major mode of the current buffer."
  (if (derived-mode-p 'prog-mode) ; current buffer is a programming language buffer
      (let* ((major-mode-str (symbol-name major-mode))
             (lang
              (replace-regexp-in-string
               "\\(?:-ts\\)?-mode$" "" major-mode-str)))
        lang)
    nil))

(defun gh-copilot-chat--format-code (code)
  "Format code according to the frontend.
Argument CODE is the code to be formatted."
  (let ((format-fn
         (gh-copilot-chat-frontend-format-code-fn
          (gh-copilot-chat--get-frontend))))
    (if format-fn
        (funcall format-fn code (gh-copilot-chat--get-language))
      code)))

;;;###autoload (autoload 'gh-copilot-chat-explain-symbol-at-line "gh-copilot-chat" nil t)
(defun gh-copilot-chat-explain-symbol-at-line ()
  "Ask Copilot to explain symbol under point.
Given the code line as background info."
  (interactive)
  (let*
      ((instance (gh-copilot-chat--current-instance))
       (symbol (thing-at-point 'symbol))
       (line
        (buffer-substring-no-properties
         (line-beginning-position) (line-end-position)))
       (prompt
        (format
         "Please explain what '%s' means in the context of this code line:\n%s"
         symbol (gh-copilot-chat--format-code line))))
    (gh-copilot-chat--insert-and-send-prompt instance prompt)))

;;;###autoload (autoload 'gh-copilot-chat-explain-defun "gh-copilot-chat" nil t)
(defun gh-copilot-chat-explain-defun ()
  "Mark current function definition and ask Copilot to explain it, then unmark."
  (interactive)
  (save-excursion
    (mark-defun)
    (call-interactively 'gh-copilot-chat-explain)
    (deactivate-mark)))

;;;###autoload (autoload 'gh-copilot-chat-custom-prompt-function "gh-copilot-chat" nil t)
(defun gh-copilot-chat-custom-prompt-function ()
  "Mark current function and ask `gh-copilot-chat' with custom prompt."
  (interactive)
  (save-excursion
    (mark-defun)
    (gh-copilot-chat-custom-prompt-selection)
    (deactivate-mark)))

;;;###autoload (autoload 'gh-copilot-chat-review-whole-buffer "gh-copilot-chat" nil t)
(defun gh-copilot-chat-review-whole-buffer ()
  "Mark whole buffer, ask Copilot to review it, then unmark.
It can be used to review the magit diff for my change, or other people's"
  (interactive)
  (save-excursion (gh-copilot-chat-review (point-min) (point-max))))

;;;###autoload (autoload 'gh-copilot-chat-switch-to-buffer "gh-copilot-chat" nil t)
(defun gh-copilot-chat-switch-to-buffer ()
  "Switch to Copilot Chat buffer.
Side by side with the current code editing buffer."
  (interactive)
  (let ((instance (gh-copilot-chat--current-instance)))
    (unless (equal (pm-base-buffer) (gh-copilot-chat--get-buffer instance))
      (switch-to-buffer-other-window (gh-copilot-chat--get-buffer instance))))
  (gh-copilot-chat-goto-input))

;;;###autoload (autoload 'gh-copilot-chat-custom-prompt-selection "gh-copilot-chat" nil t)
(defun gh-copilot-chat-custom-prompt-selection (&optional custom-prompt)
  "Send to Copilot a custom prompt followed by the current selected code/buffer.
If CUSTOM-PROMPT is provided, use it instead of reading from the mini-buffer."
  (interactive)
  (let* ((instance (gh-copilot-chat--current-instance))
         (prompt (or custom-prompt (read-from-minibuffer "Copilot prompt: ")))
         (code
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (buffer-substring-no-properties (point-min) (point-max))))
         (formatted-prompt
          (concat prompt "\n" (gh-copilot-chat--format-code code))))
    (gh-copilot-chat--insert-and-send-prompt instance formatted-prompt)))

;;;###autoload (autoload 'gh-copilot-chat-custom-prompt-mini-buffer "gh-copilot-chat" nil t)
(defun gh-copilot-chat-custom-prompt-mini-buffer ()
  "Read a string with Helm completion, showing historical inputs."
  (interactive)
  (let* ((instance (gh-copilot-chat--current-instance))
         (prompt "Question for gh-copilot-chat: "))
    (setq gh-copilot-chat--prompt-history
          (mapcar
           (lambda (msg) (plist-get msg :content))
           (seq-filter
            (lambda (msg)
              (equal (plist-get msg :role) "user"))
            (gh-copilot-chat-history instance))))
    (let ((input (read-string prompt nil 'gh-copilot-chat--prompt-history 0)))
      (gh-copilot-chat--insert-and-send-prompt instance input))))

;;;###autoload (autoload 'gh-copilot-chat-list "gh-copilot-chat" nil t)
(defun gh-copilot-chat-list ()
  "Open Copilot Chat list buffer."
  (interactive)
  (let* ((instance (gh-copilot-chat--current-instance))
         (buffer (gh-copilot-chat--get-list-buffer-create instance)))
    (with-current-buffer buffer
      (gh-copilot-chat-list-mode))
    (gh-copilot-chat-list-refresh instance)
    (switch-to-buffer buffer)))

;;;###autoload (autoload 'gh-copilot-chat-display "gh-copilot-chat" nil t)
(defun gh-copilot-chat-display (&optional arg)
  "Display copilot chat buffer.
With prefix argument, explicitly ask for which instance to use.
Optional argument ARG if non-nil, force instance selection."
  (interactive "P")
  (let ((instance
         (if arg
             (gh-copilot-chat--ask-for-instance)
           (gh-copilot-chat--current-instance))))
    (gh-copilot-chat--display instance)))

;;;###autoload (autoload 'gh-copilot-chat "gh-copilot-chat" nil t)
(defalias 'gh-copilot-chat 'gh-copilot-chat-display)

;;;###autoload (autoload 'gh-copilot-chat-hide "gh-copilot-chat" nil t)
(defun gh-copilot-chat-hide ()
  "Hide copilot chat buffer."
  (interactive)
  (let* ((instance (gh-copilot-chat--current-instance))
         (base-buffer (gh-copilot-chat--get-buffer instance)))
    (dolist (window (window-list))
      (let ((buf (window-buffer window)))
        (when (or (eq buf base-buffer)
                  (eq
                   (with-current-buffer buf
                     (pm-base-buffer))
                   base-buffer))
          (delete-window window))))))

(defun gh-copilot-chat-add-current-buffer ()
  "Add current buffer in sent buffers list."
  (interactive)
  (let ((instance (gh-copilot-chat--current-instance)))
    (gh-copilot-chat--add-buffer instance (current-buffer))
    (gh-copilot-chat-list-refresh instance)))

(defun gh-copilot-chat-del-current-buffer ()
  "Remove current buffer from sent buffers list."
  (interactive)
  (let ((instance (gh-copilot-chat--current-instance)))
    (gh-copilot-chat--del-buffer instance (current-buffer))
    (gh-copilot-chat-list-refresh instance)))

(defun gh-copilot-chat-add-buffers (buffers)
  "Add BUFFERS to sent buffers list."
  (interactive (list
                (completing-read-multiple
                 "Buffers: "
                 (mapcar #'buffer-name (buffer-list))
                 nil
                 t
                 (buffer-name (current-buffer)))))
  (let ((instance (gh-copilot-chat--current-instance)))
    (mapc (lambda (buf) (gh-copilot-chat--add-buffer instance buf)) buffers)
    (gh-copilot-chat-list-refresh instance)))

(defun gh-copilot-chat-del-buffers (buffers)
  "Remove BUFFERS from sent buffers list."
  (interactive (list
                (completing-read-multiple
                 "Buffers: "
                 (mapcar #'buffer-name (buffer-list))
                 nil
                 t
                 (buffer-name (current-buffer)))))
  (let ((instance (gh-copilot-chat--current-instance)))
    (mapc (lambda (buf) (gh-copilot-chat--del-buffer instance buf)) buffers)
    (gh-copilot-chat-list-refresh instance)))

(defun gh-copilot-chat-add-file (file-path)
  "Add FILE-PATH to `gh-copilot-chat' buffers without changing current window layout."
  (interactive "fFile to add: ")
  (save-window-excursion
    (let ((current-buf (current-buffer)))
      (find-file file-path)
      (gh-copilot-chat-add-current-buffer)
      (switch-to-buffer current-buf))))

(defun gh-copilot-chat-add-buffers-in-current-window ()
  "Add files in all buffers in the current Emacs window to the Copilot chat."
  (interactive)
  (let ((buffers (mapcar 'window-buffer (window-list)))
        (added-buffers '()))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when buffer-file-name
          (gh-copilot-chat-add-current-buffer)
          (push (buffer-name buffer) added-buffers))))
    (message "Added buffers: %s" (string-join added-buffers ", "))))

(defun gh-copilot-chat-add-files-under-dir ()
  "Add all files with same suffix as current file under current directory.
If there are more than 40 files, refuse to add and show warning message."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file")
    (let* ((current-suffix (file-name-extension buffer-file-name))
           (dir (file-name-directory buffer-file-name))
           (max-files 40)
           (files
            (directory-files dir
                             t (concat "\\." current-suffix "$")
                             t))) ; t means don't include . and ..
      (if (> (length files) max-files)
          (message "Too many files (%d, > %d) found with suffix .%s. Aborting."
                   (length files)
                   max-files
                   current-suffix)
        (dolist (file files)
          (gh-copilot-chat-add-file file))
        (message "Added %d files with suffix .%s"
                 (length files)
                 current-suffix)))))

(defun gh-copilot-chat--buffer-list (instance)
  "Return a list of buffer with files in INSTANCE directory."
  (let* ((dir (gh-copilot-chat-directory instance))
         (bufs-under-dir
          (cl-remove-if-not
           (lambda (buf)
             (with-current-buffer buf
               (and buffer-file-name
                    (file-in-directory-p buffer-file-name dir))))
           (buffer-list))))
    (cl-union bufs-under-dir (gh-copilot-chat-buffers instance) :test #'eq)))

(defun gh-copilot-chat-list-refresh (&optional instance)
  "Refresh the list of buffers in the current Copilot chat list buffer.
Optional argument INSTANCE specifies which instance to refresh the list for."
  (interactive)
  (unless instance
    (setq instance (gh-copilot-chat--current-instance)))
  (setf (gh-copilot-chat-buffers instance)
        (cl-remove-if-not #'buffer-live-p (gh-copilot-chat-buffers instance)))
  (with-current-buffer (gh-copilot-chat--get-list-buffer-create instance)
    (let* ((pt (point))
           (inhibit-read-only t)
           (buffers
            (if gh-copilot-chat-list-added-buffers-only
                (gh-copilot-chat-buffers instance)
              (gh-copilot-chat--buffer-list instance)))
           (sorted-buffers
            (sort buffers
                  (lambda (a b)
                    (string<
                     (symbol-name (buffer-local-value 'major-mode a))
                     (symbol-name (buffer-local-value 'major-mode b)))))))
      (erase-buffer)
      (setq-local header-line-format
                  (concat "Copilot chat " (gh-copilot-chat-directory instance)))
      (dolist (buffer sorted-buffers)
        (let* ((file-name (buffer-file-name buffer))
               (buffer-name
                (if (and file-name gh-copilot-chat-list-show-path)
                    (if gh-copilot-chat-list-show-relative-path
                        (file-relative-name file-name
                                            (gh-copilot-chat-directory
                                             instance))
                      file-name)
                  (buffer-name buffer)))
               (cop-bufs (gh-copilot-chat--get-buffers instance)))
          (when (and (not (string-prefix-p " " buffer-name))
                     (not (string-prefix-p "*" buffer-name)))
            (insert
             (propertize buffer-name
                         'face
                         (if (member buffer cop-bufs)
                             'gh-copilot-chat-list-selected-buffer-face
                           'gh-copilot-chat-list-default-face))
             "\n"))))
      (goto-char pt))))


(defun gh-copilot-chat-list-add-or-remove-buffer ()
  "Add or remove the buffer at point from the Copilot chat list."
  (interactive)
  (let* ((instance (gh-copilot-chat--current-instance))
         (buffer-name
          (buffer-substring (line-beginning-position) (line-end-position)))
         (buffer
          (if gh-copilot-chat-list-show-path
              (or (get-file-buffer
                   (if gh-copilot-chat-list-show-relative-path
                       (concat
                        (gh-copilot-chat-directory instance) "/" buffer-name)
                     buffer-name))
                  (get-buffer buffer-name))
            (get-buffer buffer-name)))
         (cop-bufs (gh-copilot-chat--get-buffers instance)))
    (when buffer
      (if (member buffer cop-bufs)
          (progn
            (gh-copilot-chat--del-buffer instance buffer)
            (message "Buffer '%s' removed from Copilot chat list." buffer-name))
        (gh-copilot-chat--add-buffer instance buffer)
        (message "Buffer '%s' added to Copilot chat list." buffer-name)))
    (gh-copilot-chat-list-refresh instance)))

(defun gh-copilot-chat-list-clear-buffers ()
  "Clear all buffers from the Copilot chat list."
  (interactive)
  (let ((instance (gh-copilot-chat--current-instance)))
    (gh-copilot-chat--clear-buffers instance)
    (message "Cleared all buffers from Copilot chat list.")
    (gh-copilot-chat-list-refresh instance)))

(defun gh-copilot-chat-prompt-split-and-list ()
  "Split prompt window and display buffer list."
  (interactive)
  (let ((split-window-preferred-function nil)
        (split-height-threshold nil)
        (split-width-threshold nil))
    (split-window-right (floor (* 0.8 (window-total-width)))))
  (other-window 1)
  (gh-copilot-chat-list))

(defun gh-copilot-chat-prompt-history-previous ()
  "Insert previous prompt in prompt buffer."
  (interactive)
  (let* ((instance (gh-copilot-chat--current-instance))
         (user-messages
          (seq-filter
           (lambda (msg)
             (equal (plist-get msg :role) "user"))
           (gh-copilot-chat-history instance)))
         (index (gh-copilot-chat-prompt-history-position instance))
         (prompt
          (when (gh-copilot-chat-history instance)
            (if (null index)
                (progn
                  (setf (gh-copilot-chat-prompt-history-position instance) 0)
                  (plist-get (car user-messages) :content))
              (if (= index (1- (length user-messages)))
                  (plist-get (car (last user-messages)) :content)
                (setf (gh-copilot-chat-prompt-history-position instance)
                      (1+ index))
                (plist-get (nth index user-messages) :content))))))
    (when prompt
      (gh-copilot-chat--insert-prompt instance prompt))))

(defun gh-copilot-chat-prompt-history-next ()
  "Insert next prompt in prompt buffer."
  (interactive)
  (let* ((instance (gh-copilot-chat--current-instance))
         (user-messages
          (seq-filter
           (lambda (msg)
             (equal (plist-get msg :role) "user"))
           (gh-copilot-chat-history instance)))
         (index (gh-copilot-chat-prompt-history-position instance))
         (prompt
          (when (and (gh-copilot-chat-history instance) index)
            (if (= 0 index)
                ""
              (setf
               index (1- index)
               (gh-copilot-chat-prompt-history-position instance) index)
              (plist-get (nth index user-messages) :content)))))
    (when prompt
      (gh-copilot-chat--insert-prompt instance prompt))))

(defun gh-copilot-chat-reset (&optional keep-buffers)
  "Reset copilot chat session.
When called interactively with prefix argument, preserve the buffer list.
Optional argument KEEP-BUFFERS if non-nil, preserve the current buffer list."
  (interactive "P")
  (let* ((instance (gh-copilot-chat--current-instance))
         (old-buffers
          (when keep-buffers
            (gh-copilot-chat-buffers instance)))
         (buf (gh-copilot-chat--get-buffer instance)))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (setf
     (gh-copilot-chat-history instance) nil
     (gh-copilot-chat-prompt-history-position instance) nil
     (gh-copilot-chat-yank-index instance) 1
     (gh-copilot-chat-last-yank-start instance) nil
     (gh-copilot-chat-last-yank-end instance) nil
     (gh-copilot-chat-spinner-timer instance) nil
     (gh-copilot-chat-spinner-index instance) 0
     (gh-copilot-chat-spinner-status instance) nil)
    (unless old-buffers
      (setf (gh-copilot-chat-buffers instance) nil))
    (gh-copilot-chat--display instance)
    (gh-copilot-chat-list-refresh instance)))

(defun gh-copilot-chat-frontend-clean ()
  "Cleaning function."
  (interactive)
  (let ((clean-fn
         (gh-copilot-chat-frontend-clean-fn (gh-copilot-chat--get-frontend))))
    (when clean-fn
      (funcall clean-fn))
    (setq gh-copilot-chat--frontend-init-p nil)))


(defun gh-copilot-chat-send-to-buffer ()
  "Send the code block at point to buffer.
Replace selection if any."
  (interactive)
  (let ((send-fn
         (gh-copilot-chat-frontend-send-to-buffer-fn
          (gh-copilot-chat--get-frontend))))
    (when send-fn
      (funcall send-fn))))

(defun gh-copilot-chat-copy-code-at-point ()
  "Copy the code block at point into kill ring."
  (interactive)
  (let ((copy-fn
         (gh-copilot-chat-frontend-copy-fn (gh-copilot-chat--get-frontend))))
    (when copy-fn
      (funcall copy-fn))))

(defun gh-copilot-chat-goto-input ()
  "Go to the input area."
  (interactive)
  (let ((instance (gh-copilot-chat--current-instance)))
    (when (equal (pm-base-buffer) (gh-copilot-chat--get-buffer instance))
      (let ((goto-fn
             (gh-copilot-chat-frontend-goto-input-fn
              (gh-copilot-chat--get-frontend))))
        (when goto-fn
          (funcall goto-fn))))))

(defun gh-copilot-chat--get-model-choices-with-wait ()
  "Get the list of available models for Copilot Chat.
waiting for fetch if needed.
If models haven't been fetched yet and no cache exists,
wait for the fetch to complete."
  (let ((models
         (seq-filter
          #'gh-copilot-chat--model-enabled-p
          (if gh-copilot-chat-model-ignore-picker
              (gh-copilot-chat-connection-models gh-copilot-chat--connection)
            (seq-filter
             #'gh-copilot-chat--model-picker-enabled
             (gh-copilot-chat-connection-models
              gh-copilot-chat--connection))))))
    (if models
        (let* ((model-info-list
                (mapcar
                 (lambda (model)
                   (let* ((id (alist-get 'id model))
                          (name (alist-get 'name model))
                          (clean-name
                           (replace-regexp-in-string " (Preview)$" "" name))
                          (vendor (alist-get 'vendor model))
                          (capabilities (alist-get 'capabilities model))
                          (limits (alist-get 'limits capabilities))
                          (preview-p (eq t (alist-get 'preview model)))
                          (preview-text
                           (if preview-p
                               " (Preview)"
                             ""))
                          (prompt-tokens (alist-get 'max_prompt_tokens limits))
                          (output-tokens
                           (or (alist-get 'max_output_tokens limits)
                               (when (string-prefix-p "o1" id)
                                 100000)))
                          (prompt-text
                           (if prompt-tokens
                               (format "%dk" (round (/ prompt-tokens 1000)))
                             "?"))
                          (output-text
                           (if output-tokens
                               (format "%dk" (round (/ output-tokens 1000)))
                             "?")))
                     (list
                      :id id
                      :vendor vendor
                      :clean-name clean-name
                      :preview-text preview-text
                      :prompt-text prompt-text
                      :output-text output-text)))
                 models))
               (max-vendor-width
                (apply #'max
                       (mapcar
                        (lambda (info)
                          (length (plist-get info :vendor)))
                        model-info-list)))
               (max-name-width
                (apply #'max
                       (mapcar
                        (lambda (info)
                          (length (plist-get info :clean-name)))
                        model-info-list)))
               (max-preview-width
                (apply #'max
                       (mapcar
                        (lambda (info)
                          (length (plist-get info :preview-text)))
                        model-info-list)))
               (max-prompt-width
                (apply #'max
                       (mapcar
                        (lambda (info)
                          (length (plist-get info :prompt-text)))
                        model-info-list)))
               (max-output-width
                (apply #'max
                       (mapcar
                        (lambda (info)
                          (length (plist-get info :output-text)))
                        model-info-list)))
               (format-str
                (format "[%%-%ds] %%-%ds%%-%ds (Tokens in/out: %%%ds/%%%ds)"
                        max-vendor-width
                        max-name-width
                        max-preview-width
                        max-prompt-width
                        max-output-width)))
          ;; Return list of (name . id) pairs from fetched models, sorted by ID
          (sort (mapcar
                 (lambda (info)
                   (cons
                    (format format-str
                            (plist-get info :vendor)
                            (plist-get info :clean-name)
                            (plist-get info :preview-text)
                            (plist-get info :prompt-text)
                            (plist-get info :output-text))
                    (plist-get info :id)))
                 model-info-list)
                (lambda (a b) (string< (cdr a) (cdr b)))))
      ;; No models available - fetch and wait
      (progn
        ;; Try loading from cache first
        (let ((cached-models (gh-copilot-chat--load-models-from-cache)))
          (if cached-models
              (progn
                (setf (gh-copilot-chat-connection-models
                       gh-copilot-chat--connection)
                      cached-models)
                (gh-copilot-chat--get-model-choices-with-wait))
            ;; No cache - need to fetch
            (message "No models available. Fetching from API...")
            (gh-copilot-chat--auth)
            (let ((inhibit-quit t)) ; Prevent C-g during fetch
              (gh-copilot-chat--request-models t)
              ;; Wait for models to be fetched (with timeout)
              (with-timeout (10 (error
                                 "Timeout waiting for models to be fetched"))
                (while (not
                        (gh-copilot-chat-connection-models
                         gh-copilot-chat--connection))
                  (sit-for 0.1)))
              (gh-copilot-chat--get-model-choices-with-wait))))))))

;;;###autoload (autoload 'gh-copilot-chat-set-model "gh-copilot-chat" nil t)
(defun gh-copilot-chat-set-model (model)
  "Set the Copilot Chat model to MODEL for the current instance.
Fetches available models from the API if not already fetched."
  (interactive
   (let*
       ((choices (gh-copilot-chat--get-model-choices-with-wait))
        (max-id-width
         (apply #'max (mapcar (lambda (choics) (length (cdr choics))) choices)))
        ;; Create completion list with ID as prefix for unique identification
        (completion-choices
         (mapcar
          (lambda (choice)
            (let ((name (car choice))
                  (id (cdr choice)))
              (cons (format (format "[%%-%ds] %%s" max-id-width) id name) id)))
          choices))
        (choice
         (completing-read
          "Select Copilot Chat model: " (mapcar 'car completion-choices)
          nil t)))
     ;; Extract model ID from the selected choice
     (let ((model-value (cdr (assoc choice completion-choices))))
       (when gh-copilot-chat-debug
         (message "Setting model to: %s" model-value))
       (list model-value))))

  ;; Set the model value only for current instance
  (let ((instance (gh-copilot-chat--current-instance)))
    (setf (gh-copilot-chat-model instance) model)
    (message "Copilot Chat model set to %s for current instance" model)))

(defun gh-copilot-chat-yank ()
  "Insert last code block given by `gh-copilot-chat'."
  (interactive)
  (let ((instance (gh-copilot-chat--current-instance)))
    (setf
     (gh-copilot-chat-yank-index instance) 1
     (gh-copilot-chat-last-yank-start instance) nil
     (gh-copilot-chat-last-yank-end instance) nil)
    (gh-copilot-chat--yank instance)))

(defun gh-copilot-chat-yank-pop (&optional inc)
  "Replace just-yanked code block with a different block.
INC is the number to use as increment for index in block ring."
  (interactive "*p")
  (let ((instance (gh-copilot-chat--current-instance)))
    (if (not (eq last-command 'gh-copilot-chat-yank-pop))
        (unless (eq last-command 'gh-copilot-chat-yank)
          (error "Previous command was not a yank")))
    (if inc
        (setf (gh-copilot-chat-yank-index instance)
              (+ (gh-copilot-chat-yank-index instance) inc))
      (setf (gh-copilot-chat-yank-index instance)
            (1+ (gh-copilot-chat-yank-index instance))))
    (gh-copilot-chat--yank instance)
    (setq this-command 'gh-copilot-chat-yank-pop)))

(defun gh-copilot-chat--yank (instance)
  "Insert at point the code block at the current index in the block ring.
Argument INSTANCE is the copilot chat instance to use."
  (when-let* ((yank-fn
               (gh-copilot-chat-frontend-yank-fn
                (gh-copilot-chat--get-frontend))))
    (funcall yank-fn instance)))

;;;###autoload (autoload 'gh-copilot-chat-clear-auth-cache "gh-copilot-chat" nil t)
(defun gh-copilot-chat-clear-auth-cache ()
  "Clear the auth cache for Copilot Chat."
  (interactive)
  (gh-copilot-chat-reset)
  ;; remove gh-copilot-chat-token-cache file
  (let ((token-cache-file (expand-file-name gh-copilot-chat-token-cache))
        (github-token-file
         (expand-file-name gh-copilot-chat-github-token-file)))
    (when (file-exists-p token-cache-file)
      (delete-file token-cache-file))
    (when (file-exists-p github-token-file)
      (delete-file github-token-file)))
  (message "Auth cache cleared.")
  (setq gh-copilot-chat--connection (gh-copilot-chat-connection--make)))

;;;###autoload (autoload 'gh-copilot-chat-reset-models "gh-copilot-chat" nil t)
(defun gh-copilot-chat-reset-models ()
  "Reset model cache and fetch models again.
This is useful when GitHub adds new models or updates model capabilities.
Clears model cache from memory and disk, then triggers background fetch."
  (interactive)
  ;; Clear models
  (setf (gh-copilot-chat-connection-models gh-copilot-chat--connection) nil)
  (setf (gh-copilot-chat-connection-last-models-fetch-time
         gh-copilot-chat--connection)
        0)

  ;; Remove cached models file if it exists
  (let ((models-cache-file
         (expand-file-name gh-copilot-chat-models-cache-file)))
    (when (file-exists-p models-cache-file)
      (delete-file models-cache-file)
      (when gh-copilot-chat-debug
        (message "Removed models cache file: %s" models-cache-file))))

  ;; Trigger a background fetch
  (message "Models cache cleared. Fetching updated models...")
  (gh-copilot-chat--fetch-models-async)

  ;; Return nil for programmatic usage
  nil)

(aio-defun
 gh-copilot-chat--add-workspace (instance only-open)
 "Add all files matching an instance to buffer list.
INSTANCE is the copilot chat instance to use.  If ONLY-OPEN is non-nil,
add only files already visited by a buffer."
 (gh-copilot-chat--clear-buffers instance)
 (let* ((default-directory (gh-copilot-chat-directory instance))
        (repo-root (aio-await (gh-copilot-chat--git-top-level)))
        (files
         (if (null repo-root)
             (directory-files-recursively default-directory ".*" nil)
           (aio-await (gh-copilot-chat--git-ls-files repo-root))))
        (file-count (length files)))
   (if (and (> file-count 50)
            (not only-open)
            (not
             (yes-or-no-p
              (format "Found %d files, add them all? " file-count))))
       (message "Workspace file addition cancelled.")
     (if only-open
         (mapcar
          (lambda (buffer)
            (let ((file (buffer-file-name buffer)))
              (when (and file (member (expand-file-name file) files))
                (gh-copilot-chat--add-buffer instance buffer))))
          (buffer-list))
       (mapcar (lambda (file) (gh-copilot-chat-add-file file)) files)))))

(defun gh-copilot-chat-add-workspace-buffers ()
  "Add all open files matching an instance.
If a gitignore file is present in the instance directory, it will be used to
filter files.  Buffer list is cleared and all buffer displaying a file in the
instance directory will be added."
  (interactive)
  (let ((instance (gh-copilot-chat--current-instance)))
    (aio-with-async
     (aio-await (gh-copilot-chat--add-workspace instance t))
     (gh-copilot-chat-list-refresh instance))))

(defun gh-copilot-chat-add-workspace ()
  "Add all files in instance's directory.
All files in instance's directory and its subdirectories are added to
context.  If a gitignore file is present in the instance directory, it
will be used to filter files.  Buffer list is cleared and all buffer
displaying a file in the instance directory will be added."
  (interactive)
  (let ((instance (gh-copilot-chat--current-instance)))
    (aio-with-async
     (aio-await (gh-copilot-chat--add-workspace instance nil))
     (gh-copilot-chat-list-refresh instance))))

;;;###autoload (autoload 'gh-copilot-chat-kill-instance "gh-copilot-chat" nil t)
(defun gh-copilot-chat-kill-instance ()
  "Interactively kill a selected copilot chat instance.
All its associated buffers are killed."
  (interactive)
  (let* ((instance (gh-copilot-chat--choose-instance)))
    (when instance
      (gh-copilot-chat--kill-instance instance))))

;;;###autoload (autoload 'gh-copilot-chat-set-commit-model "gh-copilot-chat" nil t)
(defun gh-copilot-chat-set-commit-model (model)
  "Set the model to use specifically for commit message generation to MODEL."
  (interactive (let* ((choices (gh-copilot-chat--get-model-choices-with-wait))
                      (max-id-width
                       (apply #'max
                              (mapcar
                               (lambda (choics)
                                 (length (cdr choics)))
                               choices)))
                      (completion-choices
                       (mapcar
                        (lambda (choice)
                          (let ((name (car choice))
                                (id (cdr choice)))
                            (cons
                             (format (format "[%%-%ds] %%s" max-id-width)
                                     id
                                     name)
                             id)))
                        choices))
                      (choice
                       (completing-read "Select commit message model: "
                                        (mapcar 'car completion-choices)
                                        nil
                                        t)))
                 (let ((model-value (cdr (assoc choice completion-choices))))
                   (when gh-copilot-chat-debug
                     (message "Setting commit model to: %s" model-value))
                   (list model-value))))

  (setq gh-copilot-chat-commit-model model)
  (when gh-copilot-chat--git-commit-instance
    (setf (gh-copilot-chat-model gh-copilot-chat--git-commit-instance)
          (or model gh-copilot-chat-default-model)))
  (message "Commit message model set to %s" model))

;;;###autoload (autoload 'gh-copilot-chat-save "gh-copilot-chat" nil t)
(defun gh-copilot-chat-save ()
  "Save an instance to a file."
  (interactive)
  (let ((instance (gh-copilot-chat--current-instance))
        (current-date (format-time-string "%Y_%m_%d_%H%M%S")))
    (when instance
      (let* ((default-path
              (or (gh-copilot-chat-file-path instance)
                  (format "%s/%s_%s.el"
                          gh-copilot-chat-default-save-dir
                          (replace-regexp-in-string
                           "/" "_" (gh-copilot-chat-directory instance))
                          current-date)))
             (default-dir (file-name-directory default-path))
             (default-file (file-name-nondirectory default-path))
             (file
              (read-file-name "Save instance to file: "
                              default-dir
                              nil
                              nil
                              default-file)))
        (gh-copilot-chat--save-instance instance file)
        (setf (gh-copilot-chat-file-path instance) file)
        (message "Saved instance to %s" file)))))

;;;###autoload (autoload 'gh-copilot-chat-load "gh-copilot-chat" nil t)
(defun gh-copilot-chat-load ()
  "Load an instance from a file."
  (interactive)
  (let ((file
         (read-file-name "File to load: "
                         gh-copilot-chat-default-save-dir
                         nil
                         t)))
    (gh-copilot-chat--load-instance file)
    (message "Loaded instance from %s" file)))

;;;###autoload (autoload 'gh-copilot-chat-quotas "gh-copilot-chat" nil t)
(defun gh-copilot-chat-quotas ()
  "Display the current Copilot Chat quotas."
  (interactive)
  (gh-copilot-chat--quotas))

;;;###autoload (autoload 'gh-copilot-chat-cancel "gh-copilot-chat" nil t)
(defun gh-copilot-chat-cancel ()
  "Cancel the current Copilot Chat request."
  (interactive)
  (let ((instance (gh-copilot-chat--current-instance)))
    (gh-copilot-chat--cancel instance)))

(provide 'gh-copilot-chat-command)
;;; gh-copilot-chat-command.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
