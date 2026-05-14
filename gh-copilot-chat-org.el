;;; gh-copilot-chat --- gh-copilot-chat-org.el --- copilot chat interface, org frontend -*- lexical-binding: t; -*-

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

(require 'org)
(require 'org-element)
(require 'polymode)

(require 'gh-copilot-chat-prompt-mode)
(require 'gh-copilot-chat-prompts)

;;; Constants
(defconst gh-copilot-chat--org-answer-tag "copilot"
  "The tag used to identify copilot chat answers.")
(defconst gh-copilot-chat--org-delimiter "* ╭──── Chat Input ────╮"
  "The delimiter used to identify copilot chat input.")

;;; Polymode
(define-derived-mode
 gh-copilot-chat-org-prompt-mode
 org-mode
 "Copilot Chat org Prompt"
 "Major mode for the Copilot Chat Prompt region."
 (setq
  major-mode 'gh-copilot-chat-org-prompt-mode
  mode-name "Copilot Chat org prompt")
 (gh-copilot-chat-prompt-mode))

(define-hostmode poly-copilot-org-hostmode :mode 'org-mode)

(define-innermode
 poly-copilot-org-prompt-innermode
 :mode 'gh-copilot-chat-org-prompt-mode
 :head-matcher (concat gh-copilot-chat--org-delimiter "\n")
 :tail-matcher "\\'"
 :head-mode 'host
 :tail-mode 'host)

(declare-function gh-copilot-chat-org-poly-mode "gh-copilot-chat-org"
                  "Polymode for Copilot Chat Org.")

(define-polymode
 gh-copilot-chat-org-poly-mode
 :hostmode 'poly-copilot-org-hostmode
 :innermodes '(poly-copilot-org-prompt-innermode))


;;; Functions
(defun gh-copilot-chat--org-format-data (instance content type)
  "Format data for org frontend.
INSTANCE is `gh-copilot-chat' instance to use.
Argument CONTENT is the data to format.
Argument TYPE is the type of the data (prompt or answer)."
  (let ((data ""))
    (if (eq type 'prompt)
        (progn
          (setf (gh-copilot-chat-first-word-answer instance) t)
          (setq data
                (concat
                 "\n* "
                 (format-time-string "*[%T]* You\n")
                 (format "%s\n" content))))
      (when (gh-copilot-chat-first-word-answer instance)
        (setf (gh-copilot-chat-first-word-answer instance) nil)
        (setq data
              (concat
               "\n** " (format-time-string "*[%T]* ")
               (format "Copilot(%s)                 :%s:\n"
                       (gh-copilot-chat-model instance)
                       gh-copilot-chat--org-answer-tag))))
      (setq data (concat data content)))
    data))

(defun gh-copilot-chat--org-format-code (code language)
  "Format code for org frontend.
Argument CODE is the code to format.
Argument LANGUAGE is the language of the code."
  (if language
      (format "\n#+BEGIN_SRC %s\n%s\n#+END_SRC\n" language code)
    code))

(defun gh-copilot-chat--org-format-buffer (buffer instance)
  "Format the content of a buffer into an org compatible string.
This function extracts the content of the specified BUFFER, determines
its file name, relative path, and programming language, and formats the
content as a org mode code block.
INSTANCE is `gh-copilot-chat' instance, used to retrieve relative file path."
  (with-current-buffer buffer
    (let* ((file-name (buffer-file-name))
           (relative-path
            (if file-name
                (file-relative-name file-name
                                    (gh-copilot-chat-directory instance))
              (buffer-name)))
           (language
            (if (derived-mode-p 'prog-mode)
                (replace-regexp-in-string
                 "\\(?:-ts\\)?-mode\\'" "" (symbol-name major-mode))
              "text"))
           (content
            (gh-copilot-chat--org-format-code
             (buffer-substring-no-properties (point-min) (point-max))
             language)))

      ;; Return the formatted string with metadata
      (format "* FILE %s\n%s" relative-path content))))

(defun gh-copilot-chat--org-create-req (prompt no-context)
  "Create a request with `org-mode' syntax reminder.
PROMPT is the input text.  If NO-CONTEXT is t, do nothing because we are
asking for a commit message."
  (if (or no-context (not (stringp prompt)))
      prompt
    (format
     "%s\n\nUse only Emacs org-mode formatting in your answers:
- Use ~ for inline code
- Use * for headers (starting at level 3 with ~***~)
- Use + for unordered lists
- Use 1. for ordered lists
- Use = or ~ for inline code
- Use #+BEGIN_QUOTE and #+END_QUOTE for quotes
- Use #+BEGIN_SRC and #+END_SRC for code blocks with language specification
- Use _ for underlining
- Use * for bold
- Use / for italics"
     prompt)))

(defun gh-copilot-chat--get-org-block-content-at-point ()
  "Get the content of the org block at point."
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))
    (when (memq type '(src-block quote-block example-block))
      (let ((content (org-element-property :value element)))
        content))))

(defun gh-copilot-chat--get-language-mode (element)
  "Get major mode name from org source block language.
When ELEMENT is a source block (`src-block`), extracts its language property."
  (when (eq (org-element-type element) 'src-block)
    (let ((language (org-element-property :language element)))
      (org-src-get-lang-mode language))))

(defun gh-copilot-chat--find-matching-buffer (mode)
  "Find most recent buffer with major-mode matching MODE."
  (seq-find
   (lambda (buf)
     (with-current-buffer buf
       (eq major-mode mode)))
   (buffer-list)))

(defun gh-copilot-chat--org-send-to-buffer ()
  "Send the code block at point to buffer.
Replace selection if any."
  (let* ((element (org-element-at-point))
         (mode (gh-copilot-chat--get-language-mode element))
         (matching-buffer
          (when mode
            (gh-copilot-chat--find-matching-buffer mode)))
         (default-buffer (or matching-buffer (current-buffer)))
         (buffer
          (completing-read
           "Choose buffer: "
           (mapcar #'buffer-name (buffer-list))
           nil ; PREDICATE
           t ; REQUIRE-MATCH
           nil ; INITIAL-INPUT
           'buffer-name-history
           (buffer-name default-buffer)))
         (content (gh-copilot-chat--get-org-block-content-at-point)))
    (when content
      (with-current-buffer buffer
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert content))
      (let ((window (get-buffer-window buffer)))
        (if window
            (select-window window)
          (switch-to-buffer buffer))))))

(defun gh-copilot-chat--org-copy ()
  "Copy the code block at point into kill ring."
  (let ((content (gh-copilot-chat--get-org-block-content-at-point)))
    (when content
      (kill-new content))))


(defun gh-copilot-chat--org-get-code-blocks-under-heading (heading-regex)
  "Get source blocks under headings matching HEADING-REGEX."
  (let ((blocks))
    (org-map-entries
     (lambda ()
       (let* ((heading-end (save-excursion (org-end-of-subtree t)))
              (element-start (point)))
         (setq blocks
               (append
                blocks
                (org-element-map
                 (org-element-parse-buffer 'element) 'src-block
                 (lambda (src-block)
                   (when (and (>= (org-element-property :begin src-block)
                                  element-start)
                              (<= (org-element-property :begin src-block)
                                  heading-end))
                     (list
                      :language (org-element-property :language src-block)
                      :content (org-element-property :value src-block)
                      :begin (org-element-property :begin src-block)
                      :end (org-element-property :end src-block)))))))))
     heading-regex)
    (seq-uniq blocks #'equal)))

(defun gh-copilot-chat--org-yank (instance)
  "Insert code block from Copilot Chat's org buffer at point.
INSTANCE is `gh-copilot-chat' instance to use."
  (let ((content ""))
    (with-current-buffer (gh-copilot-chat-chat-buffer instance)
      (let ((blocks
             (gh-copilot-chat--org-get-code-blocks-under-heading
              gh-copilot-chat--org-answer-tag)))
        (when blocks
          (while (< (gh-copilot-chat-yank-index instance) 1)
            (setf (gh-copilot-chat-yank-index instance)
                  (+ (length blocks) (gh-copilot-chat-yank-index instance))))
          (when (> (gh-copilot-chat-yank-index instance) (length blocks))
            (setf (gh-copilot-chat-yank-index instance)
                  (- (gh-copilot-chat-yank-index instance) (length blocks))))
          (setq content
                (plist-get
                 (car (last blocks (gh-copilot-chat-yank-index instance)))
                 :content)))))
    ;; Delete previous yank if exists
    (when (and (gh-copilot-chat-last-yank-start instance)
               (gh-copilot-chat-last-yank-end instance))
      (delete-region
       (gh-copilot-chat-last-yank-start instance)
       (gh-copilot-chat-last-yank-end instance)))
    ;; Insert new content
    (setf (gh-copilot-chat-last-yank-start instance) (point))
    (insert content)
    (setf (gh-copilot-chat-last-yank-end instance) (point))))

(defun gh-copilot-chat--org-write (data)
  "Write DATA at the end of the chat part of the buffer."
  (gh-copilot-chat--org-goto-input)
  (forward-line -3)
  (end-of-line)
  (insert data))


(defun gh-copilot-chat--org-goto-input ()
  "Go to the input part of the chat buffer.
The input is created if not found."
  (goto-char (point-max))
  (let ((span (pm-innermost-span (point))))
    (if (and span
             (not (eq (car span) nil))) ; nil span-type means host mode
        (goto-char (+ 1 (car (pm-innermost-range (point)))))
      (insert "\n\n")
      (let ((start (point))
            (inhibit-read-only t))
        (insert gh-copilot-chat--org-delimiter "\n\n")
        (add-text-properties
         start (point)
         '(read-only t front-sticky t rear-nonsticky (read-only)))))))

(defun gh-copilot-chat--org-get-buffer (instance)
  "Create `gh-copilot-chat' buffers for INSTANCE."
  (unless (buffer-live-p (gh-copilot-chat-chat-buffer instance))
    (setf (gh-copilot-chat-chat-buffer instance)
          (get-buffer-create
           (gh-copilot-chat--get-buffer-name
            (gh-copilot-chat-directory instance))))
    (with-current-buffer (gh-copilot-chat-chat-buffer instance)
      (gh-copilot-chat-org-poly-mode)
      (setq-local default-directory (gh-copilot-chat-directory instance))
      (gh-copilot-chat--org-goto-input)))
  (gh-copilot-chat-chat-buffer instance))

(defun gh-copilot-chat--org-insert-prompt (instance prompt)
  "Insert PROMPT in the chat buffer of INSTANCE."
  (with-current-buffer (gh-copilot-chat--org-get-buffer instance)
    (gh-copilot-chat--org-goto-input)
    (unless (eobp)
      (delete-region (point) (point-max)))
    (insert prompt)))

(defun gh-copilot-chat--org-pop-prompt (instance)
  "Get current prompt to send and clean it.
INSTANCE is `gh-copilot-chat' instance to use."
  (with-current-buffer (gh-copilot-chat--org-get-buffer instance)
    (gh-copilot-chat--org-goto-input)
    (let ((prompt (buffer-substring-no-properties (point) (point-max))))
      (delete-region (point) (point-max))
      prompt)))

(defun gh-copilot-chat--org-get-spinner-buffers (instance)
  "Get org spinner buffers for INSTANCE."
  (let* ((buffer (gh-copilot-chat--org-get-buffer instance))
         (prompt-buffer buffer))
    (with-current-buffer buffer
      (pm-map-over-spans
       (lambda (span)
         (let ((obj (nth 3 span)))
           (when (and (eq (car span) 'body)
                      (object-of-class-p obj 'pm-inner-chunkmode))
             (setq prompt-buffer (slot-value obj '-buffer)))))
       (point-min) (point-max)))
    (list buffer prompt-buffer)))

(defun gh-copilot-chat--org-init ()
  "Initialize the copilot chat org frontend."
  (setq gh-copilot-chat-prompt gh-copilot-chat-org-prompt))

;; Top-level execute code.

(cl-pushnew
 (make-gh-copilot-chat-frontend
  :id 'org
  :init-fn #'gh-copilot-chat--org-init
  :clean-fn nil
  :instance-init-fn nil
  :instance-clean-fn nil
  :save-fn nil
  :load-fn nil
  :format-fn #'gh-copilot-chat--org-format-data
  :format-code-fn #'gh-copilot-chat--org-format-code
  :format-buffer-fn #'gh-copilot-chat--org-format-buffer
  :create-req-fn #'gh-copilot-chat--org-create-req
  :send-to-buffer-fn #'gh-copilot-chat--org-send-to-buffer
  :copy-fn #'gh-copilot-chat--org-copy
  :yank-fn #'gh-copilot-chat--org-yank
  :write-fn #'gh-copilot-chat--org-write
  :get-buffer-fn #'gh-copilot-chat--org-get-buffer
  :insert-prompt-fn #'gh-copilot-chat--org-insert-prompt
  :pop-prompt-fn #'gh-copilot-chat--org-pop-prompt
  :goto-input-fn #'gh-copilot-chat--org-goto-input
  :get-spinner-buffers-fn #'gh-copilot-chat--org-get-spinner-buffers)
 gh-copilot-chat--frontend-list
 :test #'equal)

(provide 'gh-copilot-chat-org)
;;; gh-copilot-chat-org.el ends here


;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
