;;; gh-copilot-chat --- gh-copilot-chat-body.el --- create request body for copilot -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'image)

(require 'gh-copilot-chat-frontend)
(require 'gh-copilot-chat-instance)
(require 'gh-copilot-chat-model)
(require 'gh-copilot-chat-prompts)
(require 'gh-copilot-chat-mcp)

(defcustom gh-copilot-chat-use-copilot-instruction-files t
  "Use custom instructions from `.github/copilot-instructions.md'."
  :type 'boolean
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-use-git-commit-instruction-files t
  "Use custom git commit instructions from `.github/git-commit-instructions.md'."
  :type 'boolean
  :group 'gh-copilot-chat)

(defcustom gh-copilot-chat-max-instruction-size 65536
  "Maximum size in bytes of instruction files."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'gh-copilot-chat)

(defun gh-copilot-chat--read-instruction-file (file-name)
  "Return the content of instruction file FILE-NAME or nil.
If the file is larger than `gh-copilot-chat-max-instruction-size',
ignore it and emit a message."
  (let* ((starting-path (or buffer-file-name default-directory))
         (github-dir (locate-dominating-file starting-path ".github"))
         (instruction-file
          (and github-dir
               (expand-file-name (concat ".github/" file-name) github-dir))))
    (when (and instruction-file (file-readable-p instruction-file))
      ;; Skip the file if it exceeds the configured size limit.
      (when (and gh-copilot-chat-max-instruction-size
                 (> (file-attribute-size (file-attributes instruction-file))
                    gh-copilot-chat-max-instruction-size))
        (message "[gh-copilot-chat] `%s` is larger than %d bytes; ignored."
                 instruction-file
                 gh-copilot-chat-max-instruction-size)
        (cl-return-from gh-copilot-chat--read-instruction-file nil))
      (with-temp-buffer
        (insert-file-contents instruction-file)
        (buffer-string)))))

(defun gh-copilot-chat--read-copilot-instructions-file ()
  "Return the content of `.github/copilot-instructions.md' or nil."
  (when gh-copilot-chat-use-copilot-instruction-files
    (gh-copilot-chat--read-instruction-file "copilot-instructions.md")))

(defun gh-copilot-chat--read-git-commit-instructions-file ()
  "Return the content of `.github/git-commit-instructions.md' or nil."
  (when gh-copilot-chat-use-git-commit-instruction-files
    (gh-copilot-chat--read-instruction-file "git-commit-instructions.md")))

(defun gh-copilot-chat--format-copilot-instructions (instruction-content)
  "Format instruction content according to Copilot's expected format.
INSTRUCTION-CONTENT is the content read from the instructions file."
  (when instruction-content
    (concat
     "When generating code, please follow these user provided coding instructions. "
     "You can ignore an instruction if it contradicts a system message.\n\n"
     "<instructions>\n"
     instruction-content
     "\n</instructions>")))

(defun gh-copilot-chat--format-buffer-for-copilot (buffer instance)
  "Format BUFFER content for Copilot with metadata to improve understanding.
INSTANCE is the `gh-copilot-chat' instance being used."
  (let ((format-buffer-fn
         (gh-copilot-chat-frontend-format-buffer-fn
          (gh-copilot-chat--get-frontend))))
    (if format-buffer-fn
        (funcall format-buffer-fn buffer instance)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun gh-copilot-chat--image-to-base64 (file)
  "Convert an image FILE to a base64 encoded string with MIME type."
  (let ((mime-type
         (or (mailcap-file-name-to-mime-type file) "application/octet-stream")))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (base64-encode-region (point-min) (point-max) t)
      (concat "data:" mime-type ";base64," (buffer-string)))))

(defun gh-copilot-chat--add-buffer-to-req (buffer instance messages)
  "Add BUFFER content to MESSAGES.
INSTANCE is the `gh-copilot-chat' instance being used."
  (when (buffer-live-p buffer)
    (let ((filename (buffer-file-name buffer)))
      (if (and filename
               (gh-copilot-chat--instance-support-vision instance)
               (image-supported-file-p filename))
          (progn
            (setf (gh-copilot-chat-uses-vision instance) t)
            (push (list
                   `(content
                     .
                     ,(vconcat
                       (list
                        (list
                         `(type . "text") `(text . ,(concat "FILE " filename)))
                        (list
                         `(type . "image_url")
                         `(image_url
                           .
                           ,(list
                             `(url
                               .
                               ,(gh-copilot-chat--image-to-base64
                                 filename))))))))
                   `(role . "user"))
                  messages))
        (push (list
               `(content
                 .
                 ,(gh-copilot-chat--format-buffer-for-copilot buffer instance))
               `(role . "user"))
              messages))))
  messages)

(provide 'gh-copilot-chat-body)
;;; gh-copilot-chat-body.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
