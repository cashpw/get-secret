;;; secret.el --- Simple secret management -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver
;; Maintainer: Cash Prokop-Weaver
;; Created: July 02, 2024
;; Modified: July 02, 2024
;; Version: 1.0.0
;; Homepage: https://github.com/cashweaver/secret
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Access secrets stored in files.
;;
;;  Example:
;;
;;    1. Save an API key in ~/foo/my-secret.txt
;;    2. (set-secret-dir "~/foo")
;;    3. (secret "my-secret.txt")
;;
;;; Code:

(defgroup secret nil
  "Simple secret management."
  :group 'emacs)

(defcustom secret--dir user-emacs-directory
  "Path to directory containing secret files."
  :group 'secret
  :type 'string)

(defun secret-get (name &optional type)
  "Get TYPE secret by NAME."
  (pcase (or type 'file)
    ('file
     (secret-get--file (format "%s/%s" secret--dir name)))
    (_
     (error "Unknown secret type: %s." type))))

(defun secret-get--file (path)
  "Get secret from file at PATH."
  (if (file-exists-p path)
      (string-clean-whitespace
       (with-temp-buffer
         (insert-file-contents path)
         (buffer-substring-no-properties (point-min) (point-max))))
    (error "Secret not found. No file found at: %s" path)))

(defun set-secret-dir (dir)
  "Set `secret--dir' to DIR."
  (setq secret--dir dir))

(provide 'secret)
;;; secret.el ends here
