;;; eglot-gopls.el --- Go gopls integration with eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  zsxh

;; Author: zsxh <bnbvbchen@gmail.com>
;; Maintainer: zsxh <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/eglot-gopls
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1") (compat "30.1.0.0") (eglot "1.17.30"))
;; Keywords: eglot tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides enhanced integration between gopls (the Go language
;; server) and Eglot (the Emacs LSP client).
;;
;; Features:
;;
;; This package enhances the following gopls codelens commands:
;;
;; - `gopls.run_tests' - Run tests and benchmarks in a compilation buffer
;;   with error navigation.  Output is parsed automatically, allowing
;;   `next-error' to jump to failures.
;;
;; - `gopls.run_govulncheck' / `gopls.vulncheck' - Run vulnerability
;;   checking on Go dependencies.
;;
;; Usage:
;;
;;   (require 'eglot-gopls)
;;   (push '((go-mode go-dot-mod-mode go-dot-work-mode
;;            go-ts-mode go-mod-ts-mode go-work-ts-mode)
;;           . (eglot-gopls-server . ("gopls")))
;;         eglot-server-programs)
;;
;; Then enable `eglot' in Go buffers with `M-x eglot'.
;;
;; Configuration:
;;
;; To use a custom vulnerability database, customize
;; `eglot-gopls-vulncheck-db':
;;
;;   (setq eglot-gopls-vulncheck-db "https://vuln.go.dev")
;;

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'eglot)


(defgroup eglot-gopls nil
  "Settings for gopls integration with eglot."
  :group 'eglot
  :prefix "eglot-gopls-"
  :link '(url-link :tag "GitHub" "https://github.com/zsxh/eglot-gopls"))

(defclass eglot-gopls-server (eglot-lsp-server)
  ()
  :documentation "go langserver."
  :group 'eglot-gopls)

(defcustom eglot-gopls-vulncheck-db (getenv "GOVULNDB")
  "Vulnerability database url."
  :type 'string
  :group 'eglot-gopls)

(defvar eglot-gopls-compilation-error-regexp-alist '(go-test)
  "Alist that specifies how to match errors in Go test output.
See `compilation-error-regexp-alist' for more information.")

(defvar eglot-gopls-compilation-error-regexp-alist-alist
  '((go-test "^[\t ]*\\([/][^ \t\n:]+\\):\\([0-9]+\\):" 1 2))
  "Alist of values for `eglot-gopls-compilation-error-regexp-alist'.")

(define-derived-mode eglot-gopls-compilation-mode compilation-mode
  "GoTest"
  "Major mode for Go test output.

Specialized compilation mode for parsing Go test with error regex patterns."
  (setq-local compilation-error-regexp-alist-alist
              eglot-gopls-compilation-error-regexp-alist-alist)
  (setq-local compilation-error-regexp-alist
              eglot-gopls-compilation-error-regexp-alist))


(defun eglot-gopls--exec-codelens (arguments)
  "Execute gopls test or benchmark codelens action.

ARGUMENTS is a vector, [(:URI :Tests :Benchmarks) ...]."
  (pcase-let* ((args (aref arguments 0))
               ((map (:Tests tests) (:Benchmarks benchmarks)) args)
               (test-regexp (when (and tests (vectorp tests) (> (length tests) 0))
                             (mapconcat #'identity tests "|")))
               (bench-regexp (when (and benchmarks (vectorp benchmarks) (> (length benchmarks) 0))
                              (mapconcat #'identity benchmarks "|"))))
    (cond
     (test-regexp
      (let ((cmd (format "go test -v -test.fullpath=true -timeout 30s -run %s ."
                         (shell-quote-argument (concat "^" test-regexp "$")))))
        (compile cmd 'eglot-gopls-compilation-mode)))
     (bench-regexp
      (let ((cmd
             (format
              "go test -v -test.fullpath=true -benchmem -run='^$' -bench %s ."
              (shell-quote-argument (concat "^" bench-regexp "$")))))
        (compile cmd 'eglot-gopls-compilation-mode)))
     (t (message "[eglot-gopls] No tests or benchmarks to run.")))))

(defun eglot-gopls--vulncheck (arguments)
  "Run govulncheck for the given file URI.

ARGUMENTS is a vector, [(:URI :Pattern) ...]."
  (pcase-let* ((args (aref arguments 0))
               ((map (:URI uri)
                     (:Pattern _)) args)
               (dir (file-name-directory (eglot-uri-to-path uri)))
               (db eglot-gopls-vulncheck-db)
               (proj (project-current))
               (default-directory (when proj (project-root proj))))
    (when (and dir proj)
      (compile
       (format
        "govulncheck -json -mode source -scan symbol %s %s && govulncheck %s ./..."
        (concat "-C " dir)
        (if db (concat " -db " (shell-quote-argument db)) "")
        (concat "-C " dir))
       'eglot-gopls-compilation-mode))))

(cl-defmethod eglot-execute :around ((_server eglot-gopls-server) action)
  "Handle gopls-specific code actions.

ACTION is a map containing :title, :command, and :arguments keys."
  (pcase-let* (((map (:title _) :command :arguments) action))
    (pcase command
      ("gopls.run_tests"
       (eglot-gopls--exec-codelens arguments))
      ((or "gopls.run_govulncheck" "gopls.vulncheck")
       (eglot-gopls--vulncheck arguments))
      (_ (cl-call-next-method)))))


(provide 'eglot-gopls)
;;; eglot-gopls.el ends here
