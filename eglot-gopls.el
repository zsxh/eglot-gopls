;;; eglot-gopls.el --- Go gopls integration with eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Zsxh Chen

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; Maintainer: Zsxh Chen <bnbvbchen@gmail.com>
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
;; - `go.test.cursor' / `go.benchmark.cursor' - Run individual tests or
;;   benchmarks.
;;
;; - `go.debug.cursor' - Debug tests and benchmarks using dape.
;;   Automatically registers the `go-debug-test' dape adapter for seamless
;;   debugging.
;;
;; - `gopls.run_govulncheck' / `gopls.vulncheck' - Run vulnerability
;;   checking on Go dependencies.
;;
;; Codelens Transformation:
;;
;; When [eglot-codelens](https://github.com/zsxh/eglot-codelens) is
;; available, this package transforms \"run test\" and \"run benchmark\"
;; code lenses into pairs:
;;
;; - `go.test.cursor' / `go.benchmark.cursor' - for running the test/benchmark
;; - `go.debug.cursor' - for debugging the test/benchmark with dape
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
;; To pass environment variables to the debugger when debugging tests:
;;
;;   (setq eglot-gopls-test-env-vars '(:KEY "value" :ANOTHER_KEY "another_value"))
;;

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'eglot)

(declare-function dape "dape" (config &optional skip-compile))

(defvar dape-configs)

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

(defvar-local eglot-gopls-test-env-vars nil
  "Buffer-local environment variables for debugging tests.

A plist of KEY VALUE pairs passed to the Go debugger when running
test or benchmark debug sessions via the `go.debug.cursor' codelens.")

(defvar eglot-gopls-compilation-error-regexp-alist '(go-test)
  "Alist that specifies how to match errors in Go test output.
See `compilation-error-regexp-alist' for more information.")

(defvar eglot-gopls-compilation-error-regexp-alist-alist
  '((go-test "[\t ]*\\([/][^ \t\n:]+\\):\\([0-9]+\\):" 1 2))
  "Alist of values for `eglot-gopls-compilation-error-regexp-alist'.")

(define-derived-mode eglot-gopls-compilation-mode compilation-mode
  "GoTest"
  "Major mode for Go test output.

Specialized compilation mode for parsing Go test with error regex patterns."
  (setq-local compilation-error-regexp-alist-alist
              eglot-gopls-compilation-error-regexp-alist-alist)
  (setq-local compilation-error-regexp-alist
              eglot-gopls-compilation-error-regexp-alist))

(defun eglot-gopls--vector-length> (vec len)
  "Return non-nil if VEC is a vector with length greater than LEN."
  (and vec (vectorp vec) (integerp len)
       (length> vec len)))

(defun eglot-gopls--vector-not-empty-p (vec)
  "Return non-nil if VEC is a non-empty vector."
  (eglot-gopls--vector-length> vec 0))

(defun eglot-gopls--vector-first-item (vec)
  "Return the first item of VEC, or nil if VEC is empty or not a vector."
  (eglot-gopls--vector-idx-item vec 0))

(defun eglot-gopls--vector-idx-item (vec idx)
  "Return the item at index IDX of VEC, or nil if IDX is out of bounds."
  (and (integerp idx) (>= idx 0)
       (eglot-gopls--vector-length> vec idx)
       (aref vec idx)))

(defun eglot-gopls--run-tests (arguments)
  "Execute gopls `gopls.run_tests' codelens action.

ARGUMENTS is a vector, [(:URI :Tests :Benchmarks) ...]."
  (when-let* ((args (eglot-gopls--vector-first-item arguments)))
    (pcase-let*
        (((map (:Tests tests) (:Benchmarks benchmarks)) args)
         (test-regexp (when (and tests
                                 (vectorp tests)
                                 (> (length tests) 0))
                        (mapconcat #'identity tests "|")))
         (bench-regexp (when (and benchmarks
                                  (vectorp benchmarks)
                                  (> (length benchmarks) 0))
                         (mapconcat #'identity benchmarks "|"))))
      (cond
       (test-regexp
        (let ((cmd (format
                    "go test -v -test.fullpath=true -timeout 30s -run %s ."
                    (shell-quote-argument (concat "^" test-regexp "$")))))
          (compile cmd 'eglot-gopls-compilation-mode)))
       (bench-regexp
        (let ((cmd
               (format
                "go test -v -test.fullpath=true -benchmem -run='^$' -bench %s ."
                (shell-quote-argument (concat "^" bench-regexp "$")))))
          (compile cmd 'eglot-gopls-compilation-mode)))
       (t (message "[eglot-gopls] No tests or benchmarks to run."))))))

(defun eglot-gopls--test (arguments)
  "Handle `go.test.cursor' command.

ARGUMENTS is a vector containing test information."
  (when-let* ((arg (eglot-gopls--vector-first-item arguments))
              (func-name (plist-get arg :functionName)))
    (compile
     (format "go test -v -test.fullpath=true -timeout 30s -run %s ."
             (shell-quote-argument (concat "^" func-name "$")))
     'eglot-gopls-compilation-mode)))

(defun eglot-gopls--benchmark (arguments)
  "Handle `go.benchmark.cursor' command.

ARGUMENTS is a vector containing benchmark information."
  (when-let* ((arg (eglot-gopls--vector-first-item arguments))
              (func-name (plist-get arg :functionName)))
    (compile
     (format
      "go test -v -test.fullpath=true -benchmem -run='^$' -bench %s ."
      (shell-quote-argument (concat "^" func-name "$")))
     'eglot-gopls-compilation-mode)))

(defun eglot-gopls--build-flags ()
  "Return build flags from gopls workspace configuration as a string."
  (if-let* ((gopls-config (plist-get eglot-workspace-configuration :gopls))
            (build-flags (plist-get gopls-config :buildFlags)))
      (mapconcat #'identity build-flags " ")
    ""))

(defun eglot-gopls--env ()
  "Return test environment variables as a plist, or empty plist if not set."
  (or (and (plistp eglot-gopls-test-env-vars) eglot-gopls-test-env-vars)
      eglot-{}))

(defun eglot-gopls--debug-args (func-name &optional benchmark-p)
  "Return debug arguments vector for FUNC-NAME.

If BENCHMARK-P is non-nil, return arguments for debugging a benchmark,
otherwise return arguments for debugging a test."
  (if benchmark-p
      (vector
       "-test.bench"
       (concat "^" func-name "$")
       "-test.run"
       "a^")
    (vector
     "-test.run"
     (concat "^" func-name "$"))))

(defun eglot-gopls--debug (arguments)
  "Execute gopls debug `go.debug.cursor' codelens action.

ARGUMENTS is a vector, [(:URI :Tests :Benchmarks) ...]."
  (unless (package-installed-p 'dape)
    (user-error "[eglot-gopls] dape.el not installed"))
  (require 'dape)
  (when-let* ((arg (eglot-gopls--vector-first-item arguments))
              (func-name (plist-get arg :functionName)))
    (when func-name
      (let* ((benchmark-p (string-prefix-p "Benchmark" func-name))
             (debug-args (eglot-gopls--debug-args func-name benchmark-p))
             (cmd-cwd default-directory)
             (dape-config `(:name "Debug Test"
                            modes (go-mode go-ts-mode)
                            ensure dape-ensure-command
                            command "dlv"
                            command-args ("dap" "--listen"
                                          "127.0.0.1::autoport")
                            command-cwd ,cmd-cwd
                            command-insert-stderr t
                            port :autoport
                            :type "go"
                            :request "launch"
                            :mode "test"
                            :program "."
                            :args ,debug-args
                            :buildFlags ,(eglot-gopls--build-flags)
                            :env ,(eglot-gopls--env))))
        (setf (alist-get 'go-debug-test dape-configs) dape-config)
        (dape dape-config)))))

(defun eglot-gopls--vulncheck (arguments)
  "Run govulncheck for the given file URI.

ARGUMENTS is a vector, [(:URI :Pattern) ...]."
  (when-let* ((args (eglot-gopls--vector-first-item arguments)))
    (pcase-let* (((map (:URI uri)
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
         'eglot-gopls-compilation-mode)))))

(defun eglot-gopls--create-test-codelens (lens)
  "Create test code lenses from LENS.

Returns a list of lenses: `go.test.cursor' with unchanged title
and `go.debug.cursor'.

LENS format: (:range RANGE
              :command (:title TITLE
                        :command CMD
                        :arguments [(:URI :Tests :Benchmarks) ...]))"
  (pcase-let*
      (((map :range :command) lens)
       ((map :title (:command _) (:arguments args)) command))
    (if-let* ((arg (eglot-gopls--vector-first-item args))
              (test-fns (plist-get arg :Tests))
              (test-fn (eglot-gopls--vector-first-item test-fns)))
        `((:range ,range
           :command (:title ,title
                     :command "go.test.cursor"
                     :arguments [(:functionName ,test-fn)]))
          (:range ,range
           :command (:title "debug test"
                     :command "go.debug.cursor"
                     :arguments [(:functionName ,test-fn)])))
      (list lens))))

(defun eglot-gopls--create-benchmark-codelens (lens)
  "Create benchmark code lenses from LENS.

Returns a list of lenses: `go.benchmark.cursor' with unchanged title
and `go.debug.cursor'.

LENS format: (:range RANGE
              :command (:title TITLE
                        :command CMD
                        :arguments [(:URI :Tests :Benchmarks) ...]))"
  (pcase-let*
      (((map :range :command) lens)
       ((map :title (:command _) (:arguments args)) command))
    (if-let* ((arg (eglot-gopls--vector-first-item args))
              (benchmark-fns (plist-get arg :Benchmarks))
              (benchmark-fn (eglot-gopls--vector-first-item benchmark-fns)))
        `((:range ,range
           :command (:title ,title
                     :command "go.benchmark.cursor"
                     :arguments [(:functionName ,benchmark-fn)]))
          (:range ,range
           :command (:title "debug benchmark"
                     :command "go.debug.cursor"
                     :arguments [(:functionName ,benchmark-fn)])))
      (list lens))))

(defun eglot-gopls--provide-codelens (codelens)
  "Transform gopls code lenses to add debug options.

CODELENS is a list of codelens maps from gopls.  Each lens has the format:
  (:range RANGE :command (:title TITLE :command CMD :arguments ARGS))

This function transforms \"run test\" and \"run benchmark\" lenses into
pairs of lenses with:
  - go.test.cursor/go.benchmark.cursor with unchanged title (for run)
  - go.debug.cursor with \"debug test\"/\"debug benchmark\" title

Returns the transformed list of codelens."
  (if (not (eglot-gopls--vector-not-empty-p codelens))
      codelens
    (let ((result '()))
      (dolist (lens (append codelens '()))
        (let* ((command (plist-get lens :command))
               (title (plist-get command :title)))
          (setq result
                (append result
                        (cond
                         ((string= title "run test")
                          (eglot-gopls--create-test-codelens lens))
                         ((string= title "run benchmark")
                          (eglot-gopls--create-benchmark-codelens lens))
                         (t (list lens)))))))
      (apply #'vector result))))

;; TODO: testify suite
(when (require 'eglot-codelens nil t)
  (cl-defmethod eglot-codelens-provide-codelens :around
    ((_server eglot-gopls-server) codelens)
    "Transform gopls code lenses to add debug options for tests and benchmarks."
    (eglot-gopls--provide-codelens codelens)))

(cl-defmethod eglot-execute :around ((_server eglot-gopls-server) action)
  "Handle gopls-specific code actions.

ACTION is a map containing :title, :command, and :arguments keys."
  (pcase-let* (((map (:title _) :command :arguments) action))
    (pcase command
      ("gopls.run_tests" (eglot-gopls--run-tests arguments))
      ("go.test.cursor" (eglot-gopls--test arguments))
      ("go.debug.cursor" (eglot-gopls--debug arguments))
      ("go.benchmark.cursor" (eglot-gopls--benchmark arguments))
      ((or "gopls.run_govulncheck"
           "gopls.vulncheck") (eglot-gopls--vulncheck arguments))
      (_ (cl-call-next-method)))))


(provide 'eglot-gopls)
;;; eglot-gopls.el ends here
