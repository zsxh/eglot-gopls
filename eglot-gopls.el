;;; eglot-gopls.el --- Go gopls integration with eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Zsxh Chen

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; Maintainer: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/eglot-gopls
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1") (compat "30.1.0.0") (eglot "1.17.30") (jsonrpc "1.0.24"))
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
;;   benchmarks.  Supports testify suite test methods and fuzz functions.
;;
;; - `go.test.package' / `go.test.file' - Run all tests in the current
;;   package or file.  Automatically enables code coverage for package runs.
;;
;; - `go.benchmark.package' / `go.benchmark.file' - Run all benchmarks in
;;   the current package or file.
;;
;; - `go.debug.cursor' - Debug tests and benchmarks using dape.
;;   Automatically registers the `go-debug-test' dape adapter for seamless
;;   debugging.  Supports test function debugging.
;;
;; - `gopls.run_govulncheck' / `gopls.vulncheck' - Run vulnerability
;;   checking on Go dependencies.
;;
;; Installation:
;;
;; Install [eglot-codelens](https://github.com/zsxh/eglot-codelens) first to
;; enable the codelens UI.
;;
;; Then install eglot-gopls:
;;
;;   (unless (package-installed-p 'eglot-gopls)
;;     (package-vc-install
;;      '(eglot-gopls :url "https://github.com/zsxh/eglot-gopls")))
;;
;; Or manually: download `eglot-gopls.el' and add it to your `load-path':
;;
;;   (add-to-list 'load-path "/path/to/eglot-gopls")
;;   (require 'eglot-gopls)
;;;;
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
;; To configure test coverage:
;;
;;   (setq eglot-gopls-test-cover-path "/path/to/coverage")
;;   (setq eglot-gopls-test-covermode 'atomic)
;;
;; To set test timeout:
;;
;;   (setq eglot-gopls-test-timeout "1m")
;;
;; To pass additional flags to `go test':
;;
;;   (setq eglot-gopls-test-flags '("-tags" "integration"))
;;

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'eglot)
(require 'jsonrpc)


(declare-function eglot--TextDocumentIdentifier "eglot")
(declare-function dape "dape" (config &optional skip-compile))

(defvar dape-configs)


(defgroup eglot-gopls nil
  "Settings for gopls integration with eglot."
  :group 'eglot
  :prefix "eglot-gopls-"
  :link '(url-link :tag "GitHub" "https://github.com/zsxh/eglot-gopls"))

;;;###autoload
(defclass eglot-gopls-server (eglot-lsp-server)
  ()
  :documentation "go langserver."
  :group 'eglot-gopls)


(defcustom eglot-gopls-vulncheck-db (getenv "GOVULNDB")
  "URL of the vulnerability database for govulncheck.

Used by the `gopls.run_govulncheck' and `gopls.vulncheck' commands.
If not set, defaults to the GOVULNDB environment variable.  The official
Go vulnerability database is https://vuln.go.dev."
  :type '(choice (const :tag "Use GOVULNDB env var" nil)
                 (string :tag "Custom URL"))
  :group 'eglot-gopls)

(defcustom eglot-gopls-test-cover-path (expand-file-name
                                        "eglot-gopls"
                                        (temporary-file-directory))
  "Directory path for storing Go test coverage profile files."
  :type 'string
  :group 'eglot-gopls)

(defcustom eglot-gopls-test-covermode nil
  "Coverage mode for Go tests.

Must be one of:
- set    :: whether each statement was run.
- count  :: how many times each statement was run.
- atomic :: atomic counts for mutual exclusion.

See `go help testflag` for details."
  :type '(choice (const :tag "None" nil)
                 (const :tag "set" set)
                 (const :tag "count" count)
                 (const :tag "atomic" atomic))
  :group 'eglot-gopls)

(defcustom eglot-gopls-test-env-vars nil
  "Environment variables for debugging tests.

A plist of KEY VALUE pairs passed to the Go debugger when running
test or benchmark debug sessions via the `go.debug.cursor' codelens.

Example: (:KEY \"value\" :ANOTHER_KEY \"another_value\" :NUM_VALUE 123)"
  :type '(plist :key-type symbol :value-type (choice string number))
  :group 'eglot-gopls)

(defcustom eglot-gopls-test-flags '()
  "Additional flags passed to `go test' and the debugger.

A list of string flags to pass when running tests or debugging.
If nil or empty, falls back to using `gopls.buildFlags' from the
gopls workspace configuration."
  :type '(repeat string)
  :group 'eglot-gopls)

(defcustom eglot-gopls-test-timeout "30s"
  "Timeout for Go tests.

A string specifying the timeout duration, such as \"30s\", \"1m\", etc.
Passed to `go test' via the -timeout flag.  See `go help testflag'
for details on the format."
  :type 'string
  :group 'eglot-gopls)


;; const
(defconst eglot-gopls-test-function-regex
  "^\\(?:Test\\|Example\\)\\(?:[^[:lower:]].*\\)?$")
(defconst eglot-gopls-testify-method-regex "^(\\*[^)]+)\\.Test[^[:lower:]].*$")
(defconst eglot-gopls-benchmark-regex "^Benchmark\\(?:[^[:lower:]].*\\)?$")
(defconst eglot-gopls-fuzz-function-regex "^Fuzz\\(?:[^[:lower:]].*\\)?$")
;; Matches run suites of the types:
;; type1: suite.Run(t, MySuite{     => MySuite
;; type1: suite.Run(t, &MySuite{    => MySuite
;; type2: suite.Run(t, new(MySuite) => MySuite
;;
;; ```elisp
;; (rx bol
;;     (* space)
;;     "suite.Run("
;;     (+ word)
;;     ","
;;     (* space)
;;     (or (seq (opt "&") (group-n 1 (+ word)) "{")
;;         (seq "new(" (group-n 2 (+ word)) ")")))
;; ```
(defconst eglot-gopls-suite-run-regex
  "^[[:space:]]*suite\\.Run([[:word:]]+,[[:space:]]*\\(?:&?\\(?1:[[:word:]]+\\){\\|new(\\(?2:[[:word:]]+\\))\\)")
(defconst eglot-gopls-test-method-group-regex
  "^(\\*\\([^)]+\\))\\.\\(Test[^[:lower:]].*\\)$")
(defconst eglot-gopls-symbol-kind-method 6
  "LSP SymbolKind value for method, as per the LSP specification.")
(defconst eglot-gopls-symbol-kind-function 12
  "LSP SymbolKind value for function, as per the LSP specification.")


;; go compilation mode
(defvar eglot-gopls-compilation-error-regexp-alist '(go-test)
  "Alist that specifies how to match errors in Go test output.
See `compilation-error-regexp-alist' for more information.")

(defvar eglot-gopls-compilation-error-regexp-alist-alist
  '((go-test "\\(/[^ \t\n:]+\\):\\([0-9]+\\)" 1 2))
  "Alist of values for `eglot-gopls-compilation-error-regexp-alist'.")

(define-derived-mode eglot-gopls-compilation-mode compilation-mode
  "GoTest"
  "Major mode for Go test output.

Specialized compilation mode for parsing Go test with error regex patterns."
  (setq-local compilation-error-regexp-alist-alist
              eglot-gopls-compilation-error-regexp-alist-alist)
  (setq-local compilation-error-regexp-alist
              eglot-gopls-compilation-error-regexp-alist))


(defun eglot-gopls--test-file-p ()
  "Return non-nil if the current buffer is a Go test file.

A Go test file is identified by having a filename ending in \"_test.go\"."
  (and buffer-file-name
       (let ((case-fold-search nil))
         (string-match-p "_test\\.go\\'" buffer-file-name))))

(defun eglot-gopls--workspace-conf ()
  "Return the gopls configuration from the workspace configuration.

Retrieves the gopls-specific settings from `eglot-workspace-configuration'.
The gopls configuration is nested under the :gopls key.

Returns a plist of gopls settings, or nil if not available."
  (when-let* ((server (eglot-current-server))
              (eglot-conf (if (functionp eglot-workspace-configuration)
                              (funcall eglot-workspace-configuration server)
                            eglot-workspace-configuration))
              (gopls-conf (plist-get eglot-conf :gopls)))
    gopls-conf))

(defun eglot-gopls--test-enable-p ()
  "Return non-nil if the test codelens is enabled in gopls configuration.

Checks the gopls workspace configuration for the
`gopls.codelenses.test' setting."
  (when-let* ((conf (eglot-gopls--workspace-conf))
              (codelenses (plist-get conf :codelenses))
              (test (plist-get codelenses :test)))
    (eq test t)))

(defun eglot-gopls--test-function-p (func-name)
  "Return non-nil if FUNC-NAME is a Go test function.

A Go test function name starts with \"Test\" or \"Example\" followed
by a capital letter."
  (let ((case-fold-search nil))
    (string-match-p eglot-gopls-test-function-regex func-name)))

(defun eglot-gopls--testify-method-p (func-name)
  "Return non-nil if FUNC-NAME is a testify suite test method.

A testify test method has the form \"(*SuiteType).TestXXX\"."
  (let ((case-fold-search nil))
    (string-match-p eglot-gopls-testify-method-regex func-name)))

(defun eglot-gopls--benchmark-function-p (func-name)
  "Return non-nil if FUNC-NAME is a Go benchmark function.

A Go benchmark function name starts with \"Benchmark\" followed
by a capital letter."
  (let ((case-fold-search nil))
    (string-match-p eglot-gopls-benchmark-regex func-name)))

(defun eglot-gopls--fuzz-function-p (func-name)
  "Return non-nil if FUNC-NAME is a Go fuzz function.

A Go fuzz function name starts with \"Fuzz\" followed
by a capital letter."
  (let ((case-fold-search nil))
    (string-match-p eglot-gopls-fuzz-function-regex func-name)))

(defun eglot-gopls--test-main-p (func-name)
  "Return non-nil if FUNC-NAME is \"TestMain\".

TestMain is a special test function that allows custom test setup."
  (string= "TestMain" func-name))

(defun eglot-gopls--import-testify-p ()
  "Return non-nil if the current file imports testify/suite.

Checks the package imports for the testify/suite package from
github.com/stretchr/testify."
  (when-let* ((imports (eglot-gopls--list-imports))
              (pkg-imports (plist-get imports :PackageImports)))
    (cl-find-if
     (lambda (e)
       (let ((path (plist-get e :Path)))
         (or (string= path "github.com/stretchr/testify/suite")
             (string= path "\"github.com/stretchr/testify/suite\""))))
     pkg-imports)))

(defun eglot-gopls--test-functions (doc-syms &optional doc-sym-p)
  "Extract test and fuzz functions from DOC-SYMS.

DOC-SYMS is a list of document symbol maps from the LSP.
If DOC-SYM-P is non-nil, return the full symbol maps.
Otherwise, return just the function names as strings."
  (when (length> doc-syms 0)
    (cl-loop for sym in doc-syms
             for name = (plist-get sym :name)
             for kind = (plist-get sym :kind)
             when (and (= kind eglot-gopls-symbol-kind-function)
                       (not (eglot-gopls--test-main-p name))
                       (or (eglot-gopls--test-function-p name)
                           (eglot-gopls--fuzz-function-p name)))
             collect (if doc-sym-p sym name))))

(defun eglot-gopls--testify-methods (doc-syms &optional doc-sym-p)
  "Extract testify test methods from DOC-SYMS.

Only returns methods if the current file imports testify/suite.
DOC-SYMS is a list of document symbol maps from the LSP.
If DOC-SYM-P is non-nil, return the full symbol maps.
Otherwise, return just the method names as strings."
  (when (and (length> doc-syms 0)
             (eglot-gopls--import-testify-p))
    (cl-loop for sym in doc-syms
             for name = (plist-get sym :name)
             for kind = (plist-get sym :kind)
             when (and (= kind eglot-gopls-symbol-kind-method)
                       (eglot-gopls--testify-method-p name))
             collect (if doc-sym-p sym name))))

(defun eglot-gopls--benchmark-functions (doc-syms &optional doc-sym-p)
  "Extract benchmark functions from DOC-SYMS.

DOC-SYMS is a list of document symbol maps from the LSP.
If DOC-SYM-P is non-nil, return the full symbol maps.
Otherwise, return just the function names as strings."
  (when (length> doc-syms 0)
    (cl-loop for sym in doc-syms
             for name = (plist-get sym :name)
             for kind = (plist-get sym :kind)
             when (and (= kind eglot-gopls-symbol-kind-function)
                       (eglot-gopls--benchmark-function-p name))
             collect (if doc-sym-p sym name))))

(defun eglot-gopls--test-flags ()
  "Return build flags as a list of strings.

First checks `eglot-gopls-test-flags', falling back to
`gopls.buildFlags' from the workspace configuration if nil or empty."
  (or eglot-gopls-test-flags
      (when-let* ((gopls-config (eglot-gopls--workspace-conf))
                  (build-flags (plist-get gopls-config :buildFlags)))
        (append build-flags '()))))

(defun eglot-gopls--split-flags-at-args (flags)
  "Split FLAGS at the \"-args\" marker into (flags . argsFlags).

The car contains flags before \"-args\" (build/test flags).
The cdr contains flags after \"-args\" (test arguments).

In Go test, \"-args\" is a special marker that separates build/test
flags from arguments passed to the test itself.

Returns a cons cell: (flags . argsFlags)"
  (if-let* ((args-idx (seq-position flags "-args" #'string=)))
      (cons (seq-subseq flags 0 args-idx)
            (seq-subseq flags (1+ args-idx)))
    (cons flags nil)))

(defun eglot-gopls--doc-symbols (&optional file)
  "Return document symbols for the current buffer or FILE.

If FILE is provided, returns symbols for that file.
Otherwise, returns symbols for the current buffer.

Returns a list of symbol maps from the LSP, or nil if no symbols found."
  (when-let* ((server (eglot-current-server))
              (text-doc (if file
                            `(:uri ,(eglot-path-to-uri
                                     (file-truename file) :truenamep t))
                          (eglot--TextDocumentIdentifier)))
              (res (jsonrpc-request server
                                    :textDocument/documentSymbol
                                    (list :textDocument text-doc))))
    (and (length> res 0)
         (append res '()))))

(defun eglot-gopls--list-imports ()
  "List imports in the current buffer using gopls.

Returns a plist with:
:Imports - vector of individual import maps with :Path and :Name keys
:PackageImports - vector of package import maps with :Path key

Returns nil if the request fails."
  (when-let* ((server (eglot-current-server))
              (res (jsonrpc-request
                    server
                    :workspace/executeCommand
                    `(:command "gopls.list_imports"
                      :arguments [,(eglot--TextDocumentIdentifier)]))))
    res))

(defvar eglot-gopls--suite-to-test-cache (make-hash-table :test #'equal)
  "Cache mapping package directories to suite-to-function mappings.

Key is the absolute path of the package directory.
Value is a plist with:
:sig - signature of the test files (list of (FILE SIZE MTIME))
:map - hash table mapping suite type names to test function names")

(defun eglot-gopls--package-test-signature (dir)
  "Return a list of signatures for all *_test.go files in DIR.

Each signature item is a list (FILE SIZE MTIME) where:
- FILE is the absolute path to the test file
- SIZE is the file size in bytes
- MTIME is the file modification time"
  (let* ((dir (file-truename dir))
         (files (directory-files dir t "^[^.].*_test\\.go$")))
    (mapcar
     (lambda (f)
       (let* ((attr (file-attributes f 'string))
              (size (file-attribute-size attr))
              (mtime (file-attribute-modification-time attr)))
         (list f size mtime)))
     files)))

(defun eglot-gopls--suite-to-test-map-internal (pkg-dir)
  "Build a map of testify suite types to their run functions.

PKG-DIR is the package directory to search for test files.
Searches all test files in the current package for suite.Run calls
and extracts the mapping from suite type names to the test function
names that run them.

Returns a hash table with suite type names as keys and test function
names as values."
  (let* ((go-test-files (directory-files pkg-dir t "^[^.].*_test\\.go$"))
         (suite-func-map (make-hash-table :test #'equal)))
    (dolist (file go-test-files)
      (let* ((doc-syms (eglot-gopls--doc-symbols file))
             (test-func-syms (eglot-gopls--test-functions doc-syms t)))
        (when (length> test-func-syms 0)
          (with-temp-buffer
            (insert-file-contents file)
            (dolist (test-func-sym test-func-syms)
              (pcase-let* (((map :range (:name func-name)) test-func-sym)
                           (`(,beg . ,end) (eglot-range-region range))
                           (func-text (buffer-substring-no-properties beg end)))
                (when (string-match eglot-gopls-suite-run-regex func-text)
                  (let* ((type1 (match-string 1 func-text))
                         (type2 (match-string 2 func-text))
                         (suite (or type1 type2)))
                    (when suite
                      (puthash suite func-name suite-func-map))))))))))
    suite-func-map))

(defun eglot-gopls--suite-to-test-map ()
  "Return a hash table mapping testify suite types to their test functions.

Returns a cached hash table where keys are suite type names
\(e.g., \\\"MySuite\\\") and values are the names of the test functions that
run them.

The cache is keyed by package directory and invalidated when test files
are modified (based on file size and modification time)."
  (let* ((pkg-dir (file-truename default-directory))
         (sig (eglot-gopls--package-test-signature pkg-dir))
         (cached (gethash pkg-dir eglot-gopls--suite-to-test-cache)))
    (if (and cached (equal sig (plist-get cached :sig)))
        (plist-get cached :map) ;; 命中缓存
      (let ((map (eglot-gopls--suite-to-test-map-internal pkg-dir)))
        (puthash pkg-dir (list :sig sig :map map)
                 eglot-gopls--suite-to-test-cache)
        map))))

(defun eglot-gopls--extract-testify-method (func-name)
  "Extract suite receiver and method name from a testify test method.

FUNC-NAME should be in the form \"(*SuiteType).TestXXX\".

Returns a cons cell (SUITE-RECEIVER . METHOD-NAME) or nil if not a
testify method."
  (let ((case-fold-search nil))
    (when (string-match eglot-gopls-test-method-group-regex func-name)
      (cons (match-string 1 func-name) ; test suite instant
            (match-string 2 func-name) ; method name
            ))))

(defun eglot-gopls--extract-testify-method-name (func-name)
  "Extract the method name from a testify test method name.

FUNC-NAME should be in the form \"(*SuiteType).TestXXX\".

Returns just the method name (e.g., \"TestXXX\") or nil if not a
testify method."
  (when-let* ((pair (eglot-gopls--extract-testify-method func-name)))
    (cdr pair)))

(defun eglot-gopls--test-suite-run-fn (test-suite suite-run-fn-map)
  "Look up the run function for a testify suite type.

TEST-SUITE is the suite type name (e.g., \"MySuite\").
SUITE-RUN-FN-MAP is a hash table from `eglot-gopls--suite-to-test-map'.

Returns the name of the test function that runs this suite, or nil
if not found."
  (when (and test-suite suite-run-fn-map (hash-table-p suite-run-fn-map))
    (gethash test-suite suite-run-fn-map)))

(defun eglot-gopls--build-run-regex (funcs &optional shell-p)
  "Return a regexp string for FUNCS.

FUNCS is a list of function/method name strings to run.
If SHELL-P is non-nil, wrap the regexp in single quotes for shell use.
Returns nil if FUNCS is nil or empty."
  (when funcs
    (let ((regexp (if (length= funcs 1)
                      (format "^%s$" (elt funcs 0))
                    (format "^(%s)$" (mapconcat #'identity funcs "|")))))
      (if shell-p
          (format "'%s'" regexp)
        regexp))))

(defun eglot-gopls--test-command (test-config)
  "Build the command list for running Go tests or benchmarks.

TEST-CONFIG is a map with:
- :test-fns - list of function names to run (nil for all)
- :benchmark-p - non-nil if running benchmarks
- :code-coverage-p - non-nil to enable coverage
- :flags - additional flags to pass to `go test'

Returns a list of command arguments suitable for `compile'."
  (pcase-let*
      (((map :test-fns :benchmark-p :code-coverage-p :flags) test-config)
       (cmd (list "go" "test" "-test.fullpath=true")))
    ;; NOTE: Always use (list ...) for `nconc' arguments, never literal '(...).
    ;; `nconc' modifies the list structure, and literals are compile-time
    ;; constants that get corrupted when modified across calls, causing
    ;; circular lists.
    (if benchmark-p
        (nconc cmd (list "-benchmem" "-run='^$'"))
      (nconc cmd (list "-timeout" eglot-gopls-test-timeout)))
    ;; coverage flags
    (when code-coverage-p
      (let ((cover-path (or (expand-file-name eglot-gopls-test-cover-path)
                            (expand-file-name "eglot-gopls"
                                              (temporary-file-directory))))
            (cover-mode eglot-gopls-test-covermode))
        (when cover-path
          (unless (file-exists-p cover-path)
            (mkdir cover-path t))
          (nconc cmd (list
                      (format "-coverprofile=%s/go-code-cover" cover-path))))
        (when cover-mode
          (nconc cmd (list
                      (concat "-covermode=" (if (stringp cover-mode)
                                                cover-mode
                                              (symbol-name cover-mode))))))))
    ;; test/benchmark flags
    (if benchmark-p
        (if test-fns
            ;; functions benchmark
            (nconc cmd (list "-bench"
                             (eglot-gopls--build-run-regex test-fns t)))
          ;; package benchmark
          (nconc cmd (list "-bench" ".")))
      ;; functions/testify methods test
      (let (test-functions testify-methods)
        (dolist (test-fn test-fns)
          (if-let* ((method (eglot-gopls--extract-testify-method-name test-fn)))
              (push method testify-methods)
            (push test-fn test-functions)))
        (when test-functions
          (nconc cmd (list "-run"
                           (eglot-gopls--build-run-regex
                            (nreverse test-functions) t))))
        (when testify-methods
          (nconc cmd (list "-testify.m"
                           (eglot-gopls--build-run-regex
                            (nreverse testify-methods) t))))))
    ;; user test flags (might contains -args ...)
    (when flags
      (nconc cmd flags))
    ;; current package
    (nconc cmd (list "."))
    cmd))

(defun eglot-gopls--debug-args (test-fn benchmark-p)
  "Build debugger arguments for a test or benchmark.

TEST-FN is the function name to debug.
BENCHMARK-P is non-nil if debugging a benchmark.

Returns a list of command arguments for the Go debugger."
  (if benchmark-p
      (list "-test.bench" (format "^%s$" test-fn) "-test.run" "a^")
    (if-let* ((pair (eglot-gopls--extract-testify-method test-fn)))
        (let* ((suite-to-test (eglot-gopls--suite-to-test-map))
               (suite (car pair))
               (test-method (cdr pair))
               (test-function (eglot-gopls--test-suite-run-fn
                               suite suite-to-test)))
          (unless (and test-function test-method)
            (user-error "[eglot-gopls] No matching testify suite runner for %s"
                        test-fn))
          (list "-test.run" (format "^%s$/^%s$" test-function test-method)))
      (list "-test.run" (format "^%s$" test-fn)))))

(defun eglot-gopls--debug (debug-config)
  "Start a dape debugging session for a test or benchmark.

DEBUG-CONFIG is a map with:
- :test-fn - the function name to debug
- :benchmark-p - non-nil if debugging a benchmark
- :flags - additional flags to pass

Requires dape.el to be installed."
  (unless (require 'dape nil 'noerror)
    (user-error "[eglot-gopls] dape.el not installed"))
  (pcase-let*
      (((map :test-fn :benchmark-p :flags) debug-config)
       (debug-args (eglot-gopls--debug-args test-fn benchmark-p))
       (`(,build-flags . ,args-flags) (eglot-gopls--split-flags-at-args flags))
       (args (append debug-args args-flags))
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
                      :args ,(apply #'vector args)
                      :buildFlags ,(mapconcat #'identity build-flags " ")
                      :env ,(or eglot-gopls-test-env-vars
                                (make-hash-table :size 0)))))
    (setf (alist-get 'go-debug-test dape-configs) dape-config)
    (dape dape-config)))

(defun eglot-gopls--test-at-cursor (cmd-type args)
  "Run or debug a test/benchmark at the cursor.

CMD-TYPE is one of:
- `debug' - start a dape debugging session
- `test' - run a test
- `benchmark' - run a benchmark

ARGS is a vector containing a map with :functionName key."
  (when-let* ((arg (and (length> args 0) (aref args 0)))
              (func-name (plist-get arg :functionName)))
    (cond
     ((eq cmd-type 'debug)
      (let* ((benchmark-p (eglot-gopls--benchmark-function-p func-name))
             (flags (eglot-gopls--test-flags))
             (debug-config (list :test-fn func-name
                                 :benchmark-p benchmark-p
                                 :flags flags)))
        (eglot-gopls--debug debug-config)))
     ((memq cmd-type '(test benchmark))
      (let* ((benchmark-p (eq cmd-type 'benchmark))
             (flags (eglot-gopls--test-flags))
             (test-fns (if benchmark-p
                           (list func-name)
                         (if-let*
                             ((pair (eglot-gopls--extract-testify-method
                                     func-name))
                              (suite (car pair))
                              (suite-to-test (eglot-gopls--suite-to-test-map))
                              (test-function (eglot-gopls--test-suite-run-fn
                                              suite suite-to-test)))
                             (list func-name test-function)
                           (list func-name))))
             (test-config (list :test-fns test-fns
                                :benchmark-p benchmark-p
                                :code-coverage-p nil
                                :flags flags))
             (cmd-args (eglot-gopls--test-command test-config))
             (cmd (mapconcat #'identity cmd-args " ")))
        (compile cmd 'eglot-gopls-compilation-mode)))
     (t nil))))

(defun eglot-gopls--test-current-package (&optional benchmark-p)
  "Run all tests or benchmarks in the current package.

BENCHMARK-P is non-nil to run benchmarks instead of tests.
Enables code coverage for the package run."
  (let* ((flags (eglot-gopls--test-flags))
         (test-config (list :test-fns nil
                            :benchmark-p benchmark-p
                            :code-coverage-p t
                            :flags flags))
         (cmd-args (eglot-gopls--test-command test-config))
         (cmd (mapconcat #'identity cmd-args " ")))
    (compile cmd 'eglot-gopls-compilation-mode)))

(defun eglot-gopls--test-current-file (&optional benchmark-p)
  "Run all tests or benchmarks in the current file.

BENCHMARK-P is non-nil to run benchmarks instead of tests.
Only runs functions found in the current buffer's document symbols."
  (let* ((flags (eglot-gopls--test-flags))
         (doc-syms (eglot-gopls--doc-symbols))
         (test-fns (if benchmark-p
                       (eglot-gopls--benchmark-functions doc-syms)
                     (append (eglot-gopls--test-functions doc-syms)
                             (eglot-gopls--testify-methods doc-syms))))
         (test-config (list :test-fns test-fns
                            :benchmark-p benchmark-p
                            :code-coverage-p nil
                            :flags flags))
         (cmd-args (eglot-gopls--test-command test-config))
         (cmd (mapconcat #'identity cmd-args " ")))
    (compile cmd 'eglot-gopls-compilation-mode)))

(defun eglot-gopls--vulncheck-db ()
  "Return the vulnerability database URL.

Checks the following sources in order:
1. `eglot-gopls-vulncheck-db'
2. The GOVULNDB key in `eglot-gopls-test-env-vars'

Returns the URL as a string, or nil if not set."
  (or eglot-gopls-vulncheck-db
      (plist-get eglot-gopls-test-env-vars :GOVULNDB)
      (getenv "GOVULNDB")))

(defun eglot-gopls--vulncheck (arguments)
  "Run govulncheck for the given file URI.

ARGUMENTS is a vector, [(:URI :Pattern) ...]."
  (unless (executable-find "govulncheck")
    (user-error "[eglot-gopls] govulncheck not found in PATH"))
  (when-let* ((args (and (length> arguments 0) (aref arguments 0))))
    (pcase-let* (((map (:URI uri)
                       (:Pattern _)) args)
                 (dir (file-name-directory (eglot-uri-to-path uri)))
                 (db (eglot-gopls--vulncheck-db))
                 (proj (project-current)))
      (when (and dir proj)
        (let ((default-directory (project-root proj)))
          (compile
           (format
            "govulncheck -json -mode source -scan symbol %s %s && govulncheck %s ./..."
            (concat "-C " (shell-quote-argument dir))
            (if db (concat " -db " (shell-quote-argument db)) "")
            (concat "-C " (shell-quote-argument dir)))
           'eglot-gopls-compilation-mode))))))

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
    (if-let* ((arg (and (length> args 0) (aref args 0)))
              (test-fns (plist-get arg :Tests))
              (test-fn (and (length> test-fns 0) (aref test-fns 0))))
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
    (if-let* ((arg (and (length> args 0) (aref args 0)))
              (benchmark-fns (plist-get arg :Benchmarks))
              (benchmark-fn (and (length> benchmark-fns 0)
                                 (aref benchmark-fns 0))))
        `((:range ,range
           :command (:title ,title
                     :command "go.benchmark.cursor"
                     :arguments [(:functionName ,benchmark-fn)]))
          (:range ,range
           :command (:title "debug benchmark"
                     :command "go.debug.cursor"
                     :arguments [(:functionName ,benchmark-fn)])))
      (list lens))))

(defun eglot-gopls--create-file-benchmarks-codelens (lens)
  "Create benchmark codelenses from LENS for file-level benchmark commands.

Transforms a \"run file benchmarks\" lens into two lenses:
- go.benchmark.package - runs all benchmarks in the package
- go.benchmark.file - runs benchmarks in the current file"
  (pcase-let*
      (((map :range :command) lens)
       ((map :title (:command _) (:arguments _)) command))
    `((:range ,range
       :command (:title "run package benchmarks"
                 :command "go.benchmark.package"))
      (:range ,range
       :command (:title ,title
                 :command "go.benchmark.file")))))

(defun eglot-gopls--transform-codelens (codelens)
  "Transform gopls code lenses to add debug options and granularity.

CODELENS is a list of codelens maps from gopls.  Each lens has the format:
  (:range RANGE :command (:title TITLE :command CMD :arguments ARGS))

This function transforms the following lenses:
- \"run test\" → pairs of go.test.cursor and go.debug.cursor
- \"run benchmark\" → pairs of go.benchmark.cursor and go.debug.cursor
- \"run file benchmarks\" → pairs of go.benchmark.package and go.benchmark.file

Returns the transformed list of codelens."
  (if (length> codelens 0)
      (let (result)
        (dolist (lens (append codelens '()))
          (let* ((command (plist-get lens :command))
                 (title (plist-get command :title)))
            (setq result
                  (append
                   result
                   (cond
                    ((string= title "run test")
                     (eglot-gopls--create-test-codelens lens))
                    ((string= title "run benchmark")
                     (eglot-gopls--create-benchmark-codelens lens))
                    ((string= title "run file benchmarks")
                     (eglot-gopls--create-file-benchmarks-codelens lens))
                    (t (list lens)))))))
        result)
    codelens))

(defun eglot-gopls--package-codelens ()
  "Create package-level test codelenses.

Returns two codelenses at the top of the buffer:
- go.test.package - runs all tests in the current package
- go.test.file - runs all tests in the current file"
  (let ((range '(:start (:line 0 :character 0)
                 :end (:line 0 :character 0))))
    `((:range ,range
       :command (:title "run package tests"
                 :command "go.test.package"))
      (:range ,range
       :command (:title "run file tests"
                 :command "go.test.file")))))

(defun eglot-gopls--func-codelens ()
  "Create codelenses for testify methods and fuzz functions.

Returns codelenses for:
- Testify test methods (when testify/suite is imported)
- Fuzz functions

Each function gets both a run and debug codelens."
  (let ((doc-syms (eglot-gopls--doc-symbols))
        (import-testify-p (eglot-gopls--import-testify-p)))
    (when (length> doc-syms 0)
      (cl-loop for sym in doc-syms
               for name = (plist-get sym :name)
               for kind = (plist-get sym :kind)
               for range = (plist-get sym :range)
               when (or (and import-testify-p
                             (= kind eglot-gopls-symbol-kind-method)
                             (eglot-gopls--testify-method-p name))
                        (and (= kind eglot-gopls-symbol-kind-function)
                             (eglot-gopls--fuzz-function-p name)))
               nconc `((:range ,range
                        :command (:title "run test"
                                  :command "go.test.cursor"
                                  :arguments [(:functionName ,name)]))
                       (:range ,range
                        :command (:title "debug test"
                                  :command "go.debug.cursor"
                                  :arguments [(:functionName ,name)])))))))

(defun eglot-gopls--provide-codelens (codelens)
  "Provide enhanced codelenses for Go test files.

If the current buffer is a test file and test codelenses are enabled,
augments CODELENS with:
- Package-level test/benchmark commands
- Transformed gopls codelenses with debug options
- Function-level codelenses for testify methods and fuzz functions

Otherwise returns CODELENS unchanged."
  (if (and (eglot-gopls--test-file-p)
           (eglot-gopls--test-enable-p))
      (let ((trans-lens (eglot-gopls--transform-codelens codelens))
            (pkg-lens (eglot-gopls--package-codelens))
            (func-lens (eglot-gopls--func-codelens)))
        (apply #'vector
               (append pkg-lens trans-lens func-lens)))
    codelens))

;; eglot-codelens is an optional dependency for enhanced code lens support
(when (require 'eglot-codelens nil 'noerror)
  (cl-defmethod eglot-codelens-provide-codelens :around
    ((_server eglot-gopls-server) codelens)
    "Provide enhanced codelenses for Go test files.

Delegates to `eglot-gopls--provide-codelens' which augments CODELENS with
package-level commands, transformed gopls codelenses with debug options,
and function-level codelenses for testify methods and fuzz functions."
    (eglot-gopls--provide-codelens codelens)))

(cl-defmethod eglot-execute :around ((_server eglot-gopls-server) action)
  "Handle gopls-specific code actions.

ACTION is a map containing :title, :command, and :arguments keys."
  (pcase-let* (((map (:title _) :command :arguments) action))
    (pcase command
      ;; cursor
      ("go.test.cursor" (eglot-gopls--test-at-cursor 'test arguments))
      ("go.debug.cursor" (eglot-gopls--test-at-cursor 'debug arguments))
      ("go.benchmark.cursor" (eglot-gopls--test-at-cursor 'benchmark arguments))
      ;; package
      ("go.test.package" (eglot-gopls--test-current-package))
      ("go.benchmark.package" (eglot-gopls--test-current-package t))
      ;; file
      ("go.test.file" (eglot-gopls--test-current-file))
      ("go.benchmark.file" (eglot-gopls--test-current-file t))
      ;; others
      ((or "gopls.run_govulncheck"
           "gopls.vulncheck") (eglot-gopls--vulncheck arguments))
      (_ (cl-call-next-method)))))


(provide 'eglot-gopls)
;;; eglot-gopls.el ends here
