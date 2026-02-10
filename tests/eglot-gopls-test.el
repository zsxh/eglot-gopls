;;; eglot-gopls-test.el --- Tests for eglot-gopls -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Zsxh Chen

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; Maintainer: Zsxh Chen <bnbvbchen@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Tests for eglot-gopls package.

;;; Code:

(require 'ert)
(require 'eglot-gopls)


;;; eglot-gopls-compilation-mode tests

(ert-deftest eglot-gopls-compilation-mode-go-test-regex ()
  "Test the go-test regex pattern for matching Go test error output."
  ;; Test cases: (test-name input-line expected-match expected-file expected-line)
  (let* ((test-cases '(;; Standard Go test error format
                       ("standard error format"
                        "/path/to/file.go:42: error message"
                        t "/path/to/file.go" "42")
                       ;; Error with leading spaces
                       ("error with leading spaces"
                        "    /path/to/file.go:100: some error"
                        t "/path/to/file.go" "100")
                       ;; Error with leading tab
                       ("error with leading tab"
                        "\t/path/to/another/file.go:15: another error"
                        t "/path/to/another/file.go" "15")
                       ;; Multiple leading spaces
                       ("multiple leading spaces"
                        "        /absolute/path.go:999: test failed"
                        t "/absolute/path.go" "999")
                       ;; No file path (should not match)
                       ("no file path - should not match"
                        "error message without file path"
                        nil nil nil)
                       ;; File path without line number (should not match)
                       ("file without line number - should not match"
                        "/path/to/file.go: error message"
                        nil nil nil))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,input-line ,expected-match ,expected-file ,expected-line) case))
        (ert-info (test-name)
          (let ((pattern (cadr (assq 'go-test eglot-gopls-compilation-error-regexp-alist-alist))))
            (if expected-match
                (progn
                  (should (string-match pattern input-line))
                  (should (string= (match-string 1 input-line) expected-file))
                  (should (string= (match-string 2 input-line) expected-line)))
              (should-not (string-match pattern input-line)))))))))

;;; eglot-gopls--vector-length> tests

(ert-deftest eglot-gopls--vector-length> ()
  "Test boundary conditions and type checks for vector length comparison."
  ;; Test cases: (test-name vec len expected-result)
  (let* ((test-cases '(;; Empty vector with len=0 - should return nil (length not greater than 0)
                       ("empty vector with len=0"
                        [] 0 nil)
                       ;; Empty vector with len=-1 - should return non-nil (0 > -1)
                       ("empty vector with len=-1"
                        [] -1 t)
                       ;; Single element vector with len=0 - should return non-nil (1 > 0)
                       ("single element vector with len=0"
                        [1] 0 t)
                       ;; Single element vector with len=1 - should return nil (1 not greater than 1)
                       ("single element vector with len=1"
                        [1] 1 nil)
                       ;; Multi-element vector with len=2 - should return non-nil (3 > 2)
                       ("three elements vector with len=2"
                        [1 2 3] 2 t)
                       ;; Multi-element vector with len=3 - should return nil (3 not greater than 3)
                       ("three elements vector with len=3"
                        [1 2 3] 3 nil)
                       ;; nil as vec - should return nil
                       ("nil vec"
                        nil 0 nil)
                       ;; Non-vector as vec - should return nil
                       ("string vec"
                        "not a vector" 0 nil)
                       ;; List as vec - should return nil
                       ("list vec"
                        (1 2 3) 0 nil)
                       ;; Non-integer as len - should return nil
                       ("string len"
                        [1] "0" nil)
                       ;; Float as len - should return nil
                       ("float len"
                        [1] 1.5 nil))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,vec ,len ,expected-result) case))
        (ert-info (test-name)
          (let ((actual-result (eglot-gopls--vector-length> vec len)))
            (if expected-result
                (should actual-result)
              (should-not actual-result))))))))

;;; eglot-gopls--vector-not-empty-p tests

(ert-deftest eglot-gopls--vector-not-empty-p ()
  "Test boundary conditions and type checks for vector non-empty check."
  ;; Test cases: (test-name vec expected-result)
  (let* ((test-cases '(;; Empty vector - should return nil
                       ("empty vector"
                        [] nil)
                       ;; Single element vector - should return non-nil
                       ("single element vector"
                        [1] t)
                       ;; Multiple elements vector - should return non-nil
                       ("multiple elements vector"
                        [1 2 3] t)
                       ;; nil as vec - should return nil
                       ("nil vec"
                        nil nil)
                       ;; Non-vector as vec - should return nil
                       ("string vec"
                        "not a vector" nil)
                       ;; List as vec - should return nil
                       ("list vec"
                        (1 2 3) nil))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,vec ,expected-result) case))
        (ert-info (test-name)
          (let ((actual-result (eglot-gopls--vector-not-empty-p vec)))
            (if expected-result
                (should actual-result)
              (should-not actual-result))))))))

;;; eglot-gopls--vector-idx-item tests

(ert-deftest eglot-gopls--vector-idx-item ()
  "Test basic functionality, boundary conditions, and type checks for vector index access."
  ;; Test cases: (test-name vec idx expected-result)
  (let* ((test-cases '(;; Basic functionality - get element at valid index
                       ("normal index get element"
                        [1 2 3] 1 2)
                       ;; Index 0 - get first element
                       ("index 0 get first element"
                        [1 2 3] 0 1)
                       ;; Index = length-1 - get last element
                       ("index length-1 get last element"
                        [1 2 3] 2 3)
                       ;; Index out of bounds (equal to length) - should return nil
                       ("index out of bounds (equal to length)"
                        [1 2 3] 3 nil)
                       ;; Index out of bounds (greater than length) - should return nil
                       ("index out of bounds (greater than length)"
                        [1 2 3] 5 nil)
                       ;; Negative index - should return nil
                       ("negative index -1"
                        [1 2 3] -1 nil)
                       ;; More negative index - should return nil
                       ("negative index -10"
                        [1 2 3] -10 nil)
                       ;; Non-integer idx (string) - should return nil
                       ("non-integer idx (string)"
                        [1 2 3] "1" nil)
                       ;; Non-integer idx (float) - should return nil
                       ("non-integer idx (float)"
                        [1 2 3] 1.5 nil)
                       ;; nil as vec - should return nil
                       ("nil vec"
                        nil 0 nil)
                       ;; Empty vector - should return nil
                       ("empty vector"
                        [] 0 nil)
                       ;; Non-vector as vec (string) - should return nil
                       ("string vec"
                        "not a vector" 0 nil)
                       ;; Non-vector as vec (list) - should return nil
                       ("list vec"
                        (1 2 3) 0 nil))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,vec ,idx ,expected-result) case))
        (ert-info (test-name)
          (let ((actual-result (eglot-gopls--vector-idx-item vec idx)))
            (if expected-result
                (should (equal actual-result expected-result))
              (should-not actual-result))))))))

;;; eglot-gopls--vector-first-item tests

(ert-deftest eglot-gopls--vector-first-item ()
  "Test basic functionality, boundary conditions, and type checks for getting first item."
  ;; Test cases: (test-name vec expected-result)
  (let* ((test-cases '(;; Basic functionality - get first element from multi-element vector
                       ("multi-element vector get first element"
                        [1 2 3] 1)
                       ;; Single element vector - get that element
                       ("single element vector"
                        [42] 42)
                       ;; Empty vector - should return nil
                       ("empty vector"
                        [] nil)
                       ;; nil as vec - should return nil
                       ("nil vec"
                        nil nil)
                       ;; Non-vector types - should return nil
                       ("string vec"
                        "not a vector" nil)
                       ("list vec"
                        (1 2 3) nil)
                       ;; Vector with nil as first element
                       ("vector with nil as first element"
                        [nil 2 3] nil)
                       ;; Vector with string as first element
                       ("vector with string as first element"
                        ["first" "second"] "first"))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,vec ,expected-result) case))
        (ert-info (test-name)
          (let ((actual-result (eglot-gopls--vector-first-item vec)))
            (if expected-result
                (should (equal actual-result expected-result))
              (should-not actual-result))))))))

;;; eglot-gopls--run-tests tests

(ert-deftest eglot-gopls--run-tests ()
  "Test basic functionality and edge cases for running tests."
  ;; Test cases: (test-name arguments expected-compile-call expected-message)
  (let* ((test-cases
          '(;; Single test - should call compile with test command
            ("single test"
             [(:Tests ["TestFoo"])]
             "go test -v -test.fullpath=true -timeout 30s -run \\^TestFoo\\$ ."
             nil)
            ;; Multiple tests - should join with pipe
            ("multiple tests"
             [(:Tests ["TestFoo" "TestBar"])]
             "go test -v -test.fullpath=true -timeout 30s -run \\^TestFoo\\|TestBar\\$ ."
             nil)
            ;; Single benchmark - should call compile with bench command
            ("single benchmark"
             [(:Benchmarks ["BenchmarkFoo"])]
             "go test -v -test.fullpath=true -benchmem -run='^$' -bench \\^BenchmarkFoo\\$ ."
             nil)
            ;; Multiple benchmarks - should join with pipe
            ("multiple benchmarks"
             [(:Benchmarks ["BenchmarkFoo" "BenchmarkBar"])]
             "go test -v -test.fullpath=true -benchmem -run='^$' -bench \\^BenchmarkFoo\\|BenchmarkBar\\$ ."
             nil)
            ;; Both tests and benchmarks - tests should take priority
            ("both tests and benchmarks"
             [(:Tests ["TestFoo"] :Benchmarks ["BenchmarkFoo"])]
             "go test -v -test.fullpath=true -timeout 30s -run \\^TestFoo\\$ ."
             nil)
            ;; Empty tests vector - should output message
            ("empty tests vector"
             [(:Tests [])]
             nil
             "[eglot-gopls] No tests or benchmarks to run.")
            ;; Empty benchmarks vector - should output message
            ("empty benchmarks vector"
             [(:Benchmarks [])]
             nil
             "[eglot-gopls] No tests or benchmarks to run.")
            ;; No tests or benchmarks - should output message
            ("no tests or benchmarks"
             [(:URI "file:///path/to/file.go")]
             nil
             "[eglot-gopls] No tests or benchmarks to run.")
            ;; Empty vector - should not call compile or output message
            ("empty vector"
             []
             nil
             nil)
            ;; nil input - should not call compile or output message
            ("nil input"
             nil
             nil
             nil))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,arguments ,expected-compile-call ,expected-message) case))
        (ert-info (test-name)
          (let* ((actual-compile-call nil)
                 (actual-message nil))
            (cl-letf* (((symbol-function 'compile)
                        (lambda (command &optional comint)
                          (setq actual-compile-call command)))
                       ((symbol-function 'message)
                        (lambda (&rest args)
                          (setq actual-message (car args)))))
              (eglot-gopls--run-tests arguments)
              (if expected-compile-call
                  (should (string= actual-compile-call expected-compile-call))
                (should-not actual-compile-call))
              (if expected-message
                  (should (string= actual-message expected-message))
                (should-not actual-message)))))))))

;;; eglot-gopls--test tests

(ert-deftest eglot-gopls--test ()
  "Test basic functionality and edge cases for running single test."
  ;; Test cases: (test-name arguments expected-compile-call)
  (let* ((test-cases
          '(;; Single test function - should call compile with correct command
            ("single test function"
             [(:functionName "TestFoo")]
             "go test -v -test.fullpath=true -timeout 30s -run \\^TestFoo\\$ .")
            ;; Empty vector - should not call compile
            ("empty vector"
             []
             nil)
            ;; nil input - should not call compile
            ("nil input"
             nil
             nil)
            ;; Argument without functionName - should not call compile
            ("argument without functionName"
             [(:URI "file:///path/to/file.go")]
             nil)
            ;; functionName is empty string - should call compile with empty regex
            ("functionName is empty string"
             [(:functionName "")]
             "go test -v -test.fullpath=true -timeout 30s -run \\^\\$ .")
            ;; functionName with special regex characters - should be quoted
            ("functionName with special characters"
             [(:functionName "TestFoo.Bar")]
             "go test -v -test.fullpath=true -timeout 30s -run \\^TestFoo.Bar\\$ ."))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,arguments ,expected-compile-call) case))
        (ert-info (test-name)
          (let* ((actual-compile-call nil))
            (cl-letf* (((symbol-function 'compile)
                        (lambda (command &optional comint)
                          (setq actual-compile-call command))))
              (eglot-gopls--test arguments)
              (if expected-compile-call
                  (should (string= actual-compile-call expected-compile-call))
                (should-not actual-compile-call)))))))))

;;; eglot-gopls--benchmark tests

(ert-deftest eglot-gopls--benchmark ()
  "Test basic functionality and edge cases for running single benchmark."
  ;; Test cases: (test-name arguments expected-compile-call)
  (let* ((test-cases
          '(;; Single benchmark function - should call compile with correct command
            ("single benchmark function"
             [(:functionName "BenchmarkFoo")]
             "go test -v -test.fullpath=true -benchmem -run='^$' -bench \\^BenchmarkFoo\\$ .")
            ;; Empty vector - should not call compile
            ("empty vector"
             []
             nil)
            ;; nil input - should not call compile
            ("nil input"
             nil
             nil)
            ;; Argument without functionName - should not call compile
            ("argument without functionName"
             [(:URI "file:///path/to/file.go")]
             nil)
            ;; functionName is empty string - should call compile with empty regex
            ("functionName is empty string"
             [(:functionName "")]
             "go test -v -test.fullpath=true -benchmem -run='^$' -bench \\^\\$ .")
            ;; functionName with special regex characters - should be quoted
            ("functionName with special characters"
             [(:functionName "BenchmarkFoo.Bar")]
             "go test -v -test.fullpath=true -benchmem -run='^$' -bench \\^BenchmarkFoo.Bar\\$ ."))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,arguments ,expected-compile-call) case))
        (ert-info (test-name)
          (let* ((actual-compile-call nil))
            (cl-letf* (((symbol-function 'compile)
                        (lambda (command &optional comint)
                          (setq actual-compile-call command))))
              (eglot-gopls--benchmark arguments)
              (if expected-compile-call
                  (should (string= actual-compile-call expected-compile-call))
                (should-not actual-compile-call)))))))))

;;; eglot-gopls--debug-args tests

(ert-deftest eglot-gopls--debug-args ()
  "Test basic functionality and edge cases for debug arguments."
  ;; Test cases: (test-name func-name benchmark-p expected-result)
  (let* ((test-cases
          '(;; Normal test function - should return test.run vector
            ("normal test function"
             "TestFoo" nil
             ["-test.run" "^TestFoo$"])
            ;; Normal benchmark function - should return bench vector with a^ run pattern
            ("normal benchmark function"
             "BenchmarkFoo" t
             ["-test.bench" "^BenchmarkFoo$" "-test.run" "a^"])
            ;; Empty string func-name - should return empty regex
            ("empty string func-name"
             "" nil
             ["-test.run" "^$"])
            ;; Empty string func-name with benchmark-p
            ("empty string func-name with benchmark-p"
             "" t
             ["-test.bench" "^$" "-test.run" "a^"])
            ;; func-name with special characters - should be concatenated correctly
            ("func-name with special characters"
             "TestFoo.Bar/Baz" nil
             ["-test.run" "^TestFoo.Bar/Baz$"])
            ;; benchmark-name with special characters
            ("benchmark-name with special characters"
             "BenchmarkFoo.Bar" t
             ["-test.bench" "^BenchmarkFoo.Bar$" "-test.run" "a^"])
            ;; benchmark-p is non-nil non-t value - should treat as benchmark
            ("benchmark-p is non-nil non-t value"
             "TestFoo" "non-nil"
             ["-test.bench" "^TestFoo$" "-test.run" "a^"])
            ;; Omit benchmark-p - should default to test mode
            ("omit benchmark-p argument"
             "TestFoo" nil
             ["-test.run" "^TestFoo$"]))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,func-name ,benchmark-p ,expected-result) case))
        (ert-info (test-name)
          (let ((actual-result (eglot-gopls--debug-args func-name benchmark-p)))
            (should (equal actual-result expected-result))))))))

;;; eglot-gopls--vulncheck tests

(ert-deftest eglot-gopls--vulncheck ()
  "Test basic functionality and edge cases for running govulncheck."
  ;; Test cases: (test-name arguments vulncheck-db expected-compile-call)
  (let* ((test-cases
          '(;; Single argument with URI - should call compile with correct command
            ("single argument with URI"
             [(:URI "file:///path/to/file.go")]
             nil
             "govulncheck -json -mode source -scan symbol -C /path/to/  && govulncheck -C /path/to/ ./...")
            ;; Empty vector - should not call compile
            ("empty vector"
             []
             nil
             nil)
            ;; nil input - should not call compile
            ("nil input"
             nil
             nil
             nil)
            ;; Argument without URI - should not call compile
            ("argument without URI"
             [(:Pattern "some-pattern")]
             nil
             nil)
            ;; No project context - should not call compile
            ("no project context"
             [(:URI "file:///path/to/file.go")]
             nil
             nil)
            ;; With vulncheck-db - should include -db parameter
            ("with vulncheck-db"
             [(:URI "file:///path/to/file.go")]
             "/custom/vuln.db"
             "govulncheck -json -mode source -scan symbol -C /path/to/  -db /custom/vuln.db && govulncheck -C /path/to/ ./..."))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,arguments ,vulncheck-db ,expected-compile-call) case))
        (ert-info (test-name)
          (let* ((actual-compile-call nil)
                 (mock-project (unless (string= test-name "no project context")
                                 `(project-root . "/project/root"))))
            (cl-letf* (((symbol-function 'compile)
                        (lambda (command &optional comint)
                          (setq actual-compile-call command)))
                       ((symbol-function 'eglot-uri-to-path)
                        (lambda (uri)
                          (when uri
                            (concat (substring uri 8) ".go"))))
                       ((symbol-function 'file-name-directory)
                        (lambda (path)
                          (when path
                            "/path/to/")))
                       ((symbol-function 'project-current)
                        (lambda (&optional _maybe-prompt _dir)
                          (when mock-project 'mock-project)))
                       ((symbol-function 'project-root)
                        (lambda (proj)
                          "/project/root"))
                       (eglot-gopls-vulncheck-db vulncheck-db))
              (eglot-gopls--vulncheck arguments)
              (if expected-compile-call
                  (should (string= actual-compile-call expected-compile-call))
                (should-not actual-compile-call)))))))))

;;; eglot-gopls--create-test-codelens tests

(ert-deftest eglot-gopls--create-test-codelens ()
  "Test basic functionality and edge cases for creating test code lenses."
  ;; Test cases: (test-name lens expected-result)
  (let* ((test-cases
          '(;; Normal case - has Tests - should return two lenses (go.test.cursor and go.debug.cursor)
            ("normal case - has Tests"
             (:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
              :command (:title "run test"
                        :command "gopls.run_tests"
                        :arguments [(:URI "file:///path/to/file.go"
                                     :Tests ["TestFoo"])]))
             ((:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run test"
                         :command "go.test.cursor"
                         :arguments [(:functionName "TestFoo")]))
              (:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "debug test"
                         :command "go.debug.cursor"
                         :arguments [(:functionName "TestFoo")]))))
            ;; Empty arguments vector - should return original lens wrapped in list
            ("empty arguments vector"
             (:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
              :command (:title "run test"
                        :command "gopls.run_tests"
                        :arguments []))
             ((:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run test"
                         :command "gopls.run_tests"
                         :arguments []))))
            ;; nil arguments - should return original lens wrapped in list
            ("nil arguments"
             (:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
              :command (:title "run test"
                        :command "gopls.run_tests"
                        :arguments nil))
             ((:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run test"
                         :command "gopls.run_tests"
                         :arguments nil))))
            ;; First argument has no Tests - should return original lens wrapped in list
            ("first argument has no Tests"
             (:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
              :command (:title "run test"
                        :command "gopls.run_tests"
                        :arguments [(:URI "file:///path/to/file.go")]))
             ((:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run test"
                         :command "gopls.run_tests"
                         :arguments [(:URI "file:///path/to/file.go")])))
             ;; Empty Tests vector - should return original lens wrapped in list
             ("empty Tests vector"
              (:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run test"
                         :command "gopls.run_tests"
                         :arguments [(:URI "file:///path/to/file.go"
                                      :Tests [])]))
              ((:range (:start (:line 1 :character 1)
                        :end (:line 1 :character 1))
                :command (:title "run test"
                          :command "gopls.run_tests"
                          :arguments [(:URI "file:///path/to/file.go"
                                       :Tests [])]))))))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,lens ,expected-result) case))
        (ert-info (test-name)
          (let ((actual-result (eglot-gopls--create-test-codelens lens)))
            (should (equal actual-result expected-result))))))))

;;; eglot-gopls--create-benchmark-codelens tests

(ert-deftest eglot-gopls--create-benchmark-codelens ()
  "Test basic functionality and edge cases for creating benchmark code lenses."
  ;; Test cases: (test-name lens expected-result)
  (let* ((test-cases
          '(;; Normal case - has Benchmarks - should return two lenses (go.benchmark.cursor and go.debug.cursor)
            ("normal case - has Benchmarks"
             (:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
              :command (:title "run benchmark"
                        :command "gopls.run_tests"
                        :arguments [(:URI "file:///path/to/file.go"
                                     :Benchmarks ["BenchmarkFoo"])]))
             ((:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run benchmark"
                         :command "go.benchmark.cursor"
                         :arguments [(:functionName "BenchmarkFoo")]))
              (:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "debug benchmark"
                         :command "go.debug.cursor"
                         :arguments [(:functionName "BenchmarkFoo")]))))
            ;; Empty arguments vector - should return original lens wrapped in list
            ("empty arguments vector"
             (:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
              :command (:title "run benchmark"
                        :command "gopls.run_tests"
                        :arguments []))
             ((:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run benchmark"
                         :command "gopls.run_tests"
                         :arguments []))))
            ;; nil arguments - should return original lens wrapped in list
            ("nil arguments"
             (:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
              :command (:title "run benchmark"
                        :command "gopls.run_tests"
                        :arguments nil))
             ((:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run benchmark"
                         :command "gopls.run_tests"
                         :arguments nil))))
            ;; First argument has no Benchmarks - should return original lens wrapped in list
            ("first argument has no Benchmarks"
             (:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
              :command (:title "run benchmark"
                        :command "gopls.run_tests"
                        :arguments [(:URI "file:///path/to/file.go")]))
             ((:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run benchmark"
                         :command "gopls.run_tests"
                         :arguments [(:URI "file:///path/to/file.go")])))
             ;; Empty Benchmarks vector - should return original lens wrapped in list
             ("empty Benchmarks vector"
              (:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run benchmark"
                         :command "gopls.run_tests"
                         :arguments [(:URI "file:///path/to/file.go"
                                      :Benchmarks [])]))
              ((:range (:start (:line 1 :character 1)
                        :end (:line 1 :character 1))
                :command (:title "run benchmark"
                          :command "gopls.run_tests"
                          :arguments [(:URI "file:///path/to/file.go"
                                       :Benchmarks [])]))))))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,lens ,expected-result) case))
        (ert-info (test-name)
          (let ((actual-result (eglot-gopls--create-benchmark-codelens lens)))
            (should (equal actual-result expected-result))))))))

;;; eglot-gopls--provide-codelens tests

(ert-deftest eglot-gopls--provide-codelens ()
  "Test basic functionality and edge cases for providing code lenses."
  ;; Test cases: (test-name codelens expected-result)
  (let* ((test-cases
          '(;; Empty vector - should return empty vector
            ("empty vector"
             []
             [])
            ;; nil input - should return nil
            ("nil input"
             nil
             nil)
            ;; Single run test lens - should return 2 lenses (go.test.cursor and go.debug.cursor)
            ("single run test lens"
             [(:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
               :command (:title "run test"
                         :command "gopls.run_tests"
                         :arguments [(:URI "file:///path/to/file.go"
                                      :Tests ["TestFoo"])]))]
             [(:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run test"
                         :command "go.test.cursor"
                         :arguments [(:functionName "TestFoo")]))
              (:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "debug test"
                         :command "go.debug.cursor"
                         :arguments [(:functionName "TestFoo")]))])
            ;; Single run benchmark lens - should return 2 lenses (go.benchmark.cursor and go.debug.cursor)
            ("single run benchmark lens"
             [(:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
               :command (:title "run benchmark"
                         :command "gopls.run_tests"
                         :arguments [(:URI "file:///path/to/file.go"
                                      :Benchmarks ["BenchmarkFoo"])]))]
             [(:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run benchmark"
                         :command "go.benchmark.cursor"
                         :arguments [(:functionName "BenchmarkFoo")]))
              (:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "debug benchmark"
                         :command "go.debug.cursor"
                         :arguments [(:functionName "BenchmarkFoo")]))])
            ;; Mixed lenses - run test, run benchmark, and other title
            ("mixed lenses"
             [(:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
               :command (:title "run test"
                         :command "gopls.run_tests"
                         :arguments [(:URI "file:///path/to/file.go"
                                      :Tests ["TestFoo"])]))
              (:range (:start (:line 2 :character 1)
                      :end (:line 2 :character 1))
               :command (:title "run benchmark"
                         :command "gopls.run_tests"
                         :arguments [(:URI "file:///path/to/file.go"
                                      :Benchmarks ["BenchmarkBar"])]))
              (:range (:start (:line 3 :character 1)
                      :end (:line 3 :character 1))
               :command (:title "other action"
                         :command "gopls.other"
                         :arguments [(:URI "file:///path/to/file.go")]))]
             [(:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "run test"
                         :command "go.test.cursor"
                         :arguments [(:functionName "TestFoo")]))
              (:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:title "debug test"
                         :command "go.debug.cursor"
                         :arguments [(:functionName "TestFoo")]))
              (:range (:start (:line 2 :character 1)
                       :end (:line 2 :character 1))
               :command (:title "run benchmark"
                         :command "go.benchmark.cursor"
                         :arguments [(:functionName "BenchmarkBar")]))
              (:range (:start (:line 2 :character 1)
                       :end (:line 2 :character 1))
               :command (:title "debug benchmark"
                         :command "go.debug.cursor"
                         :arguments [(:functionName "BenchmarkBar")]))
              (:range (:start (:line 3 :character 1)
                       :end (:line 3 :character 1))
               :command (:title "other action"
                         :command "gopls.other"
                         :arguments [(:URI "file:///path/to/file.go")]))])
            ;; Lens without command - should keep as is
            ("lens without command"
             [(:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1)))]
             [(:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1)))])
            ;; Command without title - should keep as is
            ("command without title"
             [(:range (:start (:line 1 :character 1)
                      :end (:line 1 :character 1))
               :command (:command "gopls.run_tests"
                         :arguments [(:URI "file:///path/to/file.go")]))]
             [(:range (:start (:line 1 :character 1)
                       :end (:line 1 :character 1))
               :command (:command "gopls.run_tests"
                         :arguments [(:URI "file:///path/to/file.go")]))]))))
    (dolist (case test-cases)
      (pcase-let* ((`(,test-name ,codelens ,expected-result) case))
        (ert-info (test-name)
          (let ((actual-result (eglot-gopls--provide-codelens codelens)))
            (should (equal actual-result expected-result))))))))


(provide 'eglot-gopls-test)
;;; eglot-gopls-test.el ends here
