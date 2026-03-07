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

;; Declare dape-configs variable for testing
(defvar dape-configs nil)


;;; Compilation error regexp tests

(ert-deftest eglot-gopls-test-compilation-error-regexp ()
  "Test that the compilation error regexp correctly matches Go test output."
  (let* ((alist eglot-gopls-compilation-error-regexp-alist-alist)
         (entry (assq 'go-test alist))
         (regexp (nth 1 entry))
         (file-group (nth 2 entry))
         (line-group (nth 3 entry)))

    ;; Basic file:line format with trailing colon and space
    (ert-info ("Basic file:line format with trailing colon and space")
      (let ((input "/Users/zsxh/workspace/demo/demo/c_test.go:56: "))
        (should (string-match regexp input))
        (should (string= (match-string file-group input)
                         "/Users/zsxh/workspace/demo/demo/c_test.go"))
        (should (string= (match-string line-group input) "56"))))

    ;; Error Trace format with leading spaces
    (ert-info ("Error Trace format with leading spaces")
      (let ((input "Error Trace:    /Users/zsxh/workspace/demo/demo/c_test.go:56"))
        (should (string-match regexp input))
        (should (string= (match-string file-group input)
                         "/Users/zsxh/workspace/demo/demo/c_test.go"))
        (should (string= (match-string line-group input) "56"))))

    ;; Absolute path on Linux
    (ert-info ("Absolute path on Linux")
      (let ((input "/home/user/project/main_test.go:123"))
        (should (string-match regexp input))
        (should (string= (match-string file-group input)
                         "/home/user/project/main_test.go"))
        (should (string= (match-string line-group input) "123"))))

    ;; File name with special characters (underscore)
    (ert-info ("File name with special characters (underscore)")
      (let ((input "/path/to/my_test_file.go:7"))
        (should (string-match regexp input))
        (should (string= (match-string file-group input)
                         "/path/to/my_test_file.go"))
        (should (string= (match-string line-group input) "7"))))

    ;; Nested directory structure
    (ert-info ("Nested directory structure")
      (let ((input "/path/to/very/deeply/nested/project/pkg/subpkg/file_test.go:999"))
        (should (string-match regexp input))
        (should (string= (match-string file-group input)
                         "/path/to/very/deeply/nested/project/pkg/subpkg/file_test.go"))
        (should (string= (match-string line-group input) "999"))))))


;;; Test file detection tests

(ert-deftest eglot-gopls-test-test-file-p ()
  "Test that eglot-gopls--test-file-p correctly identifies Go test files."

  ;; File ending with _test.go should be recognized
  (ert-info ("File ending with _test.go should be recognized")
    (with-temp-buffer
      (setq buffer-file-name "/path/to/main_test.go")
      (should (eglot-gopls--test-file-p))))

  ;; File ending with _test.go in nested directory
  (ert-info ("File ending with _test.go in nested directory")
    (with-temp-buffer
      (setq buffer-file-name "/home/user/project/pkg/handler_test.go")
      (should (eglot-gopls--test-file-p))))

  ;; File ending with _test.go on Windows-style path
  (ert-info ("File ending with _test.go on Windows-style path")
    (with-temp-buffer
      (setq buffer-file-name "C:/Users/user/project/main_test.go")
      (should (eglot-gopls--test-file-p))))

  ;; Regular .go file (not a test file) should not be recognized
  (ert-info ("Regular .go file (not a test file) should not be recognized")
    (with-temp-buffer
      (setq buffer-file-name "/path/to/main.go")
      (should-not (eglot-gopls--test-file-p))))

  ;; File with 'test' in middle but not at end should not be recognized
  (ert-info ("File with 'test' in middle but not at end should not be recognized")
    (with-temp-buffer
      (setq buffer-file-name "/path/to/test_helper.go")
      (should-not (eglot-gopls--test-file-p))))

  ;; File with _test_ in middle should not be recognized
  (ert-info ("File with _test_ in middle should not be recognized")
    (with-temp-buffer
      (setq buffer-file-name "/path/to/my_test_helper.go")
      (should-not (eglot-gopls--test-file-p))))

  ;; Buffer without a file should return nil
  (ert-info ("Buffer without a file should return nil")
    (with-temp-buffer
      (setq buffer-file-name nil)
      (should-not (eglot-gopls--test-file-p))))

  ;; Empty filename should return nil
  (ert-info ("Empty filename should return nil")
    (with-temp-buffer
      (setq buffer-file-name "")
      (should-not (eglot-gopls--test-file-p))))

  ;; Case sensitivity - _TEST.go should NOT be recognized (Go is case-sensitive)
  (ert-info ("Case sensitivity - _TEST.go should NOT be recognized (Go is case-sensitive)")
    (with-temp-buffer
      (setq buffer-file-name "/path/to/main_TEST.go")
      (should-not (eglot-gopls--test-file-p))))

  ;; .go file with multiple underscores before _test
  (ert-info (".go file with multiple underscores before _test")
    (with-temp-buffer
      (setq buffer-file-name "/path/to/my_special_handler_test.go")
      (should (eglot-gopls--test-file-p)))))


;;; Workspace configuration tests

(ert-deftest eglot-gopls-test-workspace-conf ()
  "Test that eglot-gopls--workspace-conf correctly retrieves gopls configuration."

  ;; With full gopls configuration (plist)
  (ert-info ("With full gopls configuration (plist)")
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'mock-server))
              (eglot-workspace-configuration '(:gopls (:buildFlags ["-tags" "integration"]))))
      (should (eglot-gopls--workspace-conf))
      (let ((result (eglot-gopls--workspace-conf)))
        (should (plist-get result :buildFlags)))))

  ;; With gopls codelenses configuration
  (ert-info ("With gopls codelenses configuration")
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'mock-server))
              (eglot-workspace-configuration '(:gopls (:codelenses (:test t)))))
      (should (eglot-gopls--workspace-conf))
      (let ((result (eglot-gopls--workspace-conf)))
        (should (plist-get result :codelenses)))))

  ;; eglot-workspace-configuration as a function
  (ert-info ("eglot-workspace-configuration as a function")
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'mock-server))
              (eglot-workspace-configuration
               (lambda (_server) '(:gopls (:buildFlags ["-tags" "e2e"])))))
      (should (eglot-gopls--workspace-conf))
      (let ((result (eglot-gopls--workspace-conf)))
        (should (plist-get result :buildFlags)))))

  ;; No gopls key in workspace configuration
  (ert-info ("No gopls key in workspace configuration")
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'mock-server))
              (eglot-workspace-configuration '(:other-lsp (:setting "value"))))
      (should-not (eglot-gopls--workspace-conf))))

  ;; No eglot-current-server (returns nil)
  (ert-info ("No eglot-current-server (returns nil)")
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () nil))
              (eglot-workspace-configuration '(:gopls (:buildFlags ["-tags" "test"]))))
      (should-not (eglot-gopls--workspace-conf))))

  ;; Empty workspace configuration
  (ert-info ("Empty workspace configuration")
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'mock-server))
              (eglot-workspace-configuration nil))
      (should-not (eglot-gopls--workspace-conf))))

  ;; Workspace configuration function returns nil
  (ert-info ("Workspace configuration function returns nil")
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'mock-server))
              (eglot-workspace-configuration (lambda (_server) nil)))
      (should-not (eglot-gopls--workspace-conf))))

  ;; Nested gopls configuration with multiple settings
  (ert-info ("Nested gopls configuration with multiple settings")
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'mock-server))
              (eglot-workspace-configuration
               '(:gopls (:buildFlags ["-tags" "integration"]
                         :codelenses (:test t :gc_details t)))))
      (should (eglot-gopls--workspace-conf))
      (let ((result (eglot-gopls--workspace-conf)))
        (should (plist-get result :buildFlags))
        (should (plist-get result :codelenses))))))


;;; Test enable detection tests

(ert-deftest eglot-gopls-test-test-enable-p ()
  "Test that eglot-gopls--test-enable-p correctly detects test codelens state."

  ;; Test codelens explicitly enabled (test = t)
  (ert-info ("Test codelens explicitly enabled (test = t)")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:codelenses (:test t)))))
      (should (eglot-gopls--test-enable-p))))

  ;; Test codelens disabled (test = nil)
  (ert-info ("Test codelens disabled (test = nil)")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:codelenses (:test nil)))))
      (should-not (eglot-gopls--test-enable-p))))

  ;; Test codelens set to false (test = :false)
  (ert-info ("Test codelens set to false (test = :false)")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:codelenses (:test :false)))))
      (should-not (eglot-gopls--test-enable-p))))

  ;; No codelenses configuration
  (ert-info ("No codelenses configuration")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:buildFlags ["-tags" "integration"]))))
      (should-not (eglot-gopls--test-enable-p))))

  ;; No workspace configuration
  (ert-info ("No workspace configuration")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () nil)))
      (should-not (eglot-gopls--test-enable-p))))

  ;; Empty codelenses configuration
  (ert-info ("Empty codelenses configuration")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:codelenses))))
      (should-not (eglot-gopls--test-enable-p))))

  ;; Multiple codelenses with test enabled
  (ert-info ("Multiple codelenses with test enabled")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:codelenses (:test t :gc_details t :generate t)))))
      (should (eglot-gopls--test-enable-p))))

  ;; Multiple codelenses with test disabled
  (ert-info ("Multiple codelenses with test disabled")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:codelenses (:test nil :gc_details t :generate t)))))
      (should-not (eglot-gopls--test-enable-p))))

  ;; Test codelens with other configuration present
  (ert-info ("Test codelens with other configuration present")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:buildFlags ["-tags" "test"]
                            :codelenses (:test t)
                            :analyses (:unusedparams t)))))
      (should (eglot-gopls--test-enable-p)))))


;;; Test function detection tests

(ert-deftest eglot-gopls-test-test-function-p ()
  "Test that eglot-gopls--test-function-p correctly identifies Go test functions."

  ;; Standard Test function with capital letter
  (ert-info ("Standard Test function with capital letter")
    (should (eglot-gopls--test-function-p "TestMyFunction")))

  ;; Example function with capital letter
  (ert-info ("Example function with capital letter")
    (should (eglot-gopls--test-function-p "ExampleAdd")))

  ;; Test with single character after Test
  (ert-info ("Test with single character after Test")
    (should (eglot-gopls--test-function-p "TestA")))

  ;; Example with single character after Example
  (ert-info ("Example with single character after Example")
    (should (eglot-gopls--test-function-p "ExampleX")))

  ;; Test followed by digit (allowed in Go)
  (ert-info ("Test followed by digit (allowed in Go)")
    (should (eglot-gopls--test-function-p "Test1")))

  ;; Test followed by underscore (allowed)
  (ert-info ("Test followed by underscore (allowed)")
    (should (eglot-gopls--test-function-p "Test_MyFunc")))

  ;; Test alone (valid but edge case)
  (ert-info ("Test alone (valid but edge case)")
    (should (eglot-gopls--test-function-p "Test")))

  ;; Example alone (valid but edge case)
  (ert-info ("Example alone (valid but edge case)")
    (should (eglot-gopls--test-function-p "Example")))

  ;; TestFunction (standard Go naming)
  (ert-info ("TestFunction (standard Go naming)")
    (should (eglot-gopls--test-function-p "TestGetUser")))

  ;; ExampleComplexName
  (ert-info ("ExampleComplexName")
    (should (eglot-gopls--test-function-p "ExampleAPIHandler")))

  ;; Negative tests - should NOT be recognized

  ;; test with lowercase t
  (ert-info ("test with lowercase t")
    (should-not (eglot-gopls--test-function-p "testMyFunction")))

  ;; Test followed by lowercase letter (invalid Go test name)
  (ert-info ("Test followed by lowercase letter (invalid Go test name)")
    (should-not (eglot-gopls--test-function-p "TestmyFunction")))

  ;; example with lowercase e
  (ert-info ("example with lowercase e")
    (should-not (eglot-gopls--test-function-p "exampleAdd")))

  ;; Example followed by lowercase letter
  (ert-info ("Example followed by lowercase letter")
    (should-not (eglot-gopls--test-function-p "exampleAPI")))

  ;; Regular function not starting with Test or Example
  (ert-info ("Regular function not starting with Test or Example")
    (should-not (eglot-gopls--test-function-p "MyFunction")))

  ;; Benchmark function (different category)
  (ert-info ("Benchmark function (different category)")
    (should-not (eglot-gopls--test-function-p "BenchmarkMyFunc")))

  ;; Fuzz function (different category)
  (ert-info ("Fuzz function (different category)")
    (should-not (eglot-gopls--test-function-p "FuzzMyFuzz")))

  ;; Testify method format (different detection method)
  (ert-info ("Testify method format (different detection method)")
    (should-not (eglot-gopls--test-function-p "(*MySuite).TestMethod")))

  ;; Empty string
  (ert-info ("Empty string")
    (should-not (eglot-gopls--test-function-p ""))))


;;; Testify method detection tests

(ert-deftest eglot-gopls-test-testify-method-p ()
  "Test that eglot-gopls--testify-method-p correctly identifies testify suite test methods."

  ;; Positive tests - should be recognized

  ;; Standard testify method format
  (ert-info ("Standard testify method format")
    (should (eglot-gopls--testify-method-p "(*MySuite).TestMethod")))

  ;; Testify method with complex suite name
  (ert-info ("Testify method with complex suite name")
    (should (eglot-gopls--testify-method-p "(*UserHandlerSuite).TestCreateUser")))

  ;; Testify method with numbers
  (ert-info ("Testify method with numbers")
    (should (eglot-gopls--testify-method-p "(*Suite1).Test1")))

  ;; Testify method with underscores in suite name
  (ert-info ("Testify method with underscores in suite name")
    (should (eglot-gopls--testify-method-p "(*my_test_suite).TestFunc")))

  ;; Testify method with single character suite
  (ert-info ("Testify method with single character suite")
    (should (eglot-gopls--testify-method-p "(*S).TestA")))

  ;; Negative tests - should NOT be recognized

  ;; Missing asterisk before suite name
  (ert-info ("Missing asterisk before suite name")
    (should-not (eglot-gopls--testify-method-p "(MySuite).TestMethod")))

  ;; Missing closing parenthesis
  (ert-info ("Missing closing parenthesis")
    (should-not (eglot-gopls--testify-method-p "(*MySuite.TestMethod")))

  ;; Test with lowercase after Test (invalid Go test name)
  (ert-info ("Test with lowercase after Test (invalid Go test name)")
    (should-not (eglot-gopls--testify-method-p "(*MySuite).Testmethod")))

  ;; Non-test method
  (ert-info ("Non-test method")
    (should-not (eglot-gopls--testify-method-p "(*MySuite).SetupTest")))

  ;; TearDown method
  (ert-info ("TearDown method")
    (should-not (eglot-gopls--testify-method-p "(*MySuite).TearDownTest")))

  ;; Regular test function (not a testify method)
  (ert-info ("Regular test function (not a testify method)")
    (should-not (eglot-gopls--testify-method-p "TestMethod")))

  ;; Example method
  (ert-info ("Example method")
    (should-not (eglot-gopls--testify-method-p "(*MySuite).ExampleMethod")))

  ;; Benchmark method
  (ert-info ("Benchmark method")
    (should-not (eglot-gopls--testify-method-p "(*MySuite).BenchmarkMethod")))

  ;; Empty string
  (ert-info ("Empty string")
    (should-not (eglot-gopls--testify-method-p "")))

  ;; Method with lowercase test
  (ert-info ("Method with lowercase test")
    (should-not (eglot-gopls--testify-method-p "(*MySuite).testMethod")))

  ;; Missing dot before method
  (ert-info ("Missing dot before method")
    (should-not (eglot-gopls--testify-method-p "(*MySuite)TestMethod"))))


;;; Benchmark function detection tests

(ert-deftest eglot-gopls-test-benchmark-function-p ()
  "Test that eglot-gopls--benchmark-function-p correctly identifies Go benchmark functions."

  ;; Positive tests - should be recognized

  ;; Standard Benchmark function with capital letter
  (ert-info ("Standard Benchmark function with capital letter")
    (should (eglot-gopls--benchmark-function-p "BenchmarkMyFunction")))

  ;; Benchmark with single character after Benchmark
  (ert-info ("Benchmark with single character after Benchmark")
    (should (eglot-gopls--benchmark-function-p "BenchmarkA")))

  ;; Benchmark followed by digit (allowed in Go)
  (ert-info ("Benchmark followed by digit (allowed in Go)")
    (should (eglot-gopls--benchmark-function-p "Benchmark1")))

  ;; Benchmark followed by underscore (allowed)
  (ert-info ("Benchmark followed by underscore (allowed)")
    (should (eglot-gopls--benchmark-function-p "Benchmark_MyFunc")))

  ;; Benchmark alone (valid but edge case)
  (ert-info ("Benchmark alone (valid but edge case)")
    (should (eglot-gopls--benchmark-function-p "Benchmark")))

  ;; BenchmarkFunction (standard Go naming)
  (ert-info ("BenchmarkFunction (standard Go naming)")
    (should (eglot-gopls--benchmark-function-p "BenchmarkHash")))

  ;; BenchmarkComplexName
  (ert-info ("BenchmarkComplexName")
    (should (eglot-gopls--benchmark-function-p "BenchmarkAPIHandler")))

  ;; Negative tests - should NOT be recognized

  ;; benchmark with lowercase b
  (ert-info ("benchmark with lowercase b")
    (should-not (eglot-gopls--benchmark-function-p "benchmarkMyFunction")))

  ;; Benchmark followed by lowercase letter (invalid Go benchmark name)
  (ert-info ("Benchmark followed by lowercase letter (invalid Go benchmark name)")
    (should-not (eglot-gopls--benchmark-function-p "BenchmarkmyFunction")))

  ;; Regular function not starting with Benchmark
  (ert-info ("Regular function not starting with Benchmark")
    (should-not (eglot-gopls--benchmark-function-p "MyFunction")))

  ;; Test function (different category)
  (ert-info ("Test function (different category)")
    (should-not (eglot-gopls--benchmark-function-p "TestMyFunc")))

  ;; Fuzz function (different category)
  (ert-info ("Fuzz function (different category)")
    (should-not (eglot-gopls--benchmark-function-p "FuzzMyFuzz")))

  ;; Example function (different category)
  (ert-info ("Example function (different category)")
    (should-not (eglot-gopls--benchmark-function-p "ExampleMyFunc")))

  ;; Testify method format (different detection method)
  (ert-info ("Testify method format (different detection method)")
    (should-not (eglot-gopls--benchmark-function-p "(*MySuite).BenchmarkMethod")))

  ;; Empty string
  (ert-info ("Empty string")
    (should-not (eglot-gopls--benchmark-function-p ""))))


;;; Fuzz function detection tests

(ert-deftest eglot-gopls-test-fuzz-function-p ()
  "Test that eglot-gopls--fuzz-function-p correctly identifies Go fuzz functions."

  ;; Positive tests - should be recognized

  ;; Standard Fuzz function with capital letter
  (ert-info ("Standard Fuzz function with capital letter")
    (should (eglot-gopls--fuzz-function-p "FuzzMyFunction")))

  ;; Fuzz with single character after Fuzz
  (ert-info ("Fuzz with single character after Fuzz")
    (should (eglot-gopls--fuzz-function-p "FuzzA")))

  ;; Fuzz followed by digit (allowed in Go)
  (ert-info ("Fuzz followed by digit (allowed in Go)")
    (should (eglot-gopls--fuzz-function-p "Fuzz1")))

  ;; Fuzz followed by underscore (allowed)
  (ert-info ("Fuzz followed by underscore (allowed)")
    (should (eglot-gopls--fuzz-function-p "Fuzz_MyFunc")))

  ;; Fuzz alone (valid but edge case)
  (ert-info ("Fuzz alone (valid but edge case)")
    (should (eglot-gopls--fuzz-function-p "Fuzz")))

  ;; FuzzFunction (standard Go naming)
  (ert-info ("FuzzFunction (standard Go naming)")
    (should (eglot-gopls--fuzz-function-p "FuzzParse")))

  ;; FuzzComplexName
  (ert-info ("FuzzComplexName")
    (should (eglot-gopls--fuzz-function-p "FuzzJSONDecoder")))

  ;; Negative tests - should NOT be recognized

  ;; fuzz with lowercase f
  (ert-info ("fuzz with lowercase f")
    (should-not (eglot-gopls--fuzz-function-p "fuzzMyFunction")))

  ;; Fuzz followed by lowercase letter (invalid Go fuzz name)
  (ert-info ("Fuzz followed by lowercase letter (invalid Go fuzz name)")
    (should-not (eglot-gopls--fuzz-function-p "FuzzmyFunction")))

  ;; Regular function not starting with Fuzz
  (ert-info ("Regular function not starting with Fuzz")
    (should-not (eglot-gopls--fuzz-function-p "MyFunction")))

  ;; Test function (different category)
  (ert-info ("Test function (different category)")
    (should-not (eglot-gopls--fuzz-function-p "TestMyFunc")))

  ;; Benchmark function (different category)
  (ert-info ("Benchmark function (different category)")
    (should-not (eglot-gopls--fuzz-function-p "BenchmarkMyFunc")))

  ;; Example function (different category)
  (ert-info ("Example function (different category)")
    (should-not (eglot-gopls--fuzz-function-p "ExampleMyFunc")))

  ;; Testify method format (different detection method)
  (ert-info ("Testify method format (different detection method)")
    (should-not (eglot-gopls--fuzz-function-p "(*MySuite).FuzzMethod")))

  ;; Empty string
  (ert-info ("Empty string")
    (should-not (eglot-gopls--fuzz-function-p ""))))


;;; TestMain detection tests

(ert-deftest eglot-gopls-test-test-main-p ()
  "Test that eglot-gopls--test-main-p correctly identifies TestMain function."

  ;; Positive test - should be recognized

  ;; Exact TestMain match
  (ert-info ("Exact TestMain match")
    (should (eglot-gopls--test-main-p "TestMain")))

  ;; Negative tests - should NOT be recognized

  ;; TestMain with lowercase m
  (ert-info ("TestMain with lowercase m")
    (should-not (eglot-gopls--test-main-p "Testmain")))

  ;; testMain with lowercase t
  (ert-info ("testMain with lowercase t")
    (should-not (eglot-gopls--test-main-p "testMain")))

  ;; all lowercase
  (ert-info ("all lowercase")
    (should-not (eglot-gopls--test-main-p "testmain")))

  ;; TestMain with extra characters
  (ert-info ("TestMain with extra characters")
    (should-not (eglot-gopls--test-main-p "TestMainFunc")))

  ;; TestMain with underscore
  (ert-info ("TestMain with underscore")
    (should-not (eglot-gopls--test-main-p "Test_Main")))

  ;; Similar but not TestMain
  (ert-info ("Similar but not TestMain")
    (should-not (eglot-gopls--test-main-p "MyTestMain")))

  ;; Empty string
  (ert-info ("Empty string")
    (should-not (eglot-gopls--test-main-p "")))

  ;; Regular test function
  (ert-info ("Regular test function")
    (should-not (eglot-gopls--test-main-p "TestMyFunction")))

  ;; Testify method format
  (ert-info ("Testify method format")
    (should-not (eglot-gopls--test-main-p "(*MySuite).TestMain"))))


;;; Testify import detection tests

(ert-deftest eglot-gopls-test-import-testify-p ()
  "Test that eglot-gopls--import-testify-p correctly detects testify/suite imports."

  ;; Positive tests - should return non-nil

  ;; Import testify/suite without quotes
  (ert-info ("Import testify/suite without quotes")
    (cl-letf (((symbol-function 'eglot-gopls--list-imports)
               (lambda () '(:PackageImports [(:Path "github.com/stretchr/testify/suite")
                                             (:Path "fmt")]))))
      (should (eglot-gopls--import-testify-p))))

  ;; Import testify/suite with quotes
  (ert-info ("Import testify/suite with quotes")
    (cl-letf (((symbol-function 'eglot-gopls--list-imports)
               (lambda () '(:PackageImports [(:Path "\"github.com/stretchr/testify/suite\"")
                                             (:Path "os")]))))
      (should (eglot-gopls--import-testify-p))))

  ;; Multiple imports including testify/suite
  (ert-info ("Multiple imports including testify/suite")
    (cl-letf (((symbol-function 'eglot-gopls--list-imports)
               (lambda () '(:PackageImports [(:Path "fmt")
                                             (:Path "github.com/stretchr/testify/suite")
                                             (:Path "os")]))))
      (should (eglot-gopls--import-testify-p))))

  ;; Negative tests - should return nil

  ;; No imports
  (ert-info ("No imports")
    (cl-letf (((symbol-function 'eglot-gopls--list-imports)
               (lambda () '(:PackageImports []))))
      (should-not (eglot-gopls--import-testify-p))))

  ;; Import other testify packages but not suite
  (ert-info ("Import other testify packages but not suite")
    (cl-letf (((symbol-function 'eglot-gopls--list-imports)
               (lambda () '(:PackageImports [(:Path "github.com/stretchr/testify/assert")
                                             (:Path "github.com/stretchr/testify/mock")]))))
      (should-not (eglot-gopls--import-testify-p))))

  ;; Import standard library packages only
  (ert-info ("Import standard library packages only")
    (cl-letf (((symbol-function 'eglot-gopls--list-imports)
               (lambda () '(:PackageImports [(:Path "fmt")
                                             (:Path "os")
                                             (:Path "testing")]))))
      (should-not (eglot-gopls--import-testify-p))))

  ;; list-imports returns nil
  (ert-info ("list-imports returns nil")
    (cl-letf (((symbol-function 'eglot-gopls--list-imports)
               (lambda () nil)))
      (should-not (eglot-gopls--import-testify-p))))

  ;; PackageImports key missing
  (ert-info ("PackageImports key missing")
    (cl-letf (((symbol-function 'eglot-gopls--list-imports)
               (lambda () '(:Imports [(:Path "fmt")]))))
      (should-not (eglot-gopls--import-testify-p))))

  ;; Similar but not exact testify/suite path
  (ert-info ("Similar but not exact testify/suite path")
    (cl-letf (((symbol-function 'eglot-gopls--list-imports)
               (lambda () '(:PackageImports [(:Path "github.com/testify/suite")]))))
      (should-not (eglot-gopls--import-testify-p))))

  ;; testify/suite with extra path components
  (ert-info ("testify/suite with extra path components")
    (cl-letf (((symbol-function 'eglot-gopls--list-imports)
               (lambda () '(:PackageImports [(:Path "github.com/stretchr/testify/suite/extra")]))))
      (should-not (eglot-gopls--import-testify-p)))))


;;; Test functions extraction tests

(ert-deftest eglot-gopls-test-test-functions ()
  "Test that eglot-gopls--test-functions correctly extracts test and fuzz functions."

  ;; Extract Test functions
  (ert-info ("Extract Test functions")
    (let* ((doc-syms (list (list :name "TestMyFunc" :kind 12)
                           (list :name "TestAnother" :kind 12)))
           (test-result (eglot-gopls--test-functions doc-syms)))
      (should (and (member "TestMyFunc" test-result)
                   (member "TestAnother" test-result)))))

  ;; Extract Fuzz functions
  (ert-info ("Extract Fuzz functions")
    (let* ((doc-syms (list (list :name "FuzzMyFunc" :kind 12)
                           (list :name "FuzzAnother" :kind 12)))
           (test-result (eglot-gopls--test-functions doc-syms)))
      (should (and (member "FuzzMyFunc" test-result)
                   (member "FuzzAnother" test-result)))))

  ;; Exclude TestMain (special function)
  (ert-info ("Exclude TestMain (special function)")
    (let* ((doc-syms (list (list :name "TestMyFunc" :kind 12)
                           (list :name "TestMain" :kind 12)))
           (test-result (eglot-gopls--test-functions doc-syms)))
      (should (and (member "TestMyFunc" test-result)
                   (not (member "TestMain" test-result))))))

  ;; Empty doc-syms returns nil
  (ert-info ("Empty doc-syms returns nil")
    (should-not (eglot-gopls--test-functions (list))))

  ;; No test functions returns nil
  (ert-info ("No test functions returns nil")
    (let* ((doc-syms (list (list :name "RegularFunc" :kind 12)))
           (test-result (eglot-gopls--test-functions doc-syms)))
      (should-not test-result))))

;;; Testify methods extraction tests

(ert-deftest eglot-gopls-test-testify-methods ()
  "Test that eglot-gopls--testify-methods correctly extracts testify test methods."

  ;; Extract testify methods when testify is imported
  (ert-info ("Extract testify methods when testify is imported")
    (cl-letf (((symbol-function 'eglot-gopls--import-testify-p)
               (lambda () t)))
      (let* ((doc-syms (list (list :name "(*MySuite).TestMethod" :kind 6)
                             (list :name "(*AnotherSuite).TestAnother" :kind 6)))
             (result (eglot-gopls--testify-methods doc-syms)))
        (should (and (member "(*MySuite).TestMethod" result)
                     (member "(*AnotherSuite).TestAnother" result))))))

  ;; Return nil when testify is not imported
  (ert-info ("Return nil when testify is not imported")
    (cl-letf (((symbol-function 'eglot-gopls--import-testify-p)
               (lambda () nil)))
      (let* ((doc-syms (list (list :name "(*MySuite).TestMethod" :kind 6)))
             (result (eglot-gopls--testify-methods doc-syms)))
        (should-not result))))

  ;; Return nil for empty doc-syms
  (ert-info ("Return nil for empty doc-syms")
    (cl-letf (((symbol-function 'eglot-gopls--import-testify-p)
               (lambda () t)))
      (should-not (eglot-gopls--testify-methods (list)))))

  ;; Return full symbol maps when doc-sym-p is t
  (ert-info ("Return full symbol maps when doc-sym-p is t")
    (cl-letf (((symbol-function 'eglot-gopls--import-testify-p)
               (lambda () t)))
      (let* ((doc-syms (list (list :name "(*MySuite).TestMethod"
                                   :kind 6
                                   :range (list :start (list :line 1 :character 0)
                                                :end (list :line 2 :character 0)))))
             (result (eglot-gopls--testify-methods doc-syms t)))
        (should result)
        (should (listp (car result)))
        (should (plist-get (car result) :name))
        (should (plist-get (car result) :range)))))

  ;; Filter out non-method symbols (functions)
  (ert-info ("Filter out non-method symbols (functions)")
    (cl-letf (((symbol-function 'eglot-gopls--import-testify-p)
               (lambda () t)))
      (let* ((doc-syms (list (list :name "(*MySuite).TestMethod" :kind 6)
                             (list :name "TestFunction" :kind 12)))
             (result (eglot-gopls--testify-methods doc-syms)))
        (should (equal result '("(*MySuite).TestMethod"))))))

  ;; Filter out methods that don't match testify pattern
  (ert-info ("Filter out methods that don't match testify pattern")
    (cl-letf (((symbol-function 'eglot-gopls--import-testify-p)
               (lambda () t)))
      (let* ((doc-syms (list (list :name "(*MySuite).TestMethod" :kind 6)
                             (list :name "(*MySuite).SetupTest" :kind 6)
                             (list :name "(*MySuite).TearDownTest" :kind 6)))
             (result (eglot-gopls--testify-methods doc-syms)))
        (should (equal result '("(*MySuite).TestMethod"))))))

  ;; Multiple testify methods
  (ert-info ("Multiple testify methods")
    (cl-letf (((symbol-function 'eglot-gopls--import-testify-p)
               (lambda () t)))
      (let* ((doc-syms (list (list :name "(*MySuite).TestFirst" :kind 6)
                             (list :name "(*MySuite).TestSecond" :kind 6)
                             (list :name "(*MySuite).TestThird" :kind 6)))
             (result (eglot-gopls--testify-methods doc-syms)))
        (should (= (length result) 3))
        (should (member "(*MySuite).TestFirst" result))
        (should (member "(*MySuite).TestSecond" result))
        (should (member "(*MySuite).TestThird" result)))))

  ;; Mixed testify methods and other symbols
  (ert-info ("Mixed testify methods and other symbols")
    (cl-letf (((symbol-function 'eglot-gopls--import-testify-p)
               (lambda () t)))
      (let* ((doc-syms (list (list :name "TestFunction" :kind 12)
                             (list :name "(*MySuite).TestMethod" :kind 6)
                             (list :name "RegularMethod" :kind 6)
                             (list :name "(*OtherSuite).TestAnother" :kind 6)))
             (result (eglot-gopls--testify-methods doc-syms)))
        (should (= (length result) 2))
        (should (member "(*MySuite).TestMethod" result))
        (should (member "(*OtherSuite).TestAnother" result))))))


;;; Benchmark functions extraction tests

(ert-deftest eglot-gopls-test-benchmark-functions ()
  "Test that eglot-gopls--benchmark-functions correctly extracts benchmark functions."

  ;; Extract benchmark functions
  (ert-info ("Extract benchmark functions")
    (let* ((doc-syms (list (list :name "BenchmarkMyFunc" :kind 12)
                           (list :name "BenchmarkAnother" :kind 12)))
           (result (eglot-gopls--benchmark-functions doc-syms)))
      (should (and (member "BenchmarkMyFunc" result)
                   (member "BenchmarkAnother" result)))))

  ;; Return nil for empty doc-syms
  (ert-info ("Return nil for empty doc-syms")
    (should-not (eglot-gopls--benchmark-functions (list))))

  ;; Return full symbol maps when doc-sym-p is t
  (ert-info ("Return full symbol maps when doc-sym-p is t")
    (let* ((doc-syms (list (list :name "BenchmarkMyFunc"
                                 :kind 12
                                 :range (list :start (list :line 1 :character 0)
                                              :end (list :line 2 :character 0)))))
           (result (eglot-gopls--benchmark-functions doc-syms t)))
      (should result)
      (should (listp (car result)))
      (should (plist-get (car result) :name))
      (should (plist-get (car result) :range))))

  ;; Filter out non-function symbols (methods)
  (ert-info ("Filter out non-function symbols (methods)")
    (let* ((doc-syms (list (list :name "BenchmarkMyFunc" :kind 12)
                           (list :name "(*MySuite).BenchmarkMethod" :kind 6))))
      (should (equal (eglot-gopls--benchmark-functions doc-syms)
                     '("BenchmarkMyFunc")))))

  ;; Filter out functions that don't match benchmark pattern
  (ert-info ("Filter out functions that don't match benchmark pattern")
    (let* ((doc-syms (list (list :name "BenchmarkMyFunc" :kind 12)
                           (list :name "TestFunction" :kind 12)
                           (list :name "RegularFunc" :kind 12))))
      (should (equal (eglot-gopls--benchmark-functions doc-syms)
                     '("BenchmarkMyFunc")))))

  ;; Multiple benchmark functions
  (ert-info ("Multiple benchmark functions")
    (let* ((doc-syms (list (list :name "BenchmarkFirst" :kind 12)
                           (list :name "BenchmarkSecond" :kind 12)
                           (list :name "BenchmarkThird" :kind 12)))
           (result (eglot-gopls--benchmark-functions doc-syms)))
      (should (= (length result) 3))
      (should (member "BenchmarkFirst" result))
      (should (member "BenchmarkSecond" result))
      (should (member "BenchmarkThird" result))))

  ;; Mixed symbols - only extract benchmarks
  (ert-info ("Mixed symbols - only extract benchmarks")
    (let* ((doc-syms (list (list :name "TestFunction" :kind 12)
                           (list :name "BenchmarkMyFunc" :kind 12)
                           (list :name "FuzzFunction" :kind 12)
                           (list :name "BenchmarkAnother" :kind 12))))
      (should (= (length (eglot-gopls--benchmark-functions doc-syms)) 2))
      (should (member "BenchmarkMyFunc" (eglot-gopls--benchmark-functions doc-syms)))
      (should (member "BenchmarkAnother" (eglot-gopls--benchmark-functions doc-syms)))))

  ;; Benchmark with underscore
  (ert-info ("Benchmark with underscore")
    (let* ((doc-syms (list (list :name "Benchmark_MyFunc" :kind 12))))
      (should (equal (eglot-gopls--benchmark-functions doc-syms)
                     '("Benchmark_MyFunc")))))

  ;; No benchmark functions returns nil
  (ert-info ("No benchmark functions returns nil")
    (let* ((doc-syms (list (list :name "TestFunction" :kind 12)
                           (list :name "RegularFunc" :kind 12))))
      (should-not (eglot-gopls--benchmark-functions doc-syms)))))


;;; Test flags tests

(ert-deftest eglot-gopls-test-test-flags ()
  "Test that eglot-gopls--test-flags correctly returns build flags."

  ;; Return eglot-gopls-test-flags when set
  (ert-info ("Return eglot-gopls-test-flags when set")
    (let ((eglot-gopls-test-flags '("-tags" "integration")))
      (should (equal (eglot-gopls--test-flags) '("-tags" "integration")))))

  ;; Return eglot-gopls-test-flags even if workspace config has buildFlags
  (ert-info ("Return eglot-gopls-test-flags even if workspace config has buildFlags")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:buildFlags ["-tags" "e2e"]))))
      (let ((eglot-gopls-test-flags '("-tags" "custom")))
        (should (equal (eglot-gopls--test-flags) '("-tags" "custom"))))))

  ;; Return empty list when eglot-gopls-test-flags is empty
  (ert-info ("Return empty list when eglot-gopls-test-flags is empty")
    (let ((eglot-gopls-test-flags '()))
      (should-not (eglot-gopls--test-flags))))

  ;; Fall back to workspace buildFlags when eglot-gopls-test-flags is nil
  (ert-info ("Fall back to workspace buildFlags when eglot-gopls-test-flags is nil")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:buildFlags ["-tags" "integration"]))))
      (let ((eglot-gopls-test-flags nil))
        (should (equal (eglot-gopls--test-flags) '("-tags" "integration"))))))

  ;; Fall back to workspace buildFlags when eglot-gopls-test-flags is empty list
  (ert-info ("Fall back to workspace buildFlags when eglot-gopls-test-flags is empty list")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:buildFlags ["-race" "-cover"]))))
      (let ((eglot-gopls-test-flags '()))
        (should (equal (eglot-gopls--test-flags) '("-race" "-cover"))))))

  ;; Return nil when neither eglot-gopls-test-flags nor workspace buildFlags are set
  (ert-info ("Return nil when neither eglot-gopls-test-flags nor workspace buildFlags are set")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () '(:codelenses (:test t)))))
      (let ((eglot-gopls-test-flags nil))
        (should-not (eglot-gopls--test-flags)))))

  ;; Return nil when workspace configuration is nil
  (ert-info ("Return nil when workspace configuration is nil")
    (cl-letf (((symbol-function 'eglot-gopls--workspace-conf)
               (lambda () nil)))
      (let ((eglot-gopls-test-flags nil))
        (should-not (eglot-gopls--test-flags))))))


;;; Split flags at -args tests

(ert-deftest eglot-gopls-test-split-flags-at-args ()
  "Test that eglot-gopls--split-flags-at-args correctly splits flags at -args marker."

  ;; No -args marker - return (flags . nil)
  (ert-info ("No -args marker - return (flags . nil)")
    (let ((result (eglot-gopls--split-flags-at-args '("-tags" "integration" "-race"))))
      (should (equal (car result) '("-tags" "integration" "-race")))
      (should (null (cdr result)))))

  ;; Empty list - return (nil . nil)
  (ert-info ("Empty list - return (nil . nil)")
    (let ((result (eglot-gopls--split-flags-at-args '())))
      (should (null (car result)))
      (should (null (cdr result)))))

  ;; -args at beginning - return (nil . args)
  (ert-info ("-args at beginning - return (nil . args)")
    (let ((result (eglot-gopls--split-flags-at-args '("-args" "-test.v" "-run" "TestFoo"))))
      (should (null (car result)))
      (should (equal (cdr result) '("-test.v" "-run" "TestFoo")))))

  ;; -args at end - return (flags . nil)
  (ert-info ("-args at end - return (flags . nil)")
    (let ((result (eglot-gopls--split-flags-at-args '("-tags" "integration" "-args"))))
      (should (equal (car result) '("-tags" "integration")))
      (should (null (cdr result)))))

  ;; -args in middle - split correctly
  (ert-info ("-args in middle - split correctly")
    (let ((result (eglot-gopls--split-flags-at-args
                   '("-tags" "integration" "-args" "-test.v" "-run" "TestMain"))))
      (should (equal (car result) '("-tags" "integration")))
      (should (equal (cdr result) '("-test.v" "-run" "TestMain")))))

  ;; Single -args only - return (nil . nil)
  (ert-info ("Single -args only - return (nil . nil)")
    (let ((result (eglot-gopls--split-flags-at-args '("-args"))))
      (should (null (car result)))
      (should (null (cdr result)))))

  ;; Multiple build flags and test args
  (ert-info ("Multiple build flags and test args")
    (let ((result (eglot-gopls--split-flags-at-args
                   '("-race" "-cover" "-tags" "integration" "-args"
                     "-test.v" "-test.run" "TestFoo" "-test.count" "1"))))
      (should (equal (car result) '("-race" "-cover" "-tags" "integration")))
      (should (equal (cdr result)
                     '("-test.v" "-test.run" "TestFoo" "-test.count" "1")))))

  ;; Flags with values containing -args (should not split)
  (ert-info ("Flags with values containing -args (should not split)")
    (let ((result (eglot-gopls--split-flags-at-args
                   '("-tags" "integration" "-run" "my-args-test"))))
      (should (equal (car result) '("-tags" "integration" "-run" "my-args-test")))
      (should (null (cdr result))))))

;;; Package test signature tests

(ert-deftest eglot-gopls-test-package-test-signature ()
  "Test that eglot-gopls--package-test-signature correctly generates file signatures."

  ;; Directory with test files
  (ert-info ("Directory with test files")
    (let* ((temp-dir (make-temp-file "eglot-gopls-test-" t)))
      (unwind-protect
          (progn
            ;; Create test files with known content
            (write-region "package main\n\nfunc TestFoo(t *testing.T) {}" nil
                          (expand-file-name "foo_test.go" temp-dir))
            (write-region "package main\n\nfunc TestBar(t *testing.T) {}" nil
                          (expand-file-name "bar_test.go" temp-dir))
            (let* ((sig (eglot-gopls--package-test-signature temp-dir)))
              (should (= (length sig) 2))
              ;; Check that each signature has the expected structure (FILE SIZE MTIME)
              (dolist (s sig)
                (should (listp s))
                (should (= (length s) 3))
                (should (stringp (nth 0 s))) ; FILE
                (should (integerp (nth 1 s))) ; SIZE
                (should (consp (nth 2 s))) ; MTIME (should be a pair)
                (should (string-match-p "_test\\.go$" (nth 0 s))))))
        (delete-directory temp-dir t))))

  ;; Empty directory returns empty list
  (ert-info ("Empty directory returns empty list")
    (let* ((temp-dir (make-temp-file "eglot-gopls-empty-" t)))
      (unwind-protect
          (should-not (eglot-gopls--package-test-signature temp-dir))
        (delete-directory temp-dir t))))

  ;; Directory with only non-test files returns empty list
  (ert-info ("Directory with only non-test files returns empty list")
    (let* ((temp-dir (make-temp-file "eglot-gopls-notest-" t)))
      (unwind-protect
          (progn
            (write-region "package main\n\nfunc Foo() {}" nil
                          (expand-file-name "foo.go" temp-dir))
            (write-region "package main\n\nfunc Bar() {}" nil
                          (expand-file-name "bar.go" temp-dir))
            (should-not (eglot-gopls--package-test-signature temp-dir)))
        (delete-directory temp-dir t))))

  ;; Hidden files (starting with dot) are excluded
  (ert-info ("Hidden files (starting with dot) are excluded")
    (let* ((temp-dir (make-temp-file "eglot-gopls-hidden-" t)))
      (unwind-protect
          (progn
            (write-region "package main\n\nfunc TestHidden(t *testing.T) {}" nil
                          (expand-file-name ".hidden_test.go" temp-dir))
            (write-region "package main\n\nfunc TestVisible(t *testing.T) {}" nil
                          (expand-file-name "visible_test.go" temp-dir))
            (let* ((sig (eglot-gopls--package-test-signature temp-dir)))
              (should (= (length sig) 1))
              (should (string-match-p "visible_test\\.go$" (nth 0 (car sig))))))
        (delete-directory temp-dir t)))))


;;; Suite to test map tests

(ert-deftest eglot-gopls-test-suite-to-test-map ()
  "Test that eglot-gopls--suite-to-test-map correctly builds and caches suite mappings."

  ;; Cache miss triggers internal build function
  (ert-info ("Cache miss triggers internal build function")
    (clrhash eglot-gopls--suite-to-test-cache)
    (let ((default-directory "/fake/test/dir/1")
          build-called-p)
      (cl-letf (((symbol-function 'eglot-gopls--package-test-signature)
                 (lambda (_dir) '((sig))))
                ((symbol-function 'eglot-gopls--suite-to-test-map-internal)
                 (lambda (_dir)
                   (setq build-called-p t)
                   (let ((map (make-hash-table :test #'equal)))
                     (puthash "MySuite" "TestMySuite" map)
                     map))))
        (setq build-called-p nil)
        (let ((result (eglot-gopls--suite-to-test-map)))
          (should build-called-p)
          (should (hash-table-p result))
          (should (string= (gethash "MySuite" result) "TestMySuite"))))))

  ;; Cache hit returns cached map without rebuilding
  (ert-info ("Cache hit returns cached map without rebuilding")
    (clrhash eglot-gopls--suite-to-test-cache)
    (let ((default-directory "/fake/test/dir/2")
          build-count)
      (cl-letf (((symbol-function 'eglot-gopls--package-test-signature)
                 (lambda (_dir) '((sig))))
                ((symbol-function 'eglot-gopls--suite-to-test-map-internal)
                 (lambda (_dir)
                   (setq build-count (1+ build-count))
                   (let ((map (make-hash-table :test #'equal)))
                     (puthash "CachedSuite" "TestCached" map)
                     map))))
        (setq build-count 0)
        ;; First call - cache miss, triggers build
        (eglot-gopls--suite-to-test-map)
        (should (= build-count 1))
        ;; Second call - cache hit, does not trigger build
        (eglot-gopls--suite-to-test-map)
        (should (= build-count 1)))))

  ;; Signature change invalidates cache and triggers rebuild
  (ert-info ("Signature change invalidates cache and triggers rebuild")
    (clrhash eglot-gopls--suite-to-test-cache)
    (let ((default-directory "/fake/test/dir/3")
          current-sig
          build-count)
      (cl-letf (((symbol-function 'eglot-gopls--package-test-signature)
                 (lambda (_dir) current-sig))
                ((symbol-function 'eglot-gopls--suite-to-test-map-internal)
                 (lambda (_dir)
                   (setq build-count (1+ build-count))
                   (let ((map (make-hash-table :test #'equal)))
                     (puthash "InvalidatedSuite" "TestInvalidated" map)
                     map))))
        (setq build-count 0)
        ;; First call with sig 1
        (setq current-sig '((sig 1)))
        (eglot-gopls--suite-to-test-map)
        (should (= build-count 1))
        ;; Second call - same signature, cache hit
        (eglot-gopls--suite-to-test-map)
        (should (= build-count 1))
        ;; Third call - different signature (sig changed), cache miss
        (setq current-sig '((sig 2)))
        (eglot-gopls--suite-to-test-map)
        (should (= build-count 2))))))


;;; Extract testify method tests

(ert-deftest eglot-gopls-test-extract-testify-method ()
  "Test that eglot-gopls--extract-testify-method correctly extracts suite receiver and method name."

  ;; Positive tests - should return cons cell

  ;; Standard testify method format
  (ert-info ("Standard testify method format")
    (let ((result (eglot-gopls--extract-testify-method "(*MySuite).TestMethod")))
      (should result)
      (should (consp result))
      (should (string= (car result) "MySuite"))
      (should (string= (cdr result) "TestMethod"))))

  ;; Testify method with complex suite name
  (ert-info ("Testify method with complex suite name")
    (let ((result (eglot-gopls--extract-testify-method "(*UserHandlerSuite).TestCreateUser")))
      (should result)
      (should (string= (car result) "UserHandlerSuite"))
      (should (string= (cdr result) "TestCreateUser"))))

  ;; Testify method with numbers
  (ert-info ("Testify method with numbers")
    (let ((result (eglot-gopls--extract-testify-method "(*Suite1).Test1")))
      (should result)
      (should (string= (car result) "Suite1"))
      (should (string= (cdr result) "Test1"))))

  ;; Testify method with underscores in suite name
  (ert-info ("Testify method with underscores in suite name")
    (let ((result (eglot-gopls--extract-testify-method "(*my_test_suite).TestFunc")))
      (should result)
      (should (string= (car result) "my_test_suite"))
      (should (string= (cdr result) "TestFunc"))))

  ;; Testify method with single character suite
  (ert-info ("Testify method with single character suite")
    (let ((result (eglot-gopls--extract-testify-method "(*S).TestA")))
      (should result)
      (should (string= (car result) "S"))
      (should (string= (cdr result) "TestA"))))

  ;; Testify method with complex test name
  (ert-info ("Testify method with complex test name")
    (let ((result (eglot-gopls--extract-testify-method "(*APISuite).TestGetUserByID")))
      (should result)
      (should (string= (car result) "APISuite"))
      (should (string= (cdr result) "TestGetUserByID"))))

  ;; Testify method with TestX (single character after Test)
  (ert-info ("Testify method with TestX")
    (let ((result (eglot-gopls--extract-testify-method "(*Suite).TestX")))
      (should result)
      (should (string= (car result) "Suite"))
      (should (string= (cdr result) "TestX"))))

  ;; Negative tests - should return nil

  ;; Missing asterisk before suite name
  (ert-info ("Missing asterisk before suite name")
    (should-not (eglot-gopls--extract-testify-method "(MySuite).TestMethod")))

  ;; Missing closing parenthesis
  (ert-info ("Missing closing parenthesis")
    (should-not (eglot-gopls--extract-testify-method "(*MySuite.TestMethod")))

  ;; Test with lowercase after Test (invalid Go test name)
  (ert-info ("Test with lowercase after Test")
    (should-not (eglot-gopls--extract-testify-method "(*MySuite).Testmethod")))

  ;; Non-test method
  (ert-info ("Non-test method")
    (should-not (eglot-gopls--extract-testify-method "(*MySuite).SetupTest")))

  ;; TearDown method
  (ert-info ("TearDown method")
    (should-not (eglot-gopls--extract-testify-method "(*MySuite).TearDownTest")))

  ;; Regular test function (not a testify method)
  (ert-info ("Regular test function")
    (should-not (eglot-gopls--extract-testify-method "TestMethod")))

  ;; Example method
  (ert-info ("Example method")
    (should-not (eglot-gopls--extract-testify-method "(*MySuite).ExampleMethod")))

  ;; Benchmark method
  (ert-info ("Benchmark method")
    (should-not (eglot-gopls--extract-testify-method "(*MySuite).BenchmarkMethod")))

  ;; Empty string
  (ert-info ("Empty string")
    (should-not (eglot-gopls--extract-testify-method "")))

  ;; Method with lowercase test
  (ert-info ("Method with lowercase test")
    (should-not (eglot-gopls--extract-testify-method "(*MySuite).testMethod")))

  ;; Missing dot before method
  (ert-info ("Missing dot before method")
    (should-not (eglot-gopls--extract-testify-method "(*MySuite)TestMethod")))

  ;; Only receiver without method
  (ert-info ("Only receiver without method")
    (should-not (eglot-gopls--extract-testify-method "(*MySuite)")))

  ;; Method without receiver
  (ert-info ("Method without receiver")
    (should-not (eglot-gopls--extract-testify-method ".TestMethod")))

  ;; Method with lowercase Test
  (ert-info ("Method with lowercase Test")
    (should-not (eglot-gopls--extract-testify-method "(*MySuite).testMethod")))

  ;; TestFollowedByLowercase (invalid Go test naming)
  (ert-info ("TestFollowedByLowercase")
    (should-not (eglot-gopls--extract-testify-method "(*MySuite).Testmethod"))))


;;; Extract testify method name tests

(ert-deftest eglot-gopls-test-extract-testify-method-name ()
  "Test that eglot-gopls--extract-testify-method-name correctly extracts just the method name."

  ;; Positive tests - should return method name string

  ;; Standard testify method format - extract method name only
  (ert-info ("Standard testify method format")
    (let ((result (eglot-gopls--extract-testify-method-name "(*MySuite).TestMethod")))
      (should result)
      (should (stringp result))
      (should (string= result "TestMethod"))))

  ;; Testify method with complex suite name
  (ert-info ("Testify method with complex suite name")
    (let ((result (eglot-gopls--extract-testify-method-name "(*UserHandlerSuite).TestCreateUser")))
      (should (string= result "TestCreateUser"))))

  ;; Testify method with numbers
  (ert-info ("Testify method with numbers")
    (let ((result (eglot-gopls--extract-testify-method-name "(*Suite1).Test1")))
      (should (string= result "Test1"))))

  ;; Testify method with underscores in suite name - should not affect method name
  (ert-info ("Testify method with underscores in suite name")
    (let ((result (eglot-gopls--extract-testify-method-name "(*my_test_suite).TestFunc")))
      (should (string= result "TestFunc"))))

  ;; Testify method with single character suite
  (ert-info ("Testify method with single character suite")
    (let ((result (eglot-gopls--extract-testify-method-name "(*S).TestA")))
      (should (string= result "TestA"))))

  ;; Testify method with complex test name
  (ert-info ("Testify method with complex test name")
    (let ((result (eglot-gopls--extract-testify-method-name "(*APISuite).TestGetUserByID")))
      (should (string= result "TestGetUserByID"))))

  ;; Testify method with TestX (single character after Test)
  (ert-info ("Testify method with TestX")
    (let ((result (eglot-gopls--extract-testify-method-name "(*Suite).TestX")))
      (should (string= result "TestX"))))

  ;; Test method with digit
  (ert-info ("Test method with digit")
    (let ((result (eglot-gopls--extract-testify-method-name "(*MySuite).Test1")))
      (should (string= result "Test1"))))

  ;; Negative tests - should return nil

  ;; Missing asterisk before suite name
  (ert-info ("Missing asterisk before suite name")
    (should-not (eglot-gopls--extract-testify-method-name "(MySuite).TestMethod")))

  ;; Missing closing parenthesis
  (ert-info ("Missing closing parenthesis")
    (should-not (eglot-gopls--extract-testify-method-name "(*MySuite.TestMethod")))

  ;; Test with lowercase after Test (invalid Go test name)
  (ert-info ("Test with lowercase after Test")
    (should-not (eglot-gopls--extract-testify-method-name "(*MySuite).Testmethod")))

  ;; Non-test method
  (ert-info ("Non-test method")
    (should-not (eglot-gopls--extract-testify-method-name "(*MySuite).SetupTest")))

  ;; TearDown method
  (ert-info ("TearDown method")
    (should-not (eglot-gopls--extract-testify-method-name "(*MySuite).TearDownTest")))

  ;; Regular test function (not a testify method)
  (ert-info ("Regular test function")
    (should-not (eglot-gopls--extract-testify-method-name "TestMethod")))

  ;; Example method
  (ert-info ("Example method")
    (should-not (eglot-gopls--extract-testify-method-name "(*MySuite).ExampleMethod")))

  ;; Benchmark method
  (ert-info ("Benchmark method")
    (should-not (eglot-gopls--extract-testify-method-name "(*MySuite).BenchmarkMethod")))

  ;; Empty string
  (ert-info ("Empty string")
    (should-not (eglot-gopls--extract-testify-method-name "")))

  ;; Method with lowercase test
  (ert-info ("Method with lowercase test")
    (should-not (eglot-gopls--extract-testify-method-name "(*MySuite).testMethod")))

  ;; Missing dot before method
  (ert-info ("Missing dot before method")
    (should-not (eglot-gopls--extract-testify-method-name "(*MySuite)TestMethod")))

  ;; Only receiver without method
  (ert-info ("Only receiver without method")
    (should-not (eglot-gopls--extract-testify-method-name "(*MySuite)")))

  ;; Method without receiver
  (ert-info ("Method without receiver")
    (should-not (eglot-gopls--extract-testify-method-name ".TestMethod")))

  ;; TestFollowedByLowercase (invalid Go test naming)
  (ert-info ("TestFollowedByLowercase")
    (should-not (eglot-gopls--extract-testify-method-name "(*MySuite).Testmethod"))))


;;; Build run regex tests

(ert-deftest eglot-gopls-test-build-run-regex ()
  "Test that eglot-gopls--build-run-regex correctly builds regexps for test functions."

  ;; Single function without shell wrapping
  (ert-info ("Single function without shell wrapping")
    (let ((result (eglot-gopls--build-run-regex '("TestFoo"))))
      (should (string= result "^TestFoo$"))))

  ;; Single function with shell wrapping
  (ert-info ("Single function with shell wrapping")
    (let ((result (eglot-gopls--build-run-regex '("TestFoo") t)))
      (should (string= result "'^TestFoo$'"))))

  ;; Multiple functions without shell wrapping
  (ert-info ("Multiple functions without shell wrapping")
    (let ((result (eglot-gopls--build-run-regex '("TestFoo" "TestBar" "TestBaz"))))
      (should (string= result "^(TestFoo|TestBar|TestBaz)$"))))

  ;; Multiple functions with shell wrapping
  (ert-info ("Multiple functions with shell wrapping")
    (let ((result (eglot-gopls--build-run-regex '("TestFoo" "TestBar") t)))
      (should (string= result "'^(TestFoo|TestBar)$'"))))

  ;; Functions with special characters (underscores, numbers)
  (ert-info ("Functions with special characters")
    (let ((result (eglot-gopls--build-run-regex '("Test_MyFunc" "Test1" "Test2Func"))))
      (should (string= result "^(Test_MyFunc|Test1|Test2Func)$"))))

  ;; Functions with complex names
  (ert-info ("Functions with complex names")
    (let ((result (eglot-gopls--build-run-regex '("TestGetUserByID" "TestAPICall"))))
      (should (string= result "^(TestGetUserByID|TestAPICall)$"))))

  ;; Single complex function with shell wrapping
  (ert-info ("Single complex function with shell wrapping")
    (let ((result (eglot-gopls--build-run-regex '("Test_GetUser_By_ID") t)))
      (should (string= result "'^Test_GetUser_By_ID$'"))))

  ;; Empty list returns nil
  (ert-info ("Empty list returns nil")
    (should-not (eglot-gopls--build-run-regex '())))

  ;; Nil returns nil
  (ert-info ("Nil returns nil")
    (should-not (eglot-gopls--build-run-regex nil)))

  ;; Single testify method
  (ert-info ("Single testify method")
    (let ((result (eglot-gopls--build-run-regex '("TestMethod"))))
      (should (string= result "^TestMethod$"))))

  ;; Multiple testify methods
  (ert-info ("Multiple testify methods")
    (let ((result (eglot-gopls--build-run-regex '("TestFirst" "TestSecond" "TestThird"))))
      (should (string= result "^(TestFirst|TestSecond|TestThird)$"))))

  ;; Multiple methods with shell wrapping
  (ert-info ("Multiple methods with shell wrapping")
    (let ((result (eglot-gopls--build-run-regex '("TestFirst" "TestSecond") t)))
      (should (string= result "'^(TestFirst|TestSecond)$'"))))

  ;; Large number of functions
  (ert-info ("Large number of functions")
    (let ((funcs '("Test1" "Test2" "Test3" "Test4" "Test5"))
          (result (eglot-gopls--build-run-regex '("Test1" "Test2" "Test3" "Test4" "Test5"))))
      (should (string= result "^(Test1|Test2|Test3|Test4|Test5)$"))))

  ;; Function with pipe character (edge case - should work as literal)
  (ert-info ("Function containing pipe character")
    (let ((result (eglot-gopls--build-run-regex '("Test|Pipe"))))
      (should (string= result "^Test|Pipe$"))))

  ;; Functions are properly escaped (no double escaping)
  (ert-info ("Functions are not double escaped")
    (let ((result (eglot-gopls--build-run-regex '("Test\\Something"))))
      (should (stringp result))))) ; end of ert-deftest eglot-gopls-test-build-run-regex


;;; Test command tests

(ert-deftest eglot-gopls-test-test-command ()
  "Test that eglot-gopls--test-command correctly builds Go test command arguments."

  ;; Basic test with no functions (run all tests)
  (ert-info ("Basic test with no functions (run all tests)")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns nil
                         :benchmark-p nil
                         :code-coverage-p nil
                         :flags nil))))
      (should (listp result))
      (should (member "go" result))
      (should (member "test" result))
      (should (member "-test.fullpath=true" result))
      (should (member "." result))))

  ;; Single test function
  (ert-info ("Single test function")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns '("TestMyFunc")
                         :benchmark-p nil
                         :code-coverage-p nil
                         :flags nil))))
      (should (listp result))
      (should (member "-run" result))
      (should (member "'^TestMyFunc$'" result))))

  ;; Multiple test functions
  (ert-info ("Multiple test functions")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns '("TestFoo" "TestBar" "TestBaz")
                         :benchmark-p nil
                         :code-coverage-p nil
                         :flags nil))))
      (should (listp result))
      (should (member "-run" result))
      (should (member "'^(TestFoo|TestBar|TestBaz)$'" result))))

  ;; Single benchmark function
  (ert-info ("Single benchmark function")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns '("BenchmarkMyFunc")
                         :benchmark-p t
                         :code-coverage-p nil
                         :flags nil))))
      (should (listp result))
      (should (member "-benchmem" result))
      (should (member "-bench" result))
      (should (member "'^BenchmarkMyFunc$'" result))
      (should (member "-run='^$'" result))))

  ;; Multiple benchmark functions
  (ert-info ("Multiple benchmark functions")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns '("BenchmarkFoo" "BenchmarkBar")
                         :benchmark-p t
                         :code-coverage-p nil
                         :flags nil))))
      (should (listp result))
      (should (member "-benchmem" result))
      (should (member "-bench" result))
      (should (member "'^(BenchmarkFoo|BenchmarkBar)$'" result))))

  ;; All benchmarks (no functions specified)
  (ert-info ("All benchmarks (no functions specified)")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns nil
                         :benchmark-p t
                         :code-coverage-p nil
                         :flags nil))))
      (should (listp result))
      (should (member "-benchmem" result))
      (should (member "-bench" result))
      (should (member "." result))
      (should-not (member "-run" result))
      (should-not (member "-timeout" result))))

  ;; Test with code coverage (coverprofile path is included)
  (ert-info ("Test with code coverage")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns '("TestMyFunc")
                         :benchmark-p nil
                         :code-coverage-p t
                         :flags nil))))
      (should (listp result))
      ;; Should include coverprofile flag with go-code-cover
      (should (cl-loop for item in result
                       thereis (string-match-p "-coverprofile=.*go-code-cover" item)))))

  ;; Test with custom flags
  (ert-info ("Test with custom flags")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns '("TestMyFunc")
                         :benchmark-p nil
                         :code-coverage-p nil
                         :flags '("-race" "-count=1")))))
      (should (listp result))
      (should (member "-race" result))
      (should (member "-count=1" result))))

  ;; Test with testify methods
  (ert-info ("Test with testify methods")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns '("(*MySuite).TestMethod" "(*AnotherSuite).TestAnother")
                         :benchmark-p nil
                         :code-coverage-p nil
                         :flags nil))))
      (should (listp result))
      (should (member "-testify.m" result))
      (should (member "'^(TestMethod|TestAnother)$'" result))))

  ;; Test with mixed functions and testify methods
  (ert-info ("Test with mixed functions and testify methods")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns '("TestRegular" "(*MySuite).TestMethod")
                         :benchmark-p nil
                         :code-coverage-p nil
                         :flags nil))))
      (should (listp result))
      (should (member "-run" result))
      (should (member "'^TestRegular$'" result))
      (should (member "-testify.m" result))
      (should (member "'^TestMethod$'" result))))

  ;; Test with -args in flags
  (ert-info ("Test with -args in flags")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns '("TestMyFunc")
                         :benchmark-p nil
                         :code-coverage-p nil
                         :flags '("-tags" "integration" "-args" "-test.v")))))
      (should (listp result))
      (should (member "-tags" result))
      (should (member "integration" result))
      (should (member "-args" result))
      (should (member "-test.v" result))
      (should (member "." result))))

  ;; Verify timeout flag is added for regular tests (not benchmarks)
  (ert-info ("Timeout flag is added for regular tests")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns '("TestMyFunc")
                         :benchmark-p nil
                         :code-coverage-p nil
                         :flags nil))))
      (should (member "-timeout" result))
      ;; Check that timeout value follows the -timeout flag
      (let ((timeout-pos (cl-position "-timeout" result :test #'equal)))
        (should timeout-pos)
        (should (< (1+ timeout-pos) (length result))))))

  ;; Verify no timeout for benchmarks
  (ert-info ("No timeout for benchmarks")
    (let ((result (eglot-gopls--test-command
                   (list :test-fns nil
                         :benchmark-p t
                         :code-coverage-p nil
                         :flags nil))))
      (should-not (member "-timeout" result)))))


;;; Debug args tests

(ert-deftest eglot-gopls-test-debug-args ()
  "Test that eglot-gopls--debug-args correctly builds debugger arguments."

  ;; Regular test function
  (ert-info ("Regular test function")
    (let ((result (eglot-gopls--debug-args "TestMyFunc" nil)))
      (should (listp result))
      (should (= (length result) 2))
      (should (member "-test.run" result))
      (should (member "^TestMyFunc$" result))))

  ;; Test function with underscore
  (ert-info ("Test function with underscore")
    (let ((result (eglot-gopls--debug-args "Test_My_Function" nil)))
      (should (listp result))
      (should (member "-test.run" result))
      (should (member "^Test_My_Function$" result))))

  ;; Test function with numbers
  (ert-info ("Test function with numbers")
    (let ((result (eglot-gopls--debug-args "TestAPIHandlerV2" nil)))
      (should (listp result))
      (should (member "-test.run" result))
      (should (member "^TestAPIHandlerV2$" result))))

  ;; Testify method
  (ert-info ("Testify method")
    (cl-letf (((symbol-function 'eglot-gopls--suite-to-test-map)
               (lambda () (let ((map (make-hash-table :test #'equal)))
                            (puthash "MySuite" "TestMySuite" map)
                            map)))
              ((symbol-function 'eglot-gopls--test-suite-run-fn)
               (lambda (_suite _map) "TestMySuite")))
      (let ((result (eglot-gopls--debug-args "(*MySuite).TestMethod" nil)))
        (should (listp result))
        (should (= (length result) 2))
        (should (member "-test.run" result))
        (should (member "^TestMySuite$/^TestMethod$" result)))))

  ;; Testify method with complex suite name
  (ert-info ("Testify method with complex suite name")
    (cl-letf (((symbol-function 'eglot-gopls--suite-to-test-map)
               (lambda () (let ((map (make-hash-table :test #'equal)))
                            (puthash "UserHandlerSuite" "TestUserHandlerSuite" map)
                            map)))
              ((symbol-function 'eglot-gopls--test-suite-run-fn)
               (lambda (_suite _map) "TestUserHandlerSuite")))
      (let ((result (eglot-gopls--debug-args "(*UserHandlerSuite).TestCreateUser" nil)))
        (should (listp result))
        (should (member "-test.run" result))
        (should (member "^TestUserHandlerSuite$/^TestCreateUser$" result)))))

  ;; Benchmark function
  (ert-info ("Benchmark function")
    (let ((result (eglot-gopls--debug-args "BenchmarkMyFunc" t)))
      (should (listp result))
      (should (= (length result) 4))
      (should (member "-test.bench" result))
      (should (member "^BenchmarkMyFunc$" result))
      (should (member "-test.run" result))
      (should (member "a^" result))))

  ;; Benchmark function with underscore
  (ert-info ("Benchmark function with underscore")
    (let ((result (eglot-gopls--debug-args "Benchmark_MyFunc" t)))
      (should (listp result))
      (should (member "-test.bench" result))
      (should (member "^Benchmark_MyFunc$" result))
      (should (member "-test.run" result))
      (should (member "a^" result))))

  ;; Benchmark function with numbers
  (ert-info ("Benchmark function with numbers")
    (let ((result (eglot-gopls--debug-args "BenchmarkHashV2" t)))
      (should (listp result))
      (should (member "-test.bench" result))
      (should (member "^BenchmarkHashV2$" result))
      (should (member "-test.run" result))
      (should (member "a^" result))))

  ;; Example function
  (ert-info ("Example function")
    (let ((result (eglot-gopls--debug-args "ExampleAdd" nil)))
      (should (listp result))
      (should (member "-test.run" result))
      (should (member "^ExampleAdd$" result))))

  ;; TestMain
  (ert-info ("TestMain function")
    (let ((result (eglot-gopls--debug-args "TestMain" nil)))
      (should (listp result))
      (should (member "-test.run" result))
      (should (member "^TestMain$" result))))

  ;; Test function with single character
  (ert-info ("Test function with single character")
    (let ((result (eglot-gopls--debug-args "TestA" nil)))
      (should (listp result))
      (should (member "-test.run" result))
      (should (member "^TestA$" result))))

  ;; Benchmark with single character
  (ert-info ("Benchmark with single character")
    (let ((result (eglot-gopls--debug-args "BenchmarkB" t)))
      (should (listp result))
      (should (member "-test.bench" result))
      (should (member "^BenchmarkB$" result))
      (should (member "-test.run" result))
      (should (member "a^" result)))))


;;; Debug tests

(ert-deftest eglot-gopls-test-debug ()
  "Test that eglot-gopls--debug correctly builds dape-config with :args, :buildFlags, and :env."

  ;; Debug regular test function
  (ert-info ("Debug regular test function")
    (let ((dape-configs nil))
      (cl-letf (((symbol-function 'require)
                 (lambda (_feature &optional _filename &rest _loadargs) t))
                ((symbol-function 'dape)
                 (lambda (config) config)))
        (let* ((eglot-gopls-test-env-vars nil)
               (debug-config (list :test-fn "TestMyFunc"
                                   :benchmark-p nil
                                   :flags nil))
               (result (eglot-gopls--debug debug-config)))
          (should (plist-get result :args))
          (should (vectorp (plist-get result :args)))
          (should (string= (plist-get result :buildFlags) ""))
          (should (hash-table-p (plist-get result :env)))
          (should (= (hash-table-count (plist-get result :env)) 0))
          ;; Verify args contain debug-args
          (let ((args (append (plist-get result :args) nil)))
            (should (member "-test.run" args))
            (should (member "^TestMyFunc$" args)))))))

  ;; Debug test function with build flags
  (ert-info ("Debug test function with build flags")
    (let ((dape-configs nil))
      (cl-letf (((symbol-function 'require)
                 (lambda (_feature &optional _filename &rest _loadargs) t))
                ((symbol-function 'dape)
                 (lambda (config) config)))
        (let* ((eglot-gopls-test-env-vars nil)
               (debug-config (list :test-fn "TestMyFunc"
                                   :benchmark-p nil
                                   :flags '("-tags" "integration")))
               (result (eglot-gopls--debug debug-config)))
          (should (plist-get result :args))
          (should (vectorp (plist-get result :args)))
          (should (string= (plist-get result :buildFlags) "-tags integration"))
          ;; Verify args contain debug-args
          (let ((args (append (plist-get result :args) nil)))
            (should (member "-test.run" args))
            (should (member "^TestMyFunc$" args)))))))

  ;; Debug test function with build flags and args flags (after -args)
  (ert-info ("Debug test function with build flags and args flags")
    (let ((dape-configs nil))
      (cl-letf (((symbol-function 'require)
                 (lambda (_feature &optional _filename &rest _loadargs) t))
                ((symbol-function 'dape)
                 (lambda (config) config)))
        (let* ((eglot-gopls-test-env-vars nil)
               (debug-config (list :test-fn "TestMyFunc"
                                   :benchmark-p nil
                                   :flags '("-tags" "integration" "-args" "-test.v")))
               (result (eglot-gopls--debug debug-config)))
          (should (plist-get result :args))
          (should (vectorp (plist-get result :args)))
          (should (string= (plist-get result :buildFlags) "-tags integration"))
          ;; Verify args contain debug-args and args-flags
          (let ((args (append (plist-get result :args) nil)))
            (should (member "-test.run" args))
            (should (member "^TestMyFunc$" args))
            (should (member "-test.v" args)))))))

  ;; Debug benchmark function
  (ert-info ("Debug benchmark function")
    (let ((dape-configs nil))
      (cl-letf (((symbol-function 'require)
                 (lambda (_feature &optional _filename &rest _loadargs) t))
                ((symbol-function 'dape)
                 (lambda (config) config)))
        (let* ((eglot-gopls-test-env-vars nil)
               (debug-config (list :test-fn "BenchmarkMyFunc"
                                   :benchmark-p t
                                   :flags nil))
               (result (eglot-gopls--debug debug-config)))
          (should (plist-get result :args))
          (should (vectorp (plist-get result :args)))
          (should (string= (plist-get result :buildFlags) ""))
          ;; Verify args contain benchmark debug-args
          (let ((args (append (plist-get result :args) nil)))
            (should (member "-test.bench" args))
            (should (member "^BenchmarkMyFunc$" args))
            (should (member "-test.run" args))
            (should (member "a^" args)))))))

  ;; Debug with custom env vars
  (ert-info ("Debug with custom env vars")
    (let ((dape-configs nil))
      (cl-letf (((symbol-function 'require)
                 (lambda (_feature &optional _filename &rest _loadargs) t))
                ((symbol-function 'dape)
                 (lambda (config) config)))
        (let* ((custom-env (make-hash-table :test #'equal))
               (_ (puthash "GOOS" "linux" custom-env))
               (_ (puthash "GOARCH" "amd64" custom-env))
               (eglot-gopls-test-env-vars custom-env)
               (debug-config (list :test-fn "TestMyFunc"
                                   :benchmark-p nil
                                   :flags nil))
               (result (eglot-gopls--debug debug-config)))
          (should (plist-get result :args))
          (should (vectorp (plist-get result :args)))
          (should (hash-table-p (plist-get result :env)))
          (should (= (hash-table-count (plist-get result :env)) 2))
          (should (string= (gethash "GOOS" (plist-get result :env)) "linux"))
          (should (string= (gethash "GOARCH" (plist-get result :env)) "amd64"))))))


  ;; Debug testify method
  (ert-info ("Debug testify method")
    (let ((dape-configs nil))
      (cl-letf (((symbol-function 'require)
                 (lambda (_feature &optional _filename &rest _loadargs) t))
                ((symbol-function 'dape)
                 (lambda (config) config))
                ((symbol-function 'eglot-gopls--suite-to-test-map)
                 (lambda () (let ((map (make-hash-table :test #'equal)))
                              (puthash "MySuite" "TestMySuite" map)
                              map)))
                ((symbol-function 'eglot-gopls--test-suite-run-fn)
                 (lambda (_suite _map) "TestMySuite")))
        (let* ((eglot-gopls-test-env-vars nil)
               (debug-config (list :test-fn "(*MySuite).TestMethod"
                                   :benchmark-p nil
                                   :flags nil))
               (result (eglot-gopls--debug debug-config)))
          (should (plist-get result :args))
          (should (vectorp (plist-get result :args)))
          ;; Verify args contain testify debug-args
          (let ((args (append (plist-get result :args) nil)))
            (should (member "-test.run" args))
            (should (member "^TestMySuite$/^TestMethod$" args)))))))

  ;; Debug with multiple build flags
  (ert-info ("Debug with multiple build flags")
    (let ((dape-configs nil))
      (cl-letf (((symbol-function 'require)
                 (lambda (_feature &optional _filename &rest _loadargs) t))
                ((symbol-function 'dape)
                 (lambda (config) config)))
        (let* ((eglot-gopls-test-env-vars nil)
               (debug-config (list :test-fn "TestMyFunc"
                                   :benchmark-p nil
                                   :flags '("-race" "-cover" "-tags" "integration")))
               (result (eglot-gopls--debug debug-config)))
          (should (plist-get result :args))
          (should (vectorp (plist-get result :args)))
          (should (string= (plist-get result :buildFlags) "-race -cover -tags integration"))))))

  ;; Verify dape-config structure
  (ert-info ("Verify dape-config structure")
    (let ((dape-configs nil))
      (cl-letf (((symbol-function 'require)
                 (lambda (_feature &optional _filename &rest _loadargs) t))
                ((symbol-function 'dape)
                 (lambda (config) config)))
        (let* ((eglot-gopls-test-env-vars nil)
               (debug-config (list :test-fn "TestMyFunc"
                                   :benchmark-p nil
                                   :flags nil))
               (result (eglot-gopls--debug debug-config)))
          ;; Check required dape-config keys
          (should (string= (plist-get result :name) "Debug Test"))
          (should (plist-get result 'modes))
          (should (string= (plist-get result 'command) "dlv"))
          (should (string= (plist-get result :type) "go"))
          (should (string= (plist-get result :request) "launch"))
          (should (string= (plist-get result :mode) "test"))
          (should (string= (plist-get result :program) ".")))))))


;;; Test at cursor tests

(ert-deftest eglot-gopls-test-test-at-cursor ()
  "Test that eglot-gopls--test-at-cursor correctly builds debug-config and test-config."

  ;; Debug with regular test function - verify debug-config structure
  (ert-info ("Debug with regular test function builds correct debug-config")
    (let ((dape-configs nil))
      (cl-letf (((symbol-function 'require)
                 (lambda (_feature &optional _filename &rest _loadargs) t))
                ((symbol-function 'dape) (lambda (_config) nil))
                ((symbol-function 'eglot-gopls--debug)
                 (lambda (config)
                   ;; Verify debug-config structure
                   (should (plist-get config :test-fn))
                   (should (string= (plist-get config :test-fn) "TestMyFunc"))
                   (should (not (plist-get config :benchmark-p)))
                   (should-not (plist-get config :flags)))))
        (eglot-gopls--test-at-cursor 'debug (vector (list :functionName "TestMyFunc"))))))

  ;; Debug with benchmark function - verify benchmark-p is set
  (ert-info ("Debug with benchmark function sets benchmark-p in debug-config")
    (let ((dape-configs nil))
      (cl-letf (((symbol-function 'require)
                 (lambda (_feature &optional _filename &rest _loadargs) t))
                ((symbol-function 'dape) (lambda (_config) nil))
                ((symbol-function 'eglot-gopls--debug)
                 (lambda (config)
                   (should (string= (plist-get config :test-fn) "BenchmarkMyFunc"))
                   (should (plist-get config :benchmark-p)))))
        (eglot-gopls--test-at-cursor 'debug (vector (list :functionName "BenchmarkMyFunc"))))))

  ;; Test with regular test function - verify test-config structure
  (ert-info ("Test with regular test function builds correct test-config")
    (cl-letf (((symbol-function 'eglot-gopls--benchmark-function-p) (lambda (_name) nil))
              ((symbol-function 'eglot-gopls--extract-testify-method) (lambda (_name) nil))
              ((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 ;; Verify test-config structure
                 (should (plist-get config :test-fns))
                 (should (listp (plist-get config :test-fns)))
                 (should (member "TestMyFunc" (plist-get config :test-fns)))
                 (should (= (length (plist-get config :test-fns)) 1))
                 (should (not (plist-get config :benchmark-p)))
                 (should (not (plist-get config :code-coverage-p)))
                 '("go" "test" ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-at-cursor 'test (vector (list :functionName "TestMyFunc")))))

  ;; Test with testify method - verify both function and suite runner are included
  (ert-info ("Test with testify method includes both method and suite runner")
    (cl-letf (((symbol-function 'eglot-gopls--benchmark-function-p) (lambda (_name) nil))
              ((symbol-function 'eglot-gopls--extract-testify-method)
               (lambda (_name) (cons "MySuite" "TestMethod")))
              ((symbol-function 'eglot-gopls--suite-to-test-map)
               (lambda () (let ((map (make-hash-table :test #'equal)))
                            (puthash "MySuite" "TestMySuite" map)
                            map)))
              ((symbol-function 'eglot-gopls--test-suite-run-fn)
               (lambda (_suite _map) "TestMySuite"))
              ((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 (should (plist-get config :test-fns))
                 (should (member "(*MySuite).TestMethod" (plist-get config :test-fns)))
                 (should (member "TestMySuite" (plist-get config :test-fns)))
                 (should (= (length (plist-get config :test-fns)) 2))
                 '("go" "test" ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-at-cursor 'test (vector (list :functionName "(*MySuite).TestMethod")))))

  ;; Benchmark - verify test-config has benchmark-p set
  (ert-info ("Benchmark command sets benchmark-p in test-config")
    (cl-letf (((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 (should (plist-get config :test-fns))
                 (should (member "BenchmarkMyFunc" (plist-get config :test-fns)))
                 (should (plist-get config :benchmark-p))
                 '("go" "test" "-bench" "." ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-at-cursor 'benchmark (vector (list :functionName "BenchmarkMyFunc")))))

  ;; Test with custom flags - verify flags are passed through
  (ert-info ("Test with custom flags passes flags to config")
    (let ((dape-configs nil))
      (cl-letf (((symbol-function 'require)
                 (lambda (_feature &optional _filename &rest _loadargs) t))
                ((symbol-function 'dape) (lambda (_config) nil))
                ((symbol-function 'eglot-gopls--debug)
                 (lambda (config)
                   (should (plist-get config :flags))
                   (should (equal (plist-get config :flags) '("-race" "-cover"))))))
        (let ((eglot-gopls-test-flags '("-race" "-cover")))
          (eglot-gopls--test-at-cursor 'debug (vector (list :functionName "TestMyFunc")))))))

  ;; Empty args - returns nil
  (ert-info ("Empty args vector returns nil")
    (should-not (eglot-gopls--test-at-cursor 'test (vector))))

  ;; Args without functionName - returns nil
  (ert-info ("Args without functionName returns nil")
    (should-not (eglot-gopls--test-at-cursor 'test (vector (list :other "value")))))

  ;; Invalid cmd-type - returns nil
  (ert-info ("Invalid cmd-type returns nil")
    (should-not (eglot-gopls--test-at-cursor 'invalid (vector (list :functionName "TestMyFunc"))))))


;;; Test current package tests

(ert-deftest eglot-gopls-test-test-current-package ()
  "Test that eglot-gopls--test-current-package correctly builds test-config."

  ;; Test current package - verify test-config structure
  (ert-info ("Test current package builds correct test-config")
    (cl-letf (((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 ;; Verify test-config structure for package test
                 (should (not (plist-get config :test-fns))) ; nil = run all tests
                 (should (not (plist-get config :benchmark-p)))
                 (should (plist-get config :code-coverage-p)) ; coverage enabled for package
                 '("go" "test" "-test.fullpath=true" "-timeout" "30s" ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-current-package nil)))

  ;; Benchmark current package - verify test-config structure
  (ert-info ("Benchmark current package sets benchmark-p in test-config")
    (cl-letf (((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 (should (not (plist-get config :test-fns)))
                 (should (plist-get config :benchmark-p))
                 (should (plist-get config :code-coverage-p))
                 '("go" "test" "-test.fullpath=true" "-benchmem" "-bench" "." ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-current-package t)))

  ;; Test current package with custom flags
  (ert-info ("Test current package with custom flags passes flags to config")
    (let ((eglot-gopls-test-flags '("-tags" "integration")))
      (cl-letf (((symbol-function 'eglot-gopls--test-command)
                 (lambda (config)
                   (should (plist-get config :flags))
                   (should (equal (plist-get config :flags) '("-tags" "integration")))
                   '("go" "test" ".")))
                ((symbol-function 'compile) (lambda (&rest _args) nil)))
        (eglot-gopls--test-current-package nil))))

  ;; Verify code coverage is always enabled for package tests
  (ert-info ("Code coverage is always enabled for package tests")
    (cl-letf (((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 (should (plist-get config :code-coverage-p))
                 '("go" "test" ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-current-package nil))))


;;; Test current file tests

(ert-deftest eglot-gopls-test-test-current-file ()
  "Test that eglot-gopls--test-current-file correctly builds test-config."

  ;; Test current file - verify test-config structure
  (ert-info ("Test current file builds correct test-config")
    (cl-letf (((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--doc-symbols) (lambda () nil))
              ((symbol-function 'eglot-gopls--test-functions) (lambda (_syms) '()))
              ((symbol-function 'eglot-gopls--testify-methods) (lambda (_syms) '()))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 ;; Verify test-config structure for file test
                 (should-not (plist-get config :benchmark-p))
                 (should-not (plist-get config :code-coverage-p)) ; no coverage for file test
                 '("go" "test" "-test.fullpath=true" "-timeout" "30s" ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-current-file nil)))

  ;; Benchmark current file - verify test-config structure
  (ert-info ("Benchmark current file sets benchmark-p in test-config")
    (cl-letf (((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--doc-symbols) (lambda () nil))
              ((symbol-function 'eglot-gopls--benchmark-functions) (lambda (_syms) '()))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 (should (plist-get config :benchmark-p))
                 (should-not (plist-get config :code-coverage-p))
                 '("go" "test" "-test.fullpath=true" "-benchmem" "-bench" "." ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-current-file t)))

  ;; Test current file with test functions - verify functions are included
  (ert-info ("Test current file includes test functions from doc-symbols")
    (let ((doc-syms (list (list :name "TestFoo" :kind 12)
                          (list :name "TestBar" :kind 12)))))
    (cl-letf (((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--doc-symbols)
               (lambda () (list (list :name "TestFoo" :kind 12)
                                (list :name "TestBar" :kind 12))))
              ((symbol-function 'eglot-gopls--test-functions)
               (lambda (_syms) '("TestFoo" "TestBar")))
              ((symbol-function 'eglot-gopls--testify-methods) (lambda (_syms) nil))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 (should (plist-get config :test-fns))
                 (should (member "TestFoo" (plist-get config :test-fns)))
                 (should (member "TestBar" (plist-get config :test-fns)))
                 '("go" "test" ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-current-file nil)))

  ;; Test current file with testify methods - verify methods are included
  (ert-info ("Test current file includes testify methods from doc-symbols")
    (let ((doc-syms (list (list :name "(*MySuite).TestMethod" :kind 6)))))
    (cl-letf (((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--doc-symbols)
               (lambda () (list (list :name "TestFoo" :kind 12)
                                (list :name "TestBar" :kind 12))))
              ((symbol-function 'eglot-gopls--test-functions) (lambda (_syms) nil))
              ((symbol-function 'eglot-gopls--testify-methods)
               (lambda (_syms) '("(*MySuite).TestMethod")))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 (should (plist-get config :test-fns))
                 (should (member "(*MySuite).TestMethod" (plist-get config :test-fns)))
                 '("go" "test" ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-current-file nil)))

  ;; Test current file with mixed test functions and testify methods
  (ert-info ("Test current file combines test functions and testify methods")
    (let ((doc-syms (list (list :name "TestFunc" :kind 12)
                          (list :name "(*Suite).TestMethod" :kind 6)))))
    (cl-letf (((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--doc-symbols)
               (lambda () (list (list :name "TestFoo" :kind 12)
                                (list :name "TestBar" :kind 12))))
              ((symbol-function 'eglot-gopls--test-functions)
               (lambda (_syms) '("TestFunc")))
              ((symbol-function 'eglot-gopls--testify-methods)
               (lambda (_syms) '("(*Suite).TestMethod")))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 (should (plist-get config :test-fns))
                 (should (member "TestFunc" (plist-get config :test-fns)))
                 (should (member "(*Suite).TestMethod" (plist-get config :test-fns)))
                 (should (= (length (plist-get config :test-fns)) 2))
                 '("go" "test" ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-current-file nil)))

  ;; Benchmark current file with benchmark functions
  (ert-info ("Benchmark current file includes benchmark functions")
    (let ((doc-syms (list (list :name "BenchmarkFoo" :kind 12)
                          (list :name "BenchmarkBar" :kind 12)))))
    (cl-letf (((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--doc-symbols)
               (lambda () (list (list :name "TestFoo" :kind 12)
                                (list :name "TestBar" :kind 12))))
              ((symbol-function 'eglot-gopls--benchmark-functions)
               (lambda (_syms) '("BenchmarkFoo" "BenchmarkBar")))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 (should (plist-get config :test-fns))
                 (should (member "BenchmarkFoo" (plist-get config :test-fns)))
                 (should (member "BenchmarkBar" (plist-get config :test-fns)))
                 '("go" "test" "-bench" "." ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-current-file t)))

  ;; Test current file with custom flags
  (ert-info ("Test current file with custom flags passes flags to config")
    (let ((eglot-gopls-test-flags '("-race" "-cover")))
      (cl-letf (((symbol-function 'eglot-gopls--doc-symbols) (lambda () nil))
                ((symbol-function 'eglot-gopls--test-functions) (lambda (_syms) '()))
                ((symbol-function 'eglot-gopls--testify-methods) (lambda (_syms) '()))
                ((symbol-function 'eglot-gopls--test-command)
                 (lambda (config)
                   (should (plist-get config :flags))
                   (should (equal (plist-get config :flags) '("-race" "-cover")))
                   '("go" "test" ".")))
                ((symbol-function 'compile) (lambda (&rest _args) nil)))
        (eglot-gopls--test-current-file nil))))

  ;; Verify code coverage is disabled for file tests
  (ert-info ("Code coverage is disabled for file tests")
    (cl-letf (((symbol-function 'eglot-gopls--test-flags) (lambda () nil))
              ((symbol-function 'eglot-gopls--doc-symbols) (lambda () nil))
              ((symbol-function 'eglot-gopls--test-functions) (lambda (_syms) '()))
              ((symbol-function 'eglot-gopls--testify-methods) (lambda (_syms) '()))
              ((symbol-function 'eglot-gopls--test-command)
               (lambda (config)
                 (should-not (plist-get config :code-coverage-p))
                 '("go" "test" ".")))
              ((symbol-function 'compile) (lambda (&rest _args) nil)))
      (eglot-gopls--test-current-file nil))))


;;; Vulncheck database URL tests

(ert-deftest eglot-gopls-test-vulncheck-db ()
  "Test that eglot-gopls--vulncheck-db returns the correct URL in order of precedence."

  ;; eglot-gopls-vulncheck-db takes highest precedence
  (ert-info ("eglot-gopls-vulncheck-db takes highest precedence")
    (let ((eglot-gopls-vulncheck-db "https://custom.vuln.db")
          (eglot-gopls-test-env-vars '(:GOVULNDB "https://env.vars.db")))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (_name) "https://environment.var.db")))
        (should (string= (eglot-gopls--vulncheck-db) "https://custom.vuln.db")))))

  ;; GOVULNDB from eglot-gopls-test-env-vars takes second precedence
  (ert-info ("GOVULNDB from eglot-gopls-test-env-vars takes second precedence")
    (let ((eglot-gopls-vulncheck-db nil)
          (eglot-gopls-test-env-vars '(:GOVULNDB "https://env.vars.db")))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (_name) "https://environment.var.db")))
        (should (string= (eglot-gopls--vulncheck-db) "https://env.vars.db")))))

  ;; Environment variable GOVULNDB is used when custom vars are not set
  (ert-info ("Environment variable GOVULNDB is used when custom vars are not set")
    (let ((eglot-gopls-vulncheck-db nil)
          (eglot-gopls-test-env-vars nil))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (name)
                   (if (string= name "GOVULNDB")
                       "https://vuln.go.dev"
                     nil))))
        (should (string= (eglot-gopls--vulncheck-db) "https://vuln.go.dev")))))

  ;; Returns nil when no source is set
  (ert-info ("Returns nil when no source is set")
    (let ((eglot-gopls-vulncheck-db nil)
          (eglot-gopls-test-env-vars nil))
      (cl-letf (((symbol-function 'getenv) (lambda (_name) nil)))
        (should-not (eglot-gopls--vulncheck-db)))))

  ;; Empty eglot-gopls-test-env-vars plist falls through to getenv
  (ert-info ("Empty eglot-gopls-test-env-vars plist falls through to getenv")
    (let ((eglot-gopls-vulncheck-db nil)
          (eglot-gopls-test-env-vars '()))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (name)
                   (if (string= name "GOVULNDB")
                       "https://from.env.var"
                     nil))))
        (should (string= (eglot-gopls--vulncheck-db) "https://from.env.var")))))

  ;; eglot-gopls-test-env-vars without :GOVULNDB key falls through to getenv
  (ert-info ("eglot-gopls-test-env-vars without :GOVULNDB key falls through to getenv")
    (let ((eglot-gopls-vulncheck-db nil)
          (eglot-gopls-test-env-vars '(:OTHER_VAR "value")))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (name)
                   (if (string= name "GOVULNDB")
                       "https://from.getenv"
                     nil))))
        (should (string= (eglot-gopls--vulncheck-db) "https://from.getenv")))))

  ;; Official Go vulnerability database URL
  (ert-info ("Official Go vulnerability database URL from environment")
    (let ((eglot-gopls-vulncheck-db nil)
          (eglot-gopls-test-env-vars nil))
      (cl-letf (((symbol-function 'getenv)
                 (lambda (name)
                   (if (string= name "GOVULNDB")
                       "https://vuln.go.dev"
                     nil))))
        (should (string= (eglot-gopls--vulncheck-db) "https://vuln.go.dev")))))

  ;; Custom vulnerability database URL from user configuration
  (ert-info ("Custom vulnerability database URL from user configuration")
    (let ((eglot-gopls-vulncheck-db "https://internal.vuln.db.company")
          (eglot-gopls-test-env-vars nil))
      (cl-letf (((symbol-function 'getenv) (lambda (_name) nil)))
        (should (string= (eglot-gopls--vulncheck-db)
                         "https://internal.vuln.db.company"))))))

;;; Vulncheck command tests

(ert-deftest eglot-gopls-test-vulncheck ()
  "Test that eglot-gopls--vulncheck builds the correct command and passes it to compile."

  ;; Error when govulncheck is not found in PATH
  (ert-info ("Error when govulncheck is not found in PATH")
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_cmd) nil)))
      (should-error (eglot-gopls--vulncheck (vector (list :uri "file:///path/to/file.go" :pattern nil)))
                    :type 'user-error)))

  ;; Successful command with database URL
  (ert-info ("Successful command with database URL")
    (let ((eglot-gopls-vulncheck-db "https://custom.vuln.db")
          (eglot-gopls-test-env-vars nil)
          (compile-args nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) "govulncheck"))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (_uri) "/path/to/file.go"))
                ((symbol-function 'project-current)
                 (lambda () 'mock-project))
                ((symbol-function 'project-root)
                 (lambda (_proj) "/project/root"))
                ((symbol-function 'compile)
                 (lambda (command &optional _mode)
                   (setq compile-args (list command)))))
        (eglot-gopls--vulncheck (vector (list :uri "file:///path/to/file.go" :pattern nil)))
        (should compile-args)
        (should (stringp (car compile-args)))
        (should (string-match-p "govulncheck.*-json.*-mode source.*-scan symbol" (car compile-args)))
        (should (string-match-p "-db" (car compile-args)))
        (should (string-match-p "custom.vuln.db" (car compile-args))))))

  ;; Successful command without database URL
  (ert-info ("Successful command without database URL")
    (let ((eglot-gopls-vulncheck-db nil)
          (eglot-gopls-test-env-vars nil)
          (compile-args nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) "govulncheck"))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (_uri) "/path/to/file.go"))
                ((symbol-function 'project-current)
                 (lambda () 'mock-project))
                ((symbol-function 'project-root)
                 (lambda (_proj) "/project/root"))
                ((symbol-function 'compile)
                 (lambda (command &optional _mode)
                   (setq compile-args (list command)))))
        (eglot-gopls--vulncheck (vector (list :uri "file:///path/to/file.go" :pattern nil)))
        (should compile-args)
        (should (stringp (car compile-args)))
        (should (string-match-p "govulncheck.*-json.*-mode source.*-scan symbol" (car compile-args)))
        (should-not (string-match-p "-db" (car compile-args))))))

  ;; Command includes directory and project root
  (ert-info ("Command includes proper -C flags for directory")
    (let ((eglot-gopls-vulncheck-db nil)
          (eglot-gopls-test-env-vars nil)
          (compile-args nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) "govulncheck"))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (_uri) "/project/root/pkg/file.go"))
                ((symbol-function 'project-current)
                 (lambda () 'mock-project))
                ((symbol-function 'project-root)
                 (lambda (_proj) "/project/root"))
                ((symbol-function 'compile)
                 (lambda (command &optional _mode)
                   (setq compile-args (list command)))))
        (eglot-gopls--vulncheck (vector (list :uri "file:///project/root/pkg/file.go" :pattern nil)))
        (should compile-args)
        (should (string-match-p "-C.*pkg" (car compile-args)))
        (should (string-match-p "\\.\\.\\." (car compile-args))))))

  ;; Command with official Go vulnerability database
  (ert-info ("Command with official Go vulnerability database from environment")
    (let ((eglot-gopls-vulncheck-db nil)
          (eglot-gopls-test-env-vars nil)
          (compile-args nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) "govulncheck"))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (_uri) "/path/to/file.go"))
                ((symbol-function 'project-current)
                 (lambda () 'mock-project))
                ((symbol-function 'project-root)
                 (lambda (_proj) "/project/root"))
                ((symbol-function 'getenv)
                 (lambda (name)
                   (if (string= name "GOVULNDB")
                       "https://vuln.go.dev"
                     nil)))
                ((symbol-function 'compile)
                 (lambda (command &optional _mode)
                   (setq compile-args (list command)))))
        (eglot-gopls--vulncheck (vector (list :uri "file:///path/to/file.go" :pattern nil)))
        (should compile-args)
        (should (string-match-p "vuln.go.dev" (car compile-args))))))

  ;; Compile mode is eglot-gopls-compilation-mode
  (ert-info ("Compile uses eglot-gopls-compilation-mode")
    (let ((eglot-gopls-vulncheck-db nil)
          (eglot-gopls-test-env-vars nil)
          (compile-mode-arg nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) "govulncheck"))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (_uri) "/path/to/file.go"))
                ((symbol-function 'project-current)
                 (lambda () 'mock-project))
                ((symbol-function 'project-root)
                 (lambda (_proj) "/project/root"))
                ((symbol-function 'compile)
                 (lambda (_command &optional mode)
                   (setq compile-mode-arg mode))))
        (eglot-gopls--vulncheck (vector (list :uri "file:///path/to/file.go" :pattern nil)))
        (should (eq compile-mode-arg 'eglot-gopls-compilation-mode)))))

  ;; No operation when arguments vector is empty
  (ert-info ("No operation when arguments vector is empty")
    (let ((compile-called nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) "govulncheck"))
                ((symbol-function 'compile)
                 (lambda (&rest _args)
                   (setq compile-called t))))
        (eglot-gopls--vulncheck [])
        (should-not compile-called))))

  ;; No operation when project-current returns nil
  (ert-info ("No operation when project-current returns nil")
    (let ((compile-called nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (_cmd) "govulncheck"))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (_uri) "/path/to/file.go"))
                ((symbol-function 'project-current)
                 (lambda () nil))
                ((symbol-function 'compile)
                 (lambda (&rest _args)
                   (setq compile-called t))))
        (eglot-gopls--vulncheck (vector (list :uri "file:///path/to/file.go" :pattern nil)))
        (should-not compile-called)))))


;;; Create test codelens tests

(ert-deftest eglot-gopls-test-create-test-codelens ()
  "Test that eglot-gopls--create-test-codelens transforms lenses correctly."

  ;; Successful transformation with test function
  (ert-info ("Successful transformation with test function")
    (let* ((range '(:start (:line 10 :character 0)
                    :end (:line 10 :character 50)))
           (lens `(:range ,range
                   :command (:title "run test"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///test.go"
                                          :Tests ["TestFoo"])])))
           (result (eglot-gopls--create-test-codelens lens)))
      (should (= (length result) 2))
      ;; First lens: go.test.cursor with original title
      (let ((first-lens (elt result 0)))
        (should (equal (plist-get first-lens :range) range))
        (should (string= (plist-get (plist-get first-lens :command) :title) "run test"))
        (should (string= (plist-get (plist-get first-lens :command) :command) "go.test.cursor")))
      ;; Second lens: go.debug.cursor with "debug test" title
      (let ((second-lens (elt result 1)))
        (should (equal (plist-get second-lens :range) range))
        (should (string= (plist-get (plist-get second-lens :command) :title) "debug test"))
        (should (string= (plist-get (plist-get second-lens :command) :command) "go.debug.cursor")))))

  ;; Returns original lens when arguments vector is empty
  (ert-info ("Returns original lens when arguments vector is empty")
    (let* ((range '(:start (:line 5 :character 0)
                    :end (:line 5 :character 30)))
           (lens `(:range ,range
                   :command (:title "run test"
                             :command "gopls.run_tests"
                             :arguments [])))
           (result (eglot-gopls--create-test-codelens lens)))
      (should (= (length result) 1))
      (should (equal (elt result 0) lens))))

  ;; Returns original lens when Tests vector is empty
  (ert-info ("Returns original lens when Tests vector is empty")
    (let* ((range '(:start (:line 8 :character 0)
                    :end (:line 8 :character 40)))
           (lens `(:range ,range
                   :command (:title "run test"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///test.go" :Tests [])])))
           (result (eglot-gopls--create-test-codelens lens)))
      (should (= (length result) 1))
      (should (equal (elt result 0) lens))))

  ;; Preserves test function name in arguments
  (ert-info ("Preserves test function name in arguments")
    (let* ((range '(:start (:line 15 :character 2)
                    :end (:line 15 :character 60)))
           (lens `(:range ,range
                   :command (:title "run test"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///handler_test.go"
                                          :Tests ["TestCreateUser"])])))
           (result (eglot-gopls--create-test-codelens lens)))
      (should (= (length result) 2))
      ;; Check go.test.cursor has correct function name
      (let ((first-lens (elt result 0)))
        (let ((args (plist-get (plist-get first-lens :command) :arguments)))
          (should (length> args 0))
          (should (string= (plist-get (elt args 0) :functionName) "TestCreateUser"))))
      ;; Check go.debug.cursor has correct function name
      (let ((second-lens (elt result 1)))
        (let ((args (plist-get (plist-get second-lens :command) :arguments)))
          (should (length> args 0))
          (should (string= (plist-get (elt args 0) :functionName) "TestCreateUser"))))))

  ;; Both lenses share the same range
  (ert-info ("Both transformed lenses share the same range")
    (let* ((range '(:start (:line 20 :character 4)
                    :end (:line 20 :character 70)))
           (lens `(:range ,range
                   :command (:title "run test"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///api_test.go"
                                          :Tests ["TestAPIHandler"])])))
           (result (eglot-gopls--create-test-codelens lens)))
      (should (= (length result) 2))
      (should (equal (plist-get (elt result 0) :range) range))
      (should (equal (plist-get (elt result 1) :range) range))))

  ;; Original title is preserved for go.test.cursor
  (ert-info ("Original title is preserved for go.test.cursor lens")
    (let* ((lens `(:range (:start (:line 1 :character 0) :end (:line 1 :character 20))
                   :command (:title "run unit test"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///test.go"
                                          :Tests ["TestUnit"])])))
           (result (eglot-gopls--create-test-codelens lens)))
      (should (= (length result) 2))
      (should (string= (plist-get (plist-get (elt result 0) :command) :title)
                       "run unit test"))))

  ;; Debug lens always has "debug test" as title
  (ert-info ("Debug lens always has \"debug test\" as title")
    (let* ((lens `(:range (:start (:line 1 :character 0) :end (:line 1 :character 20))
                   :command (:title "some custom title"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///test.go"
                                          :Tests ["TestCustom"])])))
           (result (eglot-gopls--create-test-codelens lens)))
      (should (= (length result) 2))
      (should (string= (plist-get (plist-get (elt result 1) :command) :title)
                       "debug test")))))


;;; Benchmark codelens tests

(ert-deftest eglot-gopls-test-create-benchmark-codelens ()
  "Test that eglot-gopls--create-benchmark-codelens transforms lenses correctly."

  ;; Successful transformation with benchmark function
  (ert-info ("Successful transformation with benchmark function")
    (let* ((range '(:start (:line 10 :character 0)
                    :end (:line 10 :character 50)))
           (lens `(:range ,range
                   :command (:title "run benchmark"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///test.go"
                                          :Benchmarks ["BenchmarkFoo"])])))
           (result (eglot-gopls--create-benchmark-codelens lens)))
      (should (= (length result) 2))
      ;; First lens: go.benchmark.cursor with original title
      (let ((first-lens (elt result 0)))
        (should (equal (plist-get first-lens :range) range))
        (should (string= (plist-get (plist-get first-lens :command) :title) "run benchmark"))
        (should (string= (plist-get (plist-get first-lens :command) :command) "go.benchmark.cursor")))
      ;; Second lens: go.debug.cursor with "debug benchmark" title
      (let ((second-lens (elt result 1)))
        (should (equal (plist-get second-lens :range) range))
        (should (string= (plist-get (plist-get second-lens :command) :title) "debug benchmark"))
        (should (string= (plist-get (plist-get second-lens :command) :command) "go.debug.cursor")))))

  ;; Returns original lens when arguments vector is empty
  (ert-info ("Returns original lens when arguments vector is empty")
    (let* ((range '(:start (:line 5 :character 0)
                    :end (:line 5 :character 30)))
           (lens `(:range ,range
                   :command (:title "run benchmark"
                             :command "gopls.run_tests"
                             :arguments [])))
           (result (eglot-gopls--create-benchmark-codelens lens)))
      (should (= (length result) 1))
      (should (equal (elt result 0) lens))))

  ;; Returns original lens when Benchmarks vector is empty
  (ert-info ("Returns original lens when Benchmarks vector is empty")
    (let* ((range '(:start (:line 8 :character 0)
                    :end (:line 8 :character 40)))
           (lens `(:range ,range
                   :command (:title "run benchmark"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///test.go" :Benchmarks [])])))
           (result (eglot-gopls--create-benchmark-codelens lens)))
      (should (= (length result) 1))
      (should (equal (elt result 0) lens))))

  ;; Preserves benchmark function name in arguments
  (ert-info ("Preserves benchmark function name in arguments")
    (let* ((range '(:start (:line 15 :character 2)
                    :end (:line 15 :character 60)))
           (lens `(:range ,range
                   :command (:title "run benchmark"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///handler_test.go"
                                          :Benchmarks ["BenchmarkCreateUser"])])))
           (result (eglot-gopls--create-benchmark-codelens lens)))
      (should (= (length result) 2))
      ;; Check go.benchmark.cursor has correct function name
      (let ((first-lens (elt result 0)))
        (let ((args (plist-get (plist-get first-lens :command) :arguments)))
          (should (length> args 0))
          (should (string= (plist-get (elt args 0) :functionName) "BenchmarkCreateUser"))))
      ;; Check go.debug.cursor has correct function name
      (let ((second-lens (elt result 1)))
        (let ((args (plist-get (plist-get second-lens :command) :arguments)))
          (should (length> args 0))
          (should (string= (plist-get (elt args 0) :functionName) "BenchmarkCreateUser"))))))

  ;; Both lenses share the same range
  (ert-info ("Both transformed lenses share the same range")
    (let* ((range '(:start (:line 20 :character 4)
                    :end (:line 20 :character 70)))
           (lens `(:range ,range
                   :command (:title "run benchmark"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///api_test.go"
                                          :Benchmarks ["BenchmarkAPIHandler"])])))
           (result (eglot-gopls--create-benchmark-codelens lens)))
      (should (= (length result) 2))
      (should (equal (plist-get (elt result 0) :range) range))
      (should (equal (plist-get (elt result 1) :range) range))))

  ;; Original title is preserved for go.benchmark.cursor
  (ert-info ("Original title is preserved for go.benchmark.cursor lens")
    (let* ((lens `(:range (:start (:line 1 :character 0) :end (:line 1 :character 20))
                   :command (:title "run quick benchmark"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///test.go"
                                          :Benchmarks ["BenchmarkQuick"])])))
           (result (eglot-gopls--create-benchmark-codelens lens)))
      (should (= (length result) 2))
      (should (string= (plist-get (plist-get (elt result 0) :command) :title)
                       "run quick benchmark"))))

  ;; Debug lens always has "debug benchmark" as title
  (ert-info ("Debug lens always has \"debug benchmark\" as title")
    (let* ((lens `(:range (:start (:line 1 :character 0) :end (:line 1 :character 20))
                   :command (:title "some custom title"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///test.go"
                                          :Benchmarks ["BenchmarkCustom"])])))
           (result (eglot-gopls--create-benchmark-codelens lens)))
      (should (= (length result) 2))
      (should (string= (plist-get (plist-get (elt result 1) :command) :title)
                       "debug benchmark")))))


;;; File benchmarks codelens tests

(ert-deftest eglot-gopls-test-create-file-benchmarks-codelens ()
  "Test that eglot-gopls--create-file-benchmarks-codelens transforms lenses correctly."

  ;; Successful transformation creates two lenses
  (ert-info ("Successful transformation creates two lenses")
    (let* ((range '(:start (:line 1 :character 0)
                    :end (:line 1 :character 30)))
           (lens `(:range ,range
                   :command (:title "run file benchmarks"
                             :command "gopls.run_tests")))
           (result (eglot-gopls--create-file-benchmarks-codelens lens)))
      (should (= (length result) 2))
      ;; First lens: go.benchmark.package with "run package benchmarks" title
      (let ((first-lens (elt result 0)))
        (should (equal (plist-get first-lens :range) range))
        (should (string= (plist-get (plist-get first-lens :command) :title)
                         "run package benchmarks"))
        (should (string= (plist-get (plist-get first-lens :command) :command)
                         "go.benchmark.package")))
      ;; Second lens: go.benchmark.file with original title
      (let ((second-lens (elt result 1)))
        (should (equal (plist-get second-lens :range) range))
        (should (string= (plist-get (plist-get second-lens :command) :title)
                         "run file benchmarks"))
        (should (string= (plist-get (plist-get second-lens :command) :command)
                         "go.benchmark.file")))))

  ;; Both lenses share the same range
  (ert-info ("Both transformed lenses share the same range")
    (let* ((range '(:start (:line 5 :character 2)
                    :end (:line 5 :character 50)))
           (lens `(:range ,range
                   :command (:title "run file benchmarks"
                             :command "gopls.run_tests")))
           (result (eglot-gopls--create-file-benchmarks-codelens lens)))
      (should (= (length result) 2))
      (should (equal (plist-get (elt result 0) :range) range))
      (should (equal (plist-get (elt result 1) :range) range))))

  ;; Original title is preserved for go.benchmark.file lens
  (ert-info ("Original title is preserved for go.benchmark.file lens")
    (let* ((lens `(:range (:start (:line 10 :character 0) :end (:line 10 :character 40))
                   :command (:title "run benchmarks in current file"
                             :command "gopls.run_tests")))
           (result (eglot-gopls--create-file-benchmarks-codelens lens)))
      (should (= (length result) 2))
      (should (string= (plist-get (plist-get (elt result 1) :command) :title)
                       "run benchmarks in current file"))))

  ;; go.benchmark.package always has fixed title
  (ert-info ("go.benchmark.package always has \"run package benchmarks\" as title")
    (let* ((lens `(:range (:start (:line 1 :character 0) :end (:line 1 :character 20))
                   :command (:title "some custom title"
                             :command "gopls.run_tests")))
           (result (eglot-gopls--create-file-benchmarks-codelens lens)))
      (should (= (length result) 2))
      (should (string= (plist-get (plist-get (elt result 0) :command) :title)
                       "run package benchmarks"))
      (should (string= (plist-get (plist-get (elt result 0) :command) :command)
                       "go.benchmark.package")))))

;;; Transform codelens tests

(ert-deftest eglot-gopls-test-transform-codelens ()
  "Test that eglot-gopls--transform-codelens transforms lists of lenses correctly."

  ;; Empty list returns empty list
  (ert-info ("Empty list returns empty list")
    (let ((result (eglot-gopls--transform-codelens [])))
      (should (= (length result) 0))))

  ;; Single \"run test\" lens gets transformed
  (ert-info ("Single \"run test\" lens gets transformed")
    (let* ((lens `(:range (:start (:line 10 :character 0) :end (:line 10 :character 50))
                   :command (:title "run test"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///test.go"
                                          :Tests ["TestFoo"])])))
           (result (eglot-gopls--transform-codelens (list lens))))
      (should (= (length result) 2))
      (should (string= (plist-get (plist-get (elt result 0) :command) :command) "go.test.cursor"))
      (should (string= (plist-get (plist-get (elt result 1) :command) :command) "go.debug.cursor"))))

  ;; Single \"run benchmark\" lens gets transformed
  (ert-info ("Single \"run benchmark\" lens gets transformed")
    (let* ((lens `(:range (:start (:line 20 :character 0) :end (:line 20 :character 60))
                   :command (:title "run benchmark"
                             :command "gopls.run_tests"
                             :arguments [(:URI "file:///test.go"
                                          :Benchmarks ["BenchmarkFoo"])])))
           (result (eglot-gopls--transform-codelens (list lens))))
      (should (= (length result) 2))
      (should (string= (plist-get (plist-get (elt result 0) :command) :command) "go.benchmark.cursor"))
      (should (string= (plist-get (plist-get (elt result 1) :command) :command) "go.debug.cursor"))))

  ;; Single \"run file benchmarks\" lens gets transformed
  (ert-info ("Single \"run file benchmarks\" lens gets transformed")
    (let* ((lens `(:range (:start (:line 1 :character 0) :end (:line 1 :character 20))
                   :command (:title "run file benchmarks"
                             :command "gopls.run_tests")))
           (result (eglot-gopls--transform-codelens (list lens))))
      (should (= (length result) 2))
      (should (string= (plist-get (plist-get (elt result 0) :command) :command) "go.benchmark.package"))
      (should (string= (plist-get (plist-get (elt result 1) :command) :command) "go.benchmark.file"))))

  ;; Unknown lens title returns unchanged
  (ert-info ("Unknown lens title returns unchanged")
    (let* ((lens `(:range (:start (:line 5 :character 0) :end (:line 5 :character 30))
                   :command (:title "some other command"
                             :command "gopls.other_command")))
           (result (eglot-gopls--transform-codelens (list lens))))
      (should (= (length result) 1))
      (should (equal (elt result 0) lens))))

  ;; Mixed lenses get transformed correctly
  (ert-info ("Mixed lenses get transformed correctly")
    (let* ((test-lens `(:range (:start (:line 10 :character 0) :end (:line 10 :character 50))
                        :command (:title "run test"
                                  :command "gopls.run_tests"
                                  :arguments [(:URI "file:///test.go"
                                               :Tests ["TestFoo"])])))
           (benchmark-lens `(:range (:start (:line 20 :character 0) :end (:line 20 :character 60))
                             :command (:title "run benchmark"
                                       :command "gopls.run_tests"
                                       :arguments [(:URI "file:///test.go"
                                                    :Benchmarks ["BenchmarkBar"])])))
           (unknown-lens `(:range (:start (:line 30 :character 0) :end (:line 30 :character 70))
                           :command (:title "unknown command"
                                     :command "gopls.unknown")))
           (result (eglot-gopls--transform-codelens (list test-lens benchmark-lens unknown-lens))))
      ;; test-lens produces 2 lenses
      (should (= (length result) 5))
      ;; First two from test-lens
      (should (string= (plist-get (plist-get (elt result 0) :command) :command) "go.test.cursor"))
      (should (string= (plist-get (plist-get (elt result 1) :command) :command) "go.debug.cursor"))
      ;; Next two from benchmark-lens
      (should (string= (plist-get (plist-get (elt result 2) :command) :command) "go.benchmark.cursor"))
      (should (string= (plist-get (plist-get (elt result 3) :command) :command) "go.debug.cursor"))
      ;; Last one is the unknown lens unchanged
      (should (equal (elt result 4) unknown-lens)))))

;;; Package codelens tests

(ert-deftest eglot-gopls-test-package-codelens ()
  "Test that eglot-gopls--package-codelens returns the correct package-level codelenses."

  ;; Returns two codelenses
  (ert-info ("Returns two codelenses")
    (let ((result (eglot-gopls--package-codelens)))
      (should (= (length result) 2))))

  ;; First lens is go.test.package
  (ert-info ("First lens is go.test.package")
    (let ((result (eglot-gopls--package-codelens)))
      (let ((first-lens (elt result 0)))
        (should (string= (plist-get (plist-get first-lens :command) :command) "go.test.package"))
        (should (string= (plist-get (plist-get first-lens :command) :title) "run package tests")))))

  ;; Second lens is go.test.file
  (ert-info ("Second lens is go.test.file")
    (let ((result (eglot-gopls--package-codelens)))
      (let ((second-lens (elt result 1)))
        (should (string= (plist-get (plist-get second-lens :command) :command) "go.test.file"))
        (should (string= (plist-get (plist-get second-lens :command) :title) "run file tests")))))

  ;; Both lenses have the same range at line 0
  (ert-info ("Both lenses have the same range at line 0")
    (let ((result (eglot-gopls--package-codelens)))
      (let ((expected-range '(:start (:line 0 :character 0) :end (:line 0 :character 0))))
        (should (equal (plist-get (elt result 0) :range) expected-range))
        (should (equal (plist-get (elt result 1) :range) expected-range))))))

;;; Func codelens tests

(ert-deftest eglot-gopls-test-func-codelens ()
  "Test that eglot-gopls--func-codelens returns codelenses for testify methods and fuzz functions."

  ;; Returns nil when no document symbols
  (ert-info ("Returns nil when no document symbols")
    (cl-letf (((symbol-function 'eglot-gopls--doc-symbols) (lambda () nil)))
      (should-not (eglot-gopls--func-codelens))))

  ;; Returns nil when document symbols list is empty
  (ert-info ("Returns nil when document symbols list is empty")
    (cl-letf (((symbol-function 'eglot-gopls--doc-symbols) (lambda () [])))
      (should-not (eglot-gopls--func-codelens))))

  ;; Returns codelenses for testify methods when testify is imported
  (ert-info ("Returns codelenses for testify methods when testify is imported")
    (let* ((range '(:start (:line 10 :character 0) :end (:line 10 :character 50)))
           (doc-syms `((:name "(*MySuite).TestMethod"
                        :kind ,eglot-gopls-symbol-kind-method
                        :range ,range))))
      (cl-letf (((symbol-function 'eglot-gopls--doc-symbols) (lambda () doc-syms))
                ((symbol-function 'eglot-gopls--import-testify-p) (lambda () t)))
        (let ((result (eglot-gopls--func-codelens)))
          (should (= (length result) 2))
          ;; First lens: run test
          (let ((first-lens (elt result 0)))
            (should (string= (plist-get (plist-get first-lens :command) :command) "go.test.cursor"))
            (should (string= (plist-get (plist-get first-lens :command) :title) "run test"))
            (should (equal (plist-get first-lens :range) range)))
          ;; Second lens: debug test
          (let ((second-lens (elt result 1)))
            (should (string= (plist-get (plist-get second-lens :command) :command) "go.debug.cursor"))
            (should (string= (plist-get (plist-get second-lens :command) :title) "debug test"))
            (should (equal (plist-get second-lens :range) range)))))))

  ;; Returns codelenses for fuzz functions
  (ert-info ("Returns codelenses for fuzz functions")
    (let* ((range '(:start (:line 20 :character 0) :end (:line 20 :character 60)))
           (doc-syms `((:name "FuzzFoo"
                        :kind ,eglot-gopls-symbol-kind-function
                        :range ,range))))
      (cl-letf (((symbol-function 'eglot-gopls--doc-symbols) (lambda () doc-syms))
                ((symbol-function 'eglot-gopls--import-testify-p) (lambda () nil)))
        (let ((result (eglot-gopls--func-codelens)))
          (should (= (length result) 2))
          ;; First lens: run test
          (let ((first-lens (elt result 0)))
            (should (string= (plist-get (plist-get first-lens :command) :command) "go.test.cursor"))
            (should (string= (plist-get (plist-get first-lens :command) :title) "run test"))
            (should (equal (plist-get first-lens :range) range)))
          ;; Second lens: debug test
          (let ((second-lens (elt result 1)))
            (should (string= (plist-get (plist-get second-lens :command) :command) "go.debug.cursor"))
            (should (string= (plist-get (plist-get second-lens :command) :title) "debug test"))
            (should (equal (plist-get second-lens :range) range)))))))

  ;; Does not return codelenses for testify methods when testify is not imported
  (ert-info ("Does not return codelenses for testify methods when testify is not imported")
    (let* ((range '(:start (:line 10 :character 0) :end (:line 10 :character 50)))
           (doc-syms `((:name "(*MySuite).TestMethod"
                        :kind ,eglot-gopls-symbol-kind-method
                        :range ,range))))
      (cl-letf (((symbol-function 'eglot-gopls--doc-symbols) (lambda () doc-syms))
                ((symbol-function 'eglot-gopls--import-testify-p) (lambda () nil)))
        (should-not (eglot-gopls--func-codelens)))))

  ;; Handles mixed symbols (testify methods, fuzz functions, and other functions)
  (ert-info ("Handles mixed symbols")
    (let* ((range1 '(:start (:line 5 :character 0) :end (:line 5 :character 30)))
           (range2 '(:start (:line 10 :character 0) :end (:line 10 :character 50)))
           (range3 '(:start (:line 15 :character 0) :end (:line 15 :character 40)))
           (doc-syms `((:name "(*MySuite).TestMethod1"
                        :kind ,eglot-gopls-symbol-kind-method
                        :range ,range1)
                       (:name "FuzzBar"
                        :kind ,eglot-gopls-symbol-kind-function
                        :range ,range2)
                       (:name "OtherFunction"
                        :kind ,eglot-gopls-symbol-kind-function
                        :range ,range3))))
      (cl-letf (((symbol-function 'eglot-gopls--doc-symbols) (lambda () doc-syms))
                ((symbol-function 'eglot-gopls--import-testify-p) (lambda () t)))
        (let ((result (eglot-gopls--func-codelens)))
          (should (= (length result) 4))
          ;; First two from testify method
          (should (string= (plist-get (plist-get (elt result 0) :command) :command) "go.test.cursor"))
          (should (string= (plist-get (plist-get (elt result 1) :command) :command) "go.debug.cursor"))
          ;; Next two from fuzz function
          (should (string= (plist-get (plist-get (elt result 2) :command) :command) "go.test.cursor"))
          (should (string= (plist-get (plist-get (elt result 3) :command) :command) "go.debug.cursor")))))))

;;; Provide codelens tests

(ert-deftest eglot-gopls-test-provide-codelens ()
  "Test that eglot-gopls--provide-codelens augments codelenses for test files."

  ;; Returns original codelens when not a test file
  (ert-info ("Returns original codelens when not a test file")
    (let ((original-lens (vector '(:range (:start (:line 1 :character 0) :end (:line 1 :character 10))
                                   :command (:title "test" :command "gopls.test")))))
      (cl-letf (((symbol-function 'eglot-gopls--test-file-p) (lambda () nil))
                ((symbol-function 'eglot-gopls--test-enable-p) (lambda () t)))
        (should (eq (eglot-gopls--provide-codelens original-lens) original-lens)))))

  ;; Returns original codelens when test codelenses are disabled
  (ert-info ("Returns original codelens when test codelenses are disabled")
    (let ((original-lens (vector '(:range (:start (:line 1 :character 0) :end (:line 1 :character 10))
                                   :command (:title "test" :command "gopls.test")))))
      (cl-letf (((symbol-function 'eglot-gopls--test-file-p) (lambda () t))
                ((symbol-function 'eglot-gopls--test-enable-p) (lambda () nil)))
        (should (eq (eglot-gopls--provide-codelens original-lens) original-lens)))))

  ;; Returns original codelens when neither condition is met
  (ert-info ("Returns original codelens when neither condition is met")
    (let ((original-lens (vector '(:range (:start (:line 1 :character 0) :end (:line 1 :character 10))
                                   :command (:title "test" :command "gopls.test")))))
      (cl-letf (((symbol-function 'eglot-gopls--test-file-p) (lambda () nil))
                ((symbol-function 'eglot-gopls--test-enable-p) (lambda () nil)))
        (should (eq (eglot-gopls--provide-codelens original-lens) original-lens)))))

  ;; Augments codelenses when both conditions are met
  (ert-info ("Augments codelenses when both conditions are met")
    (let* ((original-lens (vector '(:range (:start (:line 10 :character 0) :end (:line 10 :character 50))
                                    :command (:title "run test"
                                              :command "gopls.run_tests"
                                              :arguments [(:URI "file:///test.go"
                                                           :Tests ["TestFoo"])]))))
           (pkg-lens '((:range (:start (:line 0 :character 0) :end (:line 0 :character 0))
                        :command (:title "run package tests" :command "go.test.package"))
                       (:range (:start (:line 0 :character 0) :end (:line 0 :character 0))
                        :command (:title "run file tests" :command "go.test.file"))))
           (trans-lens '((:range (:start (:line 10 :character 0) :end (:line 10 :character 50))
                          :command (:title "run test" :command "go.test.cursor"
                                    :arguments [(:functionName "TestFoo")]))
                         (:range (:start (:line 10 :character 0) :end (:line 10 :character 50))
                          :command (:title "debug test" :command "go.debug.cursor"
                                    :arguments [(:functionName "TestFoo")]))))
           (func-lens '()))
      (cl-letf (((symbol-function 'eglot-gopls--test-file-p) (lambda () t))
                ((symbol-function 'eglot-gopls--test-enable-p) (lambda () t))
                ((symbol-function 'eglot-gopls--package-codelens) (lambda () pkg-lens))
                ((symbol-function 'eglot-gopls--transform-codelens) (lambda (_lens) trans-lens))
                ((symbol-function 'eglot-gopls--func-codelens) (lambda () func-lens)))
        (let ((result (eglot-gopls--provide-codelens original-lens)))
          (should (= (length result) 4))
          ;; First two from pkg-lens
          (should (string= (plist-get (plist-get (elt result 0) :command) :command) "go.test.package"))
          (should (string= (plist-get (plist-get (elt result 1) :command) :command) "go.test.file"))
          ;; Next two from trans-lens
          (should (string= (plist-get (plist-get (elt result 2) :command) :command) "go.test.cursor"))
          (should (string= (plist-get (plist-get (elt result 3) :command) :command) "go.debug.cursor"))))))

  ;; Returns a vector combining all codelens sources
  (ert-info ("Returns a vector combining all codelens sources")
    (let* ((original-lens [])
           (pkg-lens '((:range (:start (:line 0 :character 0) :end (:line 0 :character 0))
                        :command (:title "run package tests" :command "go.test.package"))))
           (trans-lens '((:range (:start (:line 5 :character 0) :end (:line 5 :character 30))
                          :command (:title "run benchmark" :command "go.benchmark.cursor"))
                         (:range (:start (:line 5 :character 0) :end (:line 5 :character 30))
                          :command (:title "debug benchmark" :command "go.debug.cursor"))))
           (func-lens '((:range (:start (:line 10 :character 0) :end (:line 10 :character 50))
                         :command (:title "run test" :command "go.test.cursor"
                                   :arguments [(:functionName "FuzzTest")]))
                        (:range (:start (:line 10 :character 0) :end (:line 10 :character 50))
                         :command (:title "debug test" :command "go.debug.cursor"
                                   :arguments [(:functionName "FuzzTest")])))))
      (cl-letf (((symbol-function 'eglot-gopls--test-file-p) (lambda () t))
                ((symbol-function 'eglot-gopls--test-enable-p) (lambda () t))
                ((symbol-function 'eglot-gopls--package-codelens) (lambda () pkg-lens))
                ((symbol-function 'eglot-gopls--transform-codelens) (lambda (_lens) trans-lens))
                ((symbol-function 'eglot-gopls--func-codelens) (lambda () func-lens)))
        (let ((result (eglot-gopls--provide-codelens original-lens)))
          (should (vectorp result))
          (should (= (length result) 5))
          ;; One from pkg-lens
          (should (string= (plist-get (plist-get (elt result 0) :command) :command) "go.test.package"))
          ;; Two from trans-lens
          (should (string= (plist-get (plist-get (elt result 1) :command) :command) "go.benchmark.cursor"))
          (should (string= (plist-get (plist-get (elt result 2) :command) :command) "go.debug.cursor"))
          ;; Two from func-lens
          (should (string= (plist-get (plist-get (elt result 3) :command) :command) "go.test.cursor"))
          (should (string= (plist-get (plist-get (elt result 4) :command) :command) "go.debug.cursor")))))))



(provide 'eglot-gopls-test)
;;; eglot-gopls-test.el ends here
