# eglot-gopls

[![License](https://img.shields.io/badge/license-GPL_3-blue.svg)](LICENSE)

Enhanced integration between [gopls](https://github.com/golang/tools/tree/master/gopls) (the Go language server) and [Eglot](https://github.com/joaotavora/eglot) (the Emacs LSP client).

## Screenshot

<img src="https://github.com/user-attachments/assets/8ec9a401-1987-4642-b66a-9f2b43cdaa13" width=70% height=70%>

## Features

This package enhances the following gopls codelens commands:

- **`gopls.run_tests`** - Run tests and benchmarks in a compilation buffer with error navigation
  - Runs specific tests via `go test -run` or benchmarks via `go test -bench`
  - Output is parsed automatically, allowing `next-error` to jump to failures

- **`gopls.run_govulncheck` / `gopls.vulncheck`** - Run vulnerability checking on Go dependencies

## Installation

### Using package-vc

```emacs-lisp
(unless (package-installed-p 'eglot-gopls)
  (package-vc-install
   '(eglot-gopls :url "https://github.com/zsxh/eglot-gopls")))
```

### Manual installation

Download `eglot-gopls.el` and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/eglot-gopls")
(require 'eglot-gopls)
```

## Usage

Add the following to your Emacs configuration:

```elisp
(require 'eglot-gopls)
(push '((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode go-work-ts-mode)
        . (eglot-gopls-server . ("gopls")))
      eglot-server-programs)
```

Then enable `eglot` in Go buffers with `M-x eglot`.

### Advanced Configuration

For custom [gopls setting](https://github.com/golang/tools/blob/master/gopls/doc/settings.md), [eglot config](https://github.com/golang/tools/blob/master/gopls/doc/editor/emacs.md#configuring-gopls-via-eglot):

```elisp
(setq-default eglot-workspace-configuration
              '(:gopls
                (:codelenses (:generate t
                              :regenerate_cgo t
                              :tidy t
                              :upgrade_dependency t
                              :vendor t
                              :test t
                              :run_govulncheck t))))
```

#### Custom vulnerability database

To use a custom vulnerability database, set `eglot-gopls-vulncheck-db`

```elisp
(setq eglot-gopls-vulncheck-db "https://vuln.go.dev")
```

## Requirements

- Emacs 30.1+
- [eglot](https://github.com/joaotavora/eglot) 1.17.30+
- [eglot-codelens](https://github.com/zsxh/eglot-codelens) (optional)
- [gopls](https://pkg.go.dev/golang.org/x/tools/gopls)
- [govulncheck](https://pkg.go.dev/golang.org/x/vuln/cmd/govulncheck) (optional)

## License

Copyright (C) 2026 zsxh

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

See [LICENSE](LICENSE) for details.
