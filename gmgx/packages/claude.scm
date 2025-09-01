;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Guilherme Gondim

(define-module (gmgx packages claude)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages node)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module ((gmgx licenses)
                #:prefix nonfree:))

(define-public claude-code-nonfree
  (package
    (name "claude-code")
    (version "1.0.93")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@anthropic-ai/claude-code/-/"
             "claude-code-" version ".tgz"))
       (sha256
        (base32
         ;; guix download https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-1.0.93.tgz
         "1sy7m1dgk0ppii4q9zq2s3n8vf5prd5xh77rahm48x024809s3zq"))))

    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((out (assoc-ref %outputs "out"))
                          (bin (string-append out "/bin"))
                          (lib (string-append out
                                "/lib/node_modules/@anthropic-ai/claude-code"))
                          (source (assoc-ref %build-inputs "source"))
                          (tar (string-append (assoc-ref %build-inputs "tar")
                                              "/bin/tar"))
                          (gzip (string-append (assoc-ref %build-inputs "gzip")
                                               "/bin/gzip"))
                          (node (string-append (assoc-ref %build-inputs "node")
                                               "/bin/node")))

                     ;; Set up PATH for extraction tools
                     (setenv "PATH"
                             (string-append (dirname tar) ":"
                                            (dirname gzip) ":"
                                            (getenv "PATH")))

                     ;; Extract npm package
                     (invoke tar "xzf" source)

                     ;; Install package files to lib directory
                     (mkdir-p lib)
                     (copy-recursively "package" lib)

                     ;; Create executable wrapper script
                     (mkdir-p bin)
                     (let ((wrapper-script (string-append bin "/claude")))
                       (call-with-output-file wrapper-script
                         (lambda (port)
                           (format port "#!/bin/sh~%")
                           (format port "export DISABLE_AUTOUPDATER=1~%")
                           (format port "exec ~a ~a/cli.js ~s~%" node lib "$@")))
                       (chmod wrapper-script #o755))

                     #t))))

    (native-inputs `(("tar" ,tar)
                     ("gzip" ,gzip)))

    (inputs `(("node" ,node)))

    (home-page "https://docs.anthropic.com/en/docs/claude-code")
    (synopsis "Agentic coding tool powered by Claude AI")
    (description
     "Claude Code is an agentic coding tool that lives in your terminal,
understands your codebase, and helps you code faster by executing routine
tasks, explaining complex code, and handling git workflows.")
    (license (nonfree:nonfree "https://github.com/anthropics/claude-code/blob/main/LICENSE.md"))))
