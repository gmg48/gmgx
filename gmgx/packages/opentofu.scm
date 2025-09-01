;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Guilherme Gondim

(define-module (gmgx packages opentofu)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression))

(define-public opentofu-bin
  (package
    (name "opentofu")
    (version "1.10.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/opentofu/opentofu/releases/download/v"
             version "/tofu_" version "_linux_amd64.zip"))
       (sha256
        (base32 "0w3n5i4wpf3q6kzjjcb0n0bx3p6bzk9gx62f1awg9skhqgj8xzfl"))))
    (build-system copy-build-system)
    (inputs (list unzip))
    (arguments
     '(#:install-plan '(("tofu" "bin/"))))
    (home-page "https://opentofu.org/")
    (synopsis "Open-source infrastructure as code tool (binary distribution)")
    (description
     "OpenTofu is an open-source infrastructure as code tool.  It lets you define
resources and infrastructure in human-readable, declarative configuration files,
and manages your infrastructure's lifecycle.  OpenTofu is a fork of Terraform
that is open-source, community-driven, and managed by the Linux Foundation.
This package installs the precompiled binary.")
    (license license:mpl2.0)))
