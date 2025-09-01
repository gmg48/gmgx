;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Guilherme Gondim

(define-module (gmgx packages terraform)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (srfi srfi-1))

;; NOTE may require ~/.config customization, esp for plugins
(define-public packer-bin
  (package
    (name "packer")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.hashicorp.com/packer/" version
                           "/packer_" version "_linux_amd64.zip"))
       (sha256
        (base32 "0ccwln9bdfw7xz4wc3af7w1qa78q3b0aqf8nmfkccmnq1warphxr"))))
    (build-system copy-build-system)
    (inputs (list unzip))
    (arguments
     '(#:install-plan '(("packer" "bin/"))))
    (home-page "https://www.hashicorp.com/products/packer")
    (synopsis "Packer standardizes and automates the process of building
images")
    (description "Packer is a tool for creating identical machine images for
multiple platforms from a single source configuration.")
    (license license:mpl2.0)))

(define-public terraform-bin
  (package
    (name "terraform")
    (version "1.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.hashicorp.com/terraform/" version
                           "/terraform_" version "_linux_amd64.zip"))
       (sha256
        (base32 "0161zjllc3sp0x82mjazq4ijn2y4vmzrd9h2l9fnf92gz02zk3ny"))))
    (build-system copy-build-system)
    (inputs (list unzip))
    (arguments
     '(#:install-plan '(("terraform" "bin/"))))
    (home-page "https://www.hashicorp.com/products/terraform")
    (synopsis
     "Infrastructure automation to provision and manage resources in any cloud or
data center")
    (description
     "Terraform enables you to safely and predictably create, change, and improve
infrastructure. It is an open source tool that codifies APIs into declarative
configuration files that can be shared amongst team members, treated as code,
edited, reviewed, and versioned.")
    (license license:mpl2.0)))
