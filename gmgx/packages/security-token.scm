;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Guilherme Gondim

(define-module (gmgx packages security-token)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system copy)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages hardware))

(define-public age-plugin-yubikey-bin
  (let* ((bin-platform "x86_64-linux")
         (bin-version "0.5.0")
         (bin-name (string-append "age-plugin-yubikey-v" bin-version "-"
                                  bin-platform ".tar.gz")))
    (package
      (name "age-plugin-yubikey")
      (version "0.5.0")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/str4d/age-plugin-yubikey"
                             "/releases/download/"
                             "v"
                             version
                             "/"
                             bin-name))
         (sha256
          (base32 "1hxja5ziy4c1cf1wdhinr42bsbq8laq3swnhfdnya6y87yhkb6q1"))))
      (build-system binary-build-system)
      (inputs `((,gcc "lib")
                ,gcc-toolchain
                ,pcsc-lite))
      (propagated-inputs (list))
      (arguments
       (list
        #:patchelf-plan
        #~'(("age-plugin-yubikey" ("pcsc-lite" "gcc" "gcc-toolchain"))
            ;; #:include "age-plugin-yubikey"
            )
        #:install-plan
        #~'(("age-plugin-yubikey" "bin/"))))
      (home-page "https://github.com/str4d/age-plugin-yubikey")
      (synopsis "YubiKey plugin for age")
      (description
       "age-plugin-yubikey is a plugin for age clients like age and rage, which
enables files to be encrypted to age identities stored on YubiKeys.")
      (license license:expat))))

(define-public yubikey-touch-detector
  (package
    (name "yubikey-touch-detector")
    (version "1.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/maximbaz/yubikey-touch-detector/releases/download/"
             version "/yubikey-touch-detector-" version "-vendored.tar.gz"))
       (sha256
        (base32 "0xfx4w62r06n2dl4ydczi5xwn7svnmc9g7ypqb5k5rknk4xg9smd"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/maximbaz/yubikey-touch-detector"
       #:unpack-path "github.com/maximbaz/yubikey-touch-detector"
       #:install-source? #f))
    (native-inputs (list go pkg-config))
    (inputs (list gpgme))
    (home-page "https://github.com/maximbaz/yubikey-touch-detector")
    (synopsis "Detect when YubiKey is waiting for touch")
    (description "Tool to detect when your YubiKey is waiting for a touch
and send desktop notifications.")
    (license license:isc)))
