;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Guilherme Gondim

(define-module (gmgx services security-token)
  #:use-module (gmgx packages security-token)
  #:use-module (gnu services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (yubikey-touch-detector-configuration
            home-yubikey-touch-detector-service-type))

(define-record-type* <yubikey-touch-detector-configuration>
  yubikey-touch-detector-configuration make-yubikey-touch-detector-configuration
  yubikey-touch-detector-configuration?
  (dbus?         yubikey-touch-detector-configuration-dbus?
                 (default #f)
                 (documentation "Enable D-Bus server for IPC."))
  (libnotify?    yubikey-touch-detector-configuration-libnotify?
                 (default #t)
                 (documentation "Show desktop notifications using libnotify."))
  (socket?       yubikey-touch-detector-configuration-socket?
                 (default #t)
                 (documentation "Enable Unix socket notifier (when #f, disables socket)."))
  (debug?        yubikey-touch-detector-configuration-debug?
                 (default #f)
                 (documentation "Enable debug logging.")))

(define (yubikey-touch-detector-shepherd-service config)
  (let ((dbus? (yubikey-touch-detector-configuration-dbus? config))
        (libnotify? (yubikey-touch-detector-configuration-libnotify? config))
        (socket? (yubikey-touch-detector-configuration-socket? config))
        (debug? (yubikey-touch-detector-configuration-debug? config)))
    (list (shepherd-service
           (provision '(yubikey-touch-detector))
           (modules '((shepherd support)))      ;for '%user-log-dir'
           (documentation "YubiKey touch detection daemon")
           (start #~(make-forkexec-constructor
                     (append
                      (list #$(file-append
                               yubikey-touch-detector
                               "/bin/yubikey-touch-detector"))
                      (if #$dbus? '("--dbus") '())
                      (if #$libnotify? '("--libnotify") '())
                      (if (not #$socket?) '("--no-socket") '())
                      (if #$debug? '("-v") '()))
                     #:log-file (string-append %user-log-dir "/yubikey-touch-detector.log")))
           (stop #~(make-kill-destructor))
           (respawn? #t)
           (auto-start? #t)))))

(define home-yubikey-touch-detector-service-type
  (service-type
   (name 'home-yubikey-touch-detector)
   (extensions
    (list (service-extension home-shepherd-service-type
                             yubikey-touch-detector-shepherd-service)))
   (default-value (yubikey-touch-detector-configuration))
   (description "Run yubikey-touch-detector daemon.")))
