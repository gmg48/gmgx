;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Guilherme Gondim

(define-module (gmgx services podman)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services shells)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (podman-configuration
            home-podman-service-type
            home-podman-service))

(define-record-type* <podman-configuration>
  podman-configuration make-podman-configuration
  podman-configuration?
  (socket-path podman-configuration-socket-path
               (default "/run/user/1000/podman/podman.sock")))

(define (podman-shepherd-service config)
  (let ((socket-path (podman-configuration-socket-path config)))
    (list (shepherd-service
           (provision '(podman))
           (modules '((shepherd support)))      ;for '%user-log-dir'
           (start #~(lambda ()
                     ;; Create the podman directory if it doesn't exist
                     (let* ((runtime-dir (or (getenv "XDG_RUNTIME_DIR") "/run/user/1000"))
                            (podman-dir (string-append runtime-dir "/podman")))
                       (unless (file-exists? podman-dir)
                         (mkdir-p podman-dir)))
                     ;; Start the service
                     ((make-forkexec-constructor
                       (list "podman" "system" "service"
                             "--time=0"
                             #$(string-append "unix://" socket-path))
                       #:log-file (string-append %user-log-dir "/podman.log")))))
           (stop #~(make-kill-destructor))
           (respawn? #t)))))

(define (podman-environment-variables config)
  (let ((socket-path (podman-configuration-socket-path config)))
    `(("DOCKER_HOST" . ,(string-append "unix://" socket-path)))))

(define home-podman-service-type
  (service-type
   (name 'home-podman)
   (extensions
    (list (service-extension home-shepherd-service-type
                             podman-shepherd-service)
          (service-extension home-environment-variables-service-type
                             podman-environment-variables)))
   (default-value (podman-configuration))
   (description "Run Podman socket service for Docker compatibility.")))

(define* (home-podman-service #:optional (config (podman-configuration)))
  "Return a service that runs Podman socket service."
  (service home-podman-service-type config))
