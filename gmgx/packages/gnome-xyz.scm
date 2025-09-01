;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Guilherme Gondim

(define-module (gmgx packages gnome-xyz)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public gnome-shell-extension-espresso-git
  (let ((commit "eb9c2008ff9f0205163097bf0dd17eeeb6abb87b")
        (revision "4"))
    (package
      (name "gnome-shell-extension-espresso")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/coadmunkee/gnome-shell-extension-espresso")
               (commit commit)))
         (sha256
          (base32
           ;; guix hash -x --serializer=nar .
           "1ifml2wgg44lcrfqw9papg78qfs7c6ghg4rsdr11x4rlvc4ak12d"))
         (file-name (git-file-name name version))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan '(("espresso@coadmunkee.github.com" ,(string-append
                                                              "share/gnome-shell/extensions/"
                                                              "espresso@coadmunkee.github.com")))
         #:phases (modify-phases %standard-phases
                    (add-before 'install 'compile-schemas
                      (lambda _
                        (invoke "glib-compile-schemas" "--strict"
                         "--targetdir=espresso@coadmunkee.github.com/schemas/"
                         "espresso@coadmunkee.github.com/schemas"))))))
      (native-inputs (list `(,glib "bin")))
      (synopsis
       "Enable controlling conditions to prevent the usual auto suspend and
screensaver functions from taking effect")
      (description
       "Espresso disables the usual auto suspend and screensaver funcionality and
optionally Night Light. Choose options to show an Espresso icon in the top
panel, to enable Espresso when a fullscreen application is running, to restore
state across reboots, to provide notifications, to enable Espresso when
specific applications are running, or to pause Night Light when Espresso is
enabled or only when specific applications are running. Espresso also provides
some support for docking stations including options to enable Espresso when
charging and/or when docked to external monitors and to allow temporarily
overriding the docking support without affecting the stored state.")
      (home-page
       "https://github.com/coadmunkee/gnome-shell-extension-espresso/")
      (license license:gpl2+))))

(define-public gnome-shell-extension-hotedge-git
  (let ((commit "24a263f6d7401ffb12018ab9889c156f53f8f00e")
        (revision "4"))
    (package
      (name "gnome-shell-extension-hotedge")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jdoda/hotedge")
               (commit commit)))
         (sha256
          (base32
           ;; guix hash -x --serializer=nar .
           "0x5swrn74gv29l8jbagg5ljicijvdndzyd1c90s1iszvvjyh1wf1"))
         (file-name (git-file-name name version))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan '(("." ,(string-append "share/gnome-shell/extensions/"
                                 "hotedge@jonathan.jdoda.ca")
                           #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$"
                                             "\\.png$" "\\.xml$"
                                             "\\.compiled$")))
         #:phases (modify-phases %standard-phases
                    (add-before 'install 'compile-schemas
                      (lambda _
                        (with-directory-excursion "schemas"
                          (invoke "glib-compile-schemas" ".")))))))
      (native-inputs (list `(,glib "bin")))
      (synopsis
       "A GNOME Shell extension that replaces the top-left hot corner with a bottom
hot edge")
      (description
       "A GNOME Shell extension that adds a hot edge that activates the overview to
the bottom of the screen. This minimizes the pointer travel required to access
the dash when using the new GNOME Shell 40 overview layout.")
      (home-page "https://github.com/jdoda/hotedge")
      (license license:gpl2+))))

(define-public gnome-shell-extension-night-theme-switcher-gnome-46
  (package
    (inherit gnome-shell-extension-night-theme-switcher)
    (name "gnome-shell-extension-night-theme-switcher")
    (version "76")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://gitlab.com/rmnvgr/nightthemeswitcher-gnome-shell-extension")
             (commit version)))
       (sha256
        (base32 "1ih0d3g84cm2l3zq5a44vqcvr6nd0ljsbqfwxlfbs5gljwky3qzb"))
       (file-name (git-file-name name version))))
    (description
     "Automatically toggle your GNOME desktop's color scheme between light and
dark, switch backgrounds and run custom commands at sunset and sunrise.  This
version is compatible with GNOME 46.")))
