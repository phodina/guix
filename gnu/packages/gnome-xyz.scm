;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2019, 2021 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2020 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Ellis Kenyo <me@elken.dev>
;;; Copyright © 2020 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Songlin Jiang <hollowman@hollowman.ml>
;;; Copyright © 2021, 2022 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2021 Attila Lendvai <attila@lendvai.name>
;;; Copyright © 2021 Charles Jackson <charles.b.jackson@protonmail.com>
;;; Copyright © 2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2022 Sughosha <sughosha@proton.me>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages gnome-xyz)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public arc-icon-theme
  (package
    (name "arc-icon-theme")
    (version "20161122")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/horst3180/arc-icon-theme")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ch3hp08qri93510hypzz6m2x4xgg2h15wvnhjwh1x1s1b7jvxjd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-configure-during-bootstrap
           (lambda _
             (substitute* "autogen.sh"
               (("^\"\\$srcdir/configure\".*") ""))
             #t)))))
    (native-inputs
     (list autoconf automake))
    ;; When Arc is missing an icon, it looks in the Moka icon theme for it.
    (propagated-inputs
     (list moka-icon-theme))
    (synopsis "Arc icon theme")
    (description "The Arc icon theme provides a set of icons matching the
style of the Arc GTK theme.  Icons missing from the Arc theme are provided by
the Moka icon theme.")
    (home-page "https://github.com/horst3180/arc-icon-theme")
    (license license:gpl3+)))

(define-public delft-icon-theme
  (package
    (name "delft-icon-theme")
    (version "1.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/madmaxms/iconpack-delft")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1iw85cxx9lv7irs28qi3815dk9f9vldv2j7jf1x5l1dqzwaxgwpb"))
       (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("." "share/icons" #:exclude ("README.md" "LICENSE" "logo.jpg")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-index.theme
           (lambda _
            (substitute* "Delft/index.theme"
              (("gnome") "Adwaita"))
            #t)))))
    (home-page "https://www.gnome-look.org/p/1199881/")
    (synopsis "Continuation of Faenza icon theme with up to date app icons")
    (description "Delft is a fork of the popular icon theme Faenza with up to
date app icons.  It will stay optically close to the original Faenza icons,
which haven't been updated for some years.  The new app icons are ported from
the Obsidian icon theme.")
    (license license:gpl3)))

(define-public faba-icon-theme
  (package
    (name "faba-icon-theme")
    (version "4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snwh/faba-icon-theme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xh6ppr73p76z60ym49b4d0liwdc96w41cc5p07d48hxjsa6qd6n"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-post-install
           (lambda _
             (substitute* "meson.build"
               (("meson.add_install_script.*") "")))))))
    (native-inputs
     (list autoconf automake))
    (synopsis "Faba icon theme")
    (description
     "Faba is a minimal icon set used as a basis for other themes such as
Moka")
    (home-page "https://snwh.org/moka")
    (license (list license:lgpl3+
                   license:cc-by-sa4.0))))

(define-public moka-icon-theme
  (package
    (inherit faba-icon-theme)
    (name "moka-icon-theme")
    (version "5.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snwh/moka-icon-theme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "015l02im4mha5z91dbchxf6xkp66d346bg3xskwg0rh3lglhjsrd"))))
    (propagated-inputs
     ;; Moka is based on Faba by using it as a fallback icon set instead of
     ;; bundling it, so we need to add it as a propagated input.
     (list faba-icon-theme))
    (synopsis "Moka icon theme")
    (description "Moka is a stylized desktop icon set, designed to be clear,
simple and consistent.")
    (license (list license:gpl3+
                   license:cc-by-sa4.0))))

(define-public papirus-icon-theme
  (package
    (name "papirus-icon-theme")
    (version "20220508")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PapirusDevelopmentTeam/papirus-icon-theme")
             (commit version)))
       (sha256
        (base32 "0rpcniaw8xbn23q67m26vgx3fynn4v056azrfp63lxdh46gfsvmc"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no test suite
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (delete 'build))))
    (native-inputs
     (list `(,gtk+ "bin")))
    (home-page "https://git.io/papirus-icon-theme")
    (synopsis "Fork of Paper icon theme with a lot of new icons and a few extras")
    (description "Papirus is a fork of the icon theme Paper with a lot of new icons
and a few extra features.")
    (license license:gpl3)))

(define-public gnome-plots
  (package
    (name "gnome-plots")
    (version "0.6.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alexhuntley/Plots")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "168wcsrkmvq79xmwvbq615msd4q0rg7f57xqicidnr78jx4x37rd"))))
    (build-system python-build-system)
    (inputs
     (list bash-minimal                 ; for wrap-program
           gtk+
           pango
           python-freetype-py
           python-jinja2
           python-lark-parser
           python-numpy
           python-pycairo
           python-pyglm
           python-pygobject
           python-pyopengl))
    (native-inputs
     (list python-pytest))
    (arguments
     (list
      #:imported-modules `((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
      #:modules '((guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils)
                  (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'adjust-lark-requirement
            (lambda _
              (substitute* "setup.py"
                (("lark") "lark-parser"))))
          (add-after 'install 'install-more
            (lambda _
              (let* ((datadir (string-append #$output "/share"))
                     (help (string-append datadir "/help"))
                     (icons (string-append datadir "/icons/hicolor")))
                (map (lambda (filename)
                       (match (string-split filename #\/)
                         ((_ lang dir ... name)
                          (install-file filename
                                        (string-join (cons* help lang "plots" dir)
                                                     "/")))))
                     (find-files "help"))
                (install-file "res/com.github.alexhuntley.Plots.desktop"
                              (string-append datadir "/applications/"))
                (install-file "res/com.github.alexhuntley.Plots.svg"
                              (string-append icons "/scalable/apps/"))
                (install-file "res/com.github.alexhuntley.Plots-symbolic.svg"
                              (string-append icons "/symbolic/apps/")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv"))))
          (add-after 'wrap 'gi-wrap
            (lambda _
              (let ((prog (string-append #$output "/bin/plots")))
                (wrap-program prog
                  `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))
          (add-after 'wrap 'glib-or-gtk-wrap
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (home-page "https://apps.gnome.org/app/com.github.alexhuntley.Plots/")
    (synopsis "Simple graph plotting")
    (description "Plots makes it easy to visualise mathematical formulae.  In
addition to basic arithmetic operations, it supports trigonometric,
hyperbolic, exponential, and logarithmic functions, as well as arbitrary sums
and products.  Plots is designed to integrate well with the GNOME desktop and
takes advantage of modern hardware using OpenGL.")
    (license license:gpl3+)))

(define-public gnome-shell-extension-unite-shell
  (package
    (name "gnome-shell-extension-unite-shell")
    (version "65")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hardpixel/unite-shell")
                    (commit "127edac6396b89cdedec003bdff38820e6a0f91f")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lzhf7hlvzg62nxjfpv265315qibcjz5dv08dzpfckf2dx68nab4"))))
    (build-system copy-build-system)
    (native-inputs (list `(,glib "bin") gettext-minimal))
    (inputs (list xprop))
    (arguments
     (list #:install-plan ''(("./unite@hardpixel.eu"
                              "share/gnome-shell/extensions/unite@hardpixel.eu"))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-xprop-bin
                          (lambda _
                            (substitute* "unite@hardpixel.eu/window.js"
                              (("xprop")
                               (string-append #$(this-package-input "xprop")
                                              "/bin/xprop")))))
                        (add-before 'install 'compile-schemas
                          (lambda _
                            (with-directory-excursion "unite@hardpixel.eu/schemas"
                              (invoke "glib-compile-schemas" ".")))))))
    (home-page "https://github.com/hardpixel/unite-shell")
    (synopsis "Top panel and window decoration extension for GNOME Shell")
    (description
     "Unite is a GNOME Shell extension which makes a few layout
tweaks to the top panel and removes window decorations to make it look like
Ubuntu Unity Shell.")
    (license license:gpl3)))

(define-public gnome-shell-extension-appindicator
  (package
    (name "gnome-shell-extension-appindicator")
    (version "42")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/ubuntu/gnome-shell-extension-appindicator")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1lf3aqb924nzhj87rhy2zvm5pcfqcklhfw21m6ic3i7wzd9r7cnc"))
              (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs (list jq gnu-gettext
                         `(,glib "bin")))
    (synopsis "Adds KStatusNotifierItem support to GNOME Shell")
    (description "This extension integrates Ubuntu AppIndicators
and KStatusNotifierItems (KDE's successor of the systray) into
GNOME Shell.")
    (home-page "https://github.com/ubuntu/gnome-shell-extension-appindicator/")
    (license license:gpl2+)))

(define-public gnome-shell-extension-clipboard-indicator
  (package
    (name "gnome-shell-extension-clipboard-indicator")
    (version "42")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     (string-append
                      "https://github.com/Tudmotu/"
                      "gnome-shell-extension-clipboard-indicator"))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wf2k33pbwjdf8i4y3aw32fgvjbh751qh7504lwhnl02rcq5dc88"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove pre-compiled settings schemas and translations from
               ;; source, as they are generated as part of build. Upstream
               ;; includes them for people who want to run the software
               ;; directly from source tree.
               '(begin
                  (delete-file "schemas/gschemas.compiled")
                  (for-each delete-file
                            (find-files "locale" "\\.mo$"))))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("." "share/gnome-shell/extensions/clipboard-indicator@tudmotu.com"
          #:include-regexp ("\\.css$" "\\.compiled$" "\\.js(on)?$" "\\.mo$" "\\.xml$")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'compile-schemas
           (lambda _
             (with-directory-excursion "schemas"
               (invoke "glib-compile-schemas" ".")))))))
    (native-inputs
     (list `(,glib "bin") gettext-minimal))
    (home-page "https://github.com/Tudmotu/gnome-shell-extension-clipboard-indicator")
    (synopsis "Clipboard manager extension for GNOME Shell")
    (description "Clipboard Indicator is a clipboard manager for GNOME Shell
that caches clipboard history.")
    (license license:expat)))

(define-public gnome-shell-extension-customize-ibus
  (package
    (name "gnome-shell-extension-customize-ibus")
    (version "82")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openSUSE/Customize-IBus.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00brnyahphl4ql9yh74wpb9kmzyb4b5k4rkw40hvxvqw4qwgs24r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "VERSION=" ,version)
             (string-append "INSTALLBASE=" (assoc-ref %outputs "out")
                            "/share/gnome-shell/extensions"))
       #:tests? #f ; No test target
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")))
    (home-page "https://github.com/openSUSE/Customize-IBus")
    (synopsis "GNOME Shell Extension for IBus Customization")
    (description "Customize IBus provides full customization of appearance,
behavior, system tray and input source indicator for IBus.")
    (license license:gpl3+)))

(define-public gnome-shell-extension-topicons-redux
  (package
    (name "gnome-shell-extension-topicons-redux")
    (version "6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/pop-planet/TopIcons-Redux.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dli9xb545n3xlj6q4wl0y5gzkm903zs47p8fiq71pdvbr6v38rj"))))
    (build-system gnu-build-system)
    (native-inputs
     (list `(,glib "bin")))
    (arguments
     `(#:tests? #f                      ;no test defined in the project
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make"
                       "install"
                       (string-append
                        "INSTALL_PATH="
                        out
                        "/share/gnome-shell/extensions"))))))))
    (home-page "https://gitlab.com/pop-planet/TopIcons-Redux")
    (synopsis "Display legacy tray icons in the GNOME Shell top panel")
    (description "Many applications, such as chat clients, downloaders, and
some media players, are meant to run long-term in the background even after you
close their window.  These applications remain accessible by adding an icon to
the GNOME Shell Legacy Tray.  However, the Legacy Tray was removed in GNOME
3.26.  TopIcons Redux brings those icons back into the top panel so that it's
easier to keep track of applications running in the background.")
    (license license:gpl2+)))

(define-public gnome-shell-extension-dash-to-dock
  (package
    (name "gnome-shell-extension-dash-to-dock")
    (version "73")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/micheleg/dash-to-dock")
                    (commit (string-append "extensions.gnome.org-v"
                                           version))))
              (sha256
               (base32
                "1l0isbrgfc8v46l1yc5l4myz7qnlxzyfyiifipp86z9d79d8klzw"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:make-flags (list (string-append "INSTALLBASE="
                                         (assoc-ref %outputs "out")
                                         "/share/gnome-shell/extensions"))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("sassc" ,sassc)))
    (propagated-inputs
     (list glib))
    (synopsis "Transforms GNOME's dash into a dock")
    (description "This extension moves the dash out of the
overview, transforming it into a dock for easier application launching and
faster window switching.")
    (home-page "https://micheleg.github.io/dash-to-dock/")
    (license license:gpl2+)))

(define-public gnome-shell-extension-gsconnect
  (package
    (name "gnome-shell-extension-gsconnect")
    (version "50")       ; See GNOME Shell supported versions in metadata.json
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/GSConnect"
                                        "/gnome-shell-extension-gsconnect.git"))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vg87fdihs5kp7apgyd32ldjmwzmrxaimsc005yjyy8m3f65sjmr"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f ;; every test fails
       #:configure-flags
       (let* ((out (assoc-ref %outputs "out"))
              (name+version (strip-store-file-name out))
              (gschema-dir (string-append out
                                          "/share/gsettings-schemas/"
                                          name+version
                                          "/glib-2.0/schemas"))
              (gnome-shell (assoc-ref %build-inputs "gnome-shell"))
              (openssh (assoc-ref %build-inputs "openssh"))
              (openssl (assoc-ref %build-inputs "openssl")))
         (list
          (string-append "-Dgnome_shell_libdir=" gnome-shell "/lib")
          (string-append "-Dgsettings_schemadir=" gschema-dir)
          (string-append "-Dopenssl_path=" openssl "/bin/openssl")
          (string-append "-Dsshadd_path=" openssh "/bin/ssh-add")
          (string-append "-Dsshkeygen_path=" openssh "/bin/ssh-keygen")
          (string-append "-Dsession_bus_services_dir=" out "/share/dbus-1/services")
          "-Dpost_install=true"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((glib (assoc-ref inputs "glib:bin"))
                    (gapplication (string-append glib "/bin/gapplication"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (substitute* "data/org.gnome.Shell.Extensions.GSConnect.desktop.in"
                 (("gapplication") gapplication))
               (for-each
                (lambda (file)
                  (substitute* file
                    (("'use strict';")
                     (string-append "'use strict';\n\n"
                                    "'" gi-typelib-path "'.split(':').forEach("
                                    "path => imports.gi.GIRepository.Repository."
                                    "prepend_search_path(path));"))))
                '("src/extension.js" "src/prefs.js"))
               #t)))
         (add-after 'install 'wrap-daemons
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (service-dir
                     (string-append out "/share/gnome-shell/extensions"
                                    "/gsconnect@andyholmes.github.io/service"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append service-dir "/daemon.js")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))
               #t))))))
    (inputs
     `(("at-spi2-core" ,at-spi2-core)
       ("caribou" ,caribou)
       ("evolution-data-server" ,evolution-data-server)
       ("gjs" ,gjs)
       ("glib" ,glib)
       ("glib:bin" ,glib "bin")
       ("gsound" ,gsound)
       ("gnome-shell" ,gnome-shell)
       ("gtk+" ,gtk+)
       ("nautilus" ,nautilus)
       ("openssh" ,openssh)
       ("openssl" ,openssl)
       ("python-pygobject" ,python-pygobject)
       ("upower" ,upower)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/GSConnect/gnome-shell-extension-gsconnect/wiki")
    (synopsis "Connect GNOME Shell with your Android phone")
    (description "GSConnect is a complete implementation of KDE Connect
especially for GNOME Shell, allowing devices to securely share content, like
notifications or files, and other features like SMS messaging and remote
control.")
    (license license:gpl2)))

(define-public gnome-shell-extension-hide-app-icon
  (let ((commit "4188aa5f4ba24901a053a0c3eb0d83baa8625eab")
        (revision "0"))
    (package
      (name "gnome-shell-extension-hide-app-icon")
      (version (git-version "2.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url (string-append "https://github.com/michael-rapp"
                                   "/gnome-shell-extension-hide-app-icon.git"))
               (commit commit)))
         (sha256
          (base32
           "1i28n4bz6wrhn07vpxkr6l1ljyn7g8frp5xrr11z3z32h2hxxcd6"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f                ; no test target
         #:make-flags (list (string-append "EXTENSIONS_DIR="
                                           (assoc-ref %outputs "out")
                                           "/share/gnome-shell/extensions"))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)      ; no configure script
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (pre "/share/gnome-shell/extensions/")
                     (dir "hide-app-icon@mrapp.sourceforge.com"))
                 (copy-recursively dir (string-append out pre dir))
                 #t))))))
      (native-inputs
       (list `(,glib "bin") intltool))
      (propagated-inputs
       (list glib))
      (synopsis "Hide app icon from GNOME's panel")
      (description "This extension hides the icon and/or title of the
currently focused application in the top panel of the GNOME shell.")
      (home-page
       "https://github.com/michael-rapp/gnome-shell-extension-hide-app-icon/")
      (license
        ;; README.md and LICENSE.txt disagree -- the former claims v3, the
        ;; latter v2.  No mention of "or later" in either place or in the code.
        (list license:gpl2
              license:gpl3)))))

(define-public gnome-shell-extension-just-perfection
  (package
    (name "gnome-shell-extension-just-perfection")
    (version "22.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/jrahmatzadeh/just-perfection/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r4rflppcp05kwhzmh07dzi7znc4kch4nc8mzw61arj3qsfq2qqj"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("src"
          "share/gnome-shell/extensions/just-perfection-desktop@just-perfection"
          #:include-regexp ("\\.css$" "\\.compiled$" "\\.js(on)?$" "\\.ui$"))
         ("locale"
          "share/gnome-shell/extensions/just-perfection-desktop@just-perfection/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'drop-executable-bits
           (lambda _
             (for-each
              (lambda (file)
                (let ((stat (lstat file)))
                  (chmod file (logand (stat:mode stat) (lognot #o111)))))
              (find-files "." #:directories? #f))))
         (add-before 'install 'build
           (lambda _
             (invoke "glib-compile-schemas" "src/schemas")
             (for-each
              (lambda (file)
                (let* ((base (basename file))
                       (noext (substring base 0 (- (string-length base) 3)))
                       (dest (string-append "locale/" noext "/LC_MESSAGES/"))
                       (out (string-append dest "just-perfection.mo")))
                  (mkdir-p dest)
                  (invoke "msgfmt" "-c" file "-o" out)))
              (find-files "po" "\\.po$")))))))
    (native-inputs
     (list `(,glib "bin") gettext-minimal))
    (home-page "https://gitlab.gnome.org/jrahmatzadeh/just-perfection")
    (synopsis "Customize GNOME Shell behaviour")
    (description "Just Perfection allows you to change various settings, that
GNOME Shell itself does not provide out of the box, such as the ability to hide
certain elements or change animation speeds.")
    (license license:gpl3)))

(define-public gnome-shell-extension-dash-to-panel
  (package
    (name "gnome-shell-extension-dash-to-panel")
    (version "51")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/home-sweet-gnome/dash-to-panel")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "103pl77dhafi2ayds5yma2smv3b58zcysnd6vl5m5zavjvk35sz7"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "INSTALLBASE="
                                         (assoc-ref %outputs "out")
                                         "/share/gnome-shell/extensions")
                          (string-append "VERSION="
                                         ,(package-version
                                           gnome-shell-extension-dash-to-panel)))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure))))
    (native-inputs
     (list intltool pkg-config))
    (propagated-inputs
     (list glib
           `(,glib "bin")))
    (synopsis "Icon taskbar for GNOME Shell")
    (description "This extension moves the dash into the gnome main
panel so that the application launchers and system tray are combined
into a single panel, similar to that found in KDE Plasma and Windows 7+.")
    (home-page "https://github.com/home-sweet-gnome/dash-to-panel/")
    (license license:gpl2+)))

(define-public gnome-shell-extension-noannoyance
  (let ((revision "1")
        (commit "b759d10fd2799bc084007fdd927b62637c3dbd2c"))
    (package
      (name "gnome-shell-extension-noannoyance")
      ;; XXX: There is no version noted anywhere in the source.  Thus, infer it
      ;;      from <https://extensions.gnome.org/extension/2182/noannoyance/>.
      (version (git-version "16" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/bdaase/noannoyance")
                      (commit commit)))
                (sha256
                 (base32
                  "0hh7fdqvx54h9j41ia2jl0nq1d5i66k7blw41ya6hkh7201r4anp"))
                (file-name (git-file-name name version))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("." "share/gnome-shell/extensions/noannoyance@daase.net"))))
      (synopsis "Remove 'Window is ready' annotation")
      (description "One of the many extensions that remove this message.
It uses ES6 syntax and claims to be more actively maintained than others.")
      (home-page "https://extensions.gnome.org/extension/2182/noannoyance/")
      (license license:gpl2))))

(define-public gnome-shell-extension-paperwm
  (package
    (name "gnome-shell-extension-paperwm")
    (version "36.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/paperwm/PaperWM")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ssnabwxrns36c61ppspjkr9i3qifv08pf2jpwl7cjv3pvyn4kly"))
              (snippet
               '(begin (delete-file "schemas/gschemas.compiled")))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("." "share/gnome-shell/extensions/paperwm@hedning:matrix.org"
          #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                            "\\.xml$" "\\.compiled$")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'compile-schemas
           (lambda _
             (with-directory-excursion "schemas"
               (invoke "make"))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin"))) ; for glib-compile-schemas
    (home-page "https://github.com/paperwm/PaperWM")
    (synopsis "Tiled scrollable window management for GNOME Shell")
    (description "PaperWM is an experimental GNOME Shell extension providing
scrollable tiling of windows and per monitor workspaces.  It's inspired by paper
notebooks and tiling window managers.")
    (license license:gpl3)))

(define-public gpaste
  (package
    (name "gpaste")
    (version "42.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Keruspe/GPaste")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dlqa69zvzzdxyh21qfrx2nhpfy0fbihxpgkxqmramcgv3h5k4q3"))
              (patches
               (search-patches "gpaste-fix-paths.patch"))))
    (build-system meson-build-system)
    (native-inputs
     (list gettext-minimal
           gobject-introspection
           (list glib "bin")            ; for glib-compile-resources
           pkg-config
           vala))
    (inputs
     (list appstream-glib
           gjs
           gtk
           mutter
           libadwaita
           libarchive))
    (arguments
     (list #:glib-or-gtk? #true
           #:configure-flags
           #~(list
              (string-append "-Dcontrol-center-keybindings-dir="
                             #$output "/share/gnome-control-center/keybindings")
              (string-append "-Ddbus-services-dir="
                             #$output "/share/dbus-1/services")
              (string-append "-Dsystemd-user-unit-dir="
                             #$output "/etc/systemd/user"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-introspection-install-dir
                 (lambda _
                   (substitute* '("src/gnome-shell/extension.js"
                                  "src/gnome-shell/prefs.js")
                     (("@typelibPath@")
                      (string-append #$output "/lib/girepository-1.0/"))))))))
    (home-page "https://github.com/Keruspe/GPaste")
    (synopsis "Clipboard management system for GNOME Shell")
    (description "GPaste is a clipboard manager, a tool which allows you to
keep a trace of what you’re copying and pasting.  Is is really useful when
you go through tons of documentation and you want to keep around a bunch of
functions you might want to use, for example.  The clipboard manager will
store an history of everything you do, so that you can get back to older
copies you now want to paste.")
    (license license:bsd-2)))

(define-public gnome-shell-extension-vertical-overview
  (package
    (name "gnome-shell-extension-vertical-overview")
    (version "9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/RensAlthuis/vertical-overview")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0pkby00rjipj04z68d6i3rr7mzm01dckf2vl3iz6yvbl39602icl"))
       (file-name (git-file-name name version))
       (snippet
        '(begin (delete-file "schemas/gschemas.compiled")))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." ,(string-append
                "share/gnome-shell/extensions/"
                "vertical-overview@RensAlthuis.github.com")
          #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                            "\\.xml$" "\\.compiled$")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'compile-schemas
           (lambda _
             (with-directory-excursion "schemas"
               (invoke "glib-compile-schemas" ".")))))))
    (native-inputs
     (list `(,glib "bin")))  ; for glib-compile-resources
    (home-page "https://github.com/RensAlthuis/vertical-overview")
    (synopsis "Provides a vertical overview in Gnome 40 and upper")
    (description "This Gnome extension replaces the new horizontally oriented
Gnome overview with something that resembles the old vertically oriented
style.")
    (license license:gpl3)))

(define-public gnome-shell-extension-jiggle
  (package
    (name "gnome-shell-extension-jiggle")
    (version "8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jeffchannell/jiggle/")
             (commit version)))
       (sha256
        (base32
         "1wbdx2bp22bdwj51ckgivwglkmckr7z8kfwvc8nv4y376hjz5jxz"))
       (file-name (git-file-name name version))
       (snippet
        '(begin (delete-file "schemas/gschemas.compiled")))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." ,(string-append
                "share/gnome-shell/extensions/"
                "jiggle@jeffchannell.com")
          #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                            "\\.xml$" "\\.compiled$")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-version
           (lambda _
             (substitute* "metadata.json"
               (("\"40.0\"") "\"40\", \"41\""))))
         (add-before 'install 'compile-schemas
           (lambda _
             (with-directory-excursion "schemas"
               (invoke "glib-compile-schemas" ".")))))))
    (native-inputs
     (list `(,glib "bin")))  ; for glib-compile-resources
    (home-page "https://github.com/jeffchannell/jiggle")
    (synopsis "Mouse cursor enlargement for small and fast movements")
    (description "Jiggle is a Gnome Shell extension that highlights the cursor
position when the mouse is moved rapidly.")
    (license license:gpl2)))

(define-public tophat
  (package
    (name "tophat")
    (version "2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fflewddur/tophat")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0pl1s2k729abgcvsw5crh7vhrkbbzac1fr7f0w4m13jvblwvwmb2"))
       (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." ,(string-append
                "share/gnome-shell/extensions/"
                "tophat@fflewddur.github.com")
          #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                            "\\.xml$" "\\.compiled$" "\\.gresource$")))))
    (home-page "https://github.com/fflewddur/tophat")
    (synopsis "System resource monitor for the GNOME shell")
    (description "TopHat is an elegant system resource monitor for the GNOME shell.  It displays CPU, memory, and network activity in the GNOME top bar.")
    (license license:gpl3+)))

(define-public gnome-shell-extension-rounded-window-corners
  (package
    (name "gnome-shell-extension-rounded-window-corners")
    (version "5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yilozt/rounded-window-corners")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0pl1s2k729abgcvsw5crh7vhrkbbzac1fr7f0w4m13jvblwvwmb8"))
       (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." ,(string-append
                "share/gnome-shell/extensions/"
                "rounded-window-corners@yilozt.github.com")
          #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                            "\\.xml$" "\\.compiled$" "\\.gresource$")))))
    (home-page "https://github.com/yilozt/rounded-window-corners")
    (synopsis "Add rounded corners for all windows")
    (description "This gnome-shell extension try to add rounded corners for all
windows.")
    (license license:gpl3)))

(define-public gnome-shell-extension-burn-my-windows
  (package
    (name "gnome-shell-extension-burn-my-windows")
    (version "21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Schneegans/Burn-My-Windows/")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "07ckfl47pq83nhb77v230zfxlz3imga3s8nn3sr9cq4zxvbkj2r4"))
       (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." ,(string-append
                "share/gnome-shell/extensions/"
                "burn-my-windows@schneegans.github.com")
          #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                            "\\.xml$" "\\.compiled$" "\\.gresource$")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'compile-resources
           (lambda _
             (invoke "make" "resources/burn-my-windows.gresource")))
         (add-before 'install 'compile-schemas
           (lambda _
             (with-directory-excursion "schemas"
               (invoke "glib-compile-schemas" ".")))))))
    (native-inputs
     (list `(,glib "bin")))  ; for glib-compile-resources
    (home-page "https://github.com/Schneegans/Burn-My-Windows")
    (synopsis "Application closing effects extension")
    (description "Burn My Windows is a shell extension that stylizes the
animation of closing windowed applications.")
    (license license:gpl3)))

(define-public gnome-shell-extension-blur-my-shell
  (package
    (name "gnome-shell-extension-blur-my-shell")
    (version "44")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aunetx/blur-my-shell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0h7yfvrrg5r821mzrp42c09jws06mw6v9avvkfykqj8n8qnslmyx"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       (let ((extension "share/gnome-shell/extensions/blur-my-shell@aunetx"))
         `(("src/" ,extension)
           ("resources/" ,extension
            #:include-regexp ("\\.svg$" "\\.ui"))
           ("." ,extension
            #:exclude-regexp ("src/" "resources/")
            #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                              "\\.xml$" "\\.compiled$"))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'compile-schemas
           (lambda _
             (with-directory-excursion "schemas"
               (invoke "glib-compile-schemas" ".")))))))
    (native-inputs
     (list (list glib "bin"))) ; for glib-compile-schemas
    (home-page "https://github.com/aunetx/blur-my-shell")
    (synopsis "Blurs different parts of the GNOME Shell")
    (description "Blur My Shell adds a blur look to different parts of the
GNOME Shell, including the top panel, dash and overview.")
    (license license:gpl3)))

(define-public gnome-shell-extension-radio
  (package
    (name "gnome-shell-extension-radio")
    (version "19")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/hslbck/gnome-shell-extension-radio")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qsi6c57hxh4jqdw18knm06601lhag6jdbvzg0r79aa9572zy8a0"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("radio@hslbck.gmail.com"
                          "/share/gnome-shell/extensions/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'glib-compile-schemas
            (lambda _
              (invoke "glib-compile-schemas"
                      "radio@hslbck.gmail.com/schemas"))))))
    (native-inputs (list `(,glib "bin")))
    (home-page "https://github.com/hslbck/gnome-shell-extension-radio")
    (synopsis "Internet radio for GNOME Shell")
    (description "This extension implements an internet radio player
directly inside GNOME Shell.  It can manage stations and play streams.")
    (license license:gpl3+)))

(define-public gnome-shell-extension-sound-output-device-chooser
  (package
    (name "gnome-shell-extension-sound-output-device-chooser")
    (version "43")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kgshank/gse-sound-output-device-chooser")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1qk6ypyqbv8zwwlky6cgk9hgp1zh32jmzw4wza200g4v94ifkwm9"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; no check target
      #:make-flags #~(list (string-append "INSTALL_DIR="
                                          #$output
                                          "/share/gnome-shell/extensions"))
      #:phases
      #~(modify-phases %standard-phases (delete 'configure))))
    (native-inputs (list gettext-minimal `(,glib "bin")))
    (inputs (list python))
    (home-page
     "https://extensions.gnome.org/extension/906/sound-output-device-chooser")
    (synopsis "Sound output chooser for GNOME Shell")
    (description "This extension shows a list of sound output and input devices
in the status menu below the volume slider.  Various active ports like HDMI,
Speakers etc. of the same device are also displayed for selection.")
    (license license:gpl3+)))

(define-public gnome-shell-extension-transparent-window
  (let ((commit "cc9bc70c192dd565fa6f1d1b28d9a20f99684f2a")
        (revision "45"))
    (package
      (name "gnome-shell-extension-transparent-window")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url (string-append "https://github.com/pbxqdown/"
                                   "gnome-shell-extension-transparent-window"))
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1f9iqqjpmmylqz0ws8cy5rs475bwzi7jy44q9ip44ig2acz2wxzp"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~'(("."
             #$(string-append "/share/gnome-shell/extensions"
                              "/transparent-window@pbxqdown.github.com")))))
      (home-page
       "https://github.com/pbxqdown/gnome-shell-extension-transparent-window")
      (synopsis "Change the opacity of windows in GNOME Shell")
      (description "This extension adds keybindings to change the opacity
of windows.")
      (license license:expat))))

(define-public arc-theme
  (package
    (name "arc-theme")
    (version "20220405")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jnsh/arc-theme")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gjwf75sg4xyfypb08qiy2cmqyr2mamjc4i46ifrq7snj15gy608"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags
       '("-Dthemes=gnome-shell,gtk2,gtk3,metacity,plank,unity,xfwm")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-home   ;placate Inkscape
           (lambda _
             (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list `(,glib "bin") ; for glib-compile-resources
           gnome-shell
           gtk+
           inkscape/stable
           optipng
           pkg-config
           python
           sassc/libsass-3.5))
    (synopsis "Flat GTK+ theme with transparent elements")
    (description "Arc is a flat theme with transparent elements for GTK 3, GTK
2, and GNOME Shell which supports GTK 3 and GTK 2 based desktop environments
like GNOME, Unity, Budgie, Pantheon, XFCE, Mate, etc.")
    (home-page "https://github.com/horst3180/arc-theme")
    ;; No "or later" language found.
    (license license:gpl3+)))

(define-public greybird-gtk-theme
  (package
    (name "greybird-gtk-theme")
    (version "3.22.13")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/shimmerproject/Greybird")
                (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "154qawiga792iimkpk3a6q8f4gm4r158wmsagkbqqbhj33kxgxhg"))))
    (build-system meson-build-system)
    (native-inputs
     (list gtk+
           `(,glib "bin") ; for "glib-compile-resources"
           (librsvg-for-system)
           pkg-config
           ruby-sass
           sassc))
    (home-page "https://shimmerproject.org/")
    (synopsis "Grey GTK+ theme based on Bluebird")
    (description "Greybird is a grey derivative of the Bluebird theme by the
Shimmer Project.  It supports GNOME, Unity, and Xfce.")
    (license (list license:gpl2+ license:cc-by-sa3.0))))

(define-public matcha-theme
  (package
    (name "matcha-theme")
    (version "2021-01-01")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://github.com/vinceliuice/Matcha-gtk-theme")
            (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1pa6ra87wlq0gwz4n03l6xv0pxiamr5dygycvppms8v6xyc2aa0r"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (source (assoc-ref %build-inputs "source"))
                (bash (assoc-ref %build-inputs "bash"))
                (coreutils (assoc-ref %build-inputs  "coreutils"))
                (themesdir (string-append out "/share/themes")))
           (setenv "PATH"
                   (string-append coreutils "/bin:"
                                  (string-append bash "/bin:")))
           (copy-recursively source (getcwd))
           (patch-shebang "install.sh")
           (mkdir-p themesdir)
           (invoke "./install.sh" "-d" themesdir)
           #t))))
    (inputs
     (list gtk-engines))
    (native-inputs
     (list bash coreutils))
    (synopsis "Flat design theme for GTK 3, GTK 2 and GNOME-Shell")
    (description "Matcha is a flat Design theme for GTK 3, GTK 2 and
Gnome-Shell which supports GTK 3 and GTK 2 based desktop environments
like Gnome, Unity, Budgie, Pantheon, XFCE, Mate and others.")
    (home-page "https://github.com/vinceliuice/matcha")
    (license license:gpl3+)))

(define-public materia-theme
  (package
    (name "materia-theme")
    (version "20200916")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://github.com/nana-4/materia-theme")
            (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0qaxxafsn5zd2ysgr0jyv5j73360mfdmxyd55askswlsfphssn74"))))
    (build-system meson-build-system)
    (native-inputs
     (list gtk+ sassc))
    (home-page "https://github.com/nana-4/materia-theme")
    (synopsis "Material Design theme for a wide range of environments")
    (description "Materia is a Material Design theme for GNOME/GTK based
desktop environments.  It supports GTK 2, GTK 3, GNOME Shell, Budgie,
Cinnamon, MATE, Unity, Xfce, LightDM, GDM, Chrome theme, etc.")
    (license license:gpl2+)))

(define-public numix-gtk-theme
  (package
    (name "numix-gtk-theme")
    (version "2.6.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/numixproject/numix-gtk-theme")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12mw0kr0kkvg395qlbsvkvaqccr90cmxw5rrsl236zh43kj8grb7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "INSTALL_DIR="
                            (assoc-ref %outputs "out")
                            "/share/themes/Numix"))
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (native-inputs
     (list `(,glib "bin")               ; for glib-compile-schemas
           gnome-shell
           gtk+
           libxml2
           ruby-sass))
    (synopsis "Flat theme with light and dark elements")
    (description "Numix is a modern flat theme with a combination of light and
dark elements.  It supports GNOME, Unity, Xfce, and Openbox.")
    (home-page "https://numixproject.github.io")
    (license license:gpl3+)))

(define-public orchis-theme
  (package
    (name "orchis-theme")
    (version "2021-02-28")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://github.com/vinceliuice/Orchis-theme")
            (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
           "1qp3phiza93qllrjm5xjjca5b7l2sbng8c382khy9m97grxvcq0y"))
        (modules '((guix build utils)
                   (ice-9 regex)
                   (srfi srfi-26)))
        (snippet
         '(begin
            (for-each
             (lambda (f)
               (let* ((r (make-regexp "\\.scss"))
                      (f* (regexp-substitute #f (regexp-exec r f) 'pre ".css")))
                 (if (file-exists? f*)
                     (delete-file f*))))
             (find-files "." ".*\\.scss"))
            #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list
                          "--dest" (string-append
                                    (assoc-ref %outputs "out")
                                    "/share/themes")
                          "--theme" "all"
                          "--radio-color")
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (replace 'build (lambda _ (invoke "./parse-sass.sh")))
         (replace 'install
           (lambda* (#:key configure-flags #:allow-other-keys)
             (mkdir-p
              (cadr (or (member "--dest" configure-flags)
                        (member "-d" configure-flags))))
             (apply invoke "./install.sh" configure-flags)
             #t)))))
    (inputs
     (list gtk-engines))
    (native-inputs
     (list ;("coreutils" ,coreutils)
           gtk+ sassc))
    (home-page "https://github.com/vinceliuice/Orchis-theme")
    (synopsis "Material Design theme for a wide range of environments")
    (description "Orchis is a Material Design them for GNOME/GTK based
desktop environments.  It is based on materia-theme and adds more color
variants.")
    (license (list license:gpl3            ; According to COPYING.
                   license:lgpl2.1         ; Some style sheets.
                   license:cc-by-sa4.0)))) ; Some icons

(define-public eiciel
  (package
    (name "eiciel")
    (version "0.9.13.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rofirrim/eiciel")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rhhw0h1hyg5kvxhjxkdz03vylgax6912mg8j4lvcz6wlsa4wkvj"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:tests? #f ; no tests
       #:configure-flags
       (list (string-append "-Dnautilus-extension-dir="
                            (assoc-ref %outputs "out")
                            "/lib/nautilus/site-extensions"))))
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list acl attr glibmm-2.64 gtkmm-3 nautilus))
    (home-page "https://rofi.roger-ferrer.org/eiciel")
    (synopsis "Manage extended file attributes")
    (description "Eiciel is a plugin for nautilus to graphically edit ACL and
extended file attributes.  It also functions as a standalone command.")
    (license license:gpl2+)))

(define-public markets
  (package
    (name "markets")
    (version "0.5.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/bitstower/markets")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0ch6dfmdcpw32r23s58riv8agnyw0f1cqd1y6j7zkx5sb3zyn3zy"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson/postinstall.py"
               (("gtk-update-icon-cache") "true"))))
         (add-after 'unpack 'skip-update-desktop-database
           ;; Don't update desktop file database.
           (lambda _
             (substitute* "build-aux/meson/postinstall.py"
               (("update-desktop-database") "true")))))))
    (inputs
     (list gtk+
           gettext-minimal
           gsettings-desktop-schemas
           libgee
           libhandy
           libsoup-minimal-2
           json-glib
           vala))
    (native-inputs
     (list pkg-config
           python-wrapper
           `(,glib "bin"))) ; for 'glib-compile-resources'
    (home-page "https://github.com/bitstower/markets")
    (synopsis "Stock, currency and cryptocurrency tracker")
    (description
     "Markets is a GTK application that displays financial data, helping users
track stocks, currencies and cryptocurrencies.")
    (license license:gpl3+)))

(define-public vala-language-server
  (package
    (name "vala-language-server")
    ;; Note to maintainer: VLS must be built with a Vala toolchain the same
    ;; version or newer. Therefore when you update this package you may need
    ;; to update Vala too.
    (version "0.48.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/benwaffle/vala-language-server")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1gnvc91gdp3wj9r3r3xxfr09f9lw39cfypn2q5f0443dhhmp059j"))))
    (build-system meson-build-system)
    (arguments '(#:glib-or-gtk? #t))
    (inputs
     (list glib json-glib jsonrpc-glib libgee vala))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/benwaffle/vala-language-server")
    (synopsis "Language server for Vala")
    (description "The Vala language server is an implementation of the Vala
language specification for the Language Server Protocol (LSP).  This tool is
used in text editing environments to provide a complete and integrated
feature-set for programming Vala effectively.")
    (license license:lgpl2.1+)))

(define-public nordic-theme
  (let ((commit "07d764c5ebd5706e73d2e573f1a983e37b318915")
	(revision "0"))
  (package
   (name "nordic-theme")
   (version (git-version "1.9.0" revision commit))
   (source
     (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/EliverLara/Nordic")
             (commit commit)))
     (sha256
       (base32
         "0y2s9d6h1b195s6afp1gb5rb1plfslkpbw2brd30a9d66wfvsqk0"))
     (file-name (git-file-name name version))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      `(("." "share/themes/nord"
         #:exclude ("README.md" "LICENSE" "Art/" "package.json"
                    "package-lock.json" "Gulpfile.js")))))
   (home-page "https://github.com/EliverLara/Nordic")
   (synopsis "Dark Gtk3.20+ theme using the Nord color palette")
   (description "Nordic is a Gtk3.20+ theme created using the Nord color
palette.")
   (license license:gpl3))))

(define-public tiramisu
  (package
    (name "tiramisu")
    (version "2.0.20211107")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Sweets/tiramisu")
                    (commit version)))
              (sha256
               (base32
                "1n1x1ybbwbanibw7b90k7v4cadagl41li17hz2l8s2sapacvq3mw"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "tiramisu" (string-append out "/bin"))
               #t))))
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target)))))
    (inputs
     (list glib))
    (native-inputs
     (list pkg-config vala))
    (home-page "https://github.com/Sweets/tiramisu")
    (synopsis "Desktop notifications, the UNIX way")
    (description "tiramisu is a notification daemon based on dunst that outputs
notifications to STDOUT in order to allow the user to process notifications any
way they prefer.")
    (license license:expat)))
