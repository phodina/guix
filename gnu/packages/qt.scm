;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Quiliro <quiliro@fsfla.org>
;;; Copyright © 2017, 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020, 2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Mike Rosset <mike.rosset@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2020 TomZ <tomz@freedommail.ch>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021, 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2022 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
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

(define-module (gnu packages qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1))

(define-public qite
  (let ((commit "75fb3b6bbd5c6a5a8fc35e08a6efbfb588ed546a")
        (revision "74"))
    (package
      (name "qite")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Ri0n/qite")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0jmmgy9pvk9hwwph1nwy7hxhczy8drhl4ymhnjjn6yx7bckssvsq"))))
      (build-system qt-build-system)
      (arguments
       `(#:tests? #f                    ; no target
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "libqite"))))))
      (inputs
       (list qtbase-5 qtmultimedia-5))
      (home-page "https://github.com/Ri0n/qite/")
      (synopsis "Qt Interactive Text Elements")
      (description "Qite manages interactive elements on QTextEdit.")
      (license license:asl2.0))))

(define-public qt5ct
  (package
    (name "qt5ct")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/qt5ct/qt5ct-" version ".tar.bz2"))
       (sha256
        (base32 "14742vs32m98nbfb5mad0i8ciff5f45gfcb5v03p4hh2dvhhqgfn"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f                      ; No target
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              (substitute* '("src/qt5ct-qtplugin/CMakeLists.txt"
                             "src/qt5ct-style/CMakeLists.txt")
                (("\\$\\{PLUGINDIR\\}")
                 (string-append #$output "/lib/qt5/plugins"))))))))
    (native-inputs
     (list qttools-5))
    (inputs
     (list qtsvg-5))
    (synopsis "Qt5 Configuration Tool")
    (description "Qt5CT is a program that allows users to configure Qt5
settings (such as icons, themes, and fonts) in desktop environments or
window managers, that don't provide Qt integration by themselves.")
    (home-page "https://qt5ct.sourceforge.io/")
    (license license:bsd-2)))

(define-public materialdecoration
  (let ((commit "6a5de23f2e5162fbee39d16f938473ff970a2ec0")
        (revision "9"))
    (package
      (name "materialdecoration")
      (version
       (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/lirios/materialdecoration.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "1zdrcb39fhhmn76w8anv1dnspz26pdl6izmj1mlm02aza4y8ffp4"))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (srfi srfi-1)))
         (snippet
          `(begin
             (delete-file-recursively "cmake/3rdparty")))))
      (build-system qt-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:configure-flags
         ,#~(list
             (string-append "-DCMAKE_CXX_FLAGS=-I"
                            #$(this-package-input "qtbase")
                            "/include/qt5/QtXkbCommonSupport/"
                            #$(package-version qtbase-5)))))
      (native-inputs
       (list cmake-shared extra-cmake-modules pkg-config))
      (inputs
       (list qtbase-5
             qtwayland-5
             wayland
             libxkbcommon))
      (synopsis "Material Decoration for Qt")
      (description "MaterialDecoration is a client-side decoration for Qt
applications on Wayland.")
      (home-page "https://github.com/lirios/materialdecoration")
      (license license:lgpl3+))))

(define-public grantlee
  (package
    (name "grantlee")
    (version "5.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/steveire/grantlee")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02dyqxjyxiqxrlz5g7v9ly8f095vs3iha39l75q6s8axs36y01lq"))))
    (native-inputs
     ;; Optional: lcov and cccc, both are for code coverage
     (list doxygen))
    (inputs
     (list qtbase-5 qtdeclarative-5 qtscript))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (home-page "https://github.com/steveire/grantlee")
    (synopsis "Libraries for text templating with Qt")
    (description "Grantlee Templates can be used for theming and generation of
other text such as code.  The syntax uses the syntax of the Django template
system, and the core design of Django is reused in Grantlee.")
    (license license:lgpl2.1+)))

(define (qt-urls component version)
  "Return a list of URLs for VERSION of the Qt5 COMPONENT."
  ;; We can't use a mirror:// scheme because these URLs are not exact copies:
  ;; the layout differs between them.
  (list (string-append "https://download.qt.io/official_releases/qt/"
                       (version-major+minor version) "/" version
                       "/submodules/" component "-everywhere-opensource-src-"
                       version ".tar.xz")
        (string-append "https://download.qt.io/official_releases/qt/"
                       (version-major+minor version) "/" version
                       "/submodules/" component "-everywhere-src-"
                       version ".tar.xz")
        (string-append "https://download.qt.io/archive/qt/"
                       (version-major+minor version) "/" version
                       "/submodules/" component "-everywhere-opensource-src-"
                       version ".tar.xz")
        (let ((directory (string-append "qt5" (string-drop component 2))))
          (string-append "http://sources.buildroot.net/" directory "/"
                         component "-everywhere-opensource-src-" version ".tar.xz"))
        (string-append "https://distfiles.macports.org/qt5/"
                       component "-everywhere-opensource-src-" version ".tar.xz")))

(define-public qtbase-5
  (package
    (name "qtbase")
    (version "5.15.5")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "1p2fa94m1y8qzhdfi2d7dck93qh1lgsinibwl1wy92bwmacwfhhc"))
              ;; Use TZDIR to avoid depending on package "tzdata".
              (patches (search-patches "qtbase-use-TZDIR.patch"
                                       "qtbase-moc-ignore-gcc-macro.patch"
                                       "qtbase-absolute-runpath.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; corelib uses bundled harfbuzz, md4, md5, sha3
               '(begin
                  (with-directory-excursion "src/3rdparty"
                    (for-each delete-file-recursively
                              (list "double-conversion" "freetype" "harfbuzz-ng"
                                    "libpng" "libjpeg" "pcre2" "sqlite" "xcb"
                                    "zlib")))))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (propagated-inputs
     (list mesa
           ;; Use which the package, not the function
           (@ (gnu packages base) which)))
    (inputs
     (list alsa-lib
           cups
           dbus
           double-conversion
           eudev
           expat
           fontconfig
           freetype
           glib
           gtk+                   ;for GTK theme support
           harfbuzz
           icu4c
           libinput-minimal
           libjpeg-turbo
           libmng
           libpng
           libx11
           libxcomposite
           libxcursor
           libxfixes
           libxi
           libxinerama
           libxkbcommon
           libxml2
           libxrandr
           libxrender
           libxslt
           libxtst
           mtdev
           `(,mariadb "dev")
           nss
           openssl
           pcre2
           postgresql
           pulseaudio
           sqlite
           unixodbc
           xcb-util
           xcb-util-image
           xcb-util-keysyms
           xcb-util-renderutil
           xcb-util-wm
           xdg-utils
           zlib))
    (native-inputs
     (list bison
           flex
           gperf
           perl
           pkg-config
           python
           vulkan-headers
           ruby))
    (arguments
     (list #:configure-flags
       #~(let ((out #$output))
         (list "-verbose"
               "-prefix" out
               "-docdir" (string-append out "/share/doc/qt5")
               "-headerdir" (string-append out "/include/qt5")
               "-archdatadir" (string-append out "/lib/qt5")
               "-datadir" (string-append out "/share/qt5")
               "-examplesdir" (string-append
                               out "/share/doc/qt5/examples")
               "-opensource"
               "-confirm-license"

               ;; Later stripped into the :debug output.
               "-force-debug-info"

               ;; These features require higher versions of Linux than the
               ;; minimum version of the glibc.  See
               ;; src/corelib/global/minimum-linux_p.h.  By disabling these
               ;; features Qt5 applications can be used on the oldest
               ;; kernels that the glibc supports, including the RHEL6
               ;; (2.6.32) and RHEL7 (3.10) kernels.
               "-no-feature-getentropy" ; requires Linux 3.17
               "-no-feature-renameat2"  ; requires Linux 3.16

               ;; Do not build examples; if desired, these could go
               ;; into a separate output, but for the time being, we
               ;; prefer to save the space and build time.
               "-no-compile-examples"
               ;; Most "-system-..." are automatic, but some use
               ;; the bundled copy by default.
               "-system-sqlite"
               "-system-harfbuzz"
               "-system-pcre"
               ;; explicitly link with openssl instead of dlopening it
               "-openssl-linked"
               ;; explicitly link with dbus instead of dlopening it
               "-dbus-linked"
               ;; don't use the precompiled headers
               "-no-pch"
               ;; drop special machine instructions that do not have
               ;; runtime detection
               #$@(if (string-prefix? "x86_64"
                                     (or (%current-target-system)
                                         (%current-system)))
                     '()
                     '("-no-sse2"))
               "-no-mips_dsp"
               "-no-mips_dspr2"))
       #:phases
       #~(modify-phases %standard-phases
         (add-after 'configure 'patch-bin-sh
           (lambda _
             (substitute* '("config.status"
                            "configure"
                            "mkspecs/features/qt_functions.prf"
                            "qmake/library/qmakebuiltins.cpp")
               (("/bin/sh") (which "sh")))))
         (add-after 'configure 'patch-xdg-open
           (lambda _
             (substitute* '("src/platformsupport/services/genericunix/qgenericunixservices.cpp")
               (("^.*const char \\*browsers.*$" all)
                (string-append "*browser = QStringLiteral(\""
                               (which "xdg-open")
                               "\"); return true; \n" all)))))
         (replace 'configure
           ;; Overridden to not pass "--enable-fast-install", which makes the
           ;; configure process fail.
           (lambda* (#:key outputs configure-flags #:allow-other-keys)
               (substitute* "configure"
                 (("/bin/pwd") (which "pwd")))
               (substitute* "src/corelib/global/global.pri"
                 (("/bin/ls") (which "ls")))
               ;; The configuration files for other Qt5 packages are searched
               ;; through a call to "find_package" in Qt5Config.cmake, which
               ;; disables the use of CMAKE_PREFIX_PATH via the parameter
               ;; "NO_DEFAULT_PATH". Re-enable it so that the different
               ;; components can be installed in different places.
               (substitute* (find-files "." ".*\\.cmake")
                 (("NO_DEFAULT_PATH") ""))
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" configure-flags)
               (apply invoke "./configure" configure-flags)))
         (add-after 'install 'patch-mkspecs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out #$output)
                    (archdata (string-append out "/lib/qt5"))
                    (mkspecs (string-append archdata "/mkspecs"))
                    (qt_config.prf (string-append
                                    mkspecs "/features/qt_config.prf")))
               ;; For each Qt module, let `qmake' uses search paths in the
               ;; module directory instead of all in QT_INSTALL_PREFIX.
               (substitute* qt_config.prf
                 (("\\$\\$\\[QT_INSTALL_HEADERS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../include/qt5))")
                 (("\\$\\$\\[QT_INSTALL_LIBS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
                 (("\\$\\$\\[QT_HOST_LIBS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
                 (("\\$\\$\\[QT_INSTALL_BINS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../bin))"))

               ;; Searches Qt tools in the current PATH instead of QT_HOST_BINS.
               (substitute* (string-append mkspecs "/features/qt_functions.prf")
                 (("cmd = \\$\\$\\[QT_HOST_BINS\\]/\\$\\$2")
                  "cmd = $$system(which $${2}.pl 2>/dev/null || which $${2})"))

               ;; Resolve qmake spec files within qtbase by absolute paths.
               (substitute*
                   (map (lambda (file)
                          (string-append mkspecs "/features/" file))
                        '("device_config.prf" "moc.prf" "qt_build_config.prf"
                          "qt_config.prf" "winrt/package_manifest.prf"))
                 (("\\$\\$\\[QT_HOST_DATA/get\\]") archdata)
                 (("\\$\\$\\[QT_HOST_DATA/src\\]") archdata)))))
         (add-after 'patch-mkspecs 'patch-prl-files
           (lambda* (#:key outputs #:allow-other-keys)
               ;; Insert absolute references to the qtbase libraries because
               ;; QT_INSTALL_LIBS does not always resolve correctly, depending
               ;; on context.  See <https://bugs.gnu.org/38405>
               (substitute* (find-files (string-append #$output "/lib") "\\.prl$")
                 (("\\$\\$\\[QT_INSTALL_LIBS\\]")
                  (string-append #$output "/lib")))))
         (add-after 'unpack 'patch-paths
           ;; Use the absolute paths for dynamically loaded libs, otherwise
           ;; the lib will be searched in LD_LIBRARY_PATH which typically is
           ;; not set in guix.
           (lambda* (#:key inputs #:allow-other-keys)
             ;; libresolve
             (let ((glibc (assoc-ref inputs #$(if (%current-target-system)
                                                 "cross-libc" "libc"))))
               (substitute* '("src/network/kernel/qdnslookup_unix.cpp"
                              "src/network/kernel/qhostinfo_unix.cpp")
                 (("^\\s*(lib.setFileName\\(QLatin1String\\(\")(resolv\"\\)\\);)" _ a b)
                  (string-append a glibc "/lib/lib" b))))
             ;; libGL
             (substitute* "src/plugins/platforms/xcb/gl_integrations/xcb_glx/qglxintegration.cpp"
               (("^\\s*(QLibrary lib\\(QLatin1String\\(\")(GL\"\\)\\);)" _ a b)
                (string-append a #$mesa "/lib/lib" b)))
             ;; libXcursor
             (substitute* "src/plugins/platforms/xcb/qxcbcursor.cpp"
               (("^\\s*(QLibrary xcursorLib\\(QLatin1String\\(\")(Xcursor\"\\), 1\\);)" _ a b)
                (string-append a #$libxcursor "/lib/lib" b))
               (("^\\s*(xcursorLib.setFileName\\(QLatin1String\\(\")(Xcursor\"\\)\\);)" _ a b)
                (string-append a #$libxcursor "/lib/lib" b))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "QMAKEPATH")
            (files '("lib/qt5")))
           (search-path-specification
            (variable "QML2_IMPORT_PATH")
            (files '("lib/qt5/qml")))
           (search-path-specification
            (variable "QT_PLUGIN_PATH")
            (files '("lib/qt5/plugins")))
           (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))
           (search-path-specification
            (variable "XDG_CONFIG_DIRS")
            (files '("etc/xdg")))))
    (home-page "https://www.qt.io/")
    (synopsis "Cross-platform GUI library")
    (description "Qt is a cross-platform application and UI framework for
developers using C++ or QML, a CSS & JavaScript like language.")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public qtbase
  (package/inherit qtbase-5
    (name "qtbase")
    (version "6.3.1")
    (source (origin
              (inherit (package-source qtbase-5))
              (uri (qt-urls name version))
              (sha256
               (base32
                "00sfya41ihqb0zwg6wf1kiy02iymj6mk584hhk2c4s94khfl4r0a"))
              (modules '((guix build utils)))
              (snippet
               ;; corelib uses bundled harfbuzz, md4, md5, sha3
               '(with-directory-excursion "src/3rdparty"
                  (for-each delete-file-recursively
                            ;; The bundled pcre2 copy is kept, as its headers
                            ;; are required by some internal bootstrap target
                            ;; used for the tools.
                            (list "double-conversion" "freetype" "harfbuzz-ng"
                                  "libpng" "libjpeg" "sqlite" "xcb" "zlib"))))))
    (build-system cmake-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments qtbase-5)
       ((#:configure-flags _ ''())
        #~(let ((out #$output))
           (list "-GNinja"              ;the build fails otherwise
                 "-DQT_BUILD_TESTS=ON"
                 (string-append "-DINSTALL_ARCHDATADIR=" out "/lib/qt6")
                 (string-append "-DINSTALL_DATADIR=" out "/share/qt6")
                 (string-append "-DINSTALL_DOCDIR=" out "/share/doc/qt6")
                 (string-append "-DINSTALL_MKSPECSDIR=" out "/lib/qt6/mkspecs")
                 (string-append "-DINSTALL_EXAMPLESDIR=" out
                                "/share/doc/qt6/examples")
                 (string-append "-DINSTALL_INCLUDEDIR=" out "/include/qt6")
                 ;; Link with DBus and OpenSSL so they don't get dlopen'ed.
                 "-DINPUT_dbus=linked"
                 "-DINPUT_openssl=linked"
                 ;; These features require higher versions of Linux than the
                 ;; minimum version of the glibc.  See
                 ;; src/corelib/global/minimum-linux_p.h.  By disabling these
                 ;; features Qt applications can be used on the oldest kernels
                 ;; that the glibc supports, including the RHEL6 (2.6.32) and
                 ;; RHEL7 (3.10) kernels.
                 "-DFEATURE_getentropy=OFF" ; requires Linux 3.17
                 "-DFEATURE_renameat2=OFF"  ; requires Linux 3.16
                 ;; Most system libraries are used by default, except in some
                 ;; cases such as for those below.
                 "-DFEATURE_system_pcre2=ON"
                 "-DFEATURE_system_sqlite=ON"
                 "-DFEATURE_system_xcb_xinput=ON"
                 ;; Don't use the precompiled headers.
                 "-DBUILD_WITH_PCH=OFF"
                 ;; Drop special machine instructions that do not have runtime
                 ;; detection.
                 #$@(if (string-prefix? "x86_64"
                                       (or (%current-target-system)
                                           (%current-system)))
                       '()              ;implicitly enabled
                       '("-DFEATURE_sse2=OFF"
                         "-DFEATURE_sse3=OFF"
                         "-DFEATURE_ssse3=OFF"
                         "-DFEATURE_sse4_1=OFF"
                         "-DFEATURE_sse4_2=OFF"))
                 "-DFEATURE_mips_dsp=OFF"
                 "-DFEATURE_mips_dspr2=OFF")))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'honor-CMAKE_PREFIX_PATH
              (lambda _
                ;; The configuration files for other Qt packages are searched
                ;; through a call to "find_package" in Qt5Config.cmake, which
                ;; disables the use of CMAKE_PREFIX_PATH via the parameter
                ;; "NO_DEFAULT_PATH".  Re-enable it so that the different
                ;; components can be installed in different places.
                (substitute* (find-files "." "\\.cmake(\\.in)?$")
                  (("\\bNO_DEFAULT_PATH\\b") ""))
                ;; Because Qt goes against the grain of CMake and set
                ;; NO_DEFAULT_PATH, it needs to invent yet another variable
                ;; to do what CMAKE_PREFIX_PATH could have done:
                ;; QT_ADDITIONAL_PACKAGES_PREFIX_PATH.  Since we patch out
                ;; the NO_DEFAULT_PATH, we can set the default value of
                ;; QT_ADDITIONAL_PACKAGES_PREFIX_PATH to that of
                ;; CMAKE_PREFIX_PATH to ensure tools such as
                ;; 'qmlimportscanner' from qtdeclarative work out of the
                ;; box.
                (substitute* "cmake/QtConfig.cmake.in"
                  (("(set\\(QT_ADDITIONAL_PACKAGES_PREFIX_PATH )\"\"" _ head)
                   (string-append head "\"$ENV{CMAKE_PREFIX_PATH}\"")))))
            (delete 'patch-bin-sh)
            (delete 'patch-xdg-open)
            (add-after 'patch-paths 'patch-more-paths
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* (find-files "bin" "\\.in$")
                  (("/bin/pwd")
                   (search-input-file inputs "bin/pwd")))
                (substitute* "src/gui/platform/unix/qgenericunixservices.cpp"
                  (("\"xdg-open\"")
                   (format #f "~s" (search-input-file inputs "bin/xdg-open"))))
                (substitute* '("mkspecs/features/qt_functions.prf"
                               "qmake/library/qmakebuiltins.cpp")
                  (("/bin/sh")
                   (search-input-file inputs "bin/bash")))
                (substitute* "src/corelib/CMakeLists.txt"
                  (("/bin/ls")
                   (search-input-file inputs "bin/ls")))))
            (replace 'configure
              (assoc-ref %standard-phases 'configure))
            (replace 'build
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (apply invoke "cmake" "--build" "."
                       (if parallel-build?
                           `("--parallel" ,(number->string (parallel-job-count)))
                           '()))))
            (delete 'check)             ;move after patch-prl-files
            (add-after 'patch-prl-files 'check
              (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
                (when tests?
                  ;; The tests expect to find the modules provided by this
                  ;; package; extend the environment variables needed to do so.
                  (setenv "CMAKE_PREFIX_PATH"
                          (string-append #$output
                                         ":" (getenv "CMAKE_PREFIX_PATH")))
                  (setenv "QMAKEPATH" (string-append #$output "/lib/qt6"))
                  (setenv "QML2_IMPORT_PATH"
                          (string-append #$output "/lib/qt6/qml"))
                  (setenv "QT_PLUGIN_PATH"
                          (string-append #$output "/lib/qt6/plugins"))
                  (setenv "QT_QPA_PLATFORM" "offscreen")
                  ;; Skip tests known to fail on GNU/Linux, in a CI context or
                  ;; due to bitness (see: https://code.qt.io/cgit/qt/qtbase.git
                  ;; /tree/src/testlib/qtestblacklist.cpp).
                  (setenv "QTEST_ENVIRONMENT" "linux ci 32bit")
                  (setenv "HOME" "/tmp") ;some tests require a writable HOME
                  (invoke
                   "xvfb-run" "ctest" "--output-on-failure"
                   "-j" (if parallel-tests?
                            (number->string (parallel-job-count))
                            "1")
                   "-E"                 ;disable problematic tests
                   (string-append
                    "("
                    (string-join
                     (list
                      ;; The 'tst_moc' test fails with "'fi.exists()' returned FALSE".
                      "tst_moc"
                      ;; The 'test_rcc' test fails on a comparison:
                      ;; <<<<<< actual
                      ;; 0x0,0x0,0x0,0x0,0x0,0x0,0x3,0xe8,
                      ;; ======
                      ;; 0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,
                      ;; >>>>>> expected
                      "tst_rcc"
                      ;; The 'tst_qtemporarydir' and 'tst_qtemporaryfile'
                      ;; tests depend on '/home' not being writable.
                      "tst_qtemporarydir"
                      "tst_qtemporaryfile"
                      ;; The 'tst_qdir' compares two directories which are
                      ;; unexpectedly different when inside the build
                      ;; container.
                      "tst_qdir"
                      ;; This checks the last modified time of '/', and fails
                      ;; because Epoch 0 is considered to be invalid.
                      "tst_qresourceengine"
                      ;; The 'tst_qfilesystemwatcher' installs a watcher on
                      ;; '/home', which doesn't exist in the build container.
                      "tst_qfilesystemwatcher"
                      ;; The 'mockplugins' test fail following error: "Unknown
                      ;; platform linux-g++", and the other plugin tests
                      ;; depend on it.
                      "mockplugins"
                      "test_plugin_flavor.*"
                      ;; The 'test_import_plugins' fails with "Could NOT find
                      ;; Qt6MockPlugins1".
                      "test_import_plugins"
                      ;; The 'tst_QTimeZone::systemZone' validates the
                      ;; currently set timezone and fails.
                      "tst_qtimezone"
                      ;; The 'tst_qdatetime' fails with:
                      ;; FAIL!  : tst_QDateTime::offsetFromUtc() Compared values are not the same
                      ;; Actual   (dt5.offsetFromUtc()): 0
                      ;; Expected (46800)              : 46800
                      "tst_qdatetime"
                      ;; The 'tst_QSettings::fromFile' assumes the data
                      ;; location to be relative to the root directory and
                      ;; fails.
                      "tst_qsettings"
                      ;; The 'tst_qaddpreroutine',
                      ;; 'test_generating_cpp_exports' and
                      ;; 'test_static_resources' tests fail with: "Unknown
                      ;; platform linux-g++.
                      "tst_qaddpreroutine"
                      "test_generating_cpp_exports"
                      "test_static_resources"
                      ;; The 'tst_qfile' fails since there is no /home in the
                      ;; build container.
                      "tst_qfile"
                      ;; The 'tst_QGlyphRun::mixedScripts' test fails with:
                      ;; Actual   (glyphRuns.size()): 1
                      ;; Expected (2)               : 2
                      "tst_qglyphrun"
                      ;; The 'tst_qx11info' test fails with "Internal error:
                      ;; QPA plugin doesn't implement generatePeekerId",
                      ;; likely requires a real display.
                      "tst_qx11info"
                      ;; The 'tst_qgraphicswidget' test fails because "This
                      ;; plugin does not support propagateSizeHints".
                      "tst_qgraphicswidget"
                      ;; The 'tst_qdnslookup' test requires networking.
                      "tst_qdnslookup"
                      ;; The 'tst_qcompleter' and 'tst_QFiledialog::completer'
                      ;; attempt to complete paths they assume exist, such as
                      ;; "/home", "/etc" or "/root" and fail.
                      "tst_qcompleter"
                      "tst_qfiledialog") "|")
                    ")")))))
            (replace 'install
              (lambda _
                (invoke "cmake" "--install" ".")))
            (replace 'patch-mkspecs
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((archdata (search-input-directory outputs "lib/qt6"))
                       (mkspecs (search-input-directory outputs
                                                        "lib/qt6/mkspecs"))
                       (qt_config.prf
                        (search-input-file
                         outputs "lib/qt6/mkspecs/features/qt_config.prf"))
                       (qt_functions.prf
                        (search-input-file
                         outputs "lib/qt6/mkspecs/features/qt_functions.prf")))
                  ;; For each Qt module, let `qmake' uses search paths in the
                  ;; module directory instead of all in QT_INSTALL_PREFIX.
                  (substitute* qt_config.prf
                    (("\\$\\$\\[QT_INSTALL_HEADERS\\]")
                     "$$clean_path($$replace(dir, mkspecs/modules, ../../include/qt6))")
                    (("\\$\\$\\[QT_INSTALL_LIBS\\]")
                     "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
                    (("\\$\\$\\[QT_HOST_LIBS\\]")
                     "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
                    (("\\$\\$\\[QT_INSTALL_BINS\\]")
                     "$$clean_path($$replace(dir, mkspecs/modules, ../../bin))"))

                  ;; Searches Qt tools in the current PATH instead of QT_HOST_BINS.
                  (substitute* qt_functions.prf
                    (("cmd = \\$\\$\\[QT_HOST_BINS\\]/\\$\\$2")
                     "cmd = $$system(which $${2}.pl 2>/dev/null || which $${2})"))

                  ;; Resolve qmake spec files within qtbase by absolute paths.
                  (substitute*
                      (map (lambda (file)
                             (search-input-file
                              outputs
                              (string-append "lib/qt6/mkspecs/features/" file)))
                           '("device_config.prf" "moc.prf" "qt_build_config.prf"
                             "qt_config.prf"))
                    (("\\$\\$\\[QT_HOST_DATA/get\\]") archdata)
                    (("\\$\\$\\[QT_HOST_DATA/src\\]") archdata)))))))))
    (native-inputs
     (modify-inputs (package-native-inputs qtbase-5)
       (prepend ninja
                wayland-protocols
                xvfb-run)))
    (inputs
     (modify-inputs (package-inputs qtbase-5)
       (prepend bash-minimal coreutils-minimal libxcb md4c)
       (replace "postgresql" postgresql))) ;use latest postgresql
    (native-search-paths
     (list (search-path-specification
            (variable "QMAKEPATH")
            (files '("lib/qt6")))
           (search-path-specification
            (variable "QML2_IMPORT_PATH")
            (files '("lib/qt6/qml")))
           (search-path-specification
            (variable "QT_PLUGIN_PATH")
            (files '("lib/qt6/plugins")))
           (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))
           (search-path-specification
            (variable "XDG_CONFIG_DIRS")
            (files '("etc/xdg")))))))

(define-public qt3d-5
  (package
    (inherit qtbase-5)
    (name "qt3d")
    (version "5.15.5")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "1m3y7d58crn0qgfwkimxcggssn2pbs8nj5b9diwns6rwqg4aqk20"))))
    (propagated-inputs `())
    (native-inputs (list perl))
    (inputs (list mesa qtbase-5 vulkan-headers zlib))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-before 'configure 'configure-qmake
                          (lambda* (#:key inputs outputs #:allow-other-keys)
                            (let* ((tmpdir (string-append (getenv "TMPDIR")))
                                   (qmake (string-append tmpdir "/qmake"))
                                   (qt.conf (string-append tmpdir "/qt.conf")))
                              (symlink (which "qmake") qmake)
                              (setenv "PATH"
                                      (string-append tmpdir ":"
                                                     (getenv "PATH")))
                              (with-output-to-file qt.conf
                                (lambda ()
                                  (format #t "[Paths]
Prefix=~a
ArchData=lib/qt5
Data=share/qt5
Documentation=share/doc/qt5
Headers=include/qt5
Libraries=lib
LibraryExecutables=lib/qt5/libexec
Binaries=bin
Tests=tests
Plugins=lib/qt5/plugins
Imports=lib/qt5/imports
Qml2Imports=lib/qt5/qml
Translations=share/qt5/translations
Settings=etc/xdg
Examples=share/doc/qt5/examples
HostPrefix=~a
HostData=lib/qt5
HostBinaries=bin
HostLibraries=lib

[EffectiveSourcePaths]
HostPrefix=~a
HostData=lib/qt5"
                                          #$output #$output #$(this-package-input
                                                               "qtbase")))))))
                        (replace 'configure
                          (lambda* (#:key inputs outputs #:allow-other-keys)
                            (invoke "qmake"
                                    "QT_BUILD_PARTS = libs tools tests")))
                        (add-before 'check 'set-display
                          (lambda _
                            (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (synopsis "Qt module for 3D")
    (description "The Qt3d module provides classes for displaying 3D.")))

(define-public qt5compat
  (package
    (name "qt5compat")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "1zbcaswpl79ixcxzj85qzjq73962s4c7316pibwfrskqswmwcgm4"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DQT_BUILD_TESTS=ON")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'delete-installed-tests
                     (lambda _
                       (delete-file-recursively
                        (string-append #$output "/tests")))))))
    (native-inputs (list perl))
    (inputs (list icu4c libxkbcommon qtbase qtdeclarative qtshadertools))
    (home-page (package-home-page qtbase))
    (synopsis "Legacy Qt 5 APIs ported to Qt 6")
    (description "The @code{qt5compat} package includes application
programming interfaces (APIs) from Qt 5 that were ported to Qt 6, to ease
migration.  It provides for example the @code{GraphicalEffects} module that
came with the @{qtgraphicaleffects} Qt 5 package.")
    (license (list license:gpl2+ license:lgpl3+)))) ;dual licensed

(define-public qtsvg-5
  (package
    (inherit qtbase-5)
    (name "qtsvg")
    (version "5.15.5")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "0cdhmhxngv4y7kl5vbcii4l4anbz0hj7dvhlddy1agyl19j9xky4"))))
    (propagated-inputs `())
    (native-inputs (list perl))
    (inputs
     (list mesa qtbase-5 zlib))
    (arguments
     (list #:phases
       #~(modify-phases %standard-phases
         (add-before 'configure 'configure-qmake
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out #$output)
                    (qtbase #$qtbase-5)
                    (tmpdir (string-append (getenv "TMPDIR")))
                    (qmake (string-append tmpdir "/qmake"))
                    (qt.conf (string-append tmpdir "/qt.conf")))
               ;; Use qmake with a customized qt.conf to override install
               ;; paths to $out.
               (symlink (which "qmake") qmake)
               (setenv "PATH" (string-append tmpdir ":" (getenv "PATH")))
               (with-output-to-file qt.conf
                 (lambda ()
                   (format #t "[Paths]
Prefix=~a
ArchData=lib/qt5
Data=share/qt5
Documentation=share/doc/qt5
Headers=include/qt5
Libraries=lib
LibraryExecutables=lib/qt5/libexec
Binaries=bin
Tests=tests
Plugins=lib/qt5/plugins
Imports=lib/qt5/imports
Qml2Imports=lib/qt5/qml
Translations=share/qt5/translations
Settings=etc/xdg
Examples=share/doc/qt5/examples
HostPrefix=~a
HostData=lib/qt5
HostBinaries=bin
HostLibraries=lib

[EffectiveSourcePaths]
HostPrefix=~a
HostData=lib/qt5
" out out qtbase))))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Valid QT_BUILD_PARTS variables are:
             ;; libs tools tests examples demos docs translations
             (invoke "qmake" "QT_BUILD_PARTS = libs tools tests")))
         (add-before 'check 'set-display
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (synopsis "Qt module for displaying SVGs")
    (description "The QtSvg module provides classes for displaying the
 contents of SVG files.")))

(define-public qtsvg
  (package
    (name "qtsvg")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "1xvxz2jfpr1al85rhwss7ji5vkxa812d0b888hry5f7pwqcg86bv"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DQT_BUILD_TESTS=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-display
            (lambda _
              ;; Make Qt render "offscreen", required for tests.
              (setenv "QT_QPA_PLATFORM" "offscreen")))
          (add-after 'install 'delete-installed-tests
            (lambda _
              (delete-file-recursively (string-append #$output "/tests")))))))
    (native-inputs (list perl))
    (inputs (list libxkbcommon mesa qtbase zlib))
    (synopsis "Qt module for displaying SVGs")
    (description "The QtSvg module provides classes for displaying the
 contents of SVG files.")
    (home-page (package-home-page qtbase))
    (license (package-license qtbase))))

(define-public qtimageformats
  (package (inherit qtsvg-5)
    (name "qtimageformats")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "1xjb2z2h1ajw7z9cwq8djpdvjwalpnmirwcwrlbjqv5r4ghmi82a"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (delete-file-recursively "src/3rdparty")))))
    (native-inputs `())
    (inputs
     (list jasper
           libmng
           libtiff
           libwebp
           mesa
           qtbase-5
           zlib))
    (synopsis "Additional Image Format plugins for Qt")
    (description "The QtImageFormats module contains plugins for adding
support for MNG, TGA, TIFF and WBMP image formats.")))

(define-public qtx11extras
  (package (inherit qtsvg-5)
    (name "qtx11extras")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0wxsrnnkkn68myy211rfz98brs7j3qmx3hmy097vh5avgsmw11bn"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs (list perl))
    (inputs
     (list mesa qtbase-5))
    (synopsis "Qt Extras for X11")
    (description "The QtX11Extras module includes the library to access X11
from within Qt 5.")))

(define-public qtxmlpatterns
  (package (inherit qtsvg-5)
    (name "qtxmlpatterns")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "065vj1gk5i4cg0f9spksyb9ps4px0vssx262y77aakvw408vfmq5"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:phases phases)
        #~(modify-phases #$phases
           (add-after 'unpack 'disable-network-tests
             (lambda _ (substitute* "tests/auto/auto.pro"
                         (("qxmlquery") "# qxmlquery")
                         (("xmlpatterns ") "# xmlpatterns"))))
           (add-after 'unpack 'skip-qquickxmllistmodel-test
             (lambda _ (substitute* "tests/auto/auto.pro"
                         ((".*qquickxmllistmodel.*") ""))))))))
    (native-inputs (list perl qtdeclarative-5))
    (inputs (list qtbase-5))
    (synopsis "Qt XML patterns module")
    (description "The QtXmlPatterns module is a XQuery and XPath engine for
XML and custom data models.  It contains programs such as xmlpatterns and
xmlpatternsvalidator.")))

(define-public qtdeclarative-5
  (package (inherit qtsvg-5)
    (name "qtdeclarative")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0ji5131g7h2mrgxw1wxc5mcvmsn3fbw64j28gzpa25gv3vcnkhaw"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:tests? _ #f) #f)             ;TODO: Enable the tests
       ((#:phases phases)
        #~(modify-phases #$phases
           (add-after 'build 'fix-qt5core-install-prefix
             (lambda* (#:key outputs #:allow-other-keys)
                 ;; The Qt5Core install prefix is set to qtbase, but qmlcachegen
                 ;; is provided by qtdeclarative-5.
                 (substitute*
                     "lib/cmake/Qt5QuickCompiler/Qt5QuickCompilerConfig.cmake"
                   (("\\$\\{_qt5Core_install_prefix\\}") #$output))))
               (add-after 'unpack 'fix-linking-riscv64
                   (lambda _
                     (substitute* "src/qml/qml.pro"
                       (("DEFINES \\+= QT_NO_FOREACH")
                        (string-append
                          "isEqual(QT_ARCH, \"riscv64\"): QMAKE_LIBS += -latomic\n\n"
                          "DEFINES += QT_NO_FOREACH")))))))))
    (native-inputs
     (list perl
           pkg-config
           python
           python-wrapper
           qtsvg-5
           vulkan-headers))
    (inputs
     (list mesa qtbase-5))
    (synopsis "Qt QML module (Quick 2)")
    (description "The Qt QML module provides a framework for developing
applications and libraries with the QML language.  It defines and implements the
language and engine infrastructure, and provides an API to enable application
developers to extend the QML language with custom types and integrate QML code
with JavaScript and C++.")))

(define-public qtdeclarative
  (package
    (name "qtdeclarative")
    (version "6.3.1")
    ;; TODO: Package 'masm' and unbundle from sources.
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "1s268fha3650dn1lqxf8jfa07wxpw09f6p7rjyiwq3w24d0nkrq3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-GNinja" ;about twice as fast!
                                "-DQT_BUILD_TESTS=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'honor-cmake-install-rpath
            ;; The build system goes out of its way to compute a runpath it
            ;; thinks makes more sense, and fails.  Revert to the default
            ;; behavior, which is to honor CMAKE_INSTALL_RPATH.
            (lambda _
              (substitute* "src/qml/Qt6QmlMacros.cmake"
                (("set_target_properties.*PROPERTIES.*INSTALL_RPATH.*" all)
                 (string-append "# " all)))))
          (add-after 'unpack 'patch-qlibraryinfo-paths
            (lambda _
              ;; The QLibraryInfo paths are hard-coded to point to the qtbase
              ;; installation, but all the tools used in the test suite come
              ;; from this package.
              (substitute* (find-files "tests" "\\.cpp$")
                (("QLibraryInfo::path\\(QLibraryInfo::BinariesPath)")
                 (string-append "QStringLiteral(\"" #$output "/bin\")"))
                (("QLibraryInfo::path\\(QLibraryInfo::LibraryExecutablesPath)")
                 (string-append "QStringLiteral(\"" #$output
                                "/lib/qt6/libexec\")"))
                (("QLibraryInfo::path\\(QLibraryInfo::QmlImportsPath)")
                 (string-append "QStringLiteral(\"" #$output
                                "/lib/qt6/qml\")")))))
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (apply invoke "cmake" "--build" "."
                     (if parallel-build?
                         `("--parallel" ,(number->string (parallel-job-count)))
                         '()))))
          (delete 'check)               ;move after the install phase
          (replace 'install
            (lambda _
              (invoke "cmake" "--install" ".")))
          (add-after 'install 'check
            (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
              (when tests?
                ;; The tests expect to find the modules provided by this
                ;; package; extend the environment variables needed to do so.
                                        ;(setenv "CMAKE_PREFIX_PATH" #$output)
                (setenv "QML2_IMPORT_PATH"
                        (string-append #$output "/lib/qt6/qml"))
                (setenv "QT_PLUGIN_PATH"
                        (string-append #$output "/lib/qt6/plugins:"
                                       (getenv "QT_PLUGIN_PATH")))
                (setenv "QT_QPA_PLATFORM" "offscreen")
                ;; Skip tests known to fail on GNU/Linux, in a CI context or
                ;; due to bitness (see: https://code.qt.io/cgit/qt/qtbase.git
                ;; /tree/src/testlib/qtestblacklist.cpp).
                (setenv "QTEST_ENVIRONMENT" "linux ci 32bit")
                (setenv "HOME" "/tmp")  ;a few tests require a writable HOME
                (invoke
                 "ctest" "--output-on-failure"
                 "-j" (if parallel-tests?
                          (number->string (parallel-job-count))
                          "1")
                 "-E"                   ;exclude some tests by regex
                 (string-append
                  "("
                  (string-join
                   (list
                    ;; This test is marked as flaky upstream (see:
                    ;; https://bugreports.qt.io/browse/QTBUG-101488).
                    "tst_qquickfiledialogimpl"
                    ;; These tests all fail because 'test_overlappingHandles'
                    ;; (see: https://bugreports.qt.io/browse/QTBUG-95750).
                    "tst_basic"
                    "tst_fusion"
                    "tst_imagine"
                    "tst_material"
                    "tst_universal"
                    ;; Fails due to using the wrong lib/qt6/qml prefix:
                    ;; "Warning: Failed to find the following builtins:
                    ;; builtins.qmltypes, jsroot.qmltypes (so will use
                    ;; qrc). Import paths used:
                    ;; /gnu/store/...-qtbase-6.3.1/lib/qt6/qml"
                    "tst_qmltc_qprocess"
                    ;; These test fail when running qmlimportscanner; perhaps
                    ;; an extra CMAKE_PREFIX_PATH location is missing to
                    ;; correctly locate the imports.
                    "empty_qmldir"
                    "qtquickcompiler"
                    "cmake_tooling_imports"
                    ;; This test seems to hangs for a long time, possibly
                    ;; waiting for a killed process, which becomes a zombie in
                    ;; the build container (perhaps solved after
                    ;; fixing/applying #30948).
                    "tst_qqmlpreview") "|")
                  ")")))))
          (add-after 'install 'delete-installed-tests
            (lambda _
              (delete-file-recursively (string-append #$output "/tests")))))))
    (native-inputs
     (list ninja
           perl
           pkg-config
           python
           qtshadertools
           vulkan-headers))
    (inputs
     (list libxkbcommon
           mesa
           qtbase))
    (home-page (package-home-page qtbase))
    (synopsis "Qt QML module (Quick 2)")
    (description "The Qt QML module provides a framework for developing
applications and libraries with the QML language.  It defines and implements
the language and engine infrastructure, and provides an API to enable
application developers to extend the QML language with custom types and
integrate QML code with JavaScript and C++.")
    (license (package-license qtbase))))

(define-public qtconnectivity
  (package (inherit qtsvg-5)
    (name "qtconnectivity")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0b2dnxw1rjbp1srhgns148cwl99f50mx29588dal3avv0f73s597"))))
    (native-inputs
     (list perl pkg-config qtdeclarative-5))
    (inputs
     (list bluez qtbase-5))
    (synopsis "Qt Connectivity module")
    (description "The Qt Connectivity modules provides modules for interacting
with Bluetooth and NFC.")))

(define-public qtwebsockets-5
  (package (inherit qtsvg-5)
    (name "qtwebsockets")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0f120rfqnmlffjhrm5jbpipk1qsbzp1a2v3q8gz94hz6n9dqpav6"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs
     (list perl qtdeclarative-5))
    (inputs (list qtbase-5))
    (synopsis "Qt Web Sockets module")
    (description "WebSocket is a web-based protocol designed to enable two-way
communication between a client application and a remote host.  The Qt
WebSockets module provides C++ and QML interfaces that enable Qt applications
to act as a server that can process WebSocket requests, or a client that can
consume data received from the server, or both.")))

(define-public qtwebsockets
  (package
    (name "qtwebsockets")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "06hj0pkdzjicmbiinjp1dk1ziz8cb3fgcwy7a0dxxjvzr680v64z"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DQT_BUILD_TESTS=ON")
      #:phases #~(modify-phases %standard-phases
                   (delete 'check)      ;move after install
                   (add-after 'install 'prepare-for-tests
                     (lambda _
                       (setenv "QT_QPA_PLATFORM" "offscreen")
                       (setenv "QML2_IMPORT_PATH"
                               (string-append #$output "/lib/qt6/qml:"
                                              (getenv "QML2_IMPORT_PATH")))))
                   (add-after 'prepare-for-tests 'check
                     (assoc-ref %standard-phases 'check))
                   (add-after 'check 'delete-installed-tests
                     (lambda _
                       (delete-file-recursively
                        (string-append #$output "/tests")))))))
    (native-inputs (list perl))
    (inputs (list qtbase qtdeclarative))
    (synopsis "Qt Web Sockets module")
    (description "WebSocket is a web-based protocol designed to enable two-way
communication between a client application and a remote host.  The Qt
WebSockets module provides C++ and QML interfaces that enable Qt applications
to act as a server that can process WebSocket requests, or a client that can
consume data received from the server, or both.")
    (home-page (package-home-page qtbase))
    (license (package-license qtbase))))

(define-public qtsensors
  (package (inherit qtsvg-5)
    (name "qtsensors")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0zlhm4js02niibb23rw87wf4ik0gy4ai08fwprnwy7zf4rm1ss3d"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:parallel-tests? _ #f) #f) ; can lead to race condition
       ((#:phases phases)
        #~(modify-phases #$phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "tests/auto/qsensorgestures_gestures/tst_sensorgestures_gestures.cpp"
                 (("2000") "5000")                                      ;lengthen test timeout
                 (("QTest::newRow(\"twist\") << \"twist\"") ""))))))))  ;failing test
    (native-inputs
     (list perl qtdeclarative-5))
    (inputs (list qtbase-5))
    (synopsis "Qt Sensors module")
    (description "The Qt Sensors API provides access to sensor hardware via QML
and C++ interfaces.  The Qt Sensors API also provides a motion gesture
recognition API for devices.")))

(define-public qtmultimedia-5
  (package
    (inherit qtsvg-5)
    (name "qtmultimedia")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0q76iy1frcgm85mid17lh4p6gnn04n19n6zklgpv4w3md1ng97xw"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (delete-file-recursively
                   "examples/multimedia/spectrum/3rdparty")
                 ;; We also prevent the spectrum example from being built.
                 (substitute* "examples/multimedia/multimedia.pro"
                   (("spectrum") "#"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:phases phases)
        #~(modify-phases #$phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out #$output))
                 (invoke "qmake" "QT_BUILD_PARTS = libs tools tests"
                         (string-append "QMAKE_LFLAGS_RPATH=-Wl,-rpath," out "/lib -Wl,-rpath,")
                         (string-append "PREFIX=" out)))))))
       ((#:tests? _ #f) #f)))           ; TODO: Enable the tests
    (native-inputs
     (list perl pkg-config python qtdeclarative-5))
    (inputs
     (list alsa-lib
           mesa
           pulseaudio
           qtbase-5
           ;; Gstreamer is needed for the mediaplayer plugin
           gstreamer
           gst-plugins-base))
    (synopsis "Qt Multimedia module")
    (description "The Qt Multimedia module provides set of APIs to play and
record media, and manage a collection of media content.  It also contains a
set of plugins for interacting with pulseaudio and GStreamer.")))

(define-public qtshadertools
  (package
    (name "qtshadertools")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              ;; Note: the source bundles *patched* glslang and SPIRV-Cross
              ;; sources.
              (sha256
               (base32
                "0nj35s2z5n438q7nqf6bnj3slwz2am3169ck1ixwqa0mjrv73dsr"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DQT_BUILD_TESTS=ON")
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'prepare-for-tests
                     (lambda _
                       (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (native-inputs (list perl))
    (inputs (list glslang libxkbcommon qtbase))
    (home-page (package-home-page qtbase))
    (synopsis "Shader pipeline API and and tools for Qt")
    (description "The @code{qtshadertools} module provides APIs and tools
supporting shader pipeline functionality as offered in Qt Quick to operate on
Vulkan, OpenGL and other main graphic APIs.")
    (license (package-license qtbase))))

(define-public qtmultimedia
  (package
    (name "qtmultimedia")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "0dkk3lmzi2fs13cnj8q1lpcs6gghj219826gkwnzyd6nmlm280vy"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively
                   "examples/multimedia/spectrum/3rdparty")
                  ;; We also prevent the spectrum example from being built.
                  (substitute* "examples/multimedia/multimedia.pro"
                    (("spectrum") "#"))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DQT_BUILD_TESTS=ON"
                                "-DQT_FEATURE_pulseaudio=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-integration-tests
            (lambda _
              ;; XXX: The tst_qaudiodecoderbackend, tst_qaudiodevice,
              ;; tst_qaudiosource, tst_qmediaplayerbackend and
              ;; tst_qcamerabackend tests fail, presumably because they
              ;; require a functional pulseaudio daemon (which requires a dbus
              ;; session bus, which requires an X11 server, and then is still
              ;; unhappy).
              (substitute* "tests/auto/CMakeLists.txt"
                (("add_subdirectory\\(integration)") ""))))
          (add-before 'check 'prepare-for-tests
            (lambda _
              (setenv "QT_QPA_PLATFORM" "offscreen")))
          (add-after 'install 'delete-installed-tests
            (lambda _
              (delete-file-recursively (string-append #$output "/tests")))))))
    (native-inputs
     (list perl
           pkg-config
           qtshadertools
           vulkan-headers))
    (inputs
     (list alsa-lib
           glib
           gstreamer
           gst-plugins-base             ;gstreamer-gl
           gst-plugins-good             ;camera support, additional plugins
           gst-libav                    ;ffmpeg plugin
           libxkbcommon
           mesa
           qtbase
           qtdeclarative
           pulseaudio))
    (home-page (package-home-page qtbase))
    (synopsis "Qt Multimedia module")
    (description "The Qt Multimedia module provides set of APIs to play and
record media, and manage a collection of media content.  It also contains a
set of plugins for interacting with pulseaudio and GStreamer.")
    (license (package-license qtbase))))

(define-public qtwayland-5
  (package (inherit qtsvg-5)
    (name "qtwayland")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (patches (search-patches "qtwayland-gcc-11.patch"))
             (sha256
              (base32
               "0yy8qf9kn15iqsxi2r7jbcsc0vsdyfz7bbxmfn4i9qmz1yvg0jgr"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:phases phases)
        #~(modify-phases #$phases
           (add-after 'unpack 'disable-failing-tests
             (lambda _
               ;; FIXME: tst_seatv4::animatedCursor() fails for no good
               ;; reason and breaks these two tests.
               (substitute* "tests/auto/client/seatv4/tst_seatv4.cpp"
                 (((string-append "QVERIFY\\(!cursorSurface\\(\\)->"
                                  "m_waitingFrameCallbacks\\.empty\\(\\)\\);"))
                  "")
                 (("QTRY_COMPARE\\(bufferSpy\\.count\\(\\), 1\\);")
                  ""))))
           (add-before 'check 'set-test-environment
             (lambda _
               ;; Do not fail just because /etc/machine-id is missing.
               (setenv "DBUS_FATAL_WARNINGS" "0")))))))
    (native-inputs
     (list glib perl pkg-config qtdeclarative-5))
    (inputs
     (list fontconfig
           freetype
           libx11
           libxcomposite
           libxext
           libxkbcommon
           libxrender
           mesa
           mtdev
           qtbase-5
           vulkan-headers
           wayland))
    (synopsis "Qt Wayland module")
    (description "The Qt Wayland module provides the QtWayland client and
compositor libraries.")))

(define-public qtwayland
  (package
    (name "qtwayland")
    (version "6.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (qt-urls name version))
       (sha256
        (base32 "1w60p1did7awdlzq5k8vnq2ncpskb07cpvz31cbv99bjs6igw53g"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DQT_BUILD_TESTS=ON")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-failing-tests
                 (lambda _
                   ;; FIXME: tst_seatv4::animatedCursor() fails here.
                   ;; See also: <https://bugreports.qt.io/browse/QTBUG-78317>
                   (substitute* "tests/auto/client/seatv4/tst_seatv4.cpp"
                     (((string-append
                        "QVERIFY\\(!cursorSurface\\(\\)->"
                        "m_waitingFrameCallbacks\\.empty\\(\\)\\);")) "")
                     (("QTRY_COMPARE\\(bufferSpy\\.count\\(\\), 1\\);") ""))))
               (add-before 'check 'set-test-environment
                 (lambda _
                   ;; Do not fail just because /etc/machine-id is missing.
                   (setenv "DBUS_FATAL_WARNINGS" "0")
                   ;; Make Qt render "offscreen", required for tests.
                   (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (native-inputs (list glib perl pkg-config qtdeclarative))
    (inputs
     (list fontconfig
           freetype
           libx11
           libxcomposite
           libxext
           libxkbcommon
           libxrender
           mesa
           mtdev
           qtbase
           vulkan-headers
           wayland))
    (synopsis "Qt Wayland module")
    (description "The Qt Wayland module provides the QtWayland client and
compositor libraries.")
    (home-page (package-home-page qtbase))
    (license (package-license qtbase))))

(define-public qtserialport
  (package (inherit qtsvg-5)
    (name "qtserialport")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0xg2djwhrj5jqamawlp75g70nmwbp2ph2hh1pm45s36jkxm0k7al"))))
    (native-inputs (list perl))
    (inputs
     (list qtbase-5 eudev))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:phases phases)
        #~(modify-phases #$phases
           (add-after 'unpack 'patch-dlopen-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/serialport/qtudev_p.h"
               ;; Use the absolute paths for dynamically loaded libs,
               ;; otherwise the lib will be searched in LD_LIBRARY_PATH which
               ;; typically is not set in guix.
               (("^\\s*(udevLibrary->setFileNameAndVersion\\(QStringLiteral\\(\")(udev\"\\),\\s*[0-9]+\\);)" _ a b)
                (string-append a #$eudev "/lib/lib" b)))))))))
    (synopsis "Qt Serial Port module")
    (description "The Qt Serial Port module provides the library for
interacting with serial ports from within Qt.")))

(define-public qtserialbus
  (package (inherit qtsvg-5)
    (name "qtserialbus")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "180gm1jvqfn0h3251zafdd1wd3af00phwaa5qljsbrj6s6ywj79j"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
           (add-after 'unpack 'patch-libsocketcan-reference
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((libcansocket.so (string-append #$libsocketcan
                                                      "/lib/libsocketcan.so")))
                 (substitute* "src/plugins/canbus/socketcan/libsocketcan.cpp"
                   (("QStringLiteral\\(\"socketcan\"\\)")
                    (format #f "QStringLiteral(~s)" libcansocket.so))))))))))
    (inputs
     (list libsocketcan qtbase-5 qtserialport))
    (synopsis "Qt Serial Bus module")
    (description "The Qt Serial Bus API provides classes and functions to
access the various industrial serial buses and protocols, such as CAN, ModBus,
and others.")))

(define-public qtwebchannel-5
  (package (inherit qtsvg-5)
    (name "qtwebchannel")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "1w8mcpdqlphgg3a6yfq18liwlj2nkwrafv0n80h242x5l2mk3ljf"))))
    (native-inputs
     (list perl qtdeclarative-5 qtwebsockets-5))
    (inputs (list qtbase-5))
    (synopsis "Web communication library for Qt")
    (description "The Qt WebChannel module enables peer-to-peer communication
between the host (QML/C++ application) and the client (HTML/JavaScript
application).  The transport mechanism is supported out of the box by the two
popular web engines, Qt WebKit 2 and Qt WebEngine.")))

(define-public qtwebchannel
  (package
    (name "qtwebchannel")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "0s16zx3qn3byldvhmsnwijm8rmizk8vpqj7fnwhjg6c67z10m8ma"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DQT_BUILD_TESTS=ON")
      #:phases #~(modify-phases %standard-phases
                   (delete 'check)      ;move after install
                   (add-after 'install 'prepare-for-tests
                     (lambda _
                       (setenv "QT_QPA_PLATFORM" "offscreen")
                       (setenv "QML2_IMPORT_PATH"
                               (string-append #$output "/lib/qt6/qml:"
                                              (getenv "QML2_IMPORT_PATH")))))
                   (add-after 'prepare-for-tests 'check
                     (assoc-ref %standard-phases 'check))
                   (add-after 'check 'delete-installed-tests
                     (lambda _
                       (delete-file-recursively
                        (string-append #$output "/tests")))))))
    (native-inputs (list perl))
    (inputs (list qtbase qtdeclarative qtwebsockets))
    (home-page (package-home-page qtbase))
    (synopsis "Web communication library for Qt")
    (description "The Qt WebChannel module enables peer-to-peer communication
between the host (QML/C++ application) and the client (HTML/JavaScript
application).")
    (license (package-license qtbase))))

(define-public qtwebglplugin
  (package (inherit qtsvg-5)
    (name "qtwebglplugin")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "1m0p4ssykw07lbip2qyv6w34f8ng13bxb63j0w446f5w0492nn9f"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'disable-network-tests
             (lambda _ (substitute* "tests/plugins/platforms/platforms.pro"
                         (("webgl") "# webgl"))))))))
    (native-inputs '())
    (inputs
     (list mesa qtbase-5 qtdeclarative-5 qtwebsockets-5 zlib))
    (synopsis "QPA plugin for running applications via a browser using
streamed WebGL commands")
    (description "Qt back end that uses WebGL for rendering. It allows Qt
applications (with some limitations) to run in a web browser that supports
WebGL.  WebGL is a JavaScript API for rendering 2D and 3D graphics within any
compatible web browser without the use of plug-ins.  The API is similar to
OpenGL ES 2.0 and can be used in HTML5 canvas elements")))

(define-public qtwebview
  (package (inherit qtsvg-5)
    (name "qtwebview")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0arwaky3jy5ql3z4d8f7k7diidzb1kncdans7pn50hsa1bzacfal"))))
    (native-inputs
     (list perl))
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (synopsis "Display web content in a QML application")
    (description "Qt WebView provides a way to display web content in a QML
application without necessarily including a full web browser stack by using
native APIs where it makes sense.")))

(define-public qtlocation
  (package (inherit qtsvg-5)
    (name "qtlocation")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0mlhhhcxx3gpr9kh04c6fljxcj50c2j21r0wb9f7d7nk4flip7b2"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:tests? _ #f) #f)   ; TODO: Enable the tests
       ((#:phases phases)
        #~(modify-phases #$phases
           (add-before 'check 'pre-check
             (lambda _
               (setenv "HOME" "/tmp")))))))
    (native-inputs
     (list perl qtdeclarative-5 qtquickcontrols-5 qtserialport))
    (inputs
     (list icu4c openssl qtbase-5 zlib))
    (synopsis "Qt Location and Positioning modules")
    (description "The Qt Location module provides an interface for location,
positioning and geolocation plugins.")))

(define-public qtlottie
  (package
    (name "qtlottie")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "1x8wmc6gwmxk92zjcsrbhrbqbfvnk7302ggghld5wk8jk5lsf2vl"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DQT_BUILD_TESTS=ON")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)               ;move after install
          (add-after 'install 'prepare-for-tests
            (lambda _
              (setenv "QT_QPA_PLATFORM" "offscreen")
              (setenv "QML2_IMPORT_PATH"
                      (string-append #$output "/lib/qt6/qml:"
                                     (getenv "QML2_IMPORT_PATH"))))))))
    (native-inputs (list perl))
    (inputs (list libxkbcommon qtbase qtdeclarative))
    (home-page (package-home-page qtbase))
    (synopsis "QML API for rendering Bodymovin graphics and animations")
    (description "Qt Lottie Animation provides a QML API for rendering
graphics and animations that are exported in JSON format by the Bodymovin
plugin for Adobe After Effects.")
    (license (package-license qtbase))))

(define-public qttools-5
  (package (inherit qtsvg-5)
    (name "qttools")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0v7wkzq9i8w3qrw0z8al7lb6clr57lfisyb1fm9cnhi73fvph1vd"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs
     (list perl qtdeclarative-5 vulkan-headers))
    (inputs
     (list mesa qtbase-5))
    (synopsis "Qt Tools and Designer modules")
    (description "The Qt Tools module provides a set of applications to browse
the documentation, translate applications, generate help files and other stuff
that helps in Qt development.")))

(define-public qttools
  (package
    (name "qttools")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "1h96w4bzkbd80vr7lh6hnypdlmbzc1y52c2zrqzvkgm3587pa4n4"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; The build system attempts to fetch online resources and fails when
      ;; building the test suite.
      #:configure-flags #~(list "-DQT_BUILD_TESTS=OFF")))
    (native-inputs (list perl qtdeclarative vulkan-headers))
    (inputs (list libxkbcommon mesa qtbase))
    (home-page (package-home-page qtbase))
    (synopsis "Qt Tools and Designer modules")
    (description "The Qt Tools module provides a set of applications to browse
the documentation, translate applications, generate help files and other stuff
that helps in Qt development.")
    ;; GPL 3 only with Qt GPL exception 1.0 (see:
    ;; LICENSES/Qt-GPL-exception-1.0.txt).
    (license (list license:gpl3))))

(define-public qtscript
  (package (inherit qtsvg-5)
    (name "qtscript")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "17yk0p8ci47xlfpllc17arlycng47wrnnskimskzz85bspabc8pm"))
             (patches (search-patches "qtscript-disable-tests.patch"))))
    (native-inputs
     (list perl qttools-5))
    (inputs
     (list qtbase-5))
    (synopsis "Qt Script module")
    (description "Qt provides support for application scripting with ECMAScript.
The following guides and references cover aspects of programming with
ECMAScript and Qt.")))

(define-public qtquickcontrols-5
  (package (inherit qtsvg-5)
    (name "qtquickcontrols")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0mjw25wcgd2bvjz9rr4qjydb423c63615rcx1vws4jmydqdihssr"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (synopsis "Qt Quick Controls and other Quick modules")
    (description "The QtScript module provides classes for making Qt
applications scriptable.  This module provides a set of extra components that
can be used to build complete interfaces in Qt Quick.")))

(define-public qtquickcontrols2-5
  (package (inherit qtsvg-5)
    (name "qtquickcontrols2")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "1cxg4ml07k1zcyi5m4lx06sz8f5l67isb5vhk7nakxm0wnn7p8y4"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (synopsis "Qt Quick Controls 2 and other Quick 2 modules")
    (description "The Qt Quick Controls 2 module contains the Qt Labs Platform
module that provides platform integration: native dialogs, menus and menu bars,
and tray icons.  It falls back to Qt Widgets when a native implementation is
not available.")))

(define-public qtquickcontrols2
  ;; qtquickcontrols2 still exist, but was merged into qtdeclarative.
  ;; Unfortunately that hasn't been well communicated at all (see:
  ;; https://bugreports.qt.io/browse/QTBUG-79454).
  (deprecated-package "qtquickcontrols2" qtdeclarative))

(define-public qtgraphicaleffects
  (package (inherit qtsvg-5)
    (name "qtgraphicaleffects")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0xznn5zqp6xrqfgl54l8cig9asqf9m2hz0p3ga514rh8spmdazr3"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (synopsis "Qt Graphical Effects module")
    (description "The Qt Graphical Effects module provides a set of QML types
for adding visually impressive and configurable effects to user interfaces.
Effects are visual items that can be added to Qt Quick user interface as UI
components.  The API consists of over 20 effects provided as separate QML
types.  The effects cover functional areas such as blending, masking, blurring,
coloring, and many more.")))

(define-public qtgamepad
  (package (inherit qtsvg-5)
    (name "qtgamepad")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0wa4d8f025hlp4bmdzdy5wcahm9wjg6bkwig8dpw9nrsj3idz5b0"))))
    (native-inputs
     (list perl pkg-config))
    (inputs
     (list fontconfig
           freetype
           libxrender
           sdl2
           qtbase-5
           qtdeclarative-5))
    (synopsis "Qt Gamepad module")
    (description "The Qt Gamepad module is an add-on library that enables Qt
applications to support the use of gamepad hardware and in some cases remote
control equipment.  The module provides both QML and C++ interfaces.  The
primary target audience are embedded devices with fullscreen user interfaces,
and mobile applications targeting TV-like form factors.")))

(define-public qtscxml
  (package (inherit qtsvg-5)
    (name "qtscxml")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0xf5mqsrw16h8xjglymgfc8qg2qa5bi4fgdl4j3dkhvvpr7vrphp"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (delete-file-recursively "tests/3rdparty")
                 ;; the scion test refers to the bundled 3rd party test code.
                 (substitute* "tests/auto/auto.pro"
                   (("scion") "#"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (synopsis "Qt SCXML module")
    (description "The Qt SCXML module provides functionality to create state
machines from SCXML files.  This includes both dynamically creating state
machines (loading the SCXML file and instantiating states and transitions) and
generating a C++ file that has a class implementing the state machine.  It
also contains functionality to support data models and executable content.")))

(define-public qtpositioning
  (package
    (name "qtpositioning")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "0v78wamvdw02kf9rq7m5v24q2g6jmgq4ch0fnfa014p1r978wy06"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DQT_BUILD_TESTS=ON")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'delete-installed-tests
                     (lambda _
                       (delete-file-recursively
                        (string-append #$output "/tests")))))))
    (inputs (list perl qtbase))
    (home-page (package-home-page qtbase))
    (synopsis "QML and C++ positioning information API")
    (description "The Qt Positioning API provides positioning information via
QML and C++ interfaces.  The Qt Positioning API lets you to determine a
position by using a variety of possible sources, including satellite, wifi, or
text files.  That information can then be used to, for example, determine a
position on a map.  In addition, you can use to the API to retrieve satellite
information and perform area based monitoring.")
    (license (package-license qtbase))))

(define-public qtpurchasing
  (package (inherit qtsvg-5)
    (name "qtpurchasing")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "04z6mwzn73gg56hgs7gividinfgndx4kmcnp7w6h3wamrdlkfdx7"))))
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (synopsis "Qt Purchasing module")
    (description "The Qt Purchasing module provides and in-app API for
purchasing goods and services.")))

(define-public qtcharts
  (package (inherit qtsvg-5)
    (name "qtcharts")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0y051i1837bfybkf8cm7cx8k5wjmbi47pxawaaz6wm0hd2z5b4qi"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:phases phases)
        #~(modify-phases #$phases
           (add-after 'unpack 'remove-failing-test
             (lambda _
               (substitute* "tests/auto/auto.pro"
                 (("qml") "# qml")
                 (("qml-qtquicktest") "# qml-qtquicktest"))))))))
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (synopsis "Qt Charts module")
    (description "The Qt Charts module provides a set of easy to use chart
components.  It uses the Qt Graphics View Framework, therefore charts can be
easily integrated to modern user interfaces.  Qt Charts can be used as QWidgets,
QGraphicsWidget, or QML types. Users can easily create impressive graphs by
selecting one of the charts themes.")
    (license license:gpl3)))

(define-public qtdatavis3d
  (package (inherit qtsvg-5)
    (name "qtdatavis3d")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0sczwqlc36jdywf7bqxz0hm6mr7fn8p1fsnc33jliiqzn9yrg77x"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (synopsis "Qt Data Visualization module")
    (description "The Qt Data Visualization module provides a way to visualize
data in 3D as bar, scatter, and surface graphs. It is especially useful for
visualizing depth maps and large quantities of rapidly changing data, such as
data received from multiple sensors. The look and feel of graphs can be
customized by using themes or by adding custom items and labels to them.")
    (license license:gpl3)))

(define-public qtnetworkauth-5
  (package (inherit qtsvg-5)
    (name "qtnetworkauth")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0c7mz715rlpg0cqgs6s0aszmslyamkhnpamc1iij6i571sj5j2f1"))))
    (inputs
     (list qtbase-5))
    (synopsis "Qt Network Authorization module")
    (description "The Qt Network Authorization module provides an
implementation of OAuth and OAuth2 authenticathon methods for Qt.")))

(define-public qtnetworkauth
  (package
    (name "qtnetworkauth")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (qt-urls name version))
              (sha256
               (base32
                "0apvsb2ip1m3kw8vi9spvf6f6q72ys8vr40rpyysi7shsjwm83yn"))))
    (build-system cmake-build-system)
    (arguments (list #:configure-flags #~(list "-DQT_BUILD_TESTS=ON")))
    (native-inputs (list perl))
    (inputs (list qtbase))
    (home-page (package-home-page qtbase))
    (synopsis "Qt Network Authorization module")
    (description "The Qt Network Authorization module provides an
implementation of OAuth and OAuth2 authenticathon methods for Qt.")
    (license (package-license qtbase))))

(define-public qtremoteobjects
  (package (inherit qtsvg-5)
    (name "qtremoteobjects")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "1m0xcqlbxsfn0cd4ajin1h3i4l51dajmkw91v0r4a61xi14i0kks"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'remove-failing-test
             (lambda _
               ;; This test can't find its imports.
               (substitute* "tests/auto/qml/qml.pro"
                 (("integration") "# integration")
                 (("usertypes") "# usertypes"))
               ;; disable failing tests: they need network
               (substitute* "tests/auto/auto.pro"
                 (("integration_multiprocess proxy_multiprocess integration_external restart")
                   "integration_multiprocess"))))))))
    (inputs
     (list qtbase-5 qtdeclarative-5))
    (synopsis "Qt Remote Objects module")
    (description "The Qt Remote Objects module is an @dfn{inter-process
communication} (IPC) module developed for Qt.  The idea is to extend existing
Qt's functionalities to enable an easy exchange of information between
processes or computers.")))

(define-public qtspeech
  (package (inherit qtsvg-5)
    (name "qtspeech")
    (version "5.15.5")
    (source (origin
             (method url-fetch)
             (uri (qt-urls name version))
             (sha256
              (base32
               "0xskp9dzjy5nqszygk8gwvjyiylgynx5sq3nk2vi3zwgfdh5jpm4"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     (list qtbase-5))
    (native-inputs
     (list perl qtdeclarative-5 qtmultimedia-5 qtxmlpatterns))
    (synopsis "Qt Speech module")
    (description "The Qt Speech module enables a Qt application to support
accessibility features such as text-to-speech, which is useful for end-users
who are visually challenged or cannot access the application for whatever
reason.  The most common use case where text-to-speech comes in handy is when
the end-user is driving and cannot attend the incoming messages on the phone.
In such a scenario, the messaging application can read out the incoming
message.")))

(define-public qtspell
  (package
    (name "qtspell")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/manisandro/qtspell")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19ff6jzm699wrxrk57w3d4kl9qxgdipdikpwls9n4aqv4mw7g969"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no test
    (native-inputs
     (list pkg-config qttools-5))
    (inputs
     (list enchant qtbase-5))
    (home-page "https://github.com/manisandro/qtspell")
    (synopsis "Spell checking for Qt text widgets")
    (description
     "QtSpell adds spell-checking functionality to Qt's text widgets,
using the Enchant spell-checking library.")
    ;; COPYING file specify GPL3, but source code files all refer to GPL2+.
    (license license:gpl2+)))

(define-public qtwebengine-5
  (package
    (inherit qtsvg-5)
    (name "qtwebengine")
    (version "5.15.5")
    (source
     (origin
       (method url-fetch)
       (uri (qt-urls name version))
       (sha256
        (base32
         "0zahr9w6rqdxwh2whsgk3fhcszs7wa9j95lq4sqi8xzin2wcgl17"))
       (modules '((ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (guix build utils)))
       (snippet
        '(begin
           (let ((preserved-third-party-files
                  '("base/third_party/double_conversion"
                    "base/third_party/cityhash"
                    "base/third_party/cityhash_v103"
                    "base/third_party/dynamic_annotations"
                    "base/third_party/icu"
                    "base/third_party/libevent"
                    "base/third_party/nspr"
                    "base/third_party/superfasthash"
                    "base/third_party/symbolize"
                    "base/third_party/xdg_mime"
                    "base/third_party/xdg_user_dirs"
                    "net/third_party/mozilla_security_manager"
                    "net/third_party/nss"
                    "net/third_party/quiche"
                    "net/third_party/uri_template"
                    "third_party/abseil-cpp"
                    "third_party/angle"
                    "third_party/angle/src/common/third_party/base"
                    "third_party/angle/src/common/third_party/smhasher"
                    "third_party/angle/src/common/third_party/xxhash"
                    "third_party/angle/src/third_party/compiler"
                    "third_party/axe-core"
                    "third_party/blink"
                    "third_party/boringssl"
                    "third_party/boringssl/src/third_party/fiat"
                    "third_party/breakpad"
                    "third_party/brotli"
                    "third_party/catapult/common/py_vulcanize/py_vulcanize"
                    "third_party/catapult/common/py_vulcanize/third_party"
                    "third_party/catapult/third_party/beautifulsoup4"
                    "third_party/catapult/third_party/html5lib-python"
                    "third_party/catapult/third_party/polymer/components"
                    "third_party/catapult/tracing"
                    "third_party/catapult/tracing/third_party"
                    "third_party/ced"
                    "third_party/cld_3"
                    "third_party/closure_compiler"
                    "third_party/crashpad"
                    "third_party/crashpad/crashpad/third_party/lss"
                    "third_party/crashpad/crashpad/third_party/zlib"
                    "third_party/crc32c"
                    "third_party/dav1d"
                    "third_party/dawn"
                    "third_party/devtools-frontend"
                    "third_party/devtools-frontend/src/front_end/third_party"
                    "third_party/devtools-frontend/src/third_party/typescript"
                    "third_party/emoji-segmenter"
                    "third_party/ffmpeg"
                    "third_party/googletest"
                    "third_party/harfbuzz-ng/utils"
                    "third_party/hunspell"
                    "third_party/iccjpeg"
                    "third_party/icu"
                    "third_party/inspector_protocol"
                    "third_party/jinja2"
                    "third_party/jsoncpp"
                    "third_party/jstemplate"
                    "third_party/khronos"
                    "third_party/leveldatabase"
                    "third_party/libaddressinput"
                    "third_party/libavif"
                    "third_party/libgifcodec"
                    "third_party/libjingle_xmpp"
                    "third_party/libjpeg_turbo"
                    "third_party/libpng"
                    "third_party/libsrtp"
                    "third_party/libsync"
                    "third_party/libudev"
                    "third_party/libvpx"
                    "third_party/libwebm"
                    "third_party/libwebp"
                    "third_party/libxml"
                    "third_party/libxslt"
                    "third_party/libyuv"
                    "third_party/lottie"
                    "third_party/lss"
                    "third_party/mako"
                    "third_party/markupsafe"
                    "third_party/mesa_headers"
                    "third_party/metrics_proto"
                    "third_party/modp_b64"
                    "third_party/nasm"
                    "third_party/node"
                    "third_party/one_euro_filter"
                    "third_party/openh264/src/codec/api/svc"
                    "third_party/opus"
                    "third_party/ots"
                    "third_party/pdfium"
                    "third_party/pdfium/third_party/agg23"
                    "third_party/pdfium/third_party/base"
                    "third_party/pdfium/third_party/freetype"
                    "third_party/pdfium/third_party/lcms"
                    "third_party/pdfium/third_party/libopenjpeg20"
                    "third_party/pdfium/third_party/skia_shared"
                    "third_party/perfetto"
                    "third_party/pffft"
                    "third_party/ply"
                    "third_party/polymer"
                    "third_party/protobuf"
                    "third_party/protobuf/third_party/six"
                    "third_party/pyjson5"
                    "third_party/re2"
                    "third_party/rnnoise"
                    "third_party/skia"
                    "third_party/skia/include/third_party/skcms/skcms.h"
                    "third_party/skia/include/third_party/vulkan"
                    "third_party/skia/third_party/skcms"
                    "third_party/smhasher"
                    "third_party/snappy"
                    "third_party/sqlite"
                    "third_party/usb_ids"
                    "third_party/usrsctp"
                    "third_party/vulkan_memory_allocator"
                    "third_party/web-animations-js"
                    "third_party/webrtc"
                    "third_party/webrtc/common_audio/third_party/ooura/fft_size_128"
                    "third_party/webrtc/common_audio/third_party/ooura/fft_size_256"
                    "third_party/webrtc/common_audio/third_party/spl_sqrt_floor"
                    "third_party/webrtc/modules/third_party/fft"
                    "third_party/webrtc/modules/third_party/g711"
                    "third_party/webrtc/modules/third_party/g722"
                    "third_party/webrtc/rtc_base/third_party/base64"
                    "third_party/webrtc/rtc_base/third_party/sigslot"
                    "third_party/webrtc_overrides"
                    "third_party/widevine/cdm/widevine_cdm_common.h"
                    "third_party/widevine/cdm/widevine_cdm_version.h"
                    "third_party/woff2"
                    "third_party/xcbproto"
                    "third_party/zlib"
                    "url/third_party/mozilla"
                    "v8/src/third_party/utf8-decoder"
                    "v8/src/third_party/valgrind"
                    "v8/src/third_party/siphash"
                    "v8/third_party/v8/builtins"
                    "v8/third_party/inspector_protocol"))
                 (protected (make-regexp "\\.(gn|gyp)i?$")))
             (define preserved-club
               (map (lambda (member)
                      (string-append "./" member))
                    preserved-third-party-files))
             (define (empty? dir)
               (equal? (scandir dir) '("." "..")))
             (define (third-party? file)
               (string-contains file "third_party/"))
             (define (useless? file)
               (any (cute string-suffix? <> file)
                    '(".zip" ".so" ".dll" ".exe" ".jar")))
             (define (parents child)
               ;; Return all parent directories of CHILD up to and including
               ;; the closest "third_party".
               (let* ((dirs (match (string-split child #\/)
                              ((dirs ... last) dirs)))
                      (closest (list-index (lambda (dir)
                                             (string=? "third_party" dir))
                                           (reverse dirs)))
                      (delim (- (length dirs) closest)))
                 (fold (lambda (dir prev)
                         (cons (string-append (car prev) "/" dir)
                               prev))
                       (list (string-join (list-head dirs delim) "/"))
                       (list-tail dirs delim))))
             (define (remove-loudly file)
               (format #t "deleting ~a...~%" file)
               (force-output)
               (delete-file file))
             (define (delete-unwanted-files child stat flag base level)
               (match flag
                 ((or 'regular 'symlink 'stale-symlink)
                  (when (third-party? child)
                    (unless (or (member child preserved-club)
                                (any (cute member <> preserved-club)
                                     (parents child))
                                (regexp-exec protected child))
                      (remove-loudly child)))
                  (when (and (useless? child) (file-exists? child))
                    (remove-loudly child))
                  #t)
                 ('directory-processed
                  (when (empty? child)
                    (rmdir child))
                  #t)
                 (_ #t)))

             (with-directory-excursion "src/3rdparty"
               ;; TODO: Try removing "gn" too for future versions of qtwebengine-5.
               (delete-file-recursively "ninja")

               (with-directory-excursion "chromium"
                 ;; Delete bundled software and binaries that were not explicitly
                 ;; preserved above.
                 (nftw "." delete-unwanted-files 'depth 'physical)

                 ;; Assert that each preserved item is present to catch removals.
                 (for-each (lambda (third-party)
                             (unless (file-exists? third-party)
                               (error (format #f "~s does not exist!~%" third-party))))
                           preserved-club)

                 ;; Use relative header locations instead of hard coded ones.
                 (substitute*
                   "base/third_party/dynamic_annotations/dynamic_annotations.c"
                   (("base/third_party/valgrind") "valgrind"))
                 (substitute*
                   '("third_party/breakpad/breakpad/src/common/linux/http_upload.cc"
                     "third_party/breakpad/breakpad/src/common/linux/libcurl_wrapper.h")
                   (("third_party/curl") "curl"))
                 (substitute*
                   '("components/viz/common/gpu/vulkan_context_provider.h"
                     "components/viz/common/resources/resource_format_utils.h"
                     "gpu/config/gpu_info_collector_win.cc"
                     "gpu/config/gpu_util.cc"
                     "gpu/config/vulkan_info.h")
                   (("third_party/vulkan_headers/include/")
                    ""))

                 ;; Replace Google Analytics bundle with an empty file and hope
                 ;; no one notices.
                 (mkdir-p "third_party/analytics")
                 (call-with-output-file
                     "third_party/analytics/google-analytics-bundle.js"
                   (lambda (port)
                     (const #t)))))
             ;; Do not enable support for loading the Widevine DRM plugin.
             (substitute* "src/buildtools/config/common.pri"
               (("enable_widevine=true")
                "enable_widevine=false")))))))
    (build-system gnu-build-system)
    (native-inputs
     (list bison
           flex
           gperf
           ninja
           node
           perl
           pkg-config
           python2-six
           python-2
           ruby))
    (inputs
     (list alsa-lib
           atk
           cups-minimal
           curl
           dbus
           ffmpeg
           fontconfig
           harfbuzz
           icu4c
           jsoncpp
           lcms
           libcap
           libevent
           libgcrypt
           libjpeg-turbo
           libvpx
           libwebp
           libx11
           libxcb
           libxcomposite
           libxcursor
           libxkbfile
           libxi
           libxkbcommon
           ;; FIXME: libxml2 needs to built with icu support though it links to
           ;; libxml2 configure summary still states "Checking for compatible
           ;; system libxml2... no"
           libxml2
           openh264
           libxrandr
           libxrender
           libxslt
           libxtst
           mesa
           minizip
           nss
           opus
           pciutils
           protobuf
           pulseaudio
           qtbase-5
           qtdeclarative-5
           qtmultimedia-5
           qtwebchannel-5
           re2
           snappy
           eudev
           valgrind
           vulkan-headers
           xcb-util))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-5)
       ((#:modules modules '())
        `((guix build gnu-build-system)
          (guix build utils)
          (ice-9 textual-ports)))
       ((#:phases phases)
        #~(modify-phases #$phases
           (add-before 'configure 'substitute-source
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out #$output))
                 (with-atomic-file-replacement
                  "src/buildtools/config/linux.pri"
                  (lambda (in out)
                    (display (get-string-all in) out)
                    (display "\ngn_args += use_system_openh264=true\n" out)))
                 ;; Qtwebengine is not installed into the same prefix as
                 ;; qtbase.  Some qtbase QTLibraryInfo constants will not
                 ;; work.  Replace with the full path to the qtwebengine-5
                 ;; translations and locales in the store.
                 (substitute* "src/core/web_engine_library_info.cpp"
                   (("QLibraryInfo::location\\(QLibraryInfo::TranslationsPath\\)")
                    (string-append "QLatin1String(\"" out "/share/qt5/translations\")"))
                   (("QLibraryInfo::location\\(QLibraryInfo::DataPath\\)")
                    (string-append "QLatin1String(\"" out "/share/qt5\")")))
                 ;; Substitute full dynamic library path for nss.
                 (substitute* "src/3rdparty/chromium/crypto/nss_util.cc"
                   (("libnssckbi.so")
                    (search-input-file inputs "lib/nss/libnssckbi.so")))
                 ;; Substitute full dynamic library path for udev.
                 (substitute* "src/3rdparty/chromium/device/udev_linux/udev1_loader.cc"
                   (("libudev.so.1")
                    (search-input-file inputs "lib/libudev.so.1"))))))
           (add-before 'configure 'set-env
             (lambda _
               ;; Avoids potential race conditions.
               (setenv "PYTHONDONTWRITEBYTECODE" "1")
               (setenv "NINJAFLAGS"
                       (string-append "-k1" ;less verbose build output
                                      ;; Respect the '--cores' option of 'guix build'.
                                      " -j" (number->string (parallel-job-count))))))
           (replace 'configure
             (lambda _
               ;; Valid QT_BUILD_PARTS variables are:
               ;; libs tools tests examples demos docs translations
               (invoke "qmake" "QT_BUILD_PARTS = libs tools" "--"
                       "--webengine-printing-and-pdf=no"
                       "--webengine-ffmpeg=system"
                       ;; FIXME: Building qtwebengine-5 5.12.2 with
                       ;; icu4c >= 68 fails.
                       ;;"--webengine-icu=system"
                       "--webengine-pepper-plugins=no"
                       "-webengine-proprietary-codecs")))))
       ;; Tests are disabled due to "Could not find QtWebEngineProcess error"
       ;; It's possible this can be fixed by setting QTWEBENGINEPROCESS_PATH
       ;; before running tests.
       ((#:tests? _ #f) #f)))
    (native-search-paths
     (list (search-path-specification
            (file-type 'regular)
            (separator #f)
            (variable "QTWEBENGINEPROCESS_PATH")
            (files '("lib/qt5/libexec/QtWebEngineProcess")))))
    (home-page "https://wiki.qt.io/QtWebEngine")
    (synopsis "Qt WebEngine module")
    (description "The Qt5WebEngine module provides support for web applications
using the Chromium browser project.  The Chromium source code has Google services
and binaries removed, and adds modular support for using system libraries.")
    (license license:lgpl2.1+)))

(define-public qtwebengine
  (package
    (name "qtwebengine")
    (version "6.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (qt-urls name version))
       (sha256
        (base32
         "0ivfsqd5c0cxsnssj6z37901cf6a47w50zaqgjiysvcm3ar36ymd"))
       (modules '((ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (guix build utils)))
       (snippet
        '(begin
           (let ((preserved-third-party-files
                  '("base/third_party/double_conversion"
                    "base/third_party/cityhash"
                    "base/third_party/cityhash_v103"
                    "base/third_party/dynamic_annotations"
                    "base/third_party/icu"
                    "base/third_party/libevent"
                    "base/third_party/nspr"
                    "base/third_party/superfasthash"
                    "base/third_party/symbolize"
                    "base/third_party/xdg_mime"
                    "base/third_party/xdg_user_dirs"
                    "net/third_party/mozilla_security_manager"
                    "net/third_party/nss"
                    "net/third_party/quiche"
                    "net/third_party/uri_template"
                    "third_party/abseil-cpp"
                    "third_party/angle"
                    "third_party/angle/src/common/third_party/base"
                    "third_party/angle/src/common/third_party/smhasher"
                    "third_party/angle/src/common/third_party/xxhash"
                    "third_party/axe-core"
                    "third_party/blink"
                    "third_party/boringssl"
                    "third_party/boringssl/src/third_party/fiat"
                    "third_party/breakpad"
                    "third_party/brotli"
                    "third_party/ced"
                    "third_party/cld_3"
                    "third_party/closure_compiler"
                    "third_party/crashpad"
                    "third_party/crashpad/crashpad/third_party/lss"
                    "third_party/crashpad/crashpad/third_party/zlib"
                    "third_party/crc32c"
                    "third_party/dav1d"
                    "third_party/dawn"
                    "third_party/devtools-frontend"
                    "third_party/devtools-frontend/src/front_end/third_party/lighthouse"
                    "third_party/devtools-frontend/src/front_end/third_party/wasmparser"
                    "third_party/emoji-segmenter"
                    "third_party/ffmpeg"
                    "third_party/googletest"
                    "third_party/harfbuzz-ng/utils"
                    "third_party/hunspell"
                    "third_party/iccjpeg"
                    "third_party/icu"
                    "third_party/inspector_protocol"
                    "third_party/jinja2"
                    "third_party/jsoncpp"
                    "third_party/jstemplate"
                    "third_party/khronos"
                    "third_party/leveldatabase"
                    "third_party/libaddressinput"
                    "third_party/libgifcodec"
                    "third_party/libjingle_xmpp"
                    "third_party/libjpeg_turbo"
                    "third_party/libpng"
                    "third_party/libsrtp"
                    "third_party/libsync"
                    "third_party/libudev"
                    "third_party/libvpx"
                    "third_party/libwebm"
                    "third_party/libwebp"
                    "third_party/libxml"
                    "third_party/libxslt"
                    "third_party/libyuv"
                    "third_party/lss"
                    "third_party/mako"
                    "third_party/markupsafe"
                    "third_party/mesa_headers"
                    "third_party/metrics_proto"
                    "third_party/modp_b64"
                    "third_party/nasm"
                    "third_party/one_euro_filter"
                    "third_party/openh264/src/codec/api/svc"
                    "third_party/opus"
                    "third_party/ots"
                    "third_party/pdfium"
                    "third_party/pdfium/third_party/agg23"
                    "third_party/pdfium/third_party/base"
                    "third_party/pdfium/third_party/freetype"
                    "third_party/pdfium/third_party/lcms"
                    "third_party/pdfium/third_party/libopenjpeg20"
                    "third_party/pdfium/third_party/skia_shared"
                    "third_party/perfetto"
                    "third_party/pffft"
                    "third_party/ply"
                    "third_party/polymer"
                    "third_party/protobuf"
                    "third_party/protobuf/third_party/six"
                    "third_party/pyjson5"
                    "third_party/re2"
                    "third_party/rnnoise"
                    "third_party/skia"
                    "third_party/skia/include/third_party/skcms/skcms.h"
                    "third_party/skia/include/third_party/vulkan"
                    "third_party/skia/third_party/skcms"
                    "third_party/skia/third_party/vulkanmemoryallocator"
                    "third_party/smhasher"
                    "third_party/snappy"
                    "third_party/sqlite"
                    "third_party/usb_ids"
                    "third_party/usrsctp"
                    "third_party/web-animations-js"
                    "third_party/webrtc"
                    "third_party/webrtc/common_audio/third_party/ooura"
                    "third_party/webrtc/common_audio/third_party/spl_sqrt_floor"
                    "third_party/webrtc/modules/third_party/fft"
                    "third_party/webrtc/modules/third_party/g711"
                    "third_party/webrtc/modules/third_party/g722"
                    "third_party/webrtc/rtc_base/third_party/base64"
                    "third_party/webrtc/rtc_base/third_party/sigslot"
                    "third_party/webrtc_overrides"
                    "third_party/widevine/cdm/widevine_cdm_common.h"
                    "third_party/widevine/cdm/widevine_cdm_version.h"
                    "third_party/woff2"
                    "third_party/zlib"
                    "url/third_party/mozilla"
                    "v8/src/third_party/utf8-decoder"
                    "v8/src/third_party/valgrind"
                    "v8/src/third_party/siphash"
                    "v8/third_party/v8/builtins"
                    "v8/third_party/inspector_protocol"))
                 (protected (make-regexp "\\.(gn|gyp)i?$")))
             (define preserved-club
               (map (lambda (member)
                      (string-append "./" member))
                    preserved-third-party-files))
             (define (empty? dir)
               (equal? (scandir dir) '("." "..")))
             (define (third-party? file)
               (string-contains file "third_party/"))
             (define (useless? file)
               (any (cute string-suffix? <> file)
                    '(".zip" ".so" ".dll" ".exe" ".jar")))
             (define (parents child)
               ;; Return all parent directories of CHILD up to and including
               ;; the closest "third_party".
               (let* ((dirs (match (string-split child #\/)
                              ((dirs ... last) dirs)))
                      (closest (list-index (lambda (dir)
                                             (string=? "third_party" dir))
                                           (reverse dirs)))
                      (delim (- (length dirs) closest)))
                 (fold (lambda (dir prev)
                         (cons (string-append (car prev) "/" dir)
                               prev))
                       (list (string-join (list-head dirs delim) "/"))
                       (list-tail dirs delim))))
             (define (remove-loudly file)
               (format #t "deleting ~a...~%" file)
               (force-output)
               (delete-file file))
             (define (delete-unwanted-files child stat flag base level)
               (match flag
                 ((or 'regular 'symlink 'stale-symlink)
                  (when (third-party? child)
                    (unless (or (member child preserved-club)
                                (any (cute member <> preserved-club)
                                     (parents child))
                                (regexp-exec protected child))
                      (remove-loudly child)))
                  (when (and (useless? child) (file-exists? child))
                    (remove-loudly child)))
                 ('directory-processed
                  (when (empty? child)
                    (rmdir child)))
                 (_ #t)))

             (with-directory-excursion "src/3rdparty"
               (delete-file-recursively "ninja")

               (with-directory-excursion "chromium"
                 ;; Delete bundled software and binaries that were not
                 ;; explicitly preserved above.
                 (nftw "." delete-unwanted-files 'depth 'physical)

                 ;; Assert that each preserved item is present to catch
                 ;; removals.
                 (for-each (lambda (third-party)
                             (unless (file-exists? third-party)
                               (error (format #f "~s does not exist!~%"
                                              third-party))))
                           preserved-club)

                 ;; Use relative header locations instead of hard coded ones.
                 (substitute*
                     "base/third_party/dynamic_annotations/dynamic_annotations.c"
                   (("base/third_party/valgrind") "valgrind"))
                 (substitute* "third_party/breakpad/breakpad/src/common/\
linux/libcurl_wrapper.h"
                   (("third_party/curl") "curl"))
                 (substitute*
                     '("components/viz/common/gpu/vulkan_context_provider.h"
                       "gpu/config/gpu_util.cc")
                   (("third_party/vulkan/include/")
                    ""))

                 ;; Replace Google Analytics bundle with an empty file and
                 ;; hope no one notices.
                 (mkdir-p "third_party/analytics")
                 (call-with-output-file
                     "third_party/analytics/google-analytics-bundle.js"
                   (lambda (port)
                     (const #t)))))
             ;; Do not enable support for loading the Widevine DRM plugin.
             (substitute* "src/core/CMakeLists.txt"
               (("enable_widevine=true")
                "enable_widevine=false")))))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; XXX: The test suite is not built by default; leave it off to save
      ;; some build time and resources.
      #:tests? #f
      #:configure-flags
      ;; Use the CMake ninja generator, otherwise the build fails (see:
      ;; https://bugreports.qt.io/browse/QTBUG-96897).
      #~(list "-GNinja"
              ;; Manually add the NSS library prefix to the linker
              ;; search path, otherwise it fails to be linked (see:
              ;; https://bugreports.qt.io/browse/QTBUG-105053).
              (string-append "-DCMAKE_SHARED_LINKER_FLAGS=-L"
                             (search-input-directory %build-inputs "lib/nss"))

              ;; The PDF renderer plugin fails to build with errors such as
              ;; "src/3rdparty/chromium/components/pdf
              ;; /renderer/pdf_accessibility_tree.cc:1373:39:
              ;; error: use of undeclared identifier 'IDS_PDF_PAGE_INDEX'";
              ;; disable it.
              "-DQT_FEATURE_webengine_printing_and_pdf=OFF"
              "-DQT_FEATURE_webengine_pepper_plugins=OFF" ;widevine
              "-DQT_FEATURE_system_ffmpeg=ON"
              ;; Do not artificially limit codec support; video decoding is
              ;; done by ffmpeg.
              "-DQT_FEATURE_webengine_proprietary_codecs=ON"
              "-DQT_FEATURE_webengine_system_alsa=ON"
              "-DQT_FEATURE_webengine_system_icu=ON"
              "-DQT_FEATURE_webengine_system_libxml=ON"
              "-DQT_FEATURE_webengine_system_libpci=ON"
              "-DQT_FEATURE_webengine_system_libpng=ON"
              "-DQT_FEATURE_webengine_system_pulseaudio=ON"
              "-DQT_FEATURE_webengine_system_zlib=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Qtwebengine is not installed into the same prefix as qtbase.
              ;; Some qtbase QTLibraryInfo constants will not work.  Replace
              ;; with the full path to the qtwebengine translations and
              ;; locales in the store.
              (substitute* "src/core/web_engine_library_info.cpp"
                (("QLibraryInfo::path\\(QLibraryInfo::TranslationsPath)")
                 (string-append "QLatin1String(\"" #$output
                                "/share/qt6/translations\")"))
                (("QLibraryInfo::path\\(QLibraryInfo::DataPath)")
                 (string-append "QLatin1String(\"" #$output
                                "/share/qt6\")")))
              ;; Substitute full dynamic library path for nss.
              (substitute* "src/3rdparty/chromium/crypto/nss_util.cc"
                (("libnssckbi.so")
                 (search-input-file inputs "lib/nss/libnssckbi.so")))
              ;; Substitute full dynamic library path for udev.
              (substitute* "src/3rdparty/chromium/device/udev_linux/udev1_loader.cc"
                (("libudev.so.1")
                 (search-input-file inputs "lib/libudev.so.1")))
              ;; Patch the location of the X11 keywoard layouts, otherwise
              ;; webengine *crashes* at run time when the default directory,
              ;; '/usr/share/X11/xkb' is empty (see:
              ;; https://bugreports.qt.io/browse/QTBUG-105124).
              (substitute* "src/3rdparty/chromium/ui/events/ozone/layout/xkb\
/xkb_keyboard_layout_engine.cc"
                (("/usr/share/X11/xkb")
                 (search-input-directory inputs "share/X11/xkb")))))
          (add-before 'configure 'prepare-build-environment
            (lambda _
              ;; Avoids potential race conditions.
              (setenv "PYTHONDONTWRITEBYTECODE" "1")
              (setenv "NINJAFLAGS"
                      (string-append
                       "-k1"  ;less verbose build output
                       ;; Respect the '--cores' option of 'guix build'.
                       " -j" (number->string (parallel-job-count))))
              ;; Use Clang/LDD to help tame the memory requirements and hasten
              ;; the build.
              (setenv "AR" "llvm-ar") (setenv "NM" "llvm-nm")
              (setenv "CC" "clang") (setenv "CXX" "clang++")))
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (apply invoke "cmake" "--build" "."
                     (if parallel-build?
                         `("--parallel" ,(number->string (parallel-job-count)))
                         '()))))
          (replace 'install
            (lambda _
              (invoke "cmake" "--install" "."))))))
    (native-inputs
     (modify-inputs (package-native-inputs qtwebengine-5)
       (delete "python2" "python2-six")
       (replace "node" node-lts)
       (append clang-14
               lld-as-ld-wrapper
               python-wrapper
               python-html5lib)))
    (inputs
     (modify-inputs (package-inputs qtwebengine-5)
       (replace "qtbase" qtbase)
       (replace "qtdeclarative" qtdeclarative)
       (replace "qtmultimedia" qtmultimedia)
       (replace "qtwebchannel" qtwebchannel)
       (append libxkbfile xkeyboard-config)))
    (native-search-paths
     (list (search-path-specification
            (file-type 'regular)
            (separator #f)
            (variable "QTWEBENGINEPROCESS_PATH")
            (files '("lib/qt6/libexec/QtWebEngineProcess")))))
    (home-page "https://wiki.qt.io/QtWebEngine")
    (synopsis "Qt WebEngine module")
    (description "The Qt WebEngine module provides support for web
applications using the Chromium browser project.  The Chromium source code has
Google services and binaries removed, and adds modular support for using
system libraries.")
    (license license:lgpl2.1+)))

(define-public single-application-qt5
  ;; Change in function signature, nheko requires at least this commit
  (let ((commit "dc8042b5db58f36e06ba54f16f38b16c5eea9053"))
    (package
      (name "single-application-qt5")
      (version (string-append "3.2.0-" (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/itay-grudev/SingleApplication")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "163aa2x2qb0h8w26si5ql833ilj427jjbdwlz1p2p8iaq6dh0vq1"))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f                    ; no check target
         ;; Projects can decide how to build this library.  You might need to
         ;; override this flag (QApplication, QGuiApplication or
         ;; QCoreApplication).
         #:configure-flags #~(list "-DQAPPLICATION_CLASS=QApplication")
         #:phases
         #~(modify-phases %standard-phases
           ;; No install target, install things manually
           (replace 'install
             (lambda* (#:key inputs outputs source #:allow-other-keys)
               (let ((out #$output))
                 (install-file
                  "libSingleApplication.a" (string-append out "/lib"))
                 (for-each
                  (lambda (file)
                    (install-file
                     (string-append source "/" file)
                     (string-append out "/include")))
                  '("SingleApplication"
                    "singleapplication.h" "singleapplication_p.h"))))))))
      (inputs
       (list qtbase-5))
      (home-page "https://github.com/itay-grudev/SingleApplication")
      (synopsis "Replacement of QtSingleApplication for Qt5 and Qt6")
      (description
       "SingleApplication is a replacement of the QtSingleApplication for Qt5 and Qt6.

It keeps the Primary Instance of your Application and kills each subsequent
instances.  It can (if enabled) spawn secondary (non-related to the primary)
instances and can send data to the primary instance from secondary
instances.")
      (license license:expat))))

(define-public python-sip
  (package
    (name "python-sip")
    (version "5.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (list (pypi-uri "sip" version)
                   (string-append "https://www.riverbankcomputing.com/static/"
                                  "Downloads/sip/" version
                                  "/sip-" version ".tar.gz")))
        (sha256
         (base32
          "1idaivamp1jvbbai9yzv471c62xbqxhaawccvskaizihkd0lq0jx"))))
    (build-system python-build-system)
    (native-inputs
     (list python-wrapper))
    (propagated-inputs
     (list python-toml python-packaging))
    (home-page "https://www.riverbankcomputing.com/software/sip/intro")
    (synopsis "Python binding creator for C and C++ libraries")
    (description
     "SIP is a tool to create Python bindings for C and C++ libraries.  It
was originally developed to create PyQt, the Python bindings for the Qt
toolkit, but can be used to create bindings for any C or C++ library.

SIP comprises a code generator and a Python module.  The code generator
processes a set of specification files and generates C or C++ code, which
is then compiled to create the bindings extension module.  The SIP Python
module provides support functions to the automatically generated code.")
    ;; There is a choice between a python like license, gpl2 and gpl3.
    ;; For compatibility with pyqt, we need gpl3.
    (license license:gpl3)))

(define-public python-sip-4
  (package
    (inherit python-sip)
    (name "python-sip")
    (version "4.19.25")
    (source
      (origin
        (method url-fetch)
        (uri (list (pypi-uri "sip" version)
                   (string-append "https://www.riverbankcomputing.com/static/"
                                  "Downloads/sip/" version
                                  "/sip-" version ".tar.gz")))
        (sha256
         (base32
          "04a23cgsnx150xq86w1z44b6vr2zyazysy9mqax0fy346zlr77dk"))))
    (build-system gnu-build-system)
    (native-inputs
     (list python-wrapper))
    (propagated-inputs `())
    (arguments
     (list #:tests? #f ; no check target
       #:imported-modules `((guix build python-build-system)
                           ,@%gnu-build-system-modules)
       #:modules `((srfi srfi-1)
                  ((guix build python-build-system) #:select (python-version))
                  ,@%gnu-build-system-modules)
       #:phases
       #~(modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out #$output)
                    (bin (string-append out "/bin"))
                    (include (string-append out "/include"))
                    (lib (string-append out "/lib/python"
                                        (python-version #$python-wrapper)
                                        "/site-packages")))
               (invoke "python" "configure.py"
                       "--bindir" bin
                       "--destdir" lib
                       "--incdir" include)))))))
    (license license:gpl3)))

(define-public python-pyqt
  (package
    (name "python-pyqt")
    (version "5.15.5")
    (source
      (origin
        (method url-fetch)
        ;; PyPI is the canonical distribution point of PyQt.  Older
        ;; releases are available from the web site.
        (uri (list (pypi-uri "PyQt5" version)
                   (string-append "https://www.riverbankcomputing.com/static/"
                                  "Downloads/PyQt5/" version "/PyQt5-"
                                  version ".tar.gz")))
        (file-name (string-append "PyQt5-" version ".tar.gz"))
        (sha256
         (base32
          "0aya963kkmbwfwmpd0p6k85y4g7wl5zarjqxxfgir403zalbf4dl"))
        (patches (search-patches "pyqt-configure.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list qtbase-5)) ; for qmake
    (propagated-inputs
     (list python-sip python-pyqt5-sip))
    (inputs
     (list python-wrapper
           qtbase-5
           qtconnectivity
           qtdeclarative-5
           qtlocation
           qtmultimedia-5
           qtsensors
           qtserialport
           qtsvg-5
           qttools-5
           qtwebchannel-5
           qtwebkit
           qtwebsockets-5
           qtx11extras
           qtxmlpatterns))
    (arguments
     (list #:modules `((srfi srfi-1)
                  ((guix build python-build-system) #:select (python-version))
                  ,@%gnu-build-system-modules)
       #:imported-modules `((guix build python-build-system)
                           ,@%gnu-build-system-modules)
       #:phases
       #~(modify-phases %standard-phases
         ;; When building python-pyqtwebengine, <qprinter.h> can not be
         ;; included.  Here we substitute the full path to the header in the
         ;; store.
         (add-before 'configure 'substitute-source
           (lambda* (#:key inputs  #:allow-other-keys)
             (let* ((qtprinter.h (string-append "\"" #$qtbase-5 "/include/qt5/QtPrintSupport/qprinter.h\"")))
               (substitute* "sip/QtPrintSupport/qprinter.sip"
                 (("<qprinter.h>") qtprinter.h)))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out #$output)
                    (bin (string-append out "/bin"))
                    (sip (string-append out "/share/sip"))
                    (plugins (string-append out "/lib/qt5/plugins"))
                    (designer (string-append plugins "/designer"))
                    (qml (string-append plugins "/PyQt5"))
                    (lib (string-append out "/lib/python"
                                        (python-version #$python-wrapper)
                                        "/site-packages"))
                    (stubs (string-append lib "/PyQt5")))
               (invoke "python" "configure.py"
                       "--confirm-license"
                       "--bindir" bin
                       "--destdir" lib
                       "--designer-plugindir" designer
                       "--qml-plugindir" qml
                       ; Where to install the PEP 484 Type Hints stub
                       ; files. Without this the stubs are tried to be
                       ; installed into the python package's
                       ; site-package directory, which is read-only.
                       "--stubsdir" stubs
                       "--sipdir" sip)))))))
    (home-page "https://www.riverbankcomputing.com/software/pyqt/intro")
    (synopsis "Python bindings for Qt")
    (description
     "PyQt is a set of Python v2 and v3 bindings for the Qt application
framework.  The bindings are implemented as a set of Python modules and
contain over 620 classes.")
    (license license:gpl3)))

(define-public python-pyqt5-sip
  (package
    (name "python-pyqt5-sip")
    (version "12.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyQt5_sip" version))
       (sha256
        (base32
         "09771b6fdn0rx34l5a0wzcd899yd57zxp5sw3bsqhd25biwhiq4p"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; No test code.
    (home-page "https://www.riverbankcomputing.com/software/sip/")
    (synopsis "Sip module support for PyQt5")
    (description "Sip module support for PyQt5")
    (license license:lgpl2.1+)))

(define-public python-pyqtwebengine
  (package
    (name "python-pyqtwebengine")
    (version "5.15.5")
    (source
     (origin
       (method url-fetch)
       ;; The newest releases are only available on PyPI.  Older ones
       ;; are mirrored at the upstream home page.
       (uri (list (pypi-uri "PyQtWebEngine" version)
                  (string-append "https://www.riverbankcomputing.com/static"
                                 "/Downloads/PyQtWebEngine/" version
                                 "/PyQtWebEngine-" version ".tar.gz")))
       (sha256
        (base32
         "0hdr0g0rzlhsnylhfk826pq1lw8p9dqcr8yma2wy9dgjrj6n0ixb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list python python-sip
           ;; qtbase is required for qmake
           qtbase-5))
    (inputs
     `(("python" ,python-wrapper)
       ("python-sip" ,python-sip)
       ("python-pyqt" ,python-pyqt-without-qtwebkit)
       ("qtbase" ,qtbase-5)
       ("qtsvg-5" ,qtsvg-5)
       ("qtdeclarative-5" ,qtdeclarative-5)
       ("qtwebchannel-5" ,qtwebchannel-5)
       ("qtwebengine-5" ,qtwebengine-5)))
    (arguments
     `(#:modules ((srfi srfi-1)
                  ((guix build python-build-system) #:select (python-version))
                  ,@%gnu-build-system-modules)
       #:imported-modules ((guix build python-build-system)
                           ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sipdir (string-append out "/share/sip"))
                    (pyqt-sipdir (string-append
                                  (assoc-ref inputs "python-pyqt") "/share/sip"))
                    (python (assoc-ref inputs "python"))
                    (lib (string-append out "/lib/python"
                                        (python-version python)
                                        "/site-packages/PyQt5"))
                    (stubs (string-append lib "/PyQt5")))

               (mkdir-p sipdir)
               (invoke "python" "configure.py"
                       "-w"
                       "--no-dist-info"
                       "--destdir" lib
                       "--no-qsci-api"
                       "--stubsdir" stubs
                       "--sipdir" sipdir
                       "--pyqt-sipdir" pyqt-sipdir))))
         ;; Because this has a different prefix than python-pyqt then we need
         ;; to make this a namespace of its own.
         (add-after 'install 'make-namespace
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((__init__.py (string-append
                                  (assoc-ref outputs "out")
                                  "/lib/python"
                                  (python-version (assoc-ref inputs "python"))
                                  "/site-packages/PyQt5/__init__.py")))
               (with-output-to-file __init__.py
                 (lambda _ (display "
from pkgutil import extend_path
__path__ = extend_path(__path__, __name__)
")))))))))
    (home-page "https://www.riverbankcomputing.com/software/pyqtwebengine/intro")
    (synopsis "Python bindings for QtWebEngine")
    (description
     "PyQtWebEngine is a set of Python bindings for The Qt Company's Qt
WebEngine libraries.  The bindings sit on top of PyQt5 and are implemented as a
set of three modules.  Prior to v5.12 these bindings were part of PyQt
itself.")
    (license license:gpl3)))

;; XXX: This is useful for removing qtwebkit from other packages' dependency
;; graphs, as well as for preventing python-pyqtwebengine from transitively
;; depending on qtwebkit.
;; Ultimately, it would be nicer to have a more modular set of python-pyqt-*
;; packages that could be used together.
(define-public python-pyqt-without-qtwebkit
  (package/inherit python-pyqt
    (name "python-pyqt-without-qtwebkit")
    (inputs
     (alist-delete "qtwebkit" (package-inputs python-pyqt)))))

(define-public python-pyqt-builder
  (package
   (name "python-pyqt-builder")
   (version "1.9.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "PyQt-builder" version))
     (sha256
      (base32
       "0nh0054c54ji3sm6d268fccf0y5f613spswwgwqd3rnn816hnljl"))))
   (build-system python-build-system)
   (inputs
    (list python-sip))
   (home-page "https://www.riverbankcomputing.com/static/Docs/PyQt-builder/")
   (synopsis "PEP 517 compliant PyQt build system")
   (description "PyQt-builder is a tool for generating Python bindings for C++
libraries that use the Qt application framework.  The bindings are built on
top of the PyQt bindings for Qt.  PyQt-builder is used to build PyQt itself.")
   ;; Either version 2 or 3, but no other version. See the file
   ;; 'pyqtbuild/builder.py' in the source distribution for more information.
   (license (list license:gpl2 license:gpl3))))

(define-public python-qtpy
  (package
    (name "python-qtpy")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "QtPy" version))
       (sha256
          (base32
           "051rj10lbv2ny48lz34zhclcbdxxdbk4di2mdk91m9143w91npyq"))))
    (build-system python-build-system)
    (propagated-inputs (list python-packaging))
    (arguments
     `(;; Not all supported bindings are packaged. Especially PyQt4.
       #:tests? #f))
    (home-page "https://github.com/spyder-ide/qtpy")
    (synopsis
     "Qt bindings (PyQt5, PyQt4 and PySide) and additional custom QWidgets")
    (description
     "Provides an abstraction layer on top of the various Qt bindings
(PyQt5, PyQt4 and PySide) and additional custom QWidgets.")
    (license license:expat)))

(define-public python-qt.py
  (package
    (name "python-qt.py")
    (version "1.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Qt.py" version))
       (sha256
        (base32 "07rvfwzjl378j75j2va0c6xylwx16icxa6dycsjgjc329pgpng40"))))
    (build-system python-build-system)
    (native-inputs (list python-pyqt))
    (home-page "https://github.com/mottosso/Qt.py")
    (synopsis "Abstraction layer for Python Qt bindings")
    (description
     "This package provides an abstraction layer on top of the various Qt
bindings (PySide, PySide2, PyQt4 and PyQt5).")
    (license license:expat)))

(define-public qscintilla
  (package
    (name "qscintilla")
    (version "2.11.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.riverbankcomputing.com/static"
                                  "/Downloads/QScintilla/" version
                                  "/QScintilla-" version ".tar.gz"))
              (sha256
               (base32
                "19r0vpqb4m9bqwxmjp9w6x0hgahkrg7zryk78hwgplj7vdbn0d77"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (chdir "Qt4Qt5")
               (substitute* "qscintilla.pro"
                 (("\\$\\$\\[QT_INSTALL_LIBS\\]")
                  (string-append out "/lib"))
                 (("\\$\\$\\[QT_INSTALL_HEADERS\\]")
                  (string-append out "/include"))
                 (("\\$\\$\\[QT_INSTALL_TRANSLATIONS\\]")
                  (string-append out "/translations"))
                 (("\\$\\$\\[QT_INSTALL_DATA\\]")
                  (string-append out "/lib/qt$${QT_MAJOR_VERSION}"))
                 (("\\$\\$\\[QT_HOST_DATA\\]")
                 (string-append out "/lib/qt$${QT_MAJOR_VERSION}")))
               (invoke "qmake")))))))
    (native-inputs (list qtbase-5))
    (home-page "https://www.riverbankcomputing.co.uk/software/qscintilla/intro")
    (synopsis "Qt port of the Scintilla C++ editor control")
    (description "QScintilla is a port to Qt of Neil Hodgson's Scintilla C++
editor control.  QScintilla includes features especially useful when editing
and debugging source code.  These include support for syntax styling, error
indicators, code completion and call tips.")
    (license license:gpl3+)))

(define-public python-qscintilla
  (package/inherit qscintilla
    (name "python-qscintilla")
    (arguments
     `(#:configure-flags
       (list "--pyqt=PyQt5"
             (string-append "--pyqt-sipdir="
                            (assoc-ref %build-inputs "python-pyqt")
                            "/share/sip")
             (string-append "--qsci-incdir="
                            (assoc-ref %build-inputs "qscintilla")
                            "/include")
             (string-append "--qsci-libdir="
                            (assoc-ref %build-inputs "qscintilla")
                            "/lib"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
             (let ((out    (assoc-ref outputs "out"))
                   (python (assoc-ref inputs "python")))
               (chdir "Python")
               (apply invoke "python3" "configure.py"
                      configure-flags)
               ;; Install to the right directory
               (substitute* '("Makefile"
                              "Qsci/Makefile")
                 (("\\$\\(INSTALL_ROOT\\)/gnu/store/[^/]+") out)
                 (((string-append python "/lib"))
                  (string-append out "/lib")))
               ;; And fix the installed.txt file
               (substitute* "installed.txt"
                 (("/gnu/store/[^/]+") out))))))))
    (inputs
     `(("qscintilla" ,qscintilla)
       ("python" ,python)
       ("python-pyqt" ,python-pyqt)))
    (description "QScintilla is a port to Qt of Neil Hodgson's Scintilla C++
editor control.  QScintilla includes features especially useful when editing
and debugging source code.  These include support for syntax styling, error
indicators, code completion and call tips.

This package provides the Python bindings.")))

;; PyQt only looks for modules in its own directory.  It ignores environment
;; variables such as PYTHONPATH, so we need to build a union package to make
;; it work.
(define-public python-pyqt+qscintilla
  (package/inherit python-pyqt
    (name "python-pyqt+qscintilla")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (match %build-inputs
                     (((names . directories) ...)
                      (union-build (assoc-ref %outputs "out")
                                   directories))))))
    (inputs
     `(("python-pyqt" ,python-pyqt)
       ("python-qscintilla" ,python-qscintilla)))
    (synopsis "Union of PyQt and the Qscintilla extension")
    (description
     "This package contains the union of PyQt and the Qscintilla extension.")))

(define-public qtkeychain
  (package
    (name "qtkeychain")
    (version "0.13.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/frankosterfeld/qtkeychain/")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1zk6r2vc1q48qs7mw2h47bpgrfbb9r7lf9cwq4sb1a4nls87zznk"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config qttools-5))
    (inputs
     (list libsecret qtbase-5))
    (arguments
     `(#:tests? #f ; No tests included
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-qt-trans-dir
           (lambda _
             (substitute* "CMakeLists.txt"
              (("\\$\\{qt_translations_dir\\}")
               "${CMAKE_INSTALL_PREFIX}/share/qt5/translations")))))))
    (home-page "https://github.com/frankosterfeld/qtkeychain")
    (synopsis "Qt API to store passwords")
    (description
      "QtKeychain is a Qt library to store passwords and other secret data
securely.  It will not store any data unencrypted unless explicitly requested.")
    (license license:bsd-3)))

(define-public qtsolutions
  (let ((commit "9568abd142d581b67b86a5f63d823a34b0612702")
        (revision "53"))
    (package
      (name "qtsolutions")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/qtproject/qt-solutions")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17fnmassflm3vxi0krpr6fff368jy38cby31a48rban4nqqmgx7n"))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (srfi srfi-1)))
         (snippet
          ;; Unvendor QtLockFile from QtSingleApplication.
          '(begin
             (with-directory-excursion "qtsingleapplication/src"
               (for-each delete-file
                         (find-files "." "qtlockedfile.*\\.(h|cpp)"))
                 (substitute* "qtsingleapplication.pri"
                   ;; Add include path of LockedFile.
                   (("INCLUDEPATH \\+=")
                    "INCLUDEPATH += ../../qtlockedfile/src")
                   ;; Link library of LockedFile.
                   (("LIBS \\+=")
                    "LIBS += -lQtSolutions_LockedFile"))
                 (substitute* '("qtlocalpeer.h" "qtlocalpeer.cpp")
                   (("#include \"qtlockedfile.*\\.cpp\"") "")
                   ;; Unwrap namespace added in the vendoring process.
                   (("QtLP_Private::QtLockedFile")
                    "QtLockedFile")))))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:imported-modules
         ((guix build copy-build-system)
          ,@%gnu-build-system-modules)
         #:modules
         (((guix build copy-build-system) #:prefix copy:)
          (guix build gnu-build-system)
          (guix build utils))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-source
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* (find-files "." "common.pri")
                 ;; Remove unnecessary prefixes/suffixes in library names.
                 (("qt5") "qt")
                 (("-head") ""))
               ;; Disable building of examples.
               (substitute* (find-files "." "\\.pro$")
                 (("SUBDIRS\\+=examples") ""))
               ;; Fix deprecated functions.
               (substitute* "qtsoap/src/qtsoap.cpp"
                 (("toAscii") "toUtf8"))))
           (replace 'configure
             (lambda _
               (for-each (lambda (solution)
                           (with-directory-excursion solution
                             (invoke "./configure" "-library")
                             (invoke "qmake")))
                         '("qtlockedfile" "qtpropertybrowser" "qtservice"
                           "qtsingleapplication" "qtsoap"))))
           (replace 'build
             (lambda _
               (for-each (lambda (solution)
                           (with-directory-excursion solution
                             (invoke "make")))
                         '("qtlockedfile" "qtpropertybrowser" "qtservice"
                           "qtsingleapplication" "qtsoap"))))
           (replace 'install
             (lambda args
               (for-each (lambda (solution)
                           (with-directory-excursion solution
                             (apply
                              (assoc-ref copy:%standard-phases 'install)
                              #:install-plan
                              '(("src" "include" #:include-regexp ("\\.h$"))
                                ("lib" "lib"))
                              args)))
                         '("qtlockedfile" "qtpropertybrowser" "qtservice"
                           "qtsingleapplication" "qtsoap")))))))
      (inputs
       (list qtbase-5))
      (synopsis "Collection of Qt extensions")
      (description "QtSolutions is a set of components extending Qt.
@itemize
@item QtLockedFile: A class that extends QFile with advisory locking functions.
@item QtPropertyBrowser: A framework that enables the user to edit a set of
properties.
@item QtService: A helper for writing services such as Unix daemons.
@item QtSingleApplication: A component that provides support for applications
that can be only started once per user.
@item QtSoap: A component that provides basic web service support with version
1.1 of the SOAP protocol.
@end itemize\n")
      (home-page "https://doc.qt.io/archives/qq/qq09-qt-solutions.html")
      (license (list license:bsd-3
                     ;; QScriptParser and QScriptGrammar specifically allow
                     ;; redistribution under GPL3 or LGPL2.1
                     license:gpl3 license:lgpl2.1)))))

(define-public qwt
  (package
    (name "qwt")
    (version "6.1.5")
    (source
      (origin
        (method url-fetch)
        (uri
         (string-append "mirror://sourceforge/qwt/qwt/"
                        version "/qwt-" version ".tar.bz2"))
        (sha256
         (base32 "0hf0mpca248xlqn7xnzkfj8drf19gdyg5syzklvq8pibxiixwxj0"))))
  (build-system gnu-build-system)
  (inputs
   (list qtbase-5 qtsvg-5 qttools-5))
  (arguments
   `(#:phases
     (modify-phases %standard-phases
       (replace 'configure
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (docdir (string-append out "/share/doc/qwt"))
                  (incdir (string-append out "/include/qwt"))
                  (pluginsdir (string-append out "/lib/qt5/plugins/designer"))
                  (featuresdir (string-append out "/lib/qt5/mkspecs/features")))
             (substitute* '("qwtconfig.pri")
               (("^(\\s*QWT_INSTALL_PREFIX)\\s*=.*" _ x)
                (format #f "~a = ~a\n" x out))
               (("^(QWT_INSTALL_DOCS)\\s*=.*" _ x)
                (format #f "~a = ~a\n" x docdir))
               (("^(QWT_INSTALL_HEADERS)\\s*=.*" _ x)
                (format #f "~a = ~a\n" x incdir))
               (("^(QWT_INSTALL_PLUGINS)\\s*=.*" _ x)
                (format #f "~a = ~a\n" x pluginsdir))
               (("^(QWT_INSTALL_FEATURES)\\s*=.*" _ x)
                (format #f "~a = ~a\n" x featuresdir)))
             (substitute* '("doc/doc.pro")
               ;; We'll install them in the 'install-man-pages' phase.
               (("^unix:doc\\.files.*") ""))
             (invoke "qmake"))))
       (add-after 'install 'install-man-pages
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (man (string-append out "/share/man")))
             ;; Remove some incomplete manual pages.
             (for-each delete-file (find-files "doc/man/man3" "^_tmp.*"))
             (mkdir-p man)
             (copy-recursively "doc/man" man)))))))
  (home-page "http://qwt.sourceforge.net")
  (synopsis "Qt widgets for plots, scales, dials and other technical software
GUI components")
  (description
   "The Qwt library contains widgets and components which are primarily useful
for technical and scientific purposes.  It includes a 2-D plotting widget,
different kinds of sliders, and much more.")
  (license
   (list
    ;; The Qwt license is LGPL2.1 with some exceptions.
    (license:non-copyleft "http://qwt.sourceforge.net/qwtlicense.html")
    ;; textengines/mathml/qwt_mml_document.{cpp,h} is dual LGPL2.1/GPL3 (either).
    license:lgpl2.1 license:gpl3))))

(define-public qtwebkit
  (package
    (name "qtwebkit")
    (version "5.212.0-alpha4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/annulen/webkit/releases/download/"
                            "qtwebkit-" version "/qtwebkit-" version ".tar.xz"))
        (sha256
         (base32
          "1rm9sjkabxna67dl7myx9d9vpdyfxfdhrk9w7b94srkkjbd2d8cw"))
        (patches (search-patches "qtwebkit-pbutils-include.patch"
                                 "qtwebkit-fix-building-with-bison-3.7.patch"
                                 "qtwebkit-fix-building-with-glib-2.68.patch"
                                 "qtwebkit-fix-building-with-icu-68.patch"
                                 "qtwebkit-fix-building-with-python-3.9.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     (list perl
           python
           ruby
           bison
           flex
           gperf
           pkg-config))
    (inputs
     `(("icu" ,icu4c)
       ("glib" ,glib)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libwebp" ,libwebp)
       ("sqlite" ,sqlite)
       ("fontconfig" ,fontconfig)
       ("libxrender" ,libxrender)
       ("qtbase" ,qtbase-5)
       ("qtdeclarative-5" ,qtdeclarative-5)
       ("qtlocation" ,qtlocation)
       ("qtmultimedia-5" ,qtmultimedia-5)
       ("qtsensors" ,qtsensors)
       ("qtwebchannel-5" ,qtwebchannel-5)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)))
    (arguments
     `(#:tests? #f ; no apparent tests; it might be necessary to set
                   ; ENABLE_API_TESTS, see CMakeLists.txt

       ;; Parallel builds fail due to a race condition:
       ;; <https://bugs.gnu.org/34062>.
       #:parallel-build? #f

       #:configure-flags (list ;"-DENABLE_API_TESTS=TRUE"
                               "-DPORT=Qt"
                               "-DUSE_LIBHYPHEN=OFF"
                               "-DUSE_SYSTEM_MALLOC=ON"
                               ;; XXX: relative dir installs to build dir?
                               (string-append "-DECM_MKSPECS_INSTALL_DIR="
                                              %output "/lib/qt5/mkspecs/modules")
                               ;; Sacrifice a little speed in order to link
                               ;; libraries and test executables in a
                               ;; reasonable amount of memory.
                               "-DCMAKE_SHARED_LINKER_FLAGS=-Wl,--no-keep-memory"
                               "-DCMAKE_EXE_LINKER_FLAGS=-Wl,--no-keep-memory")))
    (home-page "https://www.webkit.org")
    (synopsis "Web browser engine and classes to render and interact with web
content")
    (description "QtWebKit provides a Web browser engine that makes it easy to
embed content from the World Wide Web into your Qt application.  At the same
time Web content can be enhanced with native controls.")
    ;; Building QtWebKit takes around 13 hours on an AArch64 machine.  Give some
    ;; room for slower or busy hardware.
    (properties '((timeout . 64800)))   ;18 hours

    ;; XXX: This consumes too much RAM to successfully build on AArch64 (e.g.,
    ;; SoftIron OverDrive with 8 GiB of RAM), so instead of wasting resources,
    ;; disable it on non-Intel platforms.
    (supported-systems '("x86_64-linux" "i686-linux"))

    (license license:lgpl2.1+)))

(define-public dotherside
  (package
    (name "dotherside")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/filcuc/DOtherSide")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "13n2qb8q9jz4ihwlbs7y15lw90w9113gb1bgnb1dggpxkj64r953"))))
    (build-system cmake-build-system)
    (native-inputs
     (list qttools-5))
    (inputs
     ;; TODO: Support Qt 6 (requires qtdeclarative of Qt6).
     (list qtbase-5 qtdeclarative-5))
    (home-page "https://filcuc.github.io/DOtherSide/index.html")
    (synopsis "C language library for creating bindings for the Qt QML language")
    (description
     "DOtherSide is a C language library for creating bindings for the
QT QML language.  The following features are implementable from
a binding language:
@itemize
@item Creating custom QObject
@item Creating custom QAbstractListModels
@item Creating custom properties, signals and slots
@item Creating from QML QObject defined in the binded language
@item Creating from Singleton QML QObject defined in the binded language
@end itemize\n")
    (license license:lgpl3)))                    ;version 3 only (+ exception)

(define-public qtcolorwidgets
  (package
    (name "qtcolorwidgets")
    (version "2.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/mattia.basaglia/Qt-Color-Widgets")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fp7sr5a56bjp2abc6ng331q0bwvk6mf2nxdga81aj6cd9afs22q"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ;there are no tests
    (native-inputs
     (list qttools-5))
    (inputs
     (list qtbase-5))
    (home-page "https://gitlab.com/mattia.basaglia/Qt-Color-Widgets")
    (synopsis "Color management widgets")
    (description "QtColorWidgets provides a Qt color dialog that is more
user-friendly than the default @code{QColorDialog} and several other
color-related widgets.")
    ;; Includes a license exception for combining with GPL2 code.
    (license license:lgpl3+)))

(define-public qcustomplot
  (package
    (name "qcustomplot")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.qcustomplot.com/release/"
                           version "fixed" "/QCustomPlot.tar.gz"))
       (sha256
        (base32 "1324kqyj1v1f8k8d7b15gc3apwz9qxx52p86hvchg33hjdlqhskx"))))
    (native-inputs
     `(("qcustomplot-sharedlib"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://www.qcustomplot.com/release/"
                               version "fixed" "/QCustomPlot-sharedlib.tar.gz"))
           (sha256
            (base32 "0vp8lpxvd1nlp4liqrlvslpqrgfn0wpiwizzdsjbj22zzb8vxikc"))))))
    (inputs
     (list qtbase-5))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-extra-files
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "-xvf" (assoc-ref inputs "qcustomplot-sharedlib"))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir "qcustomplot-sharedlib/sharedlib-compilation")
             (substitute* "sharedlib-compilation.pro"
               ;; Don't build debug library.
               (("debug_and_release")
                "release"))
             (invoke "qmake"
                     (string-append "DESTDIR="
                                    (assoc-ref outputs "out")
                                    "/lib"))))
         (add-after 'install 'install-header
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "../../qcustomplot.h"
                           (string-append (assoc-ref outputs "out")
                                          "/include")))))))
    (home-page "https://www.qcustomplot.com/")
    (synopsis "Qt widget for plotting and data visualization")
    (description
     "QCustomPlot is a Qt C++ widget providing 2D plots, graphs and charts.")
    (license license:gpl3+)))

;; TODO: Split shiboken2 binding generator into a dedicated output.
;; This executable requires libxml2, libxslt, clang-toolchain at runtime.
;; The libshiboken library only requires Qt and Python at runtime.
(define-public python-shiboken-2
  (package
    (name "python-shiboken-2")
    (version "5.15.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.qt.io/official_releases"
                                  "/QtForPython/pyside2/PySide2-" version
                                  "-src/pyside-setup-opensource-src-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0cwvw6695215498rsbm2xzkwaxdr3w7zfvy4kc62c01k6pxs881r"))))
    (build-system cmake-build-system)
    (inputs
     (list clang-toolchain
           libxml2
           libxslt
           python-wrapper
           qtbase-5
           qtxmlpatterns))
    (arguments
     (list
      #:tests? #f
      ;; FIXME: Building tests fails
      #:configure-flags #~(list "-DBUILD_TESTS=off")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-shiboken-dir-only
            (lambda _ (chdir "sources/shiboken2")))
          (add-before 'configure 'make-files-writable-and-update-timestamps
            (lambda _
              ;; The build scripts need to modify some files in
              ;; the read-only source directory, and also attempts
              ;; to create Zip files which fails because the Zip
              ;; format does not support timestamps before 1980.
              (let ((circa-1980 (* 10 366 24 60 60)))
                (for-each (lambda (file)
                            (make-file-writable file)
                            (utime file circa-1980 circa-1980))
                          (find-files ".")))))
          (add-before 'configure 'set-build-env
            (lambda _
              (let ((llvm #$(this-package-input "clang-toolchain")))
                (setenv "CLANG_INSTALL_DIR" llvm)))))))
    (home-page "https://wiki.qt.io/Qt_for_Python")
    (synopsis
     "Shiboken generates bindings for C++ libraries using CPython source code")
    (description
     "Shiboken generates bindings for C++ libraries using CPython source code")
    (license
     (list
      ;; The main code is GPL3 or LGPL3.
      ;; Examples are BSD-3.
      license:gpl3
      license:lgpl3
      license:bsd-3))))

(define-public python-shiboken-6
  (package
    (inherit python-shiboken-2)
    (name "python-shiboken-6")
    (version "6.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.qt.io/official_releases"
                                  "/QtForPython/pyside6/PySide6-" version
                                  "-src/pyside-setup-opensource-src-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0xwri69nnbhn6fajm7l045r0s0qv8nlq6qj8wcj87srli3b5xa75"))))
    (build-system cmake-build-system)
    (inputs
     (modify-inputs (package-inputs python-shiboken-2)
       (replace "qtbase" qtbase)
       (delete "qtxmlpatterns")))
    (arguments
     (substitute-keyword-arguments (package-arguments python-shiboken-2)
       ((#:phases p)
        #~(modify-phases #$p
            (replace 'use-shiboken-dir-only
              (lambda _ (chdir "sources/shiboken6")))))
       ((#:configure-flags flags)
        #~(cons*
           ;; The RUNPATH of shibokenmodule contains the entry in build
           ;; directory instead of install directory.
           "-DCMAKE_SKIP_RPATH=TRUE"
           (string-append "-DCMAKE_MODULE_LINKER_FLAGS=-Wl,-rpath="
                          #$output "/lib")
           #$flags))))))

(define-public python-pyside-2
  (package
    (name "python-pyside-2")
    (version (package-version python-shiboken-2))
    (source (package-source python-shiboken-2))
    (build-system cmake-build-system)
    (inputs
     (list qtbase-5
           qtdatavis3d
           qtdeclarative-5
           qtlocation
           qtmultimedia-5
           qtquickcontrols-5
           qtquickcontrols2-5
           qtscript
           qtscxml
           qtsensors
           qtspeech
           qtsvg-5
           qttools-5
           qtwebchannel-5
           qtwebengine-5
           qtwebsockets-5
           qtx11extras
           qtxmlpatterns))
    (propagated-inputs
     (list python-shiboken-2))
    (native-inputs
     (list python-wrapper))
    (arguments
     (list
      #:tests? #f
      ;; FIXME: Building tests fail.
      #:configure-flags
      #~(list "-DBUILD_TESTS=FALSE"
              (string-append "-DPYTHON_EXECUTABLE="
                             (search-input-file %build-inputs
                                                "/bin/python")))
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  (srfi srfi-1))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'go-to-source-dir
            (lambda _ (chdir "sources/pyside2")))
          (add-after 'go-to-source-dir 'fix-qt-module-detection
            (lambda _
              ;; Activate qt module support even if it not in the same
              ;; directory as qtbase.
              (substitute* "../cmake_helpers/helpers.cmake"
                (("\\(\"\\$\\{found_basepath\\}\" GREATER \"0\"\\)")
                 "true"))
              ;; Add include directories for qt modules.
              (let ((dirs (map (lambda (path)
                                 (string-append path "/include/qt5"))
                               (list
                                #$@(map (lambda (name)
                                          (this-package-input name))
                                        '("qtdatavis3d"
                                          "qtdeclarative"
                                          "qtlocation"
                                          "qtmultimedia"
                                          "qtquickcontrols"
                                          "qtquickcontrols2"
                                          "qtscript"
                                          "qtscxml"
                                          "qtsensors"
                                          "qtspeech"
                                          "qtsvg"
                                          "qttools"
                                          "qtwebchannel"
                                          "qtwebengine"
                                          "qtwebsockets"
                                          "qtx11extras"
                                          "qtxmlpatterns"))))))
                (substitute* "cmake/Macros/PySideModules.cmake"
                  (("\\$\\{PATH_SEP\\}\\$\\{core_includes\\}" all)
                   (fold (lambda (dir paths)
                           (string-append paths "${PATH_SEP}" dir))
                         all
                         dirs)))
                (setenv "CXXFLAGS" (fold (lambda (dir paths)
                                           (string-append paths " -I" dir))
                                         ""
                                         dirs))))))))
    (home-page "https://wiki.qt.io/Qt_for_Python")
    (synopsis
     "The Qt for Python product enables the use of Qt5 APIs in Python applications")
    (description
     "The Qt for Python product enables the use of Qt5 APIs in Python
applications.  It lets Python developers utilize the full potential of Qt,
using the PySide2 module.  The PySide2 module provides access to the
individual Qt modules such as QtCore, QtGui,and so on.  Qt for Python also
comes with the Shiboken2 CPython binding code generator, which can be used to
generate Python bindings for your C or C++ code.")
    (license (list
              license:lgpl3
              ;;They state that:
              ;; this file may be used under the terms of the GNU General
              ;; Public License version 2.0 or (at your option) the GNU
              ;; General Public license version 3 or any later version
              ;; approved by the KDE Free Qt Foundation.
              ;; Thus, it is currently v2 or v3, but no "+".
              license:gpl3
              license:gpl2))))

(define-public python-pyside-6
  (package
    (inherit python-pyside-2)
    (name "python-pyside-6")
    (version (package-version python-shiboken-6))
    (source (package-source python-shiboken-6))
    ;; TODO: Add more Qt components if available.
    (inputs
     (list qtbase
           qtdeclarative
           qtmultimedia
           qtnetworkauth
           qtpositioning
           qtsvg
           qttools
           qtwebchannel
           qtwebengine
           qtwebsockets))
    (propagated-inputs
     (list python-shiboken-6))
    (native-inputs
     (list python-wrapper))
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DBUILD_TESTS=FALSE"
              (string-append "-DPYTHON_EXECUTABLE="
                             (search-input-file %build-inputs
                                                "/bin/python")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'go-to-source-dir
            (lambda _ (chdir "sources/pyside6")))
          (add-after 'go-to-source-dir 'fix-qt-module-detection
            (lambda _
              (substitute* "cmake/PySideHelpers.cmake"
                (("\\(\"\\$\\{found_basepath\\}\" GREATER \"0\"\\)")
                 "true"))
              (let ((dirs (map (lambda (path)
                                 (string-append path "/include/qt6"))
                               (list
                                #$@(map (lambda (name)
                                          (this-package-input name))
                                        '("qtdeclarative"
                                          "qtmultimedia"
                                          "qtnetworkauth"
                                          "qtpositioning"
                                          "qtsvg"
                                          "qttools"
                                          "qtwebchannel"
                                          "qtwebengine"
                                          "qtwebsockets"))))))
                (substitute* "cmake/Macros/PySideModules.cmake"
                  (("set\\(shiboken_include_dir_list " all)
                   (string-append all (string-join dirs ";") " ")))
                (setenv "CXXFLAGS"
                        (string-join
                         (map (lambda (dir)
                                (string-append "-I" dir))
                              dirs)
                         " "))))))))
    (synopsis
     "The Qt for Python product enables the use of Qt6 APIs in Python applications")
    (description
     "The Qt for Python product enables the use of Qt6 APIs in Python
applications.  It lets Python developers utilize the full potential of Qt,
using the PySide6 module.  The PySide6 module provides access to the
individual Qt modules such as QtCore, QtGui,and so on.  Qt for Python also
comes with the Shiboken6 CPython binding code generator, which can be used to
generate Python bindings for your C or C++ code.")))

(define-public python-pyside-2-tools
  (package
    (name "python-pyside-2-tools")
    (version (package-version python-shiboken-2))
    (source (package-source python-shiboken-2))
    (build-system cmake-build-system)
    (inputs
     (list python-pyside-2 python-shiboken-2 qtbase-5))
    (native-inputs
     (list python-wrapper))
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DBUILD_TESTS=off"
              (string-append "-DPYTHON_EXECUTABLE="
                             (search-input-file %build-inputs
                                                "/bin/python")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'go-to-source-dir
            (lambda _ (chdir "sources/pyside2-tools"))))))
    (home-page "https://wiki.qt.io/Qt_for_Python")
    (synopsis
     "Command line tools for PySide2")
    (description
     "Python-pyside-2-tools contains lupdate, rcc and uic tools for PySide2")
    (license license:gpl2)))

(define-public libqglviewer
  (package
    (name "libqglviewer")
    (version "2.7.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://libqglviewer.com/src/libQGLViewer-"
                              version ".tar.gz"))
              (sha256
               (base32
                "023w7da1fyn2z69nbkp2rndiv886zahmc5cmira79zswxjfpklp2"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:make-flags
       (list (string-append "PREFIX="
                            (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke (cons "qmake" make-flags)))))))
    (native-inputs
     (list qtbase-5 qttools-5))
    (inputs
     (list glu))
    (home-page "http://libqglviewer.com")
    (synopsis "Qt-based C++ library for the creation of OpenGL 3D viewers")
    (description
     "@code{libQGLViewer} is a C++ library based on Qt that eases the creation
of OpenGL 3D viewers.

It provides some of the typical 3D viewer functionalities, such as the
possibility to move the camera using the mouse, which lacks in most of the
other APIs.  Other features include mouse manipulated frames, interpolated
keyFrames, object selection, stereo display, screenshot saving and much more.
It can be used by OpenGL beginners as well as to create complex applications,
being fully customizable and easy to extend.")
    ;; According to LICENSE, either version 2 or version 3 of the GNU GPL may
    ;; be used.
    (license (list license:gpl2 license:gpl3))))

(define-public qhexedit
  (package
    (name "qhexedit")
    (version "0.8.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Simsys/qhexedit2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j333kiwhbidphdx86yilkaivgl632spfh6fqx93bc80gk4is3xa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/qhexedit.pro"
               (("^unix:DESTDIR = /usr/lib")
                (string-append "unix:DESTDIR = "
                               (assoc-ref outputs "out") "/lib")))))
         (replace 'configure
           (lambda _
             (chdir "src")
             (invoke "qmake" "qhexedit.pro")))
         (add-after 'install 'install-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include-dir (string-append out "/include")))
               (mkdir-p include-dir)
               (for-each (lambda (file)
                           (install-file file include-dir))
                         (find-files "." "\\.h$"))))))))
    (inputs (list qtbase-5))
    (native-inputs (list qttools-5))
    (home-page "https://simsys.github.io")
    (synopsis "Binary editor widget for Qt")
    (description
     "@code{QHexEdit} is a hex editor widget for the Qt framework.  It is a
simple editor for binary data, just like @code{QPlainTextEdit} is for text
data.")
    (license license:lgpl2.1)))

(define-public soqt
  (let ((commit-ref "fb8f655632bb9c9c60e0ff9fa69a5ba22d3ff99d")
        (revision "1"))
    (package
    (name "soqt")
    (version (git-version "1.6.0" revision commit-ref))
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/coin3d/soqt")
               (commit commit-ref)
               (recursive? #t)))
        (file-name (git-file-name name version))
        (sha256
          (base32 "16vikb3fy8rmk10sg5g0gy2c343hi3x7zccsga90ssnkzpq6m032"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f)) ; There are no tests
    (native-inputs
      (list pkg-config cmake))
    (inputs
      (list qtbase-5 coin3D))
    (home-page "https://github.com/coin3d/soqt")
    (synopsis "Qt GUI component toolkit library for Coin")
    (description "SoQt is a Qt GUI component toolkit library for Coin.  It is
also compatible with SGI and TGS Open Inventor, and the API is based on the API
of the InventorXt GUI component toolkit.")
    (license license:bsd-3))))

(define-public libdbusmenu-qt
  (package
    (name "libdbusmenu-qt")
    (version "0.9.3+16.04.20160218-0ubuntu1")
    (source
     (origin
       (method git-fetch)
       ;; Download from github rather than launchpad because launchpad trunk
       ;; tarball hash is not deterministic.
       (uri (git-reference
             (url "https://github.com/unity8-team/libdbusmenu-qt")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b7ii1cvmpcyl79gqal9c3va9m55h055s4hx7fpxkhhqs9463ggg"))))
    (build-system cmake-build-system)
    (arguments
     ;; XXX: Tests require a dbus session and some icons.
     '(#:tests? #f))
    (native-inputs
     (list doxygen))
    (inputs
     (list qtbase-5))
    (home-page "https://launchpad.net/libdbusmenu-qt")
    (synopsis "Qt implementation of the DBusMenu spec")
    (description "This library provides a Qt implementation of the DBusMenu
protocol.  The DBusMenu protocol makes it possible for applications to export
and import their menus over DBus.")
    (license license:lgpl2.1+)))

(define-public kdsoap
  (package
    (name "kdsoap")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/KDAB/KDSoap/releases/download/"
                           "kdsoap-" version "/kdsoap-" version ".tar.gz"))
       (sha256
        (base32
         "1vh4rzb09kks1ilay1y60q7gf64gwzdwsca60hmx1xx69w8672fi"))))
    (build-system qt-build-system)
    (inputs `(("qtbase" ,qtbase-5)))
    (arguments
     '(#:configure-flags '("-DKDSoap_TESTS=true")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "ctest" "-E" ;; These tests try connect to the internet.
                       "(kdsoap-webcalls|kdsoap-webcalls_wsdl|kdsoap-test_calc)")))))))
    (home-page "https://www.kdab.com/development-resources/qt-tools/kd-soap/")
    (synopsis "Qt SOAP component")
    (description "KD SOAP is a tool for creating client applications for web
services using the XML based SOAP protocol and without the need for a dedicated
web server.")
    (license (list license:gpl2 license:gpl3))))

(define-public libaccounts-qt
  (package
    (name "libaccounts-qt")
    (version "1.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/accounts-sso/libaccounts-qt")
                    (commit (string-append "VERSION_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vmpjvysm0ld8dqnx8msa15hlhrkny02cqycsh4k2azrnijg0xjz"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ;TODO
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   (substitute* "tests/tst_libaccounts.pro"
                     (("QMAKE_RPATHDIR = \\$\\$\\{QMAKE_LIBDIR\\}")
                      (string-append "QMAKE_RPATHDIR ="
                                     #$output "/lib")))
                   (invoke "qmake"
                           (string-append "PREFIX=" #$output)
                           (string-append "LIBDIR=" #$output "/lib")))))))
    ;; * SignOnQt5 (required version >= 8.55), D-Bus service which performs
    ;; user authentication on behalf of its clients,
    ;; <https://gitlab.com/accounts-sso/signond>
    (native-inputs (list doxygen pkg-config qtbase-5 qttools-5))
    (inputs (list glib signond libaccounts-glib))
    (home-page "https://accounts-sso.gitlab.io/")
    (synopsis "Qt5 bindings for libaccounts-glib")
    (description
     "Accounts SSO is a framework for application developers who
wish to acquire, use and store web account details and credentials.  It
handles the authentication process of an account and securely stores the
credentials and service-specific settings.")
    (license license:lgpl2.1+)))

(define-public libsignon-glib
  (package
    (name "libsignon-glib")
    (version "2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/accounts-sso/libsignon-glib")
                    (commit (string-append "VERSION_" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gnx9gqsh0hcfm1lk7w60g64mkn1iicga5f5xcy1j9a9byacsfd0"))))
    (build-system meson-build-system)
    (arguments
     (list #:tests? #f                  ;TODO: ninja: no work to do.
           #:imported-modules `((guix build python-build-system)
                                ,@%meson-build-system-modules)
           #:modules '(((guix build python-build-system)
                        #:select (python-version))
                       (guix build meson-build-system)
                       (guix build utils))
           #:configure-flags
           #~(list "-Dtests=true"
                   (string-append "-Dpy-overrides-dir="
                                  #$output "/lib/python"
                                  (python-version #$(this-package-input
                                                     "python"))
                                  "/site-packages/gi/overrides"))))
    (native-inputs (list dbus
                         dbus-test-runner
                         `(,glib "bin")
                         gobject-introspection
                         gtk-doc
                         pkg-config
                         vala))
    (inputs (list check signond python python-pygobject))
    (propagated-inputs (list glib))
    (home-page "https://accounts-sso.gitlab.io/libsignon-glib/")
    (synopsis "Single signon authentication library for GLib applications")
    (description
     "This package provides single signon authentication library for
GLib applications.")
    (license license:lgpl2.1+)))

(define-public packagekit-qt5
  (package
    (name "packagekit-qt5")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
			  (uri (git-reference
			  (url "https://github.com/hughsie/PackageKit-Qt")
			  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d20r503msw1vix3nb6a8bmdqld7fj8k9jk33bkqsc610a2zsms6"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ;no test suite
    (native-inputs (list pkg-config))
    (inputs (list packagekit qtbase-5))
    (home-page "https://www.freedesktop.org/software/PackageKit/pk-intro.html")
    (synopsis "Qt5 bindings for PackageKit")
    (description "Provides Qt5 bindings to PackageKit which is a DBUS
abstraction layer that allows the session user to manage packages in
a secure way.")
    (license license:lgpl2.1+)))

(define-public libsignon-glib
  (package
    (name "libsignon-glib")
    (version "2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/accounts-sso/libsignon-glib")
                    (commit (string-append "VERSION_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vgckvv78kzp54drj0dclqi0gfgrz6ihyjlks8z0cbd6k01r1dfy"))))
    (build-system meson-build-system)
    (arguments
     (list #:tests? #f ;TODO: ninja: no work to do.
           #:imported-modules `((guix build python-build-system)
                                ,@%meson-build-system-modules)
           #:modules '(((guix build python-build-system)
                        #:select (python-version))
                       (guix build meson-build-system)
                       (guix build utils))
           #:configure-flags #~(list "-Dtests=true"
                                     (string-append "-Dpy-overrides-dir="
                                      #$output "/lib/python"
                                      (python-version #$(this-package-input
                                                         "python"))
                                      "/site-packages/gi/overrides"))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'get-submodule
                          (lambda* _
                            (copy-recursively #$(origin
                                                  (method git-fetch)
                                                  (uri (git-reference (url
                                                                       "https://gitlab.com/accounts-sso/signon-dbus-specification")
                                                                      (commit
                                                                       "67487954653006ebd0743188342df65342dc8f9b")))
                                                  (sha256 (base32
                                                           "0w2wlm2vbgdw4fr3bd8z0x9dchl3l3za1gzphwhg4s6val1yk2rj")))
                                              "libsignon-glib/interfaces"))))))
    (native-inputs (list dbus
                         dbus-test-runner
                         `(,glib "bin")
                         gobject-introspection
                         gtk-doc
                         pkg-config
                         vala))
    (inputs (list check signond python python-pygobject))
    (propagated-inputs (list glib))
    (home-page "https://accounts-sso.gitlab.io/libsignon-glib/")
    (synopsis "Single signon authentication library for GLib applications")
    (description
     "This package provides single signon authentication library for
GLib applications.")
    (license license:lgpl2.1+)))

(define-public packagekit-qt5
  (package
    (name "packagekit-qt5")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
			  (uri (git-reference
			  (url "https://github.com/hughsie/PackageKit-Qt")
			  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d20r503msw1vix3nb6a8bmdqld7fj8k9jk33bkqsc610a2zsms6"))))
    (build-system cmake-build-system)
	(arguments
	'(#:tests? #f)) ; no test suite
    (native-inputs (list pkg-config))
    (inputs (list packagekit qtbase-5))
    (home-page "https://www.freedesktop.org/software/PackageKit/pk-intro.html")
    (synopsis "Qt5 bindings for PackageKit")
    (description "Provides Qt5 bindings to PackageKit which is a DBUS
abstraction layer that allows the session user to manage packages in
a secure way.")
    (license license:lgpl2.1+)))

(define-public signond
  (package
    (name "signond")
    (version "8.61")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/accounts-sso/signond")
                    (commit (string-append "VERSION_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0k6saz5spys4a4p6ws0ayrjks2gqdqvz7zfmlhdpz5axha0gbqq4"))))
    (build-system qt-build-system)
    (native-inputs (list doxygen pkg-config qtbase-5 qttools-5))
    (inputs (list dbus glib libaccounts-glib))
    (arguments
     (list #:tests? #f                  ; Figure out how to run tests
           #:phases
           #~(modify-phases %standard-phases
               (delete 'validate-runpath)
               (replace 'configure
                 (lambda _
                   (substitute* "src/signond/signond.pro"
                     (("/etc/")
                      (string-append #$output "/etc/")))
                   (substitute*
                       '("tests/extensions/extensions.pri"
                         "tests/signond-tests/mock-ac-plugin/plugin.pro"
                         "tests/signond-tests/identity-tool.pro"
                         "tests/signond-tests/mock-ac-plugin/identity-ac-helper.pro"
                         "tests/libsignon-qt-tests/libsignon-qt-tests.pro"
                         "tests/signond-tests/signond-tests.pri")
                     (("QMAKE_RPATHDIR = \\$\\$\\{QMAKE_LIBDIR\\}")
                      (string-append "QMAKE_RPATHDIR = "
                                     #$output "/lib:"
                                     #$output "/lib/signon")))
                   (invoke "qmake"
                           (string-append "PREFIX=" #$output)
                           (string-append "LIBDIR=" #$output "/lib")))))))
    (home-page "http://accounts-sso.gitlab.io/signond/index.html")
    (synopsis "Perform user authentication over D-Bus")
    (description "This package provides a D-Bus service which performs user
authentication on behalf of its clients.")
    (license license:lgpl2.1+)))

(define-public signon-plugin-oauth2
  (package
    (name "signon-plugin-oauth2")
    (version "0.25")
    (home-page "https://gitlab.com/accounts-sso/signon-plugin-oauth2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "VERSION_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16aslnyk8jdg03zcg97rp6qzd0gmclj14hyhliksz8jgfz1l0w7c"))))
    (build-system qt-build-system)
    (native-inputs (list doxygen pkg-config))
    (inputs (list signond))
    (arguments
     (list #:tests? #f                  ;no tests
           #:make-flags #~(list (string-append "INSTALL_ROOT=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   (substitute* "common-project-config.pri"
                     (("-Werror")
                      ""))
                   (invoke "qmake"
                           (string-append "PREFIX=" #$output)
                           (string-append "LIBDIR=" #$output "/lib")))))))
    (synopsis "OAuth 2 plugin for signon")
    (description
     "This plugin for the Accounts-SSO SignOn daemon handles the OAuth
1.0 and 2.0 authentication protocols.")
    (license license:lgpl2.1+)))

(define-public signon-plugin-oauth2
  (package
    (name "signon-plugin-oauth2")
    (version "0.25")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://gitlab.com/accounts-sso/signon-plugin-oauth2")
                    (commit (string-append "VERSION_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16aslnyk8jdg03zcg97rp6qzd0gmclj14hyhliksz8jgfz1l0w7c"))))
    (build-system qt-build-system)
    (native-inputs (list doxygen pkg-config))
    (inputs (list signond))
    (arguments
     (list #:tests? #f
           #:make-flags #~(list (string-append "INSTALL_ROOT="
                                               #$output))
           #:phases #~(modify-phases %standard-phases
                        (replace 'configure
                          (lambda _
                            (substitute* "common-project-config.pri"
                              (("-Werror")
                               ""))
                            (invoke "qmake"
                                    (string-append "PREFIX="
                                                   #$output)
                                    (string-append "LIBDIR="
                                                   #$output "/lib")))))))
    (home-page "")
    (synopsis "OAuth 2 plugin for signon")
    (description
     "This plugin for the Accounts-SSO SignOn daemon handles the OAuth
1.0 and 2.0
authentication protocols.")
    (license license:lgpl2.1+)))
;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
