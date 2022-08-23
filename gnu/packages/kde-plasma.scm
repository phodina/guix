;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
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

(define-module (gnu packages kde-plasma)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages authentication)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages web))

(define-public breeze
  (package
    (name "breeze")
    (version "5.25.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0za75ckgfcdxrh2qxgyl2c1273g2xqwmd55njsis1yvwryadypqw"))))
    (build-system qt-build-system)
    ;; TODO: Warning at /gnu/store/…-kpackage-5.34.0/…/KF5PackageMacros.cmake:
    ;;   warnings during generation of metainfo for org.kde.breezedark.desktop:
    ;;   Package type "Plasma/LookAndFeel" not found
    ;; TODO: Check whether is makes sence splitting into several outputs, like
    ;; Debian does:
    ;; - breeze-cursor-theme
    ;; - "out", "devel"
    ;; - kde-style-breeze - Widget style
    ;; - kde-style-breeze-qt4 - propably not useful
    ;; - kwin-style-breeze
    ;; - qml-module-qtquick-controls-styles-breeze - QtQuick style
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list kcmutils ; optional
           kconfigwidgets
           kcoreaddons
           kde-frameworkintegration ; optional
           kdecoration
           kguiaddons
           ki18n
           kirigami
           kiconthemes ; for optional kde-frameworkintegration
           kpackage
           kwayland ; optional
           kwindowsystem
           qtbase-5
           qtdeclarative-5 ; optional
           qtx11extras))
    (home-page "https://invent.kde.org/plasma/breeze")
    (synopsis "Default KDE Plasma theme")
    (description "Artwork, styles and assets for the Breeze visual style for
the Plasma Desktop.  Breeze is the default theme for the KDE Plasma desktop.")
    (license license:gpl2+)))

(define-public breeze-gtk
  (package
    (name "breeze-gtk")
    (version "5.19.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name
                                  "-" version ".tar.xz"))
              (sha256
               (base32
                "1j2nq9yw1ragmgwrz9f6ca4ifpi86qv1bbprdgd2qm2yh7vb44sj"))))
    (build-system qt-build-system)
    (arguments
     '(#:tests? #f))                              ;no 'test' target
    (native-inputs (list breeze extra-cmake-modules sassc python
                         python-pycairo))
    (home-page "https://invent.kde.org/plasma/breeze")
    (synopsis "Default KDE Plasma theme (GTK+ port)")
    (description "GTK+ port of the Breeze visual style for the Plasma Desktop.
Breeze is the default theme for the KDE Plasma desktop.")
    (license (list license:bsd-3                  ;cmake/FindSass.cmake
                   license:lgpl2.1+))))           ;<all other files>

(define-public oxygen-sounds
  (package
    (name "oxygen-sounds")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "0v8j8kf86ri1z14mgqc1c6kkpsbih8rjph35sr5y0i4av9mh6p9b"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Oxygen provides the standard sounds for the KDE desktop")
    (description "This package provides Oxygen sounds for the KDE desktop.")
    (license license:lgpl3+)))

(define-public eink-lookandfeel
  (let ((commit "4d7799c9388e976af107ef2644621e79f3a85914")
        (revision "1"))
    (package
      (name "eink-lookandfeel")
      (version "")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://invent.kde.org/niccolove/eink-lookandfeel/-/archive/"
                      commit "/eink-lookandfeel-" commit ".tar.gz"))
                (sha256
                 (base32
                  "06qlv2zb6svhfcx4kwq3ccz95c1nzf1bhd289y9zhql1n1d2as93"))))
      (build-system copy-build-system)
      (home-page "https://invent.kde.org/apol/plasma-ink")
      (synopsis "EInk theme for Plasma")
      (description "EInk configuration for KDE Plasma.")
      ;; Missing license
      (license #f))))

(define-public kactivitymanagerd
  (package
    (name "kactivitymanagerd")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "1cpz08jkmxw9576h9mkn15vwg3bxgk8k6w4f38rkiasxzj6zfamd"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kconfig
                  kcoreaddons
                  kwindowsystem
                  kglobalaccel
                  kio
                  kxmlgui
                  kdbusaddons
                  ki18n
                  kcrash
                  boost))
    (synopsis "System service to manage user's activities")
    (description "This package provides components for managing the KDE Activity
concept.")
    (home-page "https://invent.kde.org/plasma/kactivitymanagerd")
    (license (list license:gpl2 license:gpl3))))

(define-public kdecoration
  (package
    (name "kdecoration")
    (version "5.25.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kdecoration-" version ".tar.xz"))
              (sha256
               (base32
                "0b6ynqkndmlac89hv339k365m7wykp9y238df62jlq4vpr1r9x9y"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcoreaddons ki18n qtbase-5))
    (home-page "https://invent.kde.org/plasma/kdecoration")
    (synopsis "Plugin based library to create window decorations")
    (description "KDecoration is a library to create window decorations.
These window decorations can be used by for example an X11 based window
manager which re-parents a Client window to a window decoration frame.")
    (license license:lgpl3+)))

(define-public kmenuedit
  (package
    (name "kmenuedit")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "1y3ml4jscb28nvadh7iq2y240qifv71dv2nkd32i69h52xdrvz27"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs (list ki18n
                  kxmlgui
                  kdbusaddons
                  kiconthemes
                  kio
                  kitemviews
                  sonnet
                  kglobalaccel
                  kwindowsystem))
    (synopsis "Menu Editor for Plasma Workspaces")
    (description "This package provides menu editor for Plasma Workspaces.")
    (home-page "https://invent.kde.org/plasma/kmenuedit")
    (license license:gpl2+)))

(define-public kscreen
  (package
    (name "kscreen")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "1wjpyq56iw8axbjhsa82w67g54v6y3rv5nx623d1kddvlzlhh8pf"))))
    (build-system cmake-build-system)
    (arguments
     ;; TODO: All tests fail
     (list #:tests? #f))
    (native-inputs (list extra-cmake-modules qttools-5 pkg-config))
    (inputs (list kconfig
                  kdbusaddons
                  kdeclarative
                  kglobalaccel
                  ki18n
                  kwindowsystem
                  kiconthemes
                  kcoreaddons
                  kcmutils
                  kxmlgui
                  libkscreen
                  libxi
                  plasma-wayland-protocols
                  qtsensors
                  qtbase-5
                  qtx11extras
                  xcb-util))
    (propagated-inputs (list plasma-framework))
    (home-page "https://invent.kde.org/plasma/kscreen")
    (synopsis "Screen management software")
    (description "This package provides the screen management software for
KDE Plasma Workspaces.")
    (license license:gpl2+)))

(define-public ksshaskpass
  (package
    (name "ksshaskpass")
    (version "5.25.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/ksshaskpass-" version ".tar.xz"))
              (sha256
               (base32
                "0sfl77szvfq9c7v0gsv5nnf7h5kxigyy2z2p1cwmhm1pq4n606nk"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcoreaddons ki18n kwallet kwidgetsaddons qtbase-5))
    (home-page "https://invent.kde.org/plasma/ksshaskpass")
    (synopsis "Front-end for ssh-add using kwallet")
    (description "Ksshaskpass is a front-end for @code{ssh-add} which stores the
password of the ssh key in KWallet.  Ksshaskpass is not meant to be executed
directly, you need to tell @code{ssh-add} about it.  @code{ssh-add} will then
call it if it is not associated to a terminal.")
    (license license:gpl2+)))

(define-public latte-dock
  (package
    (name "latte-dock")
    (version "0.10.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/latte-dock/"
                                  "latte-dock-" version ".tar.xz"))
              (sha256
               (base32
                "0ali9i0y0y1c5mdaps5ybhk4nqvzzs5jq27wj8rg8xxqjyfvbah0"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list qtbase-5
                  qtdeclarative-5
                  knotifications
                  kwindowsystem
                  kio
                  plasma-framework
                  kwayland
                  kactivities
                  kcrash
                  kiconthemes
                  knewstuff
                  karchive
                  kguiaddons
                  kdbusaddons
                  kglobalaccel
                  kirigami
                  ki18n
                  kdeclarative
                  kcoreaddons
                  xcb-util
                  qtx11extras
                  libsm))
    (synopsis "Latte is a dock based on plasma frameworks")
    (description
     "Latte is a dock based on plasma frameworks that provides
an elegant and intuitive experience for your tasks and plasmoids.")
    (home-page "https://github.com/KDE/latte-dock")
    (license license:gpl2+)))

(define-public layer-shell-qt
  (package
    (name "layer-shell-qt")
    (version "5.25.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/layer-shell-qt-" version ".tar.xz"))
              (sha256
               (base32
                "06rxqm4wh4mcszrwb2dbgpxj3dqfx0rccyyjp091lbsncqm1gib0"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list libxkbcommon
           qtbase-5
           qtdeclarative-5
           qtwayland
           wayland
           wayland-protocols))
    (home-page "https://invent.kde.org/plasma/layer-shell-qt")
    (synopsis "Qt component for the Wayland ql-layer-shell protocol")
    (description "Qt component for the Wayland ql-layer-shell protocol.")
    (license license:gpl2+)))

(define-public kscreenlocker
  (package
    (name "kscreenlocker")
    (version "5.25.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kscreenlocker-" version ".tar.xz"))
              (sha256
               (base32
                "1kii3r3j89avwyb00wrw80k5sj0q4wqgmy1q0yxfps9jk729k3wc"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f ;; TODO: make tests pass
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (system "Xvfb :1 -screen 0 640x480x24 &")
             (setenv "DISPLAY" ":1")
             #t))
         (delete 'check)
         ;; Tests use the installed library and require a DBus session.
         (add-after 'install 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (begin
                   (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
                   (invoke "dbus-launch" "ctest")))
             #t)))))
    (native-inputs
     (list extra-cmake-modules pkg-config
           ;; For tests.
           dbus xorg-server-for-tests))
    (inputs
     (list kcmutils
           kconfig
           kcrash
           kdeclarative
           kglobalaccel
           ki18n
           kidletime
           knotifications
           ktextwidgets
           kwayland
           kwindowsystem
           kxmlgui
           layer-shell-qt
           libseccomp ;for sandboxing the look'n'feel package
           libxcursor ;missing in CMakeList.txt
           libxi ;XInput, required for grabbing XInput2 devices
           linux-pam
           elogind ;optional loginctl support
           qtbase-5
           qtdeclarative-5
           qtx11extras
           solid
           wayland
           xcb-util-keysyms))
    (home-page "https://invent.kde.org/plasma/kscreenlocker")
    (synopsis "Screen locking library")
    (description
     "@code{kscreenlocker} is a library for creating secure lock screens.")
    (license license:gpl2+)))

(define-public ksysguard
  (package
    (name "ksysguard")
    (version "5.22.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/ksysguard/" version
                          "/ksysguard-" version ".tar.xz"))
      (sha256
       (base32 "0bb2aj46v7ig0wn3ir68igryl2gblz2n75cddn8fwamvbx76570g"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f)) ;; 2/5 tests fail, probably due to dbus issues
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
       kcoreaddons
       kdbusaddons
       ki18n
       kiconthemes
       kinit
       kio
       kitemviews
       knewstuff
       knotifications
       kwindowsystem
       libksysguard
       `(,lm-sensors "lib")
       qtbase-5))
    (home-page "https://www.kde.org/applications/system/ksysguard/")
    (synopsis "Plasma process and performance monitor")
    (description "KSysGuard is a program to monitor various elements of your
system, or any other remote system with the KSysGuard daemon (ksysgardd)
installed.")
    (license license:gpl2+)))

(define-public libkscreen
  (package
    (name "libkscreen")
    (version "5.25.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "17ib0sgrhmmf3f8w3fni0825xz5581av5vnz8gca41vyf12css25"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules
           ;; For testing.
           dbus))
    (inputs
     (list kwayland libxrandr plasma-wayland-protocols
           qtbase-5 qtwayland wayland qtx11extras))
    (arguments
     '(#:tests? #f)) ; FIXME: 55% tests passed, 5 tests failed out of 11
    (home-page "https://community.kde.org/Solid/Projects/ScreenManagement")
    (synopsis "KDE's screen management software")
    (description "KScreen is the new screen management software for KDE Plasma
Workspaces which tries to be as magic and automatic as possible for users with
basic needs and easy to configure for those who want special setups.")
    (license license:gpl2+)))

(define-public libksysguard
  (package
    (name "libksysguard")
    (version "5.25.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/libksysguard-" version ".tar.xz"))
       (sha256
        (base32 "1kzpimhkagsmqj0cky4cfav1kbzyfjaj2l5xdapnmaygbm6r8086"))))
    (native-inputs
     (list extra-cmake-modules pkg-config qttools-5))
    (inputs
     (list kauth
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kdeclarative
           kglobalaccel
           ki18n
           kiconthemes
           kio
           knewstuff
           kservice
           kwidgetsaddons
           kwindowsystem
           libnl
           libcap
           libpcap
           `(,lm-sensors "lib")
           plasma-framework
           qtbase-5
           qtdeclarative-5
           qtscript
           qtwebchannel-5
           qtwebengine-5
           qtwebkit
           qtx11extras
           zlib))
    (build-system qt-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; TODO: Fix this failing test-case
                     (invoke "ctest" "-E" "processtest")))))))
    (home-page "https://userbase.kde.org/KSysGuard")
    (synopsis "Network enabled task and system monitoring")
    (description "KSysGuard can obtain information on system load and
manage running processes.  It obtains this information by interacting
with a ksysguardd daemon, which may also run on a remote system.")
    (license license:gpl3+)))

(define-public kwayland-server
  (package
    (name "kwayland-server")
    (version "5.24.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/plasma/" version
                    "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1279nqhy1qyz84dkn23rvzak8bg71hbrp09jlhv9mkjdb3bhnyfi"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list plasma-wayland-protocols
           qtbase-5
           qtwayland
           kwayland
           wayland
           wayland-protocols-next))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-install-path
           (lambda _
             ;; Fixes errors including nonexistant /include/KF5
             (substitute* "src/server/CMakeLists.txt"
               (("KF5_INSTALL") "KDE_INSTALL"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" (getcwd))
               (setenv "XDG_RUNTIME_DIR" (getcwd))
               (setenv "QT_QPA_PLATFORM" "offscreen")
               (invoke "ctest" "-E"
                       ;; This test fails inconsistently.
                       "kwayland-testDragAndDrop")))))))
    (home-page "https://api.kde.org/kwayland-server/html/index.html")
    (synopsis "KDE wayland server component")
    (description
     "KWayland is a Qt-style API to interact with the wayland-client and
wayland-server API.")
    ;; Most files are LGPL2.1 or LGPL3.0 only, at the users option.
    (license (list license:lgpl2.1 license:lgpl3
                   ;; src/server/drm_fourcc.h carries the MIT license.
                   license:expat))))

(define-public kwayland-integration
  (package
    (name "kwayland-integration")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "1iqnwcp08kg91pwm3i4grd0i4bqf8h1z0n0fhcw6l0cbhdkcad39"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (delete 'check)
                        (add-after 'install 'check-after-install
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "HOME"
                                      (getcwd))
                              (setenv "XDG_RUNTIME_DIR"
                                      (getcwd))
                              (setenv "QT_QPA_PLATFORM" "offscreen")
                              (invoke "ctest" "-E" "(idleTest-kwayland-test)"))))
                        (add-before 'check-after-install 'check-setup
                          (lambda* (#:key outputs #:allow-other-keys)
                            (setenv "QT_PLUGIN_PATH"
                                    (string-append #$output
                                                   "/lib/qt5/plugins:"
                                                   (getenv "QT_PLUGIN_PATH"))))))))
    (native-inputs (list extra-cmake-modules wayland-protocols pkg-config))
    (inputs (list kguiaddons
                  kidletime
                  kwindowsystem
                  kwayland
                  libxkbcommon
                  wayland
                  qtbase-5
                  qtwayland))
    (synopsis "KWayland runtime integration plugins")
    (description "Provides integration plugins for various KDE Frameworks for
Wayland.")
    (home-page "https://invent.kde.org/plasma/kwayland-integration")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public kwrited
  (package
    (name "kwrited")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "0xn20irka7adbqfc8w6gnhwp0pbv7bz7l7g16qddv1wq3xix9053"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons ki18n kpty knotifications))
    (home-page "https://invent.kde.org/plasma/kwrited")
    (synopsis "Listen to traditional system notifications")
    (description "This package provides access to system notifications.")
    (license license:gpl2+)))

(define-public kgamma
  (package
    (name "kgamma")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "5-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "1m72dl1rxsh56pmacx0q0qda7lr4359azik2lnhw7nhs30z4p25a"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kauth
                  kcoreaddons
                  kconfig
                  kconfigwidgets
                  kdoctools
                  ki18n))
    (synopsis "Adjust your monitor's gamma settings")
    (description "This package allows to adjust your monitor gamma settings.")
    (home-page "https://invent.kde.org/plasma/kgamma5")
    (license license:gpl2+)))

(define-public plasma-disks
  (package
    (name "plasma-disks")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "04hs8jg7f5bm5rjcb6i6zidyamq6cfry5sm5mj6hqdj0qmn9i396"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons
                  kdbusaddons
                  knotifications
                  ki18n
                  solid
                  kservice
                  kio
                  kauth
                  kdeclarative
                  smartmontools))
    (synopsis "Monitors S.M.A.R.T. capable devices for imminent failure")
    (description "This package provides intrface to S.M.A.R.T. data of disks.")
    (home-page "https://invent.kde.org/plasma/plasma-disks")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-firewall
  (package
    (name "plasma-firewall")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "12rmf7x8dyyb1k5ycj43kn4c0wzidig4h5wdh1igrgcvyypmjjcl"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list iproute
                  kcoreaddons
                  kcmutils
                  ki18n
                  kdeclarative
                  python
                  qtdeclarative-5))
    (synopsis "Control Panel for system firewall")
    (description "This package provides interface to system firewall")
    (home-page "https://invent.kde.org/plasma/plasma-firewall")
    (license (list license:gpl2 license:gpl3))))

(define-public plasmatube
  (package
    (name "plasmatube")
    (version "22.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma-mobile/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "01bmxdh0aclm184j5s0kddjc7a14225bdnbkr8jlk21g9wlw8cyx"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kirigami kconfig ki18n qtbase-5 qtdeclarative-5 qtquickcontrols2-5
	qtsvg-5 qtmultimedia-5 youtube-dl))
    (home-page "https://apps.kde.org/plasmatube/")
    (synopsis "Kirigami YouTube video player")
    (description "This package provides YouTube video player based
on QtMultimedia and @command{yt-dlp}.")
    (license license:gpl3+)))

(define-public plasma-integration
  (package
    (name "plasma-integration")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "1cbcp722pzbfiqfl5rcw6py74jbxg83k96kdx2m0g3ix1f0dmkbi"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f ; Failing tests
           #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "HOME"
                                      (getcwd))
                              (setenv "XDG_RUNTIME_DIR"
                                      (getcwd))
                              (setenv "XDG_CACHE_HOME"
                                      (getcwd))
                              (setenv "QT_QPA_PLATFORM" "offscreen")
                              (invoke "ctest" "-E"
                               "(frameworkintegration-kdeplatformtheme_unittest|frameworkintegration-kfontsettingsdata_unittest|frameworkintegration-kfiledialog_unittest|qmltests|frameworkintegration-kfiledialogqml_unittest")))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list breeze
                  kconfig
                  kio
                  ki18n
                  kwidgetsaddons
                  kconfigwidgets
                  kiconthemes
                  knotifications
                  libxcb
                  libxcursor
                  plasma-wayland-protocols
                  qtdeclarative-5
                  qtquickcontrols2-5
                  qtwayland
                  qtx11extras
                  wayland))
    (home-page "https://invent.kde.org/plasma/plasma-integration")
    (synopsis
     "Qt Platform Theme integration plugins for the Plasma workspaces")
    (description "This package provides a set of plugins responsible for better
integration of Qt applications when running on a KDE Plasma workspace.")
    (license license:lgpl2.0)))

(define-public plasma-active-window-control
(let ((commit "0b1c091b5662fb21917064d7809b3be8b4a8be47")
       (revision "1"))
  (package
    (name "plasma-active-window-control")
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://invent.kde.org/plasma/"
			  "plasma-active-window-control/-/archive/"commit
			  "/plasma-active-window-control-" commit ".tar.gz"))
              (sha256
               (base32
                "1q1scv2x7jrda14zyaqh13aqxpxa42lysdvcfmplpr6w38gcsvlh"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
	(inputs (list kwindowsystem libsm plasma-framework qtdeclarative-5
	qtx11extras))
    (home-page "https://invent.kde.org/plasma/plasma-active-window-control")
    (synopsis "Plasma applet for controlling the currently active window")
    (description "This package provides window control applet for the current
active window on Plasma Desktop.")
    (license (list license:gpl2 license:gpl3)))))

(define-public plasma-nm
  (package
    (name "plasma-nm")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "147p572pmkrgg52pvwhzs8lvxy3rs8y622n9lj7hjc6hrlh14qk2"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "ctest" "-E" "mobileproviderstest")))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    (home-page "https://invent.kde.org/plasma/plasma-nm")
    (inputs (list kconfigwidgets
                  kcompletion
                  kcoreaddons
                  kdeclarative
                  kdbusaddons
                  kio
                  ki18n
                  knetworkmanager
                  knotifications
                  kirigami
                  plasma-framework
                  modemmanager-qt
                  network-manager
                  qca
                  kservice
                  solid
                  prison
                  kwallet
                  kwidgetsaddons
                  kwindowsystem
                  openconnect
                  qtdeclarative-5))
    (synopsis "Plasma applet written in QML for managing network connections")
    (description "Provide Plasma applet written in QML for managing network
connections.")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public plasma-pa
  (package
    (name "plasma-pa")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "0v92jk826jj2kf11hlxh3xrxl9nsj6835ik2pmb192xbn6gpb4lm"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules kdoctools pkg-config))
    (inputs (list glib
                  kcoreaddons
                  kdeclarative
                  kglobalaccel
                  knotifications
                  kwindowsystem
                  kirigami
                  ki18n
                  qtdeclarative-5))
    (propagated-inputs (list libcanberra pulseaudio plasma-framework))
    (home-page "https://invent.kde.org/plasma/plasma-pa")
    (synopsis "Plasma applet for audio volume management using PulseAudio")
    (description
     "Provide Plasma applet for audio volume management using PulseAudio")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public plasma-pass
  (package
    (name "plasma-pass")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/"
                                  name
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "107pd6cnkd46px83pm3q7vbw10g5pd0qsw77jmr0c774k4xv1w01"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list ki18n kitemmodels kwindowsystem oath-toolkit qtdeclarative-5))
    (propagated-inputs (list plasma-framework))
    (home-page "https://invent.kde.org/plasma/plasma-pass")
    (synopsis "Plasma applet for the Pass password manager")
    (description "Provides Plasma applet for the Pass password manager.")
    (license license:lgpl2.1+)))

(define-public plasma-vault
  (package
    (name "plasma-vault")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "10ym2gk46yr1vgjnm1li1shdawklvvy3yvjcanm8ic5llchbxvzq"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kio
                  ki18n
                  kconfigwidgets
                  kconfig
                  kactivities
                  kdbusaddons
                  kiconthemes
                  libksysguard
                  plasma-framework
                  qtdeclarative-5))
    (home-page "https://invent.kde.org/plasma/plasma-vault")
    (synopsis "Plasma applet and services for creating encrypted vaults")
    (description "Provides Plasma applet and services for creating encrypted
	vaults.")
    (license (list license:gpl2 license:gpl3))))
