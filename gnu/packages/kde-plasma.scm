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
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
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

(define-public libkscreen
  (package
    (name "libkscreen")
    (version "5.25.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "1mxkrk04wcyw4xbfiyxbp5iwnhqr10yk39zx5bbjd9zag0vdi7z5"))))
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
    (version "5.25.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/libksysguard-" version ".tar.xz"))
       (sha256
        (base32 "1mrrrxjvqmrnkjwafvqrd2hlvl9gr9y4hn7dv0gf70lp5bl06i89"))))
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
     `(#:phases
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
               (invoke "ctest" "-E" ;; Test fails inconsistently.
                       "kwayland-testDragAndDrop"))
             #t)))))
    (home-page "https://api.kde.org/kwayland-server/html/index.html")
    (synopsis "KDE wayland server component")
    (description "KWayland is a Qt-style API to interact with the wayland-client and
wayland-server API.")
    (license (list license:lgpl2.1
                   license:lgpl2.1+
                   license:lgpl3
                   license:expat
                   license:bsd-3))))

(define-public kwin
  (package
    (name "kwin")
    (version "5.24.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                          "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1qwcd6iw6yvpchiwmvq5nwsr465jmrmscf286mjrc65im4hj6572"))))
    (native-inputs
     (list extra-cmake-modules
           dbus
           kdoctools
           pkg-config
           qttools-5
           xorg-server-for-tests))
    (inputs
     (list breeze
           eudev
           fontconfig
           freetype
           hwdata
           kactivities
           kcmutils
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdeclarative
           kdecoration
           kglobalaccel
           ki18n
           kiconthemes
           kidletime
           kio
           kirigami
           knewstuff
           knotifications
           kpackage
           krunner
           kscreenlocker
           ktextwidgets
           kwayland
           kwayland-server
           kwindowsystem
           kxmlgui
           lcms
           libcap
           libepoxy
           libinput
           libxkbcommon
           pipewire-0.3
           plasma-framework
           plasma-wayland-protocols
           qtbase-5
           qtdeclarative-5
           qtwayland
           qtx11extras
           wayland
           wayland-protocols
           xcb-util ; fails at build time without this
           xcb-util-cursor
           xcb-util-keysyms
           xcb-util-wm
           xcmsdb
           xinput ;; XXX: Says disabled in configure phase
           xorg-server-xwayland
           zlib))
 ;;     * hwdata, <https://github.com/vcrhonek/hwdata>
 ;;   Runtime-only dependency needed for mapping monitor hardware vendor IDs to full names
 ;; * QtQuick.Controls-QMLModule, QML module 'QtQuick.Controls' is a runtime dependency.
 ;; * QtMultimedia-QMLModule, QML module 'QtMultimedia' is a runtime dependency.
 ;; * org.kde.kquickcontrolsaddons-QMLModule, QML module 'org.kde.kquickcontrolsaddons' is a runtime dependency.
 ;; * org.kde.plasma.core-QMLModule, QML module 'org.kde.plasma.core' is a runtime dependency.
 ;; * org.kde.plasma.components-QMLModule, QML module 'org.kde.plasma.components' is a runtime dependency.

   ;;   * QAccessibilityClient, KDE client-side accessibility library, <https://www.kde.org>
   ;; Required to enable accessibility features

    (build-system qt-build-system)
    (arguments
     `(#:tests? #f ;; Over 50 tests fail inconsistently.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Make QDirIterator follow symlinks
             (substitute* '("src/plugins/kdecorations/aurorae/src/aurorae.cpp")
               (("(^\\s*QDirIterator it.path, QDirIterator::Subdirectories)(\\);)"
                 _ a b)
                (string-append a " | QDirIterator::FollowSymlinks" b)))
             (substitute* '("src/xwl/xwayland.cpp")
               (("(m_xwaylandProcess->setProgram.QStringLiteral..)(Xwayland)(...;)"
                 _ a Xwayland b)
                (string-append a (which "Xwayland") b)))
             (substitute* '("cmake/modules/Findhwdata.cmake")
               (("/usr/share")
                (string-append (assoc-ref inputs "hwdata") "/share")))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "XDG_RUNTIME_DIR" (getcwd))
               (setenv "HOME"  (getcwd))
               (setenv "DISPLAY" ":1")
               (system "Xvfb :1 &")
               (sleep 5)
               ;; FIXME: Disable failing tests for now. Many of these tests fail inconsistently.
               (invoke "ctest" "-E" "(\
kwin-testActivation|\
kwin-testActivation-waylandonly|\
kwin-testDebugConsole|\
kwin-testDecorationInput-waylandonly|\
kwin-testDecorationInput|\
kwin-testDontCrashCursorPhysicalSizeEmpty-waylandonly|\
kwin-testDontCrashAuroraeDestroyDeco|\
kwin-testDontCrashNoBorder-waylandonly|\
kwin-testDontCrashCancelAnimation|\
kwin-testDontCrashGlxgears|\
kwin-testDontCrashUseractionsMenu-waylandonly|\
kwin-testDontCrashUseractionsMenu|\
kwin-testGlobalShortcuts|\
kwin-testInternalWindow-waylandonly|\
kwin-testIdleInhibition-waylandonly|\
kwin-testInputMethod|\
kwin-testInputStackingOrder-waylandonly|\
kwin-testKeyboardLayout-waylandonly|\
kwin-testKWinBindings|\
kwin-testLayerShellV1Client-waylandonly|\
kwin-testLibinputDevice|\
kwin-testLockScreen|\
kwin-testMaximized-waylandonly|\
kwin-testMaximized|\
kwin-testModiferOnlyShortcut|\
kwin-testNightColor-waylandonly|\
kwin-testNightColor|\
kwin-testNoGlobalShortcuts|\
kwin-testOutputManagement|\
kwin-testOutputManagement-waylandonly|\
kwin-testPlacement-waylandonly|\
kwin-testPlasmaSurface|\
kwin-testPlasmaSurface-waylandonly|\
kwin-testPlasmaWindow|\
kwin-testPlatformCursor|\
kwin-testPointerConstraints-waylandonly|\
kwin-testPointerInput|\
kwin-testQuickTiling|\
kwin-testSceneOpenGL-waylandonly|\
kwin-testSceneOpenGLES-waylandonly|\
kwin-testSceneOpenGLES|\
kwin-testSceneQPainter|\
kwin-testScreenChanges-waylandonly|\
kwin-testScreens|\
kwin-testShade|\
kwin-testShowingDesktop-waylandonly|\
kwin-testStruts|\
kwin-testTabBox-waylandonly|\
kwin-testTouchInput|\
kwin-testTouchInput-waylandonly|\
kwin-testVirtualDesktop-waylandonly|\
kwin-testVirtualKeyboardDBus-waylandonly|\
kwin-testWindowSelection-waylandonly|\
kwin-testX11Client|\
kwin-testXdgShellClient-waylandonly|\
kwin-testXdgShellClient|\
kwin-testXwaylandSelections)")))))))
    (home-page "https://userbase.kde.org/KWin")
    (synopsis "KDE Plasma Window Manager")
    (description
     "KWin is an easy to use, but flexible, composited Window Manager for
Xorg windowing systems (Wayland, X11) on Linux.  Its primary usage is in
conjunction with the KDE Plasma Desktop.")
    (license (list license:bsd-2
                   license:bsd-3
                   license:expat
                   license:gpl2
                   license:gpl2+
                   license:gpl3
                   license:gpl3+
                   license:lgpl2.0
                   license:lgpl2.0+
                   license:lgpl2.1
                   license:lgpl3))))

(define-public plasma-workspace
  (package
    (name "plasma-workspace")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/plasma/" version
                    "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1aw9ms6rzxrk384xzdc3sqwqs1shbnkap40vfwxp4jamjk0pyglw"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules
           pkg-config
           qtsvg-5
           qttools-5))
    (inputs
     (list ;kplasma
           ;kplasmaquick
           ;packagekit-qt
           appstream-qt
           baloo
           breeze
           breeze-icons
           fontconfig
           iso-codes
           kactivities
           kactivities-stats
           karchive
           kcmutils
           kcoreaddons
           kcrash
           kdbusaddons
           kdeclarative
           kded
           kdesu
           kdoctools
           kglobalaccel
           kguiaddons
           kholidays
           ki18n
           kiconthemes
           kidletime
           kinit
           kio
           kirigami
           kitemmodels
           knewstuff
           knotifications
           knotifyconfig
           kpackage
           kpeople
           kqtquickcharts ;; XXX: not found?
           krunner
           kscreenlocker
           ktexteditor
           ktextwidgets
           kunitconversion
           kuserfeedback
           kwallet
           kwayland
           kwin
           layer-shell-qt
           libkscreen
           libksysguard
           libqalculate gmp mpfr
           libsm
           libxft
           libxkbcommon
           libxrender
           libxtst
           networkmanager-qt
           phonon
           pipewire-0.3
           plasma-wayland-protocols
           prison
           qtbase-5
           qtdeclarative-5
           qtwayland
           qtx11extras
           wayland
           wayland-protocols-next

           xcb-util
           xcb-util-image))
    ;;     -- The following RUNTIME packages have not been found:
    ;;  * KF5QuickCharts (required version >= 5.89), Used for rendering charts
    ;;  * KIOExtras, Common KIO slaves for operations.
    ;;    Show thumbnails in wallpaper selection.
    ;;  * KIOFuse, Provide KIO support to legacy applications.
    ;;  * org.kde.prison-QMLModule, QML module 'org.kde.prison' is a runtime dependency.
    ;;  * org.kde.plasma.core-QMLModule, QML module 'org.kde.plasma.core' is a runtime dependency.
    ;;  * IsoCodes, ISO language, territory, currency, script codes and their translations, <https://salsa.debian.org/iso-codes-team/iso-codes>
    ;;    Translation of country names in digital clock applet
    ;;  * AppMenuGtkModule, Application Menu GTK+ Module, <https://github.com/rilian-la-te/vala-panel-appmenu/tree/master/subprojects/appmenu-gtk-module>
    ;; -- The following OPTIONAL packages have not been found:
    ;;  * PackageKitQt5, Software Manager integration
    ;;    Used to install additional language packages on demand
    ;; kf.package: Invalid metadata for package structure "Plasma/LookAndFeel"
    ;; Package type "Plasma/LookAndFeel" not found
    ;; TODO: startkde patch, xsetroot, xrdb, xmessage, ...
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-wallpaper
                 (lambda _
                   (substitute* "sddm-theme/theme.conf.cmake"
                     (("background=..KDE_INSTALL_FULL_WALLPAPERDIR.")
                      (string-append "background=" #$breeze "/share/wallpapers")))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" (getcwd))
                     (setenv "XDG_RUNTIME_DIR" (getcwd))
                     (setenv "XDG_CACHE_HOME" (getcwd))
                     (setenv "QT_QPA_PLATFORM" "offscreen")
                     ;; Disable failing tests for now.
                     (invoke "ctest" "-E" "lookandfeel-kcmTest|locationsrunnertest|\
tst_triangleFilter|systemtraymodeltest|testdesktop")))))))
    (home-page "https://kde.org/plasma-desktop/")
    (synopsis "Plasma for the Desktop")
    (description "Plasma Desktop offers a beautiful looking desktop that takes
complete advantage of modern computing technology. Through the use of visual
effects and scalable graphics, the desktop experience is not only smooth but
also pleasant to the eye. The looks of Plasma Desktop not only provide
beauty, they are also used to support and improve your computer
activities effectively, without being distracting.")
    (license (list license:bsd-3 license:gpl2+ license:gpl3 license:lgpl2.1+
license:lgpl3))))

(define-public plasma-desktop
  (package
    (name "plasma-desktop")
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
                "1jkjc412n1wn17qrmx0sv91pzv5xjsljms3bsln6bbxj5fkhmkfm"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules
                         kdoctools
                         intltool
                         pkg-config
                         qtsvg-5
                         qttools-5))
    (inputs (list ;kplasma
                  ;; kplasmaquick
                  ;; packagekit-qt
                  ;; signon-oauth2plugin
                  signond

                  kdelibs4support
                  plasma-workspace

                  appstream-qt
                  baloo
                  breeze
                  breeze-icons
                  eudev
                  fontconfig
                  glib
                  iso-codes
                  ibus
                  kaccounts-integration
                  kactivities
                  kactivities-stats
                  karchive
                  kcmutils
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  kdeclarative
                  kded
                  kdesu
                  kglobalaccel
                  kguiaddons
                  kholidays
                  ki18n
                  kiconthemes
                  kidletime
                  kinit
                  kio
                  kirigami
                  kitemmodels
                  knewstuff
                  knotifications
                  knotifyconfig
                  kqqc2-desktop-style
                  kpackage
                  kpeople
                  kqtquickcharts ;XXX: not found?
                  krunner
                  kscreenlocker
                  ktexteditor
                  ktextwidgets
                  kunitconversion
                  kuserfeedback
                  kwallet
                  kwayland
                  kwin
                  layer-shell-qt
                  libaccounts-qt
                  libkscreen
                  libksysguard
                  libqalculate
                  gmp
                  mpfr
                  libsm
                  libxft
                  libxkbcommon
                  libxrender
                  libxtst
                  networkmanager-qt
                  phonon
                  pipewire-0.3
                  plasma-wayland-protocols
                  prison
                  qtbase-5
                  qtdeclarative-5
                  qtwayland
                  qtx11extras
                  wayland
                  wayland-protocols
                  xcb-util
                  xcb-util-image
                  xcb-util-keysyms

                  ;; These are needed for Xserver
                  xf86-input-libinput
                  xf86-input-evdev
                  xorg-server
                  xf86-input-synaptics
                  xkeyboard-config
                  libxkbfile
                  libxcursor
                  libxkbcommon))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
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
                               "(foldermodeltest|kcm-keyboard-iso_codes)")))))))
    (home-page "https://kde.org/plasma-desktop/")
    (synopsis "")
    (description "")
    (license (list license:bsd-3 license:gpl2+ license:gpl3 license:lgpl2.1+
                   license:lgpl3))))
