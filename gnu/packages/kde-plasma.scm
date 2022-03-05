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
  #:use-module (gnu packages cups)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages web))

(define-public bluedevil
  (package
    (name "bluedevil")
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
                "1cygnmigwqx1mqynfafcypkvf9bmz05rmrfwlxsksvll8yd9xn84"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules)); qttools-5 pkg-config))
    (inputs (list
kcoreaddons kwidgetsaddons kdbusaddons
  knotifications kwindowsystem plasma-framework ki18n kio kdeclarative bluez-qt
	shared-mime-info qtdeclarative-5))
    (synopsis "Manages the Bluetooth settings of a Plasma Shell")
    (description "This package provides Bluetooth manager for Plasma Shell.")
    (home-page "https://invent.kde.org/plasma/bluedevil")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public breeze
  (package
    (name "breeze")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0lw0vj07z8l19v2z31d601kfanqixy3dbsv0lf8q273xv3li9sbp"))))
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
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name
                                  "-" version ".tar.xz"))
              (sha256
               (base32
                "1x9a74f2anybvgmj4yl7pzz14jckjly55sb8hhlyrd1mp2k8p4mi"))))
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

(define-public calindori
  (package
    (name "calindori")
    (version "22.06")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma-mobile/" version
                                  "/calindori-" version ".tar.xz"))
              (sha256
               (base32
                "0fcbkk1yisdd6z1qvac9x6i55wfppqpdma87a0n5smm191lkjg07"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kconfig kcoreaddons kdbusaddons ki18n kirigami kcalendarcore knotifications
  kpeople qtbase-5 qtdeclarative-5 qtquickcontrols2-5 qtsvg-5 qtgraphicaleffects))
    (home-page "https://invent.kde.org/plasma-mobile/calindori")
    (synopsis "Calendar for Plasma Mobile")
    (description "This package provides a touch friendly calendar application.")
    (license license:gpl3+)))

(define-public discover
  (package
    (name "discover")
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
                "154sjr7c8dwlf1m22vh3wqiyfii8xpmxmfrf36i9r0xyb0zb5zg6"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list kcoreaddons
                  kconfig
                  kcrash
                  kdbusaddons
                  ki18n
                  karchive
                  kxmlgui
                  kirigami
                  kuserfeedback
                  knotifications
                  kio
                  kdeclarative
                  kcmutils
                  kidletime
                  qtdeclarative-5))
    (synopsis "KDE and Plasma resources management GUI")
    (description "This package provides a way to find and install applications,
games, and tools.")
    (home-page "https://invent.kde.org/plasma/discover")
    (license (list license:gpl2 license:gpl3))))

(define-public drkonqi
  (package
    (name "drkonqi")
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
                "06na44x22bxd94r24xkzc373d0rhmv6l1awfq0bzh9cxpy8yg3f4"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "ctest" "-E" "connectiontest")))))))
    (native-inputs (list extra-cmake-modules))
    (inputs (list ki18n
                  kcoreaddons
                  kconfig
                  kservice
                  kdeclarative
                  kjobwidgets
                  kio
                  kcrash
                  kcompletion
                  kwidgetsaddons
                  kwallet
                  knotifications
                  kidletime
                  kwindowsystem
                  ksyntaxhighlighting
                  qtdeclarative-5))
    (synopsis "Crash handler for KDE software")
    (description "This package provides an automatic handler for crashed apps.")
    (home-page "https://invent.kde.org/plasma/drkonqi")
    (license license:gpl2+)))

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

(define-public plasma-workspace-wallpapers
  (package
    (name "plasma-workspace-wallpapers")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/plasma/"
                    version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1vjrfmzib17cb9r2q17rv4zmnqsk5mf55vy8kzx71djjif7gaqiy"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Oxygen provides the standard wallpapers for the KDE desktop")
    (description "This package provides Wallpapers for the KDE desktop.")
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

(define-public kde-cli-tools
  (package
    (name "kde-cli-tools")
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
                "00p6vm9qw871hjclvw21nh93dq60r1ylb95hscx917gfx42dan8x"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f ;TODO: Failing test
           #:phases #~(modify-phases %standard-phases
                        (add-before 'check 'setup-env
                          (lambda* _
                            (setenv "XDG_RUNTIME_DIR"
                                    (getcwd)))))))
    (native-inputs (list extra-cmake-modules))
    (inputs (list kconfig
                  kdoctools
                  kiconthemes
                  ki18n
                  kcmutils
                  kio
                  kservice
                  kwindowsystem
                  kactivities
                  kparts
                  plasma-workspace
                  qtsvg-5))
    (synopsis "Tools for KDE to better interact with the system")
    (description "This package provides tools based on KDE Frameworks 5
to better interact with the system.")
    (home-page "https://invent.kde.org/plasma/kde-cli-tools")
    (license license:lgpl2.0+)))

(define-public kde-gtk-config
  (package
    (name "kde-gtk-config")
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
                "03cr5v7sr67vhcidr87min8z1ld5dm0n6yh79c1ghp1hp5ndscl8"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules
                         `(,glib "bin")
                         `(,gtk+ "bin")
                         pkg-config
                         sassc))
    (inputs (list glib
                  gtk+
                  kcoreaddons
                  kconfig
                  kconfigwidgets
                  kdecoration
                  kguiaddons
                  kdbusaddons
                  qtsvg-5
                  xsettingsd))
    (synopsis "Syncs KDE settings to GTK applications")
    (description "Provides a good integration with GTK applications.")
    (home-page "https://invent.kde.org/plasma/kde-gtk-config/")
    (license (list license:gpl2 license:gpl3))))

(define-public kdeplasma-addons
  (package
    (name "kdeplasma-addons")
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
                "1lgmmqr798cfxmllalgzixf3v9xbiiazbn3vkcdqxcj6cjf730c0"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "ctest" "-E"
                               "(converterrunnertest|spellcheckrunnertest)")))))))
    (native-inputs (list extra-cmake-modules))
    (inputs (list karchive
                  kconfig
                  kcoreaddons
                  kdeclarative
                  kholidays
                  ki18n
                  kio
                  kcmutils
                  knotifications
                  plasma-framework
                  krunner
                  kservice
                  sonnet
                  kunitconversion
                  knewstuff
                  qtdeclarative-5))
    (synopsis "Add-ons to improve your Plasma experience")
    (description "Provides multiple addons for Plasma Desktop.")
    (home-page "https://invent.kde.org/plasma/kdeplasma-addons")
    (license license:lgpl2.0)))

(define-public khotkeys
  (package
    (name "khotkeys")
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
                "0hkicwkcjb4p4k5yh8d60h6khsvrqkhjx42aby6rxd3mgvrqd3xy"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kdbusaddons
                  kdoctools
                  kglobalaccel
                  ki18n
                  kcmutils
                  kio
                  ktextwidgets
                  kxmlgui
                  kdelibs4support
                  plasma-workspace
                  qtx11extras))
    (synopsis "Trigger actions when certain keys are pressed")
    (description "This package provide trigger actions when certain keys
are pressed.")
    (home-page "https://invent.kde.org/plasma/khotkeys")
    (license license:lgpl2.0)))

(define-public kinfocenter
  (package
    (name "kinfocenter")
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
                "0ns2xsqghglg4ikq7w556y1kh20gs677km1vs0paw50xhi7jzbd2"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-systemsettings-symlink
                          (lambda* _
                            (substitute* "CMakeLists.txt"
                              (("\\$\\{KDE_INSTALL_FULL_BINDIR\\}/systemsettings5")
                               (string-append #$system-settings
                                              "/bin/systemsettings5"))))))))
    (native-inputs (list aha extra-cmake-modules kdoctools pkg-config))
    ;; * vulkaninfo
    ;; * wayland-info, <https://gitlab.freedesktop.org/wayland/wayland-utils>
    ;; Wayland KCM
    ;; * eglinfo
    (inputs (list dmidecode
	              ;fwupdmgr
                  kconfig
                  kconfigwidgets
                  kcoreaddons
                  kirigami
                  ki18n
                  kcmutils
                  kio
                  kservice
                  solid
                  kwidgetsaddons
                  kdeclarative
                  kpackage
                  kwayland
                  mesa-utils
                  pciutils
                  solid
                  util-linux
                  vulkan-tools
                  plasma-framework
                  qtbase-5
                  xdpyinfo))
    (propagated-inputs (list system-settings))
    (home-page "https://invent.kde.org/plasma/kinfocenter")
    (synopsis "View information about computer's hardware")
    (description "This package provides tool to view information about
computer's hardware.")
    (license (list license:gpl2 license:gpl3))))

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

(define-public ksystemstats
  (package
    (name "ksystemstats")
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
                "0ray2v90vv1j1sbd7fx4x5n7s7xwlil1zynwi4pzpgnyi13zq60x"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "ctest" "-E" "ksystemstatstest")))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list glib
                  kcoreaddons
                  kdbusaddons
                  solid
                  knetworkmanager
                  kiconthemes
                  kio
                  ki18n
                  libksysguard
                  libnl
                  eudev
                  `(,lm-sensors "lib")
                  network-manager))
    (synopsis "Plugin based system monitoring daemon")
    (description "This package provides a daemon that collects statistics about
the running system.")
    (home-page "https://invent.kde.org/plasma/ksystemstats")
    (license (list license:gpl2 license:gpl3))))

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

(define-public kwallet-pam
  (package
    (name "kwallet-pam")
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
                "14j3xngwliqhhvw60izv5kdjvv8xhqw8cjsc4l22v8jj4m8yw2xk"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f)) ; no tests
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list linux-pam kwallet libgcrypt))
    (synopsis "PAM Integration with KWallet")
    (description "Provide PAM Integration with KWallet to unlock KWallet when
you login.")
    (home-page "https://invent.kde.org/plasma/kwallet-pam")
    (license (list license:lgpl2.1+))))

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

(define-public kwin
  (package
    (name "kwin")
    (version "5.24.4")
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
                 "1qwcd6iw6yvpchiwmvq5nwsr465jmrmscf286mjrc65im4hj6572"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch
                          (lambda* (#:key inputs #:allow-other-keys)
                            (substitute* '("src/plugins/kdecorations/aurorae/src/aurorae.cpp")
                              (("(^\\s*QDirIterator it.path, QDirIterator::Subdirectories)(\\);)"
                                _ a b)
                               (string-append a
                                " | QDirIterator::FollowSymlinks" b)))
                            (substitute* '("src/xwl/xwayland.cpp")
                              (("(m_xwaylandProcess->setProgram.QStringLiteral..)(Xwayland)(...;)"
                                _ a Xwayland b)
                               (string-append a
                                              (which "Xwayland") b)))
                            (substitute* '("cmake/modules/Findhwdata.cmake")
                              (("/usr/share")
                               (string-append #$hwdata "/share")))))
                        (add-after 'install 'add-symlinks
                          (lambda* (#:key outputs #:allow-other-keys)
                            (let ((kst5 (string-append #$output
                                         "/share/kservicetypes5/")))
                              (symlink (string-append kst5
                                                      "kwineffect.desktop")
                                       (string-append kst5
                                                      "kwin-effect.desktop"))
                              (symlink (string-append kst5
                                                      "kwinscript.desktop")
                                       (string-append kst5
                                                      "kwin-script.desktop")))))
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "XDG_RUNTIME_DIR"
                                      (getcwd))
                              (setenv "HOME"
                                      (getcwd))
                              (setenv "XDG_DATA_DIRS"
                                      (string-append #$output "/share:"
                                                     (getenv "XDG_DATA_DIRS")))
                              (setenv "QT_PLUGIN_PATH"
                                      (string-append #$output
                                                     "/lib/qt5/plugins:"
                                                     (getenv "QT_PLUGIN_PATH")))
                              (setenv "DISPLAY" ":1")
                              (system "Xvfb :1 &")
                              (sleep 5)
                              (invoke "ctest" "-E"
                               "(kwin-testDontCrashGlxgears|kwin-testLockScreen|kwin-testPointerInput|kwin-testDebugConsole|kwin-testXdgShellClient|kwin-testXdgShellClient-waylandonly|kwin-testSceneOpenGLES|kwin-testSceneOpenGLES-waylandonly|kwin-testModiferOnlyShortcut|kwin-testInputMethod|kwin-testInputMethod-waylandonly|kwin-testNightColor|kwin-testNightColor-waylandonly|kwin-testPlasmaWindow|kwin-testX11Client|kwin-testSceneQPainter|kwin-testLibinputDevice)")))))))
    (native-inputs (list extra-cmake-modules
                         dbus
                         kdoctools
                         pkg-config
                         qttools-5
                         xorg-server-for-tests))
    (inputs (list breeze
                  eudev
                  fontconfig
                  freetype
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
                  qtmultimedia-5
                  qtwayland
                  qtx11extras
                  wayland
                  wayland-protocols
                  xcb-util ;fails at build time without this
                  xcb-util-cursor
                  xcb-util-keysyms
                  xcb-util-wm
                  xcmsdb
                  xinput ;XXX: Says disabled in configure phase
                  xorg-server-xwayland
                  zlib))
    (propagated-inputs (list hwdata))
    ;; Runtime-only dependency needed for mapping monitor hardware vendor IDs to full names
    ;; * hwdata, <https://github.com/vcrhonek/hwdata>
    ;; * QtQuick.Controls-QMLModule, QML module 'QtQuick.Controls' is a runtime dependency.
    ;; * org.kde.kquickcontrolsaddons-QMLModule, QML module 'org.kde.kquickcontrolsaddons' is a runtime dependency.
    ;; * org.kde.plasma.core-QMLModule, QML module 'org.kde.plasma.core' is a runtime dependency.
    ;; * org.kde.plasma.components-QMLModule, QML module 'org.kde.plasma.components' is a runtime dependency.

    ;; * QAccessibilityClient, KDE client-side accessibility library, <https://www.kde.org>
    ;; Required to enable accessibility features

    (home-page "https://userbase.kde.org/KWin")
    (synopsis "KDE Plasma Window Manager")
    (description
     "KWin is an easy to use, but flexible, composited Window Manager for
Xorg windowing systems (Wayland, X11) on Linux.  Its primary usage is in
conjunction with the KDE Plasma Desktop.")
    (license license:gpl2+)))

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

(define-public milou
  (package
    (name "milou")
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
                "17npfn7gwiqrvy5w8vzpc38c4bgkx3vjgjkm1caizn04zfk7xar7"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons
                  ki18n
                  kdeclarative
                  kitemmodels
                  kservice
                  plasma-framework
                  kwindowsystem
                  krunner
                  qtdeclarative-5))
    (synopsis "Dedicated search application built on top of Baloo")
    (description "This package provides a dedicated search application built
on top of Baloo.")
    (home-page "https://invent.kde.org/plasma/milou")
    (license (list license:gpl2+))))

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

(define-public plasma-browser-integration
  (package
    (name "plasma-browser-integration")
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
                "1l3n4psbqimfar5qsmrfp3nhgg3v9yy93rkjpvyqgdmizi44scqw"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list kio
                  ki18n
                  kcoreaddons
                  kconfig
                  kcrash
                  kdbusaddons
                  knotifications
                  kitemmodels
                  krunner
                  kactivities
                  purpose
                  kfilemetadata
                  kjobwidgets
                  qtdeclarative-5))
    (propagated-inputs (list plasma-workspace))
    (home-page "https://invent.kde.org/plasma/plasma-browser-integration")
    (synopsis
     "Components necessary to integrate browsers into the Plasma Desktop")
    (description "This package aims to provide better integration of web
browsers with the KDE Plasma 5 desktop.")
    (license license:gpl3+)))

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

(define-public plasma-systemmonitor
  (package
    (name "plasma-systemmonitor")
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
                "1fj0vyjdk6h3f4p9aagh03hyhbf69y2qwlavs2zr7d0iadih7b4c"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
	(inputs (list ki18n kconfig kdeclarative kservice kiconthemes kglobalaccel
  kio kdbusaddons kirigami knewstuff ksystemstats kitemmodels libksysguard qtdeclarative-5
  qtquickcontrols2-5))
    (synopsis "System sensors, process information and other system resources
monitor")
    (description "This package provides an interface for monitoring system
sensors, process information and other system resources.")
    (home-page "https://invent.kde.org/plasma/plasma-systemmonitor")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-workspace
  (package
    (name "plasma-workspace")
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
                "1aw9ms6rzxrk384xzdc3sqwqs1shbnkap40vfwxp4jamjk0pyglw"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules kdoctools pkg-config qtsvg-5
                         qttools-5))
    (inputs (list appstream-qt
                  baloo
                  breeze
                  breeze-icons
                  dbus
                  fontconfig
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
				  kio-extras
				  kio-fuse
                  kitemmodels
				  kirigami
                  knewstuff
                  knotifications
                  knotifyconfig
				  kquickcharts
                  kpackage
                  kpeople
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
                  plasma-framework
                  plasma-workspace-wallpapers
                  plasma-wayland-protocols
                  prison
                  qtbase-5
                  qtdeclarative-5
				  qtquickcontrols2-5
                  qtwayland
				  qtgraphicaleffects
                  qtx11extras
                  wayland
                  wayland-protocols-next

                  xcb-util
                  xcb-util-image
                  xcb-util-keysyms
                  xrdb
                  xmessage
                  xsetroot
                  zlib))
    (propagated-inputs (list iso-codes))
    ;; -- The following RUNTIME packages have not been found:
    ;; * AppMenuGtkModule, Application Menu GTK+ Module, <https://github.com/rilian-la-te/vala-panel-appmenu/tree/master/subprojects/appmenu-gtk-module>
    ;; -- The following OPTIONAL packages have not been found:
    ;; * PackageKitQt5, Software Manager integration
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-wallpaper
                          (lambda _
                            (substitute* "sddm-theme/theme.conf.cmake"
                              (("background=..KDE_INSTALL_FULL_WALLPAPERDIR.")
                               (string-append "background="
                                              #$breeze "/share/wallpapers")))))
                        (add-after 'unpack 'patch-workspace-bins
                          (lambda _
                            (substitute* "startkde/startplasma.cpp"
                              (("xmessage")
                               (string-append #$xmessage "/bin/xmessage"))
                              (("xsetroot")
                               (string-append #$xsetroot "/bin/xsetroot")))
                            (substitute* (list "kcms/fonts/fontinit.cpp"
                                               "kcms/fonts/fonts.cpp"
                                               "kcms/krdb/krdb.cpp")
                              (("xrdb")
                               (string-append #$xrdb "/bin/xrdb")))
                            (substitute* "startkde/plasma-session/startup.cpp"
                              (("CMAKE_INSTALL_FULL_LIBEXECDIR_KF5..")
                               (string-append "\""
                                              #$kinit "/lib/libexec/kf5")))
                            (substitute* (list
                                          "startkde/startplasma-wayland.cpp"
                                          "startkde/startplasma-x11.cpp")
                              (("kdeinit5_shutdown")
                               (string-append #$kinit "/bin/kdeinit5_shutdown")))))
                        (delete 'check)
                        (add-after 'install 'check-after-install
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "HOME"
                                      (getcwd))
                              (setenv "XDG_RUNTIME_DIR"
                                      (getcwd))
                              (setenv "XDG_CACHE_HOME"
                                      (getcwd))
                              (setenv "QT_QPA_PLATFORM" "offscreen")
                              (setenv "QT_PLUGIN_PATH"
                                      (string-append #$output
                                                     "/lib/qt5/plugins:"
                                                     (getenv "QT_PLUGIN_PATH")))
                              (invoke "ctest" "-E"
                               "(appstreamtest|lookandfeel-kcmTest|tst_triangleFilter|systemtraymodeltest|testdesktop| screenpooltest)")))))))
    (home-page "https://invent.kde.org/plasma/plasma-workspace")
    (synopsis "Plasma workspace components")
    (description
     "Workspaces provide support for KDE Plasma Widgets,
+integrated search, hardware management and a high degree of customizability")
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
                         dbus
                         kdoctools
                         intltool
                         pkg-config
                         qtsvg-5
                         qttools-5))
    (inputs (list ;; packagekit-qt
                  signon-plugin-oauth2
                  signond

                  attica
                  appstream-qt
                  baloo
                  breeze
                  breeze-icons
                  eudev
                  fontconfig
                  glib
                  ibus
                  kaccounts-integration
                  kactivities
                  kactivities-stats
                  kauth
                  karchive
                  kcmutils
                  kconfig
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  kdeclarative
                  kded
                  kdesu
                  kdelibs4support
                  kglobalaccel
                  kguiaddons
                  kholidays
                  ki18n
                  kiconthemes
                  kidletime
                  kinit
                  kio
                  kitemmodels
                  knewstuff
                  knotifications
                  knotifyconfig
                  kpackage
                  kpeople
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
                  libcanberra
                  libkscreen
                  libksysguard
                  libqalculate
                  gmp
                  mpfr
                  libsm
                  libxi
                  libxft
                  libxkbcommon
                  libxrender
                  libxtst
                  networkmanager-qt
                  phonon
                  pipewire-0.3
                  plasma-framework
                  plasma-wayland-protocols
                  pulseaudio
                  prison
                  qqc2-desktop-style
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols2-5
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
    (propagated-inputs (list iso-codes kirigami plasma-workspace))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after `unpack `fix-paths
                          (lambda* (#:key inputs #:allow-other-keys)
                            (substitute* "kcms/keyboard/iso_codes.h"
                              (("\"/usr/share/xml/iso-codes\"")
                               (string-append "\""
                                              #$iso-codes
                                              "/share/xml/iso-codes\"")))))
                        (add-after 'unpack 'patch-qml-import-path
                          (lambda _
                            (substitute* '("applets/pager/package/contents/ui/main.qml"
                                           "containments/desktop/package/contents/ui/FolderView.qml"
                                           "containments/desktop/package/contents/ui/main.qml"
                                           "containments/panel/contents/ui/main.qml")
                              (("^import \"(utils|FolderTools|LayoutManager).js\" as "
                                line mod)
                               (string-append "import \"../code/" mod
                                              ".js\" as ")))))
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
                              (invoke "ctest" "-E" "foldermodeltest")))))))
    (home-page "https://kde.org/plasma-desktop/")
    (synopsis "Plasma for the Desktop")
    (description
     "Plasma Desktop offers a beautiful looking desktop that takes
complete advantage of modern computing technology.  Through the use of visual
effects and scalable graphics, the desktop experience is not only smooth but
also pleasant to the eye.  The looks of Plasma Desktop not only provide
beauty, they are also used to support and improve your computer
activities effectively, without being distracting.")
    (license (list license:bsd-3 license:gpl2+ license:gpl3 license:lgpl2.1+
                   license:lgpl3))))

(define-public polkit-kde-agent
  (package
    (name "polkit-kde-agent")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-1-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "0skadgzv97vfl4n2x889fiy5gsr6n894fr5viq3rizs0qsqc68b5"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list ki18n
                  kwindowsystem
                  kdbusaddons
                  kwidgetsaddons
                  kcoreaddons
                  kcrash
                  kiconthemes
                  polkit-qt))
    (synopsis "Daemon providing a Polkit authentication UI for Plasma")
    (description "This package provides daemon providing a Polkit authentication
UI for Plasma")
    (home-page "https://invent.kde.org/plasma/polkit-kde-agent-1")
    (license license:gpl2+)))

(define-public powerdevil
  (package
    (name "powerdevil")
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
                "1nznjxi59xc6pmyh0vainznhp9ig1ini3i87iapayawpnpf8sdxx"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules qttools-5 pkg-config))
    (inputs (list bluez-qt
                  glib
                  kauth
                  kactivities
                  kscreen
                  kidletime
                  kconfig
                  kdbusaddons
                  solid
                  ki18n
                  kcrash
                  knotifyconfig
                  knetworkmanager
                  kio
                  kwayland
                  kglobalaccel
                  kcrash
                  knotifications
                  kirigami
                  libkscreen
                  network-manager
                  plasma-workspace
                  eudev
                  qtx11extras))
    (synopsis "Manages the power consumption settings of a Plasma Shell")
    (description "This package provides the power consumption settings
of a Plasma shell.")
    (home-page "https://invent.kde.org/plasma/powerdevil")
    (license license:gpl2+)))

(define-public system-settings
  (package
    (name "system-settings")
    (version "5.25.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/systemsettings-" version ".tar.xz"))
              (sha256
               (base32
                "130739bfxl1jwkn3f4h7dnr7dv2i76g6sd2svmg0qy60cnwvcgcv"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kauth
                  kcrash
                  kitemviews
                  kitemmodels
                  kcmutils
                  ki18n
                  kio
                  kservice
                  kiconthemes
                  kwidgetsaddons
                  kwindowsystem
                  kxmlgui
                  kdbusaddons
                  kconfig
                  kpackage
                  kactivities
                  kactivities-stats
                  kguiaddons
                  kirigami
                  knotifications
                  krunner
                  plasma-workspace
                  qtdeclarative-5))
    (synopsis "Control center to configure Plasma Desktop")
    (description "This package provides configuration UI for Plasma Desktop.")
    (home-page "https://invent.kde.org/plasma/systemsettings")
    (license license:gpl2+)))

(define-public xdg-desktop-portal-kde
  (package
    (name "xdg-desktop-portal-kde")
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
                "13gva3mci9gawlxpw4ymdma8w6lc2b3y8z36699kmzli4vib214g"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list cups
                  kcoreaddons
                  kconfig
                  ki18n
                  kdeclarative
                  kio
                  kirigami
                  knotifications
                  plasma-framework
                  plasma-wayland-protocols
                  kwayland
                  kwidgetsaddons
                  kwindowsystem
                  kiconthemes
                  qtdeclarative-5
                  qtwayland
                  wayland))
    (synopsis "Backend implementation for xdg-desktop-portal using Qt/KF5")
    (description "This package provides a backend implementation
for xdg-desktop-portal that is using Qt/KF5.")
    (home-page "https://invent.kde.org/plasma/xdg-desktop-portal-kde")
    (license license:lgpl2.0+)))
