;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2016, 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2017, 2018, 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 nee <nee@cock.li>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018, 2019, 2020 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
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

(define-module (gnu packages bootloaders)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex))

(define unifont
  ;; GNU Unifont, <http://gnu.org/s/unifont>.
  ;; GRUB needs it for its graphical terminal, gfxterm.
  (origin
    (method url-fetch)
    (uri
     "http://unifoundry.com/pub/unifont-7.0.06/font-builds/unifont-7.0.06.bdf.gz")
    (sha256
     (base32
      "0p2vhnc18cnbmb39vq4m7hzv4mhnm2l0a2s7gx3ar277fwng3hys"))))

(define-public grub
  (package
    (name "grub")
    (version "2.06")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/grub/grub-" version ".tar.xz"))
             (sha256
              (base32
               "1qbycnxkx07arj9f2nlsi9kp0dyldspbv07ysdyd34qvz55a97mp"))
             (patches (search-patches
                       "grub-efi-fat-serial-number.patch"
                       "grub-setup-root.patch"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; Adjust QEMU invocation to not use a deprecated device
                 ;; name that was removed in QEMU 6.0.  Remove for >2.06.
                 (substitute* "tests/ahci_test.in"
                   (("ide-drive")
                    "ide-hd"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ;; Counterintuitively, this *disables* a spurious Python dependency by
       ;; calling the ‘true’ binary instead.  Python is only needed during
       ;; bootstrapping (for genptl.py), not when building from a release.
       (list "PYTHON=true")
       ;; Grub fails to load modules stripped with --strip-unneeded.
       #:strip-flags '("--strip-debug" "--enable-deterministic-archives")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-stuff
                   (lambda* (#:key native-inputs inputs #:allow-other-keys)
                     (substitute* "grub-core/Makefile.in"
                       (("/bin/sh") (which "sh")))

                     ;; Give the absolute file name of 'mdadm', used to
                     ;; determine the root file system when it's a RAID
                     ;; device.  Failing to do that, 'grub-probe' silently
                     ;; fails if 'mdadm' is not in $PATH.
                     (when (assoc-ref inputs "mdadm")
                       (substitute* "grub-core/osdep/linux/getroot.c"
                         (("argv\\[0\\] = \"mdadm\"")
                          (string-append "argv[0] = \""
                                         (assoc-ref inputs "mdadm")
                                         "/sbin/mdadm\""))))

                     ;; Make the font visible.
                     (copy-file (assoc-ref (or native-inputs inputs)
                                           "unifont")
                                "unifont.bdf.gz")
                     (system* "gunzip" "unifont.bdf.gz")

                     ;; Give the absolute file name of 'ckbcomp'.
                     (substitute* "util/grub-kbdcomp.in"
                       (("^ckbcomp ")
                        (string-append
                         (search-input-file inputs "/bin/ckbcomp")
                         " ")))))
                  (add-after 'unpack 'set-freetype-variables
                    ;; These variables need to be set to the native versions
                    ;; of the dependencies because they are used to build
                    ;; programs which are executed during build time.
                    (lambda* (#:key native-inputs #:allow-other-keys)
                      (when (assoc-ref native-inputs "freetype")
                        (let ((freetype (assoc-ref native-inputs "freetype")))
                          (setenv "BUILD_FREETYPE_LIBS"
                                  (string-append "-L" freetype
                                                 "/lib -lfreetype"))
                          (setenv "BUILD_FREETYPE_CFLAGS"
                                  (string-append "-I" freetype
                                                 "/include/freetype2"))))
                     #t))
                  (add-before 'check 'disable-flaky-test
                    (lambda _
                      ;; This test is unreliable. For more information, see:
                      ;; <https://bugs.gnu.org/26936>.
                      (substitute* "Makefile.in"
                        (("grub_cmd_date grub_cmd_set_date grub_cmd_sleep")
                          "grub_cmd_date grub_cmd_sleep"))
                      #t))
                  (add-before 'check 'disable-pixel-perfect-test
                    (lambda _
                      ;; This test compares many screenshots rendered with an
                      ;; older Unifont (9.0.06) than that packaged in Guix.
                      (substitute* "Makefile.in"
                        (("test_unset grub_func_test")
                          "test_unset"))
                      #t)))
       ;; Disable tests on ARM and AARCH64 platforms or when cross-compiling.
       #:tests? ,(not (or (any (cute string-prefix? <> (or (%current-target-system)
                                                           (%current-system)))
                               '("arm" "aarch64"))
                          (%current-target-system)))))
    (inputs
     `(("gettext" ,gettext-minimal)

       ;; Depend on LVM2 for libdevmapper, used by 'grub-probe' and
       ;; 'grub-install' to recognize mapped devices (LUKS, etc.)
       ,@(if (member (or (%current-target-system)
                         (%current-system))
                     (package-supported-systems lvm2))
             `(("lvm2" ,lvm2))
             '())

       ;; Depend on mdadm, which is invoked by 'grub-probe' and 'grub-install'
       ;; to determine whether the root file system is RAID.
       ,@(if (member (or (%current-target-system)
                         (%current-system))
                     (package-supported-systems mdadm))
             `(("mdadm" ,mdadm))
             '())

       ;; Console-setup's ckbcomp is invoked by grub-kbdcomp.  It is required
       ;; for generating alternative keyboard layouts.
       ("console-setup" ,console-setup)

       ;; Needed for ‘grub-mount’, the only reliable way to tell whether a given
       ;; file system will be readable by GRUB without rebooting.
       ,@(if (member (or (%current-target-system)
                         (%current-system))
                     (package-supported-systems fuse))
             `(("fuse" ,fuse))
             '())

       ("freetype" ,freetype)
       ;; ("libusb" ,libusb)
       ("ncurses" ,ncurses)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("unifont" ,unifont)
       ("bison" ,bison)
       ("flex" ,flex)
       ("texinfo" ,texinfo)
       ("help2man" ,help2man)
       ("freetype" ,freetype)   ; native version needed for build-grub-mkfont

       ;; XXX: When building GRUB 2.02 on 32-bit x86, we need a binutils
       ;; capable of assembling 64-bit instructions.  However, our default
       ;; binutils on 32-bit x86 is not 64-bit capable.
       ,@(if (string-match "^i[3456]86-" (%current-system))
             (let ((binutils (package/inherit
                              binutils
                              (name "binutils-i386")
                              (arguments
                               (substitute-keyword-arguments (package-arguments binutils)
                                 ((#:configure-flags flags ''())
                                  `(cons "--enable-64-bit-bfd" ,flags)))))))
               `(("ld-wrapper" ,(make-ld-wrapper "ld-wrapper-i386"
                                                 #:binutils binutils))
                 ("binutils" ,binutils)))
             '())

       ;; Dependencies for the test suite.  The "real" QEMU is needed here,
       ;; because several targets are used.
       ("parted" ,parted)
       ,@(if (member (%current-system) (package-supported-systems qemu-minimal))
             `(("qemu" ,qemu-minimal))
             '())
       ("xorriso" ,xorriso)))
    (home-page "https://www.gnu.org/software/grub/")
    (synopsis "GRand Unified Boot loader")
    (description
     "GRUB is a multiboot bootloader.  It is used for initially loading the
kernel of an operating system and then transferring control to it.  The kernel
then goes on to load the rest of the operating system.  As a multiboot
bootloader, GRUB handles the presence of multiple operating systems installed
on the same computer; upon booting the computer, the user is presented with a
menu to select one of the installed operating systems.")
    (license license:gpl3+)
    (properties '((cpe-name . "grub2")))))

(define-public grub-minimal
  (package
    (inherit grub)
    (name "grub-minimal")
    (inputs
     (modify-inputs (package-inputs grub)
       (delete "lvm2" "mdadm" "fuse" "console-setup")))
    (native-inputs
     (modify-inputs (package-native-inputs grub)
       (delete "help2man" "texinfo" "parted" "qemu" "xorriso")))
    (arguments
     (substitute-keyword-arguments (package-arguments grub)
       ((#:configure-flags _ ''())
        '(list "PYTHON=true"))
       ((#:tests? _ #t)
        #f)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (replace 'patch-stuff
             (lambda* (#:key native-inputs inputs #:allow-other-keys)
               (substitute* "grub-core/Makefile.in"
                 (("/bin/sh") (which "sh")))

               ;; Make the font visible.
               (copy-file (assoc-ref (or native-inputs inputs)
                                     "unifont")
                          "unifont.bdf.gz")
               (system* "gunzip" "unifont.bdf.gz")

               #t))))))))

(define-public grub-efi
  (package
    (inherit grub)
    (name "grub-efi")
    (synopsis "GRand Unified Boot loader (UEFI version)")
    (inputs
     (modify-inputs (package-inputs grub)
       (prepend efibootmgr mtools)))
    (arguments
     `(;; TODO: Tests need a UEFI firmware for qemu. There is one at
       ;; https://github.com/tianocore/edk2/tree/master/OvmfPkg .
       ;; Search for 'OVMF' in "tests/util/grub-shell.in".
       ,@(substitute-keyword-arguments (package-arguments grub)
           ((#:tests? _ #f) #f)
           ((#:configure-flags flags ''())
            `(cons* "--with-platform=efi"
                    ,@(if (string-prefix? "x86_64"
                                          (or (%current-target-system)
                                              (%current-system)))
                          '("--enable-stack-protector") ; EFI-only for now
                          '())
                    ,flags))
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'patch-stuff 'use-absolute-efibootmgr-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "grub-core/osdep/unix/platform.c"
                     (("efibootmgr")
                      (search-input-file inputs
                                         "/sbin/efibootmgr")))))
               (add-after 'patch-stuff 'use-absolute-mtools-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((mtools (assoc-ref inputs "mtools")))
                     (substitute* "util/grub-mkrescue.c"
                       (("\"mformat\"")
                        (string-append "\"" mtools
                                       "/bin/mformat\"")))
                     (substitute* "util/grub-mkrescue.c"
                       (("\"mcopy\"")
                        (string-append "\"" mtools
                                       "/bin/mcopy\"")))
                     #t))))))))))

(define-public grub-efi32
  (package
    (inherit grub-efi)
    (name "grub-efi32")
    (synopsis "GRand Unified Boot loader (UEFI 32bit version)")
    (arguments
     `(,@(substitute-keyword-arguments (package-arguments grub-efi)
           ((#:configure-flags flags ''())
            `(cons*
               ,@(cond ((target-x86?) '("--target=i386"))
                       ((target-aarch64?)
                        (list "--target=arm"
                              (string-append "TARGET_CC="
                                             (cc-for-target "arm-linux-gnueabihf"))))
                       ((target-arm?) '("--target=arm"))
                       (else '()))
               ,flags)))))
    (native-inputs
     (if (target-aarch64?)
       (modify-inputs (package-native-inputs grub-efi)
         (prepend
           (cross-gcc "arm-linux-gnueabihf")
           (cross-binutils "arm-linux-gnueabihf")))
       (package-native-inputs grub-efi)))))

;; Because grub searches hardcoded paths it's easiest to just build grub
;; again to make it find both grub-pc and grub-efi.  There is a command
;; line argument which allows you to specify ONE platform - but
;; grub-mkrescue will use multiple platforms if they are available
;; in the installation directory (without command line argument).
(define-public grub-hybrid
  (package
    (inherit grub-efi)
    (name "grub-hybrid")
    (synopsis "GRand Unified Boot loader (hybrid version)")
    (inputs
     (modify-inputs (package-inputs grub-efi)
       (prepend grub)))
    (arguments
     (substitute-keyword-arguments (package-arguments grub-efi)
       ((#:modules modules `((guix build utils) (guix build gnu-build-system)))
        `((ice-9 ftw) ,@modules))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'install 'install-non-efi
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((input-dir (search-input-directory inputs
                                                        "/lib/grub"))
                     (output-dir (string-append (assoc-ref outputs "out")
                                                "/lib/grub")))
                 (for-each
                  (lambda (basename)
                    (if (not (or (string-prefix? "." basename)
                                 (file-exists? (string-append output-dir "/" basename))))
                        (symlink (string-append input-dir "/" basename)
                                 (string-append output-dir "/" basename))))
                  (scandir input-dir))
                 #t)))))))))

(define-public syslinux
  (let ((commit "bb41e935cc83c6242de24d2271e067d76af3585c"))
    (package
      (name "syslinux")
      (version (git-version "6.04-pre" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/geneC/syslinux")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0k8dvafd6410kqxf3kyr4y8jzmpmrih6wbjqg6gklak7945yflrc"))
                (patches
                 (search-patches "syslinux-gcc10.patch"
                                 "syslinux-strip-gnu-property.patch"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("nasm" ,nasm)
         ("perl" ,perl)
         ("python-2" ,python-2)))
      (inputs
       `(("libuuid" ,util-linux "lib")
         ("mtools" ,mtools)))
      (arguments
       `(#:parallel-build? #f
         #:make-flags
         (list (string-append "BINDIR=" %output "/bin")
               (string-append "SBINDIR=" %output "/sbin")
               (string-append "LIBDIR=" %output "/lib")
               (string-append "INCDIR=" %output "/include")
               (string-append "DATADIR=" %output "/share")
               (string-append "MANDIR=" %output "/share/man")
               "PERL=perl"
               "bios")
         #:strip-flags '("--strip-debug" "--enable-deterministic-archives")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-files
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* (find-files "." "Makefile.*|ppmtolss16")
                 (("/bin/pwd") (which "pwd"))
                 (("/bin/echo") (which "echo"))
                 (("/usr/bin/perl") (which "perl")))
               (let ((mtools (assoc-ref inputs "mtools")))
                 (substitute* (find-files "." "\\.c$")
                   (("mcopy")
                    (string-append mtools "/bin/mcopy"))
                   (("mattrib")
                    (string-append mtools "/bin/mattrib"))))
               #t))
           (delete 'configure)
           (add-before 'build 'set-permissions
             (lambda _
               (invoke "chmod" "a+w" "utils/isohybrid.in")))
           (replace 'check
             (lambda _
               (setenv "CC" "gcc")
               (substitute* "tests/unittest/include/unittest/unittest.h"
                 ;; Don't look up headers under /usr.
                 (("/usr/include/") ""))
               (invoke "make" "unittest"))))))
      (home-page "https://www.syslinux.org")
      (synopsis "Lightweight Linux bootloader")
      (description "Syslinux is a lightweight Linux bootloader.")
      ;; The Makefile specifically targets i386 and x86_64 using nasm.
      (supported-systems '("i686-linux" "x86_64-linux"))
      (license (list license:gpl2+
                     license:bsd-3 ; gnu-efi/*
                     license:bsd-4 ; gnu-efi/inc/* gnu-efi/lib/*
                     ;; Also contains:
                     license:expat license:isc license:zlib)))))

(define-public dtc
  (package
    (name "dtc")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/software/utils/dtc/"
                    "dtc-" version ".tar.gz"))
              (sha256
               (base32
                "0xm38h31jb29xfh2sfyk48d8wdfq4b8lmb412zx9vjr35izjb9iq"))))
    (build-system gnu-build-system)
    (native-inputs
     (append
       (list bison
             flex
             libyaml
             pkg-config
             swig)
       (if (member (%current-system) (package-supported-systems valgrind))
           (list valgrind)
           '())))
    (inputs
     (list python))
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))

             ;; /bin/fdt{get,overlay,put} need help finding libfdt.so.1.
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib")

             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "SETUP_PREFIX=" (assoc-ref %outputs "out"))
             "INSTALL=install")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-pkg-config
           (lambda _
             (substitute* '("Makefile"
                            "tests/run_tests.sh")
               (("pkg-config")
                ,(pkg-config-for-target)))))
         (delete 'configure))))         ; no configure script
    (home-page "https://www.devicetree.org")
    (synopsis "Compiles device tree source files")
    (description "@command{dtc} compiles
@uref{http://elinux.org/Device_Tree_Usage, device tree source files} to device
tree binary files.  These are board description files used by Linux and BSD.")
    (license license:gpl2+)))

(define %u-boot-pinephonepro-adc-rockchip-saradc-patch
  (search-patch "u-boot-pinephonepro-adc-rockchip-saradc.patch"))

(define %u-boot-pinephonepro-device-enablement-patch
  (search-patch "u-boot-pinephonepro-device-enablement.patch"))

(define %u-boot-pinephonepro-hack-do-not-honor-patch
  (search-patch "u-boot-pinephonepro-hack-do-not-honor.patch"))

(define %u-boot-pinephonepro-rk8xx-poweroff-support-patch
  (search-patch "u-boot-pinephonepro-rk8xx-poweroff-support.patch"))

(define %u-boot-rockchip-inno-usb-patch
  ;; Fix regression in 2020.10 causing freezes on boot with USB boot enabled.
  ;; See https://gitlab.manjaro.org/manjaro-arm/packages/core/uboot-rockpro64/-/issues/4
  ;; and https://patchwork.ozlabs.org/project/uboot/patch/20210406151059.1187379-1-icenowy@aosc.io
  (search-patch "u-boot-rockchip-inno-usb.patch"))

(define %u-boot-sifive-prevent-relocating-initrd-fdt
  ;; Fix boot in 2021.07 on Hifive unmatched, see
  ;; https://bugs.launchpad.net/ubuntu/+source/u-boot/+bug/1937246
  (search-patch "u-boot-sifive-prevent-reloc-initrd-fdt.patch"))

(define %u-boot-allow-disabling-openssl-patch
  ;; Fixes build of u-boot 2021.10 without openssl
  ;; https://lists.denx.de/pipermail/u-boot/2021-October/462728.html
  (search-patch "u-boot-allow-disabling-openssl.patch"))

(define %u-boot-rk3399-enable-emmc-phy-patch
  ;; Fix emmc boot on rockpro64 and pinebook-pro, this was a regression
  ;; therefore should hopefully be fixed when updating u-boot.
  ;; https://lists.denx.de/pipermail/u-boot/2021-November/466329.html
  (search-patch "u-boot-rk3399-enable-emmc-phy.patch"))

(define u-boot
  (package
    (name "u-boot")
    (version "2022.04")
    (source (origin
	      (patches
               (list %u-boot-rockchip-inno-usb-patch
                     %u-boot-pinephonepro-adc-rockchip-saradc-patch
                     %u-boot-pinephonepro-device-enablement-patch
                     %u-boot-pinephonepro-hack-do-not-honor-patch
                     %u-boot-pinephonepro-rk8xx-poweroff-support-patch
                     %u-boot-allow-disabling-openssl-patch
                     %u-boot-sifive-prevent-relocating-initrd-fdt
                     %u-boot-rk3399-enable-emmc-phy-patch))
              (method url-fetch)
              (uri (string-append
                    "https://ftp.denx.de/pub/u-boot/"
                    "u-boot-" version ".tar.bz2"))
              (sha256
               (base32
                "1l5w13dznj0z1ibqv2d6ljx2ma1gnf5x5ay3dqkqwxr6750nbq38"))))
    (native-inputs
     `(("bc" ,bc)
       ("bison" ,bison)
       ("dtc" ,dtc)
       ("gnutls" ,gnutls)
       ("flex" ,flex)
       ("lz4" ,lz4)
       ("tinfo" ,ncurses/tinfo)
       ("perl" ,perl)
       ("python" ,python)
       ("python-coverage" ,python-coverage)
       ("python-pycryptodomex" ,python-pycryptodomex)
       ("python-pytest" ,python-pytest)
       ("swig" ,swig)
       ("libuuid" ,util-linux "lib")))
    (build-system  gnu-build-system)
    (home-page "https://www.denx.de/wiki/U-Boot/")
    (synopsis "ARM bootloader")
    (description "U-Boot is a bootloader used mostly for ARM boards.  It
also initializes the boards (RAM etc).")
    (license license:gpl2+)))

(define-public u-boot-tools
  (package
    (inherit u-boot)
    (name "u-boot-tools")
    (native-inputs
     (modify-inputs (package-native-inputs u-boot)
       (prepend sdl2)))
    (arguments
     `(#:make-flags '("HOSTCC=gcc")
       #:test-target "tcheck"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
              (("/bin/pwd") (which "pwd"))
              (("/bin/false") (which "false")))
             (substitute* "tools/dtoc/fdt_util.py"
              (("'cc'") "'gcc'"))
             (substitute* "tools/patman/test_util.py"
              ;; python3-coverage is simply called coverage in guix.
               (("python3-coverage") "coverage")

               ;; Don't require 100% coverage since it's brittle and can
               ;; fail with newer versions of coverage or dependencies.
               (("raise ValueError\\('Test coverage failure'\\)")
                "print('Continuing anyway since Guix does not care :O')"))
             (substitute* "test/run"
              ;; Make it easier to find test failures.
              (("#!/bin/bash") "#!/bin/bash -x")
              ;; This test would require git.
              (("\\./tools/patman/patman") (which "true"))
              ;; FIXME: test fails, needs further investiation
              (("run_test \"binman\"") "# run_test \"binman\"")
              ;; FIXME: test_spl fails, needs further investiation
              (("test_ofplatdata or test_handoff or test_spl")
                "test_ofplatdata or test_handoff")
              ;; FIXME: code coverage not working
              (("run_test \"binman code coverage\"")
               "# run_test \"binman code coverage\"")
              ;; This test would require internet access.
              (("\\./tools/buildman/buildman") (which "true")))
             (substitute* "test/py/tests/test_sandbox_exit.py"
              (("def test_ctrl_c")
               "@pytest.mark.skip(reason='Guix has problems with SIGINT')
def test_ctrl_c"))
             ;; Test against the tools being installed rather than tools built
             ;; for "sandbox" target.
             (substitute* "test/image/test-imagetools.sh"
               (("BASEDIR=sandbox") "BASEDIR=."))
             (for-each (lambda (file)
                              (substitute* file
                                  ;; Disable features that require OpenSSL due
                                  ;; to GPL/Openssl license incompatibilities.
                                  ;; See https://bugs.gnu.org/34717 for
                                  ;; details.
                                  (("CONFIG_FIT_SIGNATURE=y")
                                   "CONFIG_FIT_SIGNATURE=n\nCONFIG_UT_LIB_ASN1=n\nCONFIG_TOOLS_LIBCRYPTO=n")
                                  ;; This test requires a sound system, which is un-used
                                  ;; in u-boot-tools.
                                  (("CONFIG_SOUND=y") "CONFIG_SOUND=n")))
                              (find-files "configs" "sandbox_.*defconfig$|tools-only_defconfig"))
             #t))
         (replace 'configure
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "tools-only_defconfig" make-flags)))
         (replace 'build
           (lambda* (#:key inputs make-flags #:allow-other-keys)
             (apply invoke "make" "tools-all" make-flags)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (name)
                           (install-file name bin))
                         '("tools/netconsole"
                           "tools/jtagconsole"
                           "tools/gen_eth_addr"
                           "tools/gen_ethaddr_crc"
                           "tools/img2srec"
                           "tools/mkenvimage"
                           "tools/dumpimage"
                           "tools/mkimage"
                           "tools/kwboot"
                           "tools/proftool"
                           "tools/fdtgrep"
                           "tools/env/fw_printenv"
                           "tools/sunxi-spl-image-builder"))
               #t)))
           (delete 'check)
           (add-after 'install 'check
             (lambda* (#:key make-flags test-target #:allow-other-keys)
               (invoke "test/image/test-imagetools.sh")))
           ;; Only run full test suite on x86_64 systems, as many tests
           ;; assume x86_64.
           ,@(if (string-match "^x86_64-linux"
                               (or (%current-target-system)
                                   (%current-system)))
                 '((add-after 'check 'check-x86
                     (lambda* (#:key make-flags test-target #:allow-other-keys)
                       (apply invoke "make" "mrproper" make-flags)
                       (setenv "SDL_VIDEODRIVER" "dummy")
                       (setenv "PAGER" "cat")
                       (apply invoke "make" test-target make-flags))))
                 '()))))
    (description "U-Boot is a bootloader used mostly for ARM boards.  It
also initializes the boards (RAM etc).  This package provides its
board-independent tools.")))

(define-public (make-u-boot-package board triplet)
  "Returns a u-boot package for BOARD cross-compiled for TRIPLET."
  (let ((same-arch? (lambda ()
                      (string=? (%current-system)
                                (gnu-triplet->nix-system triplet)))))
    (package
      (inherit u-boot)
      (name (string-append "u-boot-"
                           (string-replace-substring (string-downcase board)
                                                     "_" "-")))
      (native-inputs
       `(,@(if (not (same-arch?))
             `(("cross-gcc" ,(cross-gcc triplet))
               ("cross-binutils" ,(cross-binutils triplet)))
             `())
         ,@(package-native-inputs u-boot)))
      (arguments
       `(#:modules ((ice-9 ftw)
                    (srfi srfi-1)
                    (guix build utils)
                    (guix build gnu-build-system))
         #:test-target "test"
         #:make-flags
         (list "HOSTCC=gcc"
               ,@(if (not (same-arch?))
                   `((string-append "CROSS_COMPILE=" ,triplet "-"))
                   '()))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs make-flags #:allow-other-keys)
               (let ((config-name (string-append ,board "_defconfig")))
                 (if (file-exists? (string-append "configs/" config-name))
                     (apply invoke "make" `(,@make-flags ,config-name))
                     (begin
                       (display "Invalid board name. Valid board names are:"
                                (current-error-port))
                       (let ((suffix-len (string-length "_defconfig"))
                             (entries (scandir "configs")))
                         (for-each (lambda (file-name)
                                     (when (string-suffix? "_defconfig" file-name)
                                       (format (current-error-port)
                                               "- ~A\n"
                                               (string-drop-right file-name
                                                                  suffix-len))))
                                   (sort entries string-ci<)))
                       (error "Invalid boardname ~s." ,board))))))
           (add-after 'configure 'disable-tools-libcrypto
             ;; Disable libcrypto due to GPL and OpenSSL license
             ;; incompatibilities
             (lambda _
               (substitute* ".config"
                 (("CONFIG_TOOLS_LIBCRYPTO=.*$") "CONFIG_TOOLS_LIBCRYPTO=n"))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (libexec (string-append out "/libexec"))
                      (uboot-files (append
                                    (remove
                                     ;; Those would not be reproducible
                                     ;; because of the randomness used
                                     ;; to produce them.
                                     ;; It's expected that the user will
                                     ;; use u-boot-tools to generate them
                                     ;; instead.
                                     (lambda (name)
                                       (string-suffix?
                                        "sunxi-spl-with-ecc.bin"
                                        name))
                                     (find-files "." ".*\\.(bin|efi|img|spl|itb|dtb|rksd)$"))
                                    (find-files "." "^(MLO|SPL)$"))))
                 (mkdir-p libexec)
                 (install-file ".config" libexec)
                 ;; Useful for "qemu -kernel".
                 (install-file "u-boot" libexec)
                 (for-each
                  (lambda (file)
                    (let ((target-file (string-append libexec "/" file)))
                      (mkdir-p (dirname target-file))
                      (copy-file file target-file)))
                  uboot-files)
                 #t)))))))))

(define-public u-boot-malta
  (make-u-boot-package "malta" "mips64el-linux-gnuabi64"))

(define-public u-boot-am335x-boneblack
  (let ((base (make-u-boot-package "am335x_evm" "arm-linux-gnueabihf")))
    (package
      (inherit base)
      (name "u-boot-am335x-boneblack")
      (description "U-Boot is a bootloader used mostly for ARM boards.  It
also initializes the boards (RAM etc).

This U-Boot is built for the BeagleBone Black, which was removed upstream,
adjusted from the am335x_evm build with several device trees removed so that
it fits within common partitioning schemes.")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'patch-defconfig
               ;; Patch out other devicetrees to build image small enough to
               ;; fit within typical partitioning schemes where the first
               ;; partition begins at sector 2048.
               (lambda _
                 (substitute* "configs/am335x_evm_defconfig"
                   (("CONFIG_OF_LIST=.*$") "CONFIG_OF_LIST=\"am335x-evm am335x-boneblack\"\n"))
                 #t)))))))))

(define-public u-boot-am335x-evm
  (make-u-boot-package "am335x_evm" "arm-linux-gnueabihf"))

(define-public (make-u-boot-sunxi64-package board triplet)
  (let ((base (make-u-boot-package board triplet)))
    (package
      (inherit base)
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key native-inputs inputs #:allow-other-keys)
                  (let ((bl31
                         (string-append
                          (assoc-ref (or native-inputs inputs) "firmware")
                          "/bl31.bin")))
                    (setenv "BL31" bl31)
                    ;; This is necessary when we're using the bundled dtc.
                    ;(setenv "PATH" (string-append (getenv "PATH") ":"
                    ;                              "scripts/dtc"))
                    )
                  #t))))))
      (native-inputs
       `(("firmware" ,arm-trusted-firmware-sun50i-a64)
         ,@(package-native-inputs base))))))

(define-public u-boot-pine64-plus
  (make-u-boot-sunxi64-package "pine64_plus" "aarch64-linux-gnu"))

(define-public u-boot-pine64-lts
  (make-u-boot-sunxi64-package "pine64-lts" "aarch64-linux-gnu"))

(define-public u-boot-pinebook
  (let ((base (make-u-boot-sunxi64-package "pinebook" "aarch64-linux-gnu")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'patch-pinebook-config
               ;; Fix regression with LCD video output introduced in 2020.01
               ;; https://patchwork.ozlabs.org/patch/1225130/
               (lambda _
                 (substitute* "configs/pinebook_defconfig"
                   (("CONFIG_VIDEO_BRIDGE_ANALOGIX_ANX6345=y") "CONFIG_VIDEO_BRIDGE_ANALOGIX_ANX6345=y\nCONFIG_VIDEO_BPP32=y"))
                 #t)))))))))

(define-public u-boot-bananapi-m2-ultra
  (make-u-boot-package "Bananapi_M2_Ultra" "arm-linux-gnueabihf"))

(define-public u-boot-a20-olinuxino-lime
  (make-u-boot-package "A20-OLinuXino-Lime" "arm-linux-gnueabihf"))

(define-public u-boot-a20-olinuxino-lime2
  (make-u-boot-package "A20-OLinuXino-Lime2" "arm-linux-gnueabihf"))

(define-public u-boot-a20-olinuxino-micro
  (make-u-boot-package "A20-OLinuXino_MICRO" "arm-linux-gnueabihf"))

(define-public u-boot-nintendo-nes-classic-edition
  (let ((base (make-u-boot-package "Nintendo_NES_Classic_Edition"
                                   "arm-linux-gnueabihf")))
    (package
      (inherit base)
      ;; Starting with 2019.01, FEL doesn't work anymore on A33.
      (version "2018.11")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://ftp.denx.de/pub/u-boot/"
                      "u-boot-" version ".tar.bz2"))
                (sha256
                 (base32
                  "0znkwljfwwn4y7j20pzz4ilqw8znphrfxns0x1lwdzh3xbr96z3k"))
                (patches (search-patches
                           "u-boot-nintendo-nes-serial.patch"))))
      (description "U-Boot is a bootloader used mostly for ARM boards.  It
also initializes the boards (RAM etc).

This version is for the Nintendo NES Classic Edition.  It is assumed that
you have added a serial port to pins PB0 and PB1 as described on
@url{https://linux-sunxi.org/Nintendo_NES_Classic_Edition}.

In order to use FEL mode on the device, hold the Reset button on the
device while it's being turned on (and a while longer).")
      (native-inputs
       `(("python" ,python-2)
         ,@(package-native-inputs base))))))

(define-public u-boot-wandboard
  (make-u-boot-package "wandboard" "arm-linux-gnueabihf"))

(define-public u-boot-mx6cuboxi
  (make-u-boot-package "mx6cuboxi" "arm-linux-gnueabihf"))

(define-public u-boot-novena
  (let ((base (make-u-boot-package "novena" "arm-linux-gnueabihf")))
    (package
      (inherit base)
      (description "U-Boot is a bootloader used mostly for ARM boards.  It
also initializes the boards (RAM etc).

This U-Boot is built for Novena.  Be advised that this version, contrary
to Novena upstream, does not load u-boot.img from the first partition.")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'patch-novena-defconfig
               ;; Patch configuration to disable loading u-boot.img from FAT partition,
               ;; allowing it to be installed at a device offset.
               (lambda _
                 (substitute* "configs/novena_defconfig"
                   (("CONFIG_SPL_FS_FAT=y") "# CONFIG_SPL_FS_FAT is not set"))
                 #t)))))))))

(define-public u-boot-cubieboard
  (make-u-boot-package "Cubieboard" "arm-linux-gnueabihf"))

(define-public u-boot-cubietruck
  (make-u-boot-package "Cubietruck" "arm-linux-gnueabihf"))

(define-public u-boot-puma-rk3399
  (let ((base (make-u-boot-package "puma-rk3399" "aarch64-linux-gnu")))
    (package
      (inherit base)
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key inputs #:allow-other-keys)
                  (setenv "BL31"
                          (search-input-file inputs "/bl31.elf"))))
              ;; Phases do not succeed on the bl31 ELF.
              (delete 'strip)
              (delete 'validate-runpath)))))
      (native-inputs
       `(("firmware" ,arm-trusted-firmware-rk3399)
         ,@(package-native-inputs base))))))

(define-public u-boot-qemu-riscv64
  (make-u-boot-package "qemu-riscv64" "riscv64-linux-gnu"))

(define-public u-boot-qemu-riscv64-smode
  (make-u-boot-package "qemu-riscv64_smode" "riscv64-linux-gnu"))

(define-public u-boot-sifive-unleashed
  (make-u-boot-package "sifive_unleashed" "riscv64-linux-gnu"))

(define-public u-boot-sifive-unmatched
  (let ((base (make-u-boot-package "sifive_unmatched" "riscv64-linux-gnu")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'set-environment
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((opensbi (string-append (assoc-ref inputs "firmware")
                                               "/fw_dynamic.bin")))
                   (setenv "OPENSBI" opensbi))))))))
      (inputs
       `(("firmware" ,opensbi-generic)
         ,@(package-inputs base))))))

(define-public u-boot-rock64-rk3328
  (let ((base (make-u-boot-package "rock64-rk3328" "aarch64-linux-gnu")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'set-environment
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((bl31 (search-input-file inputs "/bl31.elf")))
                   (setenv "BL31" bl31))))))))
      (native-inputs
       `(("firmware" ,arm-trusted-firmware-rk3328)
         ,@(package-native-inputs base))))))

(define-public u-boot-firefly-rk3399
  (let ((base (make-u-boot-package "firefly-rk3399" "aarch64-linux-gnu")))
    (package
      (inherit base)
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key inputs #:allow-other-keys)
                  (setenv "BL31" (search-input-file inputs "/bl31.elf"))))
              ;; Phases do not succeed on the bl31 ELF.
              (delete 'strip)
              (delete 'validate-runpath)))))
      (native-inputs
       `(("firmware" ,arm-trusted-firmware-rk3399)
         ,@(package-native-inputs base))))))

(define-public u-boot-rockpro64-rk3399
  (let ((base (make-u-boot-package "rockpro64-rk3399" "aarch64-linux-gnu")))
    (package
      (inherit base)
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key inputs #:allow-other-keys)
                  (setenv "BL31"
                          (search-input-file inputs "/bl31.elf"))))
              (add-after 'unpack 'patch-config
                (lambda _
                  (substitute* "configs/rockpro64-rk3399_defconfig"
                    (("CONFIG_USB=y") "\
CONFIG_USB=y
CONFIG_AHCI=y
CONFIG_AHCI_PCI=y
CONFIG_SATA=y
CONFIG_SATA_SIL=y
CONFIG_SCSI=y
CONFIG_SCSI_AHCI=y
CONFIG_DM_SCSI=y
"))
                  (substitute* "include/config_distro_bootcmd.h"
                    (("\"scsi_need_init=false")
                     "\"setenv scsi_need_init false")
                    (("#define BOOTENV_SET_SCSI_NEED_INIT \"scsi_need_init=;")
                     "#define BOOTENV_SET_SCSI_NEED_INIT \"setenv scsi_need_init;"))
                  (substitute* "include/configs/rockchip-common.h"
                    (("#define BOOT_TARGET_DEVICES\\(func\\)")
                     "
#if CONFIG_IS_ENABLED(CMD_SCSI)
       #define BOOT_TARGET_SCSI(func) func(SCSI, scsi, 0)
#else
       #define BOOT_TARGET_SCSI(func)
#endif
#define BOOT_TARGET_DEVICES(func)")
                    (("BOOT_TARGET_NVME\\(func\\) \\\\")
                     "\
BOOT_TARGET_NVME(func) \\
       BOOT_TARGET_SCSI(func) \\"))))
              ;; Phases do not succeed on the bl31 ELF.
              (delete 'strip)
              (delete 'validate-runpath)))))
      (native-inputs
       `(("firmware" ,arm-trusted-firmware-rk3399)
         ,@(package-native-inputs base))))))

(define-public u-boot-pinebook-pro-rk3399
  (let ((base (make-u-boot-package "pinebook-pro-rk3399" "aarch64-linux-gnu")))
    (package
      (inherit base)
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key inputs #:allow-other-keys)
                  (setenv "BL31"
                          (search-input-file inputs "/bl31.elf"))))
              ;; Phases do not succeed on the bl31 ELF.
              (delete 'strip)
              (delete 'validate-runpath)))))
      (native-inputs
       `(("firmware" ,arm-trusted-firmware-rk3399)
         ,@(package-native-inputs base))))))

(define-public u-boot-pinephone-pro-rk3399
  (make-u-boot-package "pinephone-pro-rk3399" "aarch64-linux-gnu"))

(define-public vboot-utils
  (package
    (name "vboot-utils")
    (version "R63-10032.B")
    (source (origin
              ;; XXX: Snapshots are available but changes timestamps every download.
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://chromium.googlesource.com"
                                        "/chromiumos/platform/vboot_reference"))
                    (commit (string-append "release-" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0h0m3l69vp9dr6xrs1p6y7ilkq3jq8jraw2z20kqfv7lvc9l1lxj"))
              (patches
               (search-patches "vboot-utils-skip-test-workbuf.patch"
                               "vboot-utils-fix-tests-show-contents.patch"
                               "vboot-utils-fix-format-load-address.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc"
                          ;; On ARM, we must pass "HOST_ARCH=arm" so that the
                          ;; ${HOST_ARCH} and ${ARCH} variables in the makefile
                          ;; match.  Otherwise, ${HOST_ARCH} will be assigned
                          ;; "armv7l", the value of `uname -m`, and will not
                          ;; match ${ARCH}, which will make the tests require
                          ;; QEMU for testing.
                          ,@(if (string-prefix? "arm"
                                                (or (%current-target-system)
                                                    (%current-system)))
                                '("HOST_ARCH=arm")
                                '())
                          (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-hard-coded-paths
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((coreutils (assoc-ref inputs "coreutils"))
                            (diffutils (assoc-ref inputs "diffutils")))
                        (substitute* "futility/misc.c"
                          (("/bin/cp") (string-append coreutils "/bin/cp")))
                        (substitute* "tests/bitmaps/TestBmpBlock.py"
                          (("/usr/bin/cmp") (string-append diffutils "/bin/cmp")))
                        (substitute* "vboot_host.pc.in"
                          (("prefix=/usr")
                           (string-append "prefix=" (assoc-ref outputs "out"))))
                        #t)))
                  (delete 'configure)
                  (add-before 'check 'patch-tests
                    (lambda _
                      ;; These tests compare diffs against known-good values.
                      ;; Patch the paths to match those in the build container.
                      (substitute* (find-files "tests/futility/expect_output")
                        (("/mnt/host/source/src/platform/vboot_reference")
                         (string-append "/tmp/guix-build-" ,name "-" ,version
                                        ".drv-0/source")))
                      ;; Tests require write permissions to many of these files.
                      (for-each make-file-writable (find-files "tests/futility"))
                      #t))
                  (add-after 'install 'install-devkeys
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (share (string-append out "/share/vboot-utils")))
                        (copy-recursively "tests/devkeys"
                                          (string-append share "/devkeys"))
                        #t))))
       #:test-target "runtests"))
    (native-inputs
     `(("pkg-config" ,pkg-config)

       ;; For tests.
       ("diffutils" ,diffutils)
       ("python@2" ,python-2)))
    (inputs
     `(("coreutils" ,coreutils)
       ("libyaml" ,libyaml)
       ("openssl" ,openssl)
       ("openssl:static" ,openssl "static")
       ("util-linux" ,util-linux "lib")))
    (home-page
     "https://dev.chromium.org/chromium-os/chromiumos-design-docs/verified-boot")
    (synopsis "ChromiumOS verified boot utilities")
    (description
     "vboot-utils is a collection of tools to facilitate booting of
Chrome-branded devices.  This includes the @command{cgpt} partitioning
program, the @command{futility} and @command{crossystem} firmware management
tools, and more.")
    (license license:bsd-3)))

(define-public os-prober
  (package
    (name "os-prober")
    (version "1.81")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://debian/pool/main/o/os-prober/os-prober_"
                           version ".tar.xz"))
       (sha256
        (base32 "10w8jz6mqhp0skdcam9mpgv79vx1sv7lkpra3rqjg0jkhvn2in9g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 regex)         ; for string-match
                  (srfi srfi-26))       ; for cut
       #:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (find-files ".")
               (("/usr") (assoc-ref outputs "out")))
             (substitute* (find-files "." "50mounted-tests$")
               (("mkdir") "mkdir -p"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (define (find-files-non-recursive directory)
               (find-files directory
                           (lambda (file stat)
                             (string-match (string-append "^" directory "/[^/]*$")
                                           file))
                           #:directories? #t))

             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (share (string-append out "/share")))
               (for-each (cut install-file <> bin)
                         (list "linux-boot-prober" "os-prober"))
               (install-file "newns" (string-append lib "/os-prober"))
               (install-file "common.sh" (string-append share "/os-prober"))
               (install-file "os-probes/mounted/powerpc/20macosx"
                             (string-append lib "/os-probes/mounted"))
               (for-each
                (lambda (directory)
                  (for-each
                   (lambda (file)
                     (let ((destination (string-append lib "/" directory
                                                       "/" (basename file))))
                       (mkdir-p (dirname destination))
                       (copy-recursively file destination)))
                   (append (find-files-non-recursive (string-append directory "/common"))
                           (find-files-non-recursive (string-append directory "/x86")))))
                (list "os-probes" "os-probes/mounted" "os-probes/init"
                      "linux-boot-probes" "linux-boot-probes/mounted"))))))))
    (home-page "https://joeyh.name/code/os-prober")
    (synopsis "Detect other operating systems")
    (description "os-prober probes disks on the system for other operating
systems so that they can be added to the bootloader.  It also works out how to
boot existing GNU/Linux systems and detects what distribution is installed in
order to add a suitable bootloader menu entry.")
    (license license:gpl2+)))

(define-public ipxe
  ;; XXX: 'BUILD_TIMESTAMP' is used to automatically select the newest version
  ;; of iPXE if multiple iPXE drivers are loaded concurrently in a UEFI system.
  ;;
  ;; TODO: Bump this timestamp at each modifications of the package (not only
  ;; for updates) by running: date +%s.
  (let ((timestamp "1591706427"))
    (package
      (name "ipxe")
      (version "1.21.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ipxe/ipxe")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (patches (search-patches "ipxe-reproducible-geniso.patch"))
                (sha256
                 (base32
                  "1pkf1n1c0rdlzfls8fvjvi1sd9xjd9ijqlyz3wigr70ijcv6x8i9"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build utils)
                    (guix build gnu-build-system)
                    (guix base32)
                    (ice-9 string-fun)
                    (ice-9 regex)
                    (rnrs bytevectors))
         #:imported-modules ((guix base32)
                             ,@%gnu-build-system-modules)
         #:make-flags
         ;; XXX: 'BUILD_ID' is used to determine when another ROM in the
         ;; system contains identical code in order to save space within the
         ;; legacy BIOS option ROM area, which is extremely limited in size.
         ;; It is supposed to be collision-free across all ROMs, to do so we
         ;; use the truncated output hash of the package.
         (let ((build-id
                (lambda (out)
                  (let* ((nix-store (string-append
                                     (or (getenv "NIX_STORE") "/gnu/store")
                                     "/"))
                         (filename
                          (string-replace-substring out nix-store ""))
                         (hash (match:substring (string-match "[0-9a-z]{32}"
                                                              filename)))
                         (bv (nix-base32-string->bytevector hash)))
                    (format #f "0x~x"
                            (bytevector-u32-ref bv 0 (endianness big))))))
               (out (assoc-ref %outputs "out"))
               (syslinux (assoc-ref %build-inputs "syslinux")))
           (list "ECHO_E_BIN_ECHO=echo"
                 "ECHO_E_BIN_ECHO_E=echo -e"

                 ;; cdrtools' mkisofs will silently ignore a missing isolinux.bin!
                 ;; Luckily xorriso is more strict.
                 (string-append "ISOLINUX_BIN=" syslinux
                                "/share/syslinux/isolinux.bin")
                 (string-append "SYSLINUX_MBR_DISK_PATH=" syslinux
                                "/share/syslinux/isohdpfx.bin")

                 ;; Build reproducibly.
                 (string-append "BUILD_ID_CMD=echo -n " (build-id out))
                 (string-append "BUILD_TIMESTAMP=" ,timestamp)
                 "everything"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-source-directory
             (lambda _ (chdir "src") #t))
           (add-after 'enter-source-directory 'set-options
             (lambda _
               (substitute* "config/general.h"
                 (("^//(#define PING_CMD.*)" _ uncommented) uncommented)
                 (("^//(#define IMAGE_TRUST_CMD.*)" _ uncommented)
                  uncommented)
                 (("^#undef.*(DOWNLOAD_PROTO_HTTPS.*)" _ option)
                  (string-append "#define " option))
                 (("^#undef.*(DOWNLOAD_PROTO_NFS.*)" _ option)
                  (string-append "#define " option)))
               #t))
           (delete 'configure)          ; no configure script
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (ipxe (string-append out "/lib/ipxe"))
                      (exts-re
                       "\\.(efi|efirom|iso|kkpxe|kpxe|lkrn|mrom|pxe|rom|usb)$")
                      (dirs '("bin" "bin-i386-linux" "bin-x86_64-pcbios"
                              "bin-x86_64-efi" "bin-x86_64-linux" "bin-i386-efi"))
                      (files (apply append
                                    (map (lambda (dir)
                                           (find-files dir exts-re)) dirs))))
                 (for-each (lambda (file)
                             (let* ((subdir (dirname file))
                                    (fn (basename file))
                                    (tgtsubdir (cond
                                                ((string=? "bin" subdir) "")
                                                ((string-prefix? "bin-" subdir)
                                                 (string-drop subdir 4)))))
                               (install-file file
                                             (string-append ipxe "/" tgtsubdir))))
                           files))
               #t))
           (add-after 'install 'leave-source-directory
             (lambda _ (chdir "..") #t)))
         #:tests? #f))                  ; no test suite
      (native-inputs
       (list perl syslinux xorriso))
      (home-page "https://ipxe.org")
      (synopsis "PXE-compliant network boot firmware")
      (description "iPXE is a network boot firmware.  It provides a full PXE
implementation enhanced with additional features such as booting from: a web
server via HTTP, an iSCSI SAN, a Fibre Channel SAN via FCoE, an AoE SAN, a
wireless network, a wide-area network, an Infiniband network.  It
controls the boot process with a script.  You can use iPXE to replace the
existing PXE ROM on your network card, or you can chainload into iPXE to obtain
the features of iPXE without the hassle of reflashing.")
      (license license:gpl2+))))

