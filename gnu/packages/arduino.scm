;;; GNU Guix --- Functional package management for GNU
;;; Copyright Â© 2016 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu packages arduino)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages avr)
  #:use-module (gnu packages flashing-tools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define (arduino-installer filename)
  `(lambda* (#:key outputs #:allow-other-keys)
    (let* ((out (assoc-ref outputs "out"))
           (out-share (string-append out "/share/arduino"))
           (out-share-part (string-append out-share "/" ,filename)))
     (mkdir-p out-share)
     (copy-recursively ,filename out-share-part))
    #t))

(define-public arduino-hardware
  (package
    (name "arduino-hardware")
    (version "1.8.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/arduino/Arduino")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y0fid2sci8npc8x8c2i0yw0s8xxl7w6pb4hc8m1vyibp38b5pz9"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled jar archives.
                  (for-each delete-file (find-files "." "\\.jar$"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No tests exist
       #:modules ((guix build utils) (ice-9 match)
                  (guix build gnu-build-system))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-after 'unpack 'prepare-dependencies
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; This is intended to just prepare arduino inputs where its
             ;; build system expects them.
             (for-each (match-lambda
                         ((key . value)
                          (let* ((out-dir (assoc-ref outputs "out"))
                                 (destination-dir
                                  (string-append out-dir "/share/arduino")))
                            (if (string-prefix? "arduino-" key)
                                (let* ((basename
                                        (string-drop key
                                                     (string-length "arduino-")))
                                       (target-outdir (assoc-ref inputs key))
                                       (target-dir
                                        (string-append target-outdir
                                                       "/share/arduino"))
                                       (target (string-append target-dir
                                                              "/" basename))
                                       (destination
                                        (string-append destination-dir "/"
                                                       basename)))
                                  (mkdir-p destination-dir)
                                  (symlink target destination)
                                  #t)))))
                       inputs)
             #t))
         (add-before 'build 'chdir
           (lambda _
             (chdir "hardware")
             #t))
         (replace 'install ,(arduino-installer "hardware")))))
    (home-page "https://www.arduino.cc/")
    (synopsis "Arduino Hardware Spec Files")
    (description "@code{arduino-hardware} contains Arduino Hardware Spec Files
(boards.txt etc).")
    ;; GPL covers the main body ("app", "core").  LGPL covers the remainder.
    (license (list license:lgpl2.1+ license:gpl3+))))

(define-public arduino-libraries
  (package (inherit arduino-hardware)
    (name "arduino-libraries")
    (inputs
     `(("arduino-hardware" ,arduino-hardware)))
    (arguments
      (substitute-keyword-arguments
        (package-arguments arduino-hardware)
        ((#:phases phases)
            `(modify-phases ,phases
              (replace 'chdir
                (lambda _
				  ;; TODO: Libraries are no zipped in build directory
                  ;(chdir "libraries")
                  #t))
              (replace 'install ,(arduino-installer "libraries"))))))
    ;; Note: Some parts are BSD and ASL-2.0 licensed.
    (license (list license:lgpl2.1+ license:gpl3+))))

(define-public arduino-makefile
  (package
    (name "arduino-makefile")
    (version "1.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sudar/Arduino-Makefile")
                    (commit version)))
              (sha256
               (base32
                "0flpl97d2231gp51n3y4qvf3y1l8xzafi1sgpwc305vwc2h4dl2x"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no tests exist
       #:phases
        (modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((avr-gcc (assoc-ref inputs "avr-toolchain")))
              (substitute* "bin/ard-reset-arduino"
                (("#!/usr/bin/env python") "#!/usr/bin/python3"))
              (substitute* "Arduino.mk"
                (("#    => ARDUINO_DIR.*")
                   (string-append "ARDUINO_DIR = "
                                  (assoc-ref %build-inputs "arduino-libraries")
                                  "/share/arduino\n"))
                ; ; defaults to "hardware/tools/avr"
                (("#    => AVR_TOOLS_DIR.*")
                   (string-append "AVR_TOOLS_DIR = "
                                  (assoc-ref %build-inputs "avrdude")
                                  "\n"))
                (("#    => ARDMK_DIR.*")
                   (string-append "ARDMK_DIR = "
                                  (assoc-ref %outputs "out")
                                  "/share/arduino\n"))
                (("CC_NAME[ ]*=.*")
                   (string-append "CC_NAME = " avr-gcc "/bin/avr-gcc\n"))
                (("CXX_NAME[ ]*=.*")
                   (string-append "CXX_NAME = " avr-gcc "/bin/avr-g++\n"))
                (("OBJCOPY_NAME[ ]*=.*")
                   (string-append "OBJCOPY_NAME = " avr-gcc "/bin/avr-objcopy\n"))
                (("OBJDUMP_NAME[ ]*=.*")
                   (string-append "OBJDUMP_NAME = " avr-gcc "/bin/avr-objdump\n"))
                (("AR_NAME[ ]*=.*")
                   (string-append "AR_NAME = " avr-gcc "/bin/avr-ar\n"))
                (("SIZE_NAME[ ]*=.*")
                   (string-append "SIZE_NAME = " avr-gcc "/bin/avr-size\n"))
                (("NM_NAME[ ]*=.*")
                   (string-append "NM_NAME = " avr-gcc "/bin/avr-nm\n"))))))
          (delete 'build)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (out-mk (string-append out "/share/arduino"))
                     (out-doc (string-append out "/share/doc"))
                     (out-bin (string-append out "/bin"))
                     (out-man (string-append out "/share/man/man1")))
                    (mkdir-p out-mk)
                    (for-each (lambda (name)
                                (copy-file name (string-append out-mk "/" name)))
                              '("Arduino.mk" "arduino-mk-vars.md"
                                "chipKIT.mk" "Common.mk"))
                    (mkdir-p out-doc)
                    (copy-recursively "examples" out-doc)
                    (mkdir-p out-bin)
                    (copy-file "bin/ard-reset-arduino"
                               (string-append out-bin "/ard-reset-arduino"))
                    (mkdir-p out-man)
                    (copy-file "ard-reset-arduino.1"
                               (string-append out-man "/ard-reset-arduino.1"))))))))
    (inputs
     `(("python" ,python)
       ("python-pyserial" ,python-pyserial)
       ("arduino-libraries" ,arduino-libraries)
       ("avrdude" ,avrdude)
       ("avr-toolchain" ,avr-toolchain)))
    (synopsis "Arduino Makefile Include Files")
    (description "Allows you to build Arduino sketches using a very tiny Makefile")
    (home-page "https://github.com/sudar/Arduino-Makefile")
    (license license:lgpl2.1)))
