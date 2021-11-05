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
  #:use-module (gnu packages python))

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
