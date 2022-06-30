;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu system images pinenote)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu packages linux)
  #:use-module (guix platforms arm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (srfi srfi-26)
  #:export (pinenote-barebones-os
            pinenote-image-type
            pinenote-raw-image))

(define pinenote-barebones-os
  (operating-system
    (host-name "eink")
    (timezone "Europe/Prague")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-pinebook-pro-rk3399-bootloader)
;                 (bootloader u-boot-pinenote-rk3568-bootloader)
                 (targets '("/dev/vda"))))
    (initrd-modules '(%base-initrd-modules))
    (kernel linux-libre-arm64-pinenote)
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (services (cons (service agetty-service-type
                             (agetty-configuration
                              (extra-options '("-L")) ; no carrier detect
                              (baud-rate "1500000")
                              (term "vt100")
                              (tty "ttyS2")))
                    %base-services))))

(define pinenote-image-type
  (image-type
   (name 'pinenote-pro-raw)
   (constructor (cut image-with-os
                     (raw-with-offset-disk-image (* 9 (expt 2 20))) ;9MiB
                     <>))))

(define pinenote-barebones-raw-image
  (image
   (inherit
    (os+platform->image pinenote-barebones-os aarch64-linux
                        #:type pinenote-image-type))
   (name 'pinenote-barebones-raw-image)))

;; Return the default image.
pinenote-barebones-raw-image
