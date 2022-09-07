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

(define-module (gnu services firmware)
  #:use-module (gnu packages firmware)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (fwupd-service-type fwupd-configuration fwupd-configuration?))

;;;
;;; Fwupd daemon.
;;;

(define-record-type* <fwupd-configuration> fwupd-configuration
                     make-fwupd-configuration
  fwupd-configuration?
  (fwupd fwupd-configuration-fwupd ;<package>
         (default fwupd)))

(define (fwupd-activation config)
  (with-imported-modules '((guix build utils))
                         #~(begin
                             (use-modules (guix build utils))
                             (mkdir-p "/var/cache/fwupd")
                             (mkdir-p "/var/lib/fwupd/local.d")
                             (mkdir-p "/var/lib/fwupd/"))))

(define (fwupd-shepherd-service config)
  "Return a shepherd service for fwupd"
  (and (fwupd-configuration? config)
       (let ((fwupd (fwupd-configuration-fwupd config)))
         (list (shepherd-service (documentation "Run firmware update daemon")
                                 (provision '(fwupd))
                                 (requirement '(dbus-system))
                                 (start #~(make-forkexec-constructor (list (string-append #$fwupd
                                                                            "/libexec/fwupd/fwupd")
                                                                      "--verbose")
                                           #:log-file "/var/log/fwupd.log"))
                                 (stop #~(make-kill-destructor)))))))

(define fwupd-service-type
  (let ((fwupd-package (compose list fwupd-configuration-fwupd)))
    (service-type (name 'fwupd)
                  (extensions (list (service-extension
                                     shepherd-root-service-type
                                     fwupd-shepherd-service)
                                    (service-extension polkit-service-type
                                                       fwupd-package)
                                    (service-extension dbus-root-service-type
                                                       fwupd-package)
                                    (service-extension activation-service-type
                                     fwupd-activation)))
                  (default-value (fwupd-configuration))
                  (description
                   "Run @command{fwupd}, the Firmware update daemon."))))
