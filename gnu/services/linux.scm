;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 raid5atemyhomework <raid5atemyhomework@protonmail.com>
;;; Copyright © 2021 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2022 Josselin Poiret <dev@jpoiret.xyz>
;;; Copyright © 2021-2022 Petr Hodina <phodina@protonmail.com>
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

(define-module (gnu services linux)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix i18n)
  #:use-module (guix ui)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (bolt-configuration
            bolt-configuration?
            bolt-service-type

            earlyoom-configuration
            earlyoom-configuration?
            earlyoom-configuration-earlyoom
            earlyoom-configuration-minimum-available-memory
            earlyoom-configuration-minimum-free-swap
            earlyoom-configuration-prefer-regexp
            earlyoom-configuration-avoid-regexp
            earlyoom-configuration-memory-report-interval
            earlyoom-configuration-ignore-positive-oom-score-adj?
            earlyoom-configuration-show-debug-messages?
            earlyoom-configuration-send-notification-command
            earlyoom-service-type

            kernel-module-loader-service-type

            rasdaemon-configuration
            rasdaemon-configuration?
            rasdaemon-configuration-record?
            rasdaemon-service-type

            zram-device-configuration
            zram-device-configuration?
            zram-device-configuration-size
            zram-device-configuration-compression-algorithm
            zram-device-configuration-memory-limit
            zram-device-configuration-priority
            zram-device-service-type))


;;;
;;; Thunderbolt daemon.
;;;

(define-record-type* <bolt-configuration>
  bolt-configuration make-bolt-configuration bolt-configuration?
  (package bolt-configuration-package ; package
           (default bolt)))

(define (bolt-shepherd-service config)
     (with-imported-modules (source-module-closure
                             '((gnu build shepherd)))
       (shepherd-service
        (documentation "Thunderbolt daemon")
        (provision '(thunderbolt))
        (requirement '(networking))
        (modules '((gnu build shepherd)))
        (start #~(make-forkexec-constructor/container
                  (list #$(file-append package "/libexec/boltd"))))
        (stop #~(make-kill-destructor)))))

(define %bolt-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/boltd")))

(define (bolt-dbus-service config)
  (list (wrapped-dbus-service (bolt-configuration-bolt config)
			      "libexec/boltd"
			      `(("BOLT_CONF_FILE_NAME"
				 '("share/dbus-1/interfaces/org.freedesktop.bolt.xml"))))))

;(define %bolt-accounts
; (list (user-group (name "boltd") (system? #t))
;       (user-account
;	 (name "boltd")
;	 (group "boltd")
;	 (system? #t)
;	 (comment "Boltd daemon user")
;	 (home-directory "/var/empty")
;	 (shell "/run/current-system/profile/sbin/nologin"))))

(define (bolt-udev-rule config)
  (file->udev-rule "90-bolt.rules" (file-append package "/lib/udev/rules.d/90-bolt.rules")))

(define bolt-service-type
  (service-type
   (name 'boltd)
   (description
    "Thunderbolt daemon")
   (extensions
    (list (service-extension udev-service-type
			     (compose list bolt-udev-rule))
	  (service-extension activation-service-type
			     (const %bolt-activation))
	  (service-extension dbus-root-service-type
	  (compose list bolt-configuration-package))
	;		     bolt-dbus-service)
	;  (service-extension account-service-type
	;		     (const %bolt-accounts))
          (service-extension shepherd-root-service-type
                             (compose list bolt-shepherd-service))))
   (default-value (bolt-configuration))))


;;;
;;; Early OOM daemon.
;;;

(define-record-type* <earlyoom-configuration>
  earlyoom-configuration make-earlyoom-configuration
  earlyoom-configuration?
  (earlyoom earlyoom-configuration-earlyoom
            (default earlyoom))
  (minimum-available-memory earlyoom-configuration-minimum-available-memory
                            (default 10)) ; in percent
  (minimum-free-swap earlyoom-configuration-minimum-free-swap
                     (default 10))      ; in percent
  (prefer-regexp earlyoom-configuration-prefer-regexp ; <string>
                 (default #f))
  (avoid-regexp earlyoom-configuration-avoid-regexp  ; <string>
                (default #f))
  (memory-report-interval earlyoom-configuration-memory-report-interval
                          (default 0)) ; in seconds; 0 means disabled
  (ignore-positive-oom-score-adj?
   earlyoom-configuration-ignore-positive-oom-score-adj? (default #f))
  (run-with-higher-priority? earlyoom-configuration-run-with-higher-priority?
                             (default #f))
  (show-debug-messages? earlyoom-configuration-show-debug-messages?
                        (default #f))
  (send-notification-command
   earlyoom-configuration-send-notification-command  ; <string>
   (default #f)))

(define (earlyoom-configuration->command-line-args config)
  "Translate a <earlyoom-configuration> object to its command line arguments
representation."
  (match config
    (($ <earlyoom-configuration> earlyoom minimum-available-memory
                                 minimum-free-swap prefer-regexp avoid-regexp
                                 memory-report-interval
                                 ignore-positive-oom-score-adj?
                                 run-with-higher-priority? show-debug-messages?
                                 send-notification-command)
     `(,(file-append earlyoom "/bin/earlyoom")
       ,@(if minimum-available-memory
             (list "-m" (format #f "~s" minimum-available-memory))
             '())
       ,@(if minimum-free-swap
             (list "-s" (format #f "~s" minimum-free-swap))
             '())
       ,@(if prefer-regexp
             (list "--prefer" prefer-regexp)
             '())
       ,@(if avoid-regexp
             (list "--avoid" avoid-regexp)
             '())
       "-r" ,(format #f "~s" memory-report-interval)
       ,@(if ignore-positive-oom-score-adj?
             (list "-i")
             '())
       ,@(if run-with-higher-priority?
             (list "-p")
             '())
       ,@(if show-debug-messages?
             (list "-d")
             '())
       ,@(if send-notification-command
             (list "-N" send-notification-command)
             '())))))

(define (earlyoom-shepherd-service config)
  (shepherd-service
   (documentation "Run the Early OOM daemon.")
   (provision '(earlyoom))
   (start #~(make-forkexec-constructor
             '#$(earlyoom-configuration->command-line-args config)
             #:log-file "/var/log/earlyoom.log"))
   (stop #~(make-kill-destructor))))

(define earlyoom-service-type
  (service-type
   (name 'earlyoom)
   (default-value (earlyoom-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list earlyoom-shepherd-service))))
   (description "Run @command{earlyoom}, the Early OOM daemon.")))


;;;
;;; Kernel module loader.
;;;

(define kernel-module-loader-shepherd-service
  (match-lambda
    ((and (? list? kernel-modules) ((? string?) ...))
     (shepherd-service
      (documentation "Load kernel modules.")
      (provision '(kernel-module-loader))
      (requirement '())
      (one-shot? #t)
      (modules `((srfi srfi-1)
                 (srfi srfi-34)
                 (srfi srfi-35)
                 (rnrs io ports)
                 ,@%default-modules))
      (start
       #~(lambda _
           (cond
            ((null? '#$kernel-modules) #t)
            ((file-exists? "/proc/sys/kernel/modprobe")
             (let ((modprobe (call-with-input-file
                                 "/proc/sys/kernel/modprobe" get-line)))
               (guard (c ((message-condition? c)
                          (format (current-error-port) "~a~%"
                                  (condition-message c))
                          #f))
                 (every (lambda (module)
                          (invoke/quiet modprobe "--" module))
                        '#$kernel-modules))))
            (else
             (format (current-error-port) "error: ~a~%"
                     "Kernel is missing loadable module support.")
             #f))))))))

(define kernel-module-loader-service-type
  (service-type
   (name 'kernel-module-loader)
   (description "Load kernel modules.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list kernel-module-loader-shepherd-service))))
   (compose concatenate)
   (extend append)
   (default-value '())))


;;;
;;; Reliability, Availability, and Serviceability (RAS) daemon
;;;

(define-record-type* <rasdaemon-configuration>
  rasdaemon-configuration make-rasdaemon-configuration
  rasdaemon-configuration?
  (record? rasdaemon-configuration-record? (default #f)))

(define (rasdaemon-configuration->command-line-args config)
  "Translate <rasdaemon-configuration> to its command line arguments
  representation"
  (let ((record? (rasdaemon-configuration-record? config)))
    `(,(file-append rasdaemon "/sbin/rasdaemon")
      "--foreground" ,@(if record? '("--record") '()))))

(define (rasdaemon-activation config)
  (let ((record? (rasdaemon-configuration-record? config))
        (rasdaemon-dir "/var/lib/rasdaemon"))
    (with-imported-modules '((guix build utils))
      #~(if #$record? (mkdir-p #$rasdaemon-dir)))))

(define (rasdaemon-shepherd-service config)
  (shepherd-service
   (documentation "Run rasdaemon")
   (provision '(rasdaemon))
   (requirement '(syslogd))
   (start #~(make-forkexec-constructor
             '#$(rasdaemon-configuration->command-line-args config)))
   (stop #~(make-kill-destructor))))

(define rasdaemon-service-type
  (service-type
   (name 'rasdaemon)
   (default-value (rasdaemon-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list rasdaemon-shepherd-service))
          (service-extension activation-service-type rasdaemon-activation)))
   (compose concatenate)
   (description "Run @command{rasdaemon}, the RAS monitor")))


;;;
;;; Kernel module loader.
;;;

(define-record-type* <zram-device-configuration>
  zram-device-configuration make-zram-device-configuration
  zram-device-configuration?
  (size                     zram-device-configuration-size
                            (default "1G"))     ; string or integer
  (compression-algorithm    zram-device-configuration-compression-algorithm
                            (default 'lzo))     ; symbol
  (memory-limit             zram-device-configuration-memory-limit
                            (default 0))        ; string or integer
  (priority                 zram-device-configuration-priority
                            (default #f)        ; integer | #f
                            (delayed) ; to avoid printing the deprecation
                                      ; warning multiple times
                            (sanitize warn-zram-priority-change)))

(define-with-syntax-properties
  (warn-zram-priority-change (priority properties))
  (if (eqv? priority -1)
      (begin
        (warning (source-properties->location properties)
                 (G_ "using -1 for zram priority is deprecated~%"))
        (display-hint (G_ "Use #f or leave as default instead (@pxref{Linux \
Services})."))
        #f)
      priority))

(define (zram-device-configuration->udev-string config)
  "Translate a <zram-device-configuration> into a string which can be
placed in a udev rules file."
  (match config
    (($ <zram-device-configuration> size compression-algorithm memory-limit priority)
     (string-append
       "KERNEL==\"zram0\", "
       "ATTR{comp_algorithm}=\"" (symbol->string compression-algorithm) "\" "
       (if (not (or (equal? "0" size)
                    (equal? 0 size)))
         (string-append "ATTR{disksize}=\"" (if (number? size)
                                              (number->string size)
                                              size)
                        "\" ")
         "")
       (if (not (or (equal? "0" memory-limit)
                    (equal? 0 memory-limit)))
         (string-append "ATTR{mem_limit}=\"" (if (number? memory-limit)
                                               (number->string memory-limit)
                                               memory-limit)
                        "\" ")
         "")
       "RUN+=\"/run/current-system/profile/sbin/mkswap /dev/zram0\" "
       "RUN+=\"/run/current-system/profile/sbin/swapon "
       ;; TODO: Revert to simply use 'priority' after removing the deprecation
       ;; warning and the delayed property of the field.
       (let ((priority* (force priority)))
         (if priority*
             (format #f "--priority ~a " priority*)
             ""))
       "/dev/zram0\"\n"))))

(define %zram-device-config
  `("modprobe.d/zram.conf"
    ,(plain-file "zram.conf"
                 "options zram num_devices=1")))

(define (zram-device-udev-rule config)
  (file->udev-rule "99-zram.rules"
                   (plain-file "99-zram.rules"
                               (zram-device-configuration->udev-string config))))

(define zram-device-service-type
  (service-type
    (name 'zram)
    (default-value (zram-device-configuration))
    (extensions
      (list (service-extension kernel-module-loader-service-type
                               (const (list "zram")))
            (service-extension etc-service-type
                               (const (list %zram-device-config)))
            (service-extension udev-service-type
                               (compose list zram-device-udev-rule))))
    (description "Creates a zram swap device.")))
