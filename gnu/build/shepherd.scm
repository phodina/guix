;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (gnu build shepherd)
  #:use-module (gnu system file-systems)
  #:use-module (gnu build linux-container)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  ;; XXX: Lazy-bind the Shepherd to avoid a compile-time dependency.
  #:autoload (shepherd service) (fork+exec-command
                                 read-pid-file
                                 exec-command
                                 %precious-signals)
  #:autoload (shepherd system) (unblock-signals)
  #:export (make-forkexec-constructor/container
            fork+exec-command/container))

;;; Commentary:
;;;
;;; This module provides extensions to the GNU Shepherd.  In particular, it
;;; provides a helper to start services in a container.
;;;
;;; Code:

(define (clean-up file)
  (when file
    (catch 'system-error
      (lambda ()
        (delete-file file))
      (lambda args
        (unless (= ENOENT (system-error-errno args))
          (apply throw args))))))

(define-syntax-rule (catch-system-error exp)
  (catch 'system-error
    (lambda ()
      exp)
    (const #f)))

(define (default-namespaces args)
  ;; Most daemons are here to talk to the network, and most of them expect to
  ;; run under a non-zero UID.
  (fold delq %namespaces '(net user)))

(define* (default-mounts #:key (namespaces (default-namespaces '())))
  (define (tmpfs directory)
    (file-system
      (device "none")
      (mount-point directory)
      (type "tmpfs")
      (check? #f)))

  (define accounts
    ;; This is for processes in the default user namespace but living in a
    ;; different mount namespace, so that they can lookup users.
    (list (file-system-mapping
           (source "/etc/passwd") (target source))
          (file-system-mapping
           (source "/etc/group") (target source))))

  (append (cons (tmpfs "/tmp") %container-file-systems)
          (let ((mappings `(,@(if (memq 'net namespaces)
                                  '()
                                  %network-file-mappings)
                            ,@(if (and (memq 'mnt namespaces)
                                       (not (memq 'user namespaces)))
                                  accounts
                                  '())

                            ;; Tell the process what timezone we're in.  This
                            ;; makes sure that, for instance, its syslog
                            ;; messages have the correct timestamp.
                            ,(file-system-mapping
                              (source "/etc/localtime")
                              (target source))

                            ,%store-mapping)))    ;XXX: coarse-grain
            (map file-system-mapping->bind-mount
                 (filter (lambda (mapping)
                           (file-exists? (file-system-mapping-source mapping)))
                         mappings)))))

(define* (read-pid-file/container pid pid-file #:key (max-delay 5))
  "Read PID-FILE in the container namespaces of PID, which exists in a
separate mount and PID name space.  Return the \"outer\" PID. "
  (match (container-excursion* pid
           (lambda ()
             (read-pid-file pid-file
                            #:max-delay max-delay)))
    (#f
     ;; Send SIGTERM to the whole process group.
     (catch-system-error (kill (- pid) SIGTERM))
     #f)
    ((? integer? container-pid)
     ;; XXX: When COMMAND is started in a separate PID namespace, its
     ;; PID is always 1, but that's not what Shepherd needs to know.
     pid)))

(define* (make-forkexec-constructor/container command
                                              #:key
                                              (namespaces
                                               (default-namespaces args))
                                              (mappings '())
                                              (user #f)
                                              (group #f)
                                              (log-file #f)
                                              pid-file
                                              (pid-file-timeout 5)
                                              (directory "/")
                                              (environment-variables
                                               (environ))
                                              #:rest args)
  "This is a variant of 'make-forkexec-constructor' that starts COMMAND in
NAMESPACES, a list of Linux namespaces such as '(mnt ipc).  MAPPINGS is the
list of <file-system-mapping> to make in the case of a separate mount
namespace, in addition to essential bind-mounts such /proc."
  (define container-directory
    (match command
      ((program _  ...)
       (string-append "/var/run/containers/" (basename program)))))

  (define auto-mappings
    `(,@(if log-file
            (list (file-system-mapping
                   (source log-file)
                   (target source)
                   (writable? #t)))
            '())))

  (define mounts
    (append (map file-system-mapping->bind-mount
                 (append auto-mappings mappings))
            (default-mounts #:namespaces namespaces)))

  (lambda args
    (mkdir-p container-directory)

    (when log-file
      ;; Create LOG-FILE so we can map it in the container.
      (unless (file-exists? log-file)
        (call-with-output-file log-file (const #t))
        (when user
          (let ((pw (getpwnam user)))
            (chown log-file (passwd:uid pw) (passwd:gid pw))))))

    (let ((pid (run-container container-directory
                              mounts namespaces 1
                              (lambda ()
                                ;; First restore the default handlers.
                                (for-each (cut sigaction <> SIG_DFL)
                                          %precious-signals)

                                ;; Unblock any signals that have been blocked
                                ;; by the parent process.
                                (unblock-signals %precious-signals)

                                (mkdir-p "/var/run")
                                (clean-up pid-file)

                                (exec-command command
                                              #:user user
                                              #:group group
                                              #:log-file log-file
                                              #:directory directory
                                              #:environment-variables
                                              environment-variables)))))
      (if pid-file
          (if (or (memq 'mnt namespaces) (memq 'pid namespaces))
              (read-pid-file/container pid pid-file
                                       #:max-delay pid-file-timeout)
              (read-pid-file pid-file #:max-delay pid-file-timeout))
          pid))))

(define* (fork+exec-command/container command
                                      #:key pid
                                      #:allow-other-keys
                                      #:rest args)
  "This is a variant of 'fork+exec-command' procedure, that joins the
namespaces of process PID beforehand.  If there is no support for containers,
on Hurd systems for instance, fallback to direct forking."
  (define (strip-pid args)
    ;; TODO: Replace with 'strip-keyword-arguments' when that no longer pulls
    ;; in (guix config).
    (let loop ((args args)
               (result '()))
      (match args
        (()
         (reverse result))
        ((#:pid _ . rest)
         (loop rest result))
        ((head . rest)
         (loop rest (cons head result))))))

  (let ((container-support?
         (file-exists? "/proc/self/ns"))
        (fork-proc (lambda ()
                     (apply fork+exec-command command
                            (strip-pid args)))))
    (if container-support?
        (container-excursion* pid fork-proc)
        (fork-proc))))

;; Local Variables:
;; eval: (put 'container-excursion* 'scheme-indent-function 1)
;; End:

;;; shepherd.scm ends here
