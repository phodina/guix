;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Stefan <stefan-guix@vodafonemail.de>
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

(define-module (guix build kconfig)
  #:use-module  (ice-9 rdelim)
  #:use-module  (ice-9 regex)
  #:use-module  (srfi srfi-1)
  #:use-module  (srfi srfi-26)
  #:export (modify-defconfig))

;; Commentary:
;;
;; Builder-side code to modify configurations for the Kconfig build system as
;; used by Linux and U-Boot.
;;
;; Code:

(define (modify-defconfig defconfig configs)
  "This function can modify a given DEFCONFIG file by adding, changing or
removing the list of strings in CONFIGS.  This allows an easy customization of
Kconfig based projects like the kernel Linux or the bootloader 'Das U-Boot'.

These are examples for CONFIGS to add or change or remove
configurations to/from DEFCONFIG:

'(\"CONFIG_A=\\\"a\\\"\"
  \"CONFIG_B=0\"
  \"CONFIG_C=y\"
  \"CONFIG_D=m\"
  \"CONFIG_E=\"
  \"CONFIG_F\"
  \"# CONFIG_G is not set\")

Instead of a list, CONFGIS can be a string with one configuration per line."
  (define config-rx
    (make-regexp
     ;; (match:substring (string-match "=(.*)" "=") 1) returns "", but the
     ;; pattern "=(.+)?" makes it return #f instead.  For a "CONFIG_A=" we like
     ;; to get #f, which as a value emits "# … is not set".
     "^(#[\\t ]*)?(CONFIG_[A-Z0-9_]+)([\\t ]*=[\\t ]*(.+)?|([\\t ]+is[\\t ]+not[\\t ]+set))?$"))

  (define (config-string->pair config-string)
    "Parse a config-string like \"CONFIG_EXAMPLE=y\" into a key-value pair.
Spaces get trimmed.
\"CONFIG_A=y\"            -> '(\"CONFIG_A\" . \"y\")
\"CONFIG_B=\\\"\\\"\"         -> '(\"CONFIG_B\" . \"\\\"\\\"\")
\"CONFIG_C=\"             -> '(\"CONFIG_C\" . #f)
\"CONFIG_D\"              -> '(\"CONFIG_D\" . #f)
\"# CONFIG_E is not set\" -> '(\"CONFIG_E\" . #f)
\"# Anything else\"       -> '(\"# Anything else\" . \"\")"
    (let ((match (regexp-exec config-rx (string-trim-both config-string))))
      (if (not match)
          ;; This is some unparsable config-string.
          ;; We keep it as it is.
          (cons config-string "")
          (let* ((comment (match:substring match 1))
                 (key (match:substring match 2))
                 (unset (match:substring match 5))
                 (value (and (not comment)
                             (not unset)
                             (match:substring match 4))))
            (if (or (and comment (not unset))
                    (and (not comment) unset))
                ;; This is just some comment or strange line, which we keep as is.
                (cons config-string "")
                (cons key value))))))

  (define (pair->config-string pair)
    "Convert a PAIR back to a config-string."
    (let* ((key (car pair))
           (value (cdr pair)))
      (if (string? value)
          (if (string-null? value)
              key
              (string-append key "=" value))
          (string-append "# " key " is not set"))))

  (define (remove-pair pair blacklist)
    "Turn a key-value PAIR into '("" . ""), if its key is listed in BLACKLIST."
    (let* ((key (first pair)))
      (if (member key blacklist)
          '("" . "")
          pair)))

  (define (remove-config-string config-string blacklist)
    "Remove the CONFIG-STRING, if its key is listed in BLACKLIST."
    (pair->config-string (remove-pair (config-string->pair config-string)
                                       blacklist)))

  (define* (write-lines input #:key (line-modifier identity))
    "Write all lines from the INPUT after applying the LINE-MODIFIER to the
 current-output-port."
    (let loop ((line (read-line input)))
      (when (not (eof-object? line))
        (display (line-modifier line))
        (newline)
        (loop (read-line input)))))

  (let* ((modified-defconfig (string-append defconfig ".mod"))
         ;; Split the configs into a list of single configuations.
         ;; To minimize mistakes, we support a string and a list of strings,
         ;; each with newlines to separate configurations.
         (config-list (fold-right append '()
                                  (map (lambda (s)
                                         (string-split s #\newline))
                                       (if (string? configs)
                                           (list configs)
                                           configs))))
         ;; Generate key-value pairs from the config-list.
         (pairs (map (lambda (config-string)
                       (config-string->pair config-string))
                     config-list))
         ;; Generate a blacklist of config keys from pairs.
         (blacklist (map (lambda (config-pair)
                           (first config-pair))
                         pairs))
         (remove-config-string (cut remove-config-string <> blacklist)))
    ;; Write to the modified-defconfig file first the content of the defconfig
    ;; file with removed lines, and afterwards the configs.
    (call-with-output-file modified-defconfig
      (lambda (output)
        (with-output-to-port output
          (lambda ()
            (call-with-input-file defconfig
              (lambda (input)
                (write-lines input #: line-modifier remove-config-string)))
            (call-with-input-string
              (string-join (map pair->config-string pairs) "\n")
              (lambda (input)
                (write-lines input)))))))
    ;; Ensure the modified-defconfig file is used.
    (delete-file defconfig)
    (rename-file modified-defconfig defconfig)))
