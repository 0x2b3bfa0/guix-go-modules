;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu services cuirass)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ci)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu system shadow)
  #:export (<cuirass-configuration>
            cuirass-configuration
            cuirass-configuration?

            cuirass-service-type))

;;;; Commentary:
;;;
;;; This module implements a service that to run instances of Cuirass, a
;;; continuous integration tool.
;;;
;;;; Code:

(define-record-type* <cuirass-configuration>
  cuirass-configuration make-cuirass-configuration
  cuirass-configuration?
  (cuirass          cuirass-configuration-cuirass ;package
                    (default cuirass))
  (log-file         cuirass-configuration-log-file ;string
                    (default "/var/log/cuirass.log"))
  (web-log-file     cuirass-configuration-web-log-file ;string
                    (default "/var/log/cuirass-web.log"))
  (queries-log-file cuirass-configuration-queries-log-file ;string
                    (default #f))
  (web-queries-log-file
                    cuirass-configuration-web-queries-log-file ;string
                    (default #f))
  (cache-directory  cuirass-configuration-cache-directory ;string (dir-name)
                    (default "/var/cache/cuirass"))
  (ttl              cuirass-configuration-ttl     ;integer
                    (default (* 30 24 3600)))
  (user             cuirass-configuration-user ;string
                    (default "cuirass"))
  (group            cuirass-configuration-group ;string
                    (default "cuirass"))
  (interval         cuirass-configuration-interval ;integer (seconds)
                    (default 60))
  (database         cuirass-configuration-database ;string (file-name)
                    (default "/var/lib/cuirass/cuirass.db"))
  (port             cuirass-configuration-port ;integer (port)
                    (default 8081))
  (host             cuirass-configuration-host ;string
                    (default "localhost"))
  (specifications   cuirass-configuration-specifications)
                                  ;gexp that evaluates to specification-alist
  (use-substitutes? cuirass-configuration-use-substitutes? ;boolean
                    (default #f))
  (one-shot?        cuirass-configuration-one-shot? ;boolean
                    (default #f))
  (fallback?        cuirass-configuration-fallback? ;boolean
                    (default #f))
  (extra-options    cuirass-configuration-extra-options
                    (default '())))

(define (cuirass-shepherd-service config)
  "Return a <shepherd-service> for the Cuirass service with CONFIG."
  (let ((cuirass          (cuirass-configuration-cuirass config))
        (cache-directory  (cuirass-configuration-cache-directory config))
        (web-log-file     (cuirass-configuration-web-log-file config))
        (log-file         (cuirass-configuration-log-file config))
        (queries-log-file (cuirass-configuration-queries-log-file config))
        (web-queries-log-file
                          (cuirass-configuration-web-queries-log-file config))
        (user             (cuirass-configuration-user config))
        (group            (cuirass-configuration-group config))
        (interval         (cuirass-configuration-interval config))
        (database         (cuirass-configuration-database config))
        (ttl              (cuirass-configuration-ttl config))
        (port             (cuirass-configuration-port config))
        (host             (cuirass-configuration-host config))
        (specs            (cuirass-configuration-specifications config))
        (use-substitutes? (cuirass-configuration-use-substitutes? config))
        (one-shot?        (cuirass-configuration-one-shot? config))
        (fallback?        (cuirass-configuration-fallback? config))
        (extra-options    (cuirass-configuration-extra-options config)))
    (list (shepherd-service
           (documentation "Run Cuirass.")
           (provision '(cuirass))
           (requirement '(guix-daemon networking))
           (start #~(make-forkexec-constructor
                     (list (string-append #$cuirass "/bin/cuirass")
                           "--cache-directory" #$cache-directory
                           "--specifications"
                           #$(scheme-file "cuirass-specs.scm" specs)
                           "--database" #$database
                           "--ttl" #$(string-append (number->string ttl) "s")
                           "--interval" #$(number->string interval)
                           #$@(if queries-log-file
                                  (list (string-append "--log-queries="
                                                       queries-log-file))
                                  '())
                           #$@(if use-substitutes? '("--use-substitutes") '())
                           #$@(if one-shot? '("--one-shot") '())
                           #$@(if fallback? '("--fallback") '())
                           #$@extra-options)

                     #:environment-variables
                     (list "GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt"
                           (string-append "GIT_EXEC_PATH=" #$git
                                          "/libexec/git-core"))

                     #:user #$user
                     #:group #$group
                     #:log-file #$log-file))
           (stop #~(make-kill-destructor)))
          (shepherd-service
           (documentation "Run Cuirass web interface.")
           (provision '(cuirass-web))
           (requirement '(guix-daemon networking))
           (start #~(make-forkexec-constructor
                     (list (string-append #$cuirass "/bin/cuirass")
                           "--cache-directory" #$cache-directory
                           "--specifications"
                           #$(scheme-file "cuirass-specs.scm" specs)
                           "--database" #$database
                           "--ttl" #$(string-append (number->string ttl) "s")
                           "--web"
                           "--port" #$(number->string port)
                           "--listen" #$host
                           "--interval" #$(number->string interval)
                           #$@(if web-queries-log-file
                                  (list (string-append "--log-queries="
                                                       web-queries-log-file))
                                  '())
                           #$@(if use-substitutes? '("--use-substitutes") '())
                           #$@(if fallback? '("--fallback") '())
                           #$@extra-options)

                     #:user #$user
                     #:group #$group
                     #:log-file #$web-log-file))
           (stop #~(make-kill-destructor))))))

(define (cuirass-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((cuirass-user  (cuirass-configuration-user config))
        (cuirass-group (cuirass-configuration-group config)))
    (list (user-group
           (name cuirass-group)
           (system? #t))
          (user-account
           (name cuirass-user)
           (group cuirass-group)
           (system? #t)
           (comment "Cuirass privilege separation user")
           (home-directory (string-append "/var/lib/" cuirass-user))
           (shell (file-append shadow "/sbin/nologin"))))))

(define (cuirass-activation config)
  "Return the activation code for CONFIG."
  (let ((cache (cuirass-configuration-cache-directory config))
        (db    (dirname (cuirass-configuration-database config)))
        (user  (cuirass-configuration-user config))
        (log   "/var/log/cuirass")
        (queries-log-file (cuirass-configuration-queries-log-file config))
        (web-queries-log-file
         (cuirass-configuration-web-queries-log-file config))
        (group (cuirass-configuration-group config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))

          (mkdir-p #$cache)
          (mkdir-p #$db)
          (mkdir-p #$log)

          (let ((uid (passwd:uid (getpw #$user)))
                (gid (group:gid (getgr #$group))))
            (chown #$cache uid gid)
            (chown #$db uid gid)
            (chown #$log uid gid)

            (let ((queries-log-file #$queries-log-file))
              (when queries-log-file
                (call-with-output-file queries-log-file (const #t))
                (chown #$queries-log-file uid gid)))

            (let ((web-queries-log-file #$web-queries-log-file))
              (when web-queries-log-file
                (call-with-output-file web-queries-log-file (const #t))
                (chown web-queries-log-file uid gid))))))))

(define (cuirass-log-rotations config)
  "Return the list of log rotations that corresponds to CONFIG."
  (let ((queries-log-file (cuirass-configuration-queries-log-file config))
        (web-queries-log-file
         (cuirass-configuration-web-queries-log-file config)))
    (list (log-rotation
           (files `(,(cuirass-configuration-log-file config)
                    ,@(if queries-log-file
                          (list queries-log-file)
                          '())
                    ,@(if web-queries-log-file
                          (list web-queries-log-file)
                          '())))
           (frequency 'weekly)
           (options '("rotate 40"))))))              ;worth keeping

(define cuirass-service-type
  (service-type
   (name 'cuirass)
   (extensions
    (list
     (service-extension profile-service-type      ;for 'info cuirass'
                        (compose list cuirass-configuration-cuirass))
     (service-extension rottlog-service-type cuirass-log-rotations)
     (service-extension activation-service-type cuirass-activation)
     (service-extension shepherd-root-service-type cuirass-shepherd-service)
     (service-extension account-service-type cuirass-account)))
   (description
    "Run the Cuirass continuous integration service.")))

