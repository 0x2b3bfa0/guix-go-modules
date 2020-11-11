;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Helio Machado <0x2b3bfa0+guix@googlemail.com>
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

(define-module (guix import go-modules)
  #:use-module (guix base32)
  #:use-module (guix build-system go)
  #:use-module ((guix download) #:prefix download:)
  #:use-module (gcrypt hash)
  #:use-module (guix http-client)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix memoization)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (go-module->guix-package
            go-module-recursive-import))


;;;
;;; Interface to https://pkg.go.dev and https://proxy.golang.org or whatever
;;; value is being assigned to %go-proxy-url.
;;;

;; Go modules and packages are an utter chaos with no centralized registry.
;; In order to gater minimal amount of useful metadata, we need to scrape
;; two web pages and a _sui generis_ @file{go.mod} format. Go Modules are
;; the idiomatic term to name a concept that is similar to Guix packages,
;; and each of these modules may contain one or more Go Packages, that are
;; a totally different beast.
(define-json-mapping <go-module> make-go-module go-module?
  json->go-module <=> go-module->json
  (name          go-module-name)           ;string
  (home-page     go-module-home-page)      ;string
  (licenses      go-module-licenses)       ;list of string
  (description   go-module-description)    ;string
  (version       go-module-version)        ;string
  (dependencies  go-module-dependencies))  ;list of <go-module-dependency>

;; Go module information structure, extracted from the @file{.info} endpoint of
;; the used proxy server. Only contains the queried/latest version of a given
;; package and the alleged timestamp for its release date.
(define-json-mapping <go-module-information> make-go-module-information
  go-module-information?
  json->go-module-information <=> go-module-information->json
  (version   go-module-information-version "Version")  ; string
  (time   go-module-information-time "Time"))          ; string

;; Go module dependency, extracted from the @file{.mod} endpoint of the used
;; proxy server. Contains a single dependency record, including its name, its
;; version and if it's an indirect/transitive dependency.
(define-json-mapping <go-module-dependency> make-go-module-dependency
  go-module-dependency?
  json->go-module-dependency <=> go-module-dependency->json
  (name      go-module-dependency-name)        ; string
  (version   go-module-dependency-version)     ; string
  (indirect   go-module-dependency-indirect))  ; boolean

;; Retrieve a license list, scraped from the pkg.go.dev web page. Apparently
;; a single module may contain various packages, and each package may have
;; many licenses, so we're returning a list with every license for the given
;; package. Passing a module name instead of a package name should also work
;; correctly in most situations, though it might not be the orthodox approach.
;; FIXME: use a proper HTML parser.
(define go-package-licenses
  (memoize
   (lambda* (name)
     (receive (response body)
         (http-get (string-append "https://pkg.go.dev" "/" name "?tab=licenses"))
       (if (eq? (response-code response) 200)
           (let ((licenses (list-matches
                            "<div id=\"#lic-[0-9]+\">([^<]+)</div>"
                            body)))
             (if licenses
                 (map (lambda* (item) (match:substring item 1)) licenses)
                 #f))
           #f)))))

;; Retrieve a short description for a given package, scraping the documentation
;; from pkg.go.dev and extracting the first paragraph. As previously stated,
;; that web site is a register for packages and not for modules, though passing
;; a module name should work as well. FIXME: use a proper HTML parser.
(define go-package-description
  (memoize
   (lambda* (name)
     (receive (response body)
         (http-get (string-append "https://pkg.go.dev/" name))
       (if (eq? (response-code response) 200)
           (let ((overview (string-match
                            (string-append ".*<\\s*h2[^>]*id\\s*=\\s*[\"']pkg-overview"
                                           "[\"'][^>]*>\\s*Overview\\s*<\\s*a\\s*href"
                                           "\\s*=\\s*[\"']#pkg-overview[\"']\\s*>.<\\s*"
                                           "/\\s*a\\s*>\\s*<\\s*/\\s**h2\\s*>\\s*<\\s*p"
                                           "\\s*>([^<]+)<\\s*/\\s*p\\s*>.*")
                            body)))
             (if overview
                 (string-trim-both (match:substring overview 1))
                 (go-package-readme-excerpt name)))
           #f)))))

;; Retrieve a short excerpt for a given package, scraping the readme
;; from pkg.go.dev and extracting the first paragraph. As previously stated,
;; that web site is a register for packages and not for modules, though passing
;; a module name should work as well. FIXME: use a proper HTML parser.
(define go-package-readme-excerpt
  (memoize
   (lambda* (name)
     (receive (response body)
         (http-get (string-append "https://pkg.go.dev/" name "?tab=overview"))
       (if (eq? (response-code response) 200)
           (let* (
                  (overview-plain
                   (string-match
                    (string-append ".*<\\s*pre[^>]*class\\s*=\\s*[\"']readme"
                                   "[\"'][^>]*>([^<]+)<\\s*/\\s*pre\\s*>.*") body))
                  (overview-markup
                   (string-match
                    (string-append ".*<\\s*div[^>]*class\\s*=\\s*[\"']"
                                   "Overview-readmeContent"
                                   "[\"'][^>]*>(.+)<\\s*/\\s*footer\\s*>.*")
                    body))
                  ;; FIXME: pkg.go.dev doesn't close some tags
                  (excerpt-plain-paragraphs
                   (if overview-plain
                       (list-matches
                        "\n?([^\n]{30,}(\n[^\n]+)*)\n\n"
                        (match:substring overview-plain 1))
                       #f))
                  (excerpt-markup-paragraphs
                   (if overview-markup
                       (list-matches
                        "<\\s*p\\s*>([^<]{10,}\\w+[^<]{10,})<\\s*/?\\s*p\\s*>"
                        (regexp-substitute/global #f
                                                  "<\\s*/?\\s*[^/p>]+\\s*[^>]*>"
                                                  (match:substring overview-markup 1)
                                                  'pre "" 'post))
                       #f))
                  (excerpt-markup-headings
                   (if overview-markup
                       (list-matches
                        "<\\s*h[0-9]+\\s*[^>]*>([^<]+)"
                        (regexp-substitute/global #f
                                                  "<\\s*/?\\s*[^/h>][^0-9>]*\\s*[^>]*>"
                                                  (match:substring overview-markup 1)
                                                  'pre "" 'post))
                       #f))
                  (excerpt-parts
                   (or excerpt-plain-paragraphs
                       excerpt-markup-paragraphs
                       excerpt-markup-headings)))
             (if (and excerpt-parts (> (length excerpt-parts) 0) )
                 (string-trim-both (match:substring (first excerpt-parts) 1))
                 ""))
           #f)))))

;; Retrieve basic version information from the module proxy, including the
;; latest version (if unspecified) and the alleged release date for that
;; version. Please refer to go-module-version-string for more information.
(define go-module-information
  (memoize
   (lambda* (name #:optional version)
     (and=> (json-fetch
             (string-append (%go-proxy-url) "/" (go-path-escape name)
                            (if version (string-append "/@v/" version ".info") "/@latest")))
            json->go-module-information))))

;; Retrieve every possible version for a given module from the module proxy.
;; Versions are being returned **unordered** and may contain different
;; versioning styles for the same package.
(define go-module-versions
  (memoize
   (lambda* (name)
     (receive (response body)
         (http-get (string-append (%go-proxy-url) "/" (go-path-escape name) "/@v/list"))
       (if (eq? (response-code response) 200)
           (let ((versions (filter
                            (lambda* (item) (not (string-null? item)))
                            (string-split body #\newline))))
             (if (null? versions) `(,(go-module-version-string name)) versions))
           #f)))))

;; Retrieve the latest known version for the given module or the full version
;; number for any given version or commit hash.
(define go-module-version-string
  (memoize
   (lambda* (name #:optional version)
     (go-module-information-version (go-module-information name version)))))

;; Retrieve every module dependency, as extracted from the @file{.mod} endpoint
;; of the used proxy server. FIXME: regular expressions are a really poor way of
;; parsing data from another language. https://golang.org/ref/mod#go-mod-edit
(define go-module-requirements
  (memoize
   (lambda* (name #:optional version)
     (receive (response body)
         (http-get (string-append
                    (%go-proxy-url) "/" (go-path-escape name) "/@v/"
                    (go-module-version-string name version) ".mod"))
       (if (eq? (response-code response) 200)
           (let* ((require-block-match (string-match
                                        ".*require(\\s*\\((.+)\\)|\\s+([^()]+)\n).*"
                                        body))
                  (require-block (if require-block-match
                                     (or (match:substring require-block-match 2)
                                         (match:substring require-block-match 3))
                                     ""))
                  (require-items-matches (list-matches
                                          (make-regexp (string-append
                                                        "^\\s*[\"']?([^ \t\v\r\"']+)[\"']?\\s+[\"']?"
                                                        "([^ \t\v\r\"']+)[\"']?(\\s*//.*indirect.*)?$")
                                                       regexp/newline) require-block))
                  (require-items (map (lambda* (item)
                                        (make-go-module-dependency
                                         (match:substring item 1)
                                         (match:substring item 2)
                                         (not (eq? (match:substring item 3) #f))))
                                      require-items-matches)))
             require-items)
           #f)))))

;; Retrieve all the available information for a given Go module.
(define lookup-go-module
  (memoize
   (lambda* (name #:optional version)
     (make-go-module
      name
      (string-append "https://pkg.go.dev/" name)
      (go-package-licenses name)
      (go-package-description name)
      (go-module-version-string name version)
      (remove (lambda (dependency)
                (string=? (go-module-dependency-name dependency) "go"))
              (go-module-requirements name))))))

;;;
;;; Converting Go modules to Guix packages.
;;;

(define* (make-go-module-sexp #:key name version go-module-inputs
                              home-page synopsis description license
                              #:allow-other-keys)
  "Return the `package' s-expression for a Go module with the given NAME,
VERSION, MODULE-INPUTS, HOME-PAGE, SYNOPSIS, DESCRIPTION, and LICENSE."
  (let* ((port (http-fetch (go-proxy-uri name version)))
         (pkg `(package
                 (name ,(go-module-name+version->package-name+version name))
                 (version ,version)
                 (source (origin
                           (method url-fetch)
                           (uri (go-proxy-uri ,name version))
                           (file-name (string-append name "-" version ".zip"))
                           (sha256
                            (base32
                             ,(bytevector->nix-base32-string (port-sha256 port))))))
                 (build-system go-build-system)
                 ,@(maybe-arguments (append (list #:import-path (string-append name "@v" version) #:module? #t) (maybe-go-module-inputs (module-name+versions->package-inputs go-module-inputs))))
                 (home-page ,(match home-page
                               (() "")
                               (_ home-page)))
                 (synopsis ,synopsis)
                 (description ,(beautify-description description))
                 (license ,(match license
                             (() #f)
                             ((license) license)
                             (_ `(list ,@license)))))))
    (close-port port)
    pkg))

(define* (go-module->guix-package name #:optional version)
  "Fetch the metadata for the module NAME from and return the `package'
s-expression corresponding to that package, or #f on failure. When VERSION is
specified, attempt to fetch that version. Otherwise, fetch the latest version."
  (guard (c ((http-get-error? c) (format (current-error-port)
                                         "error: failed to retrieve package information from ~s: ~a (~s)~%"
                                         (uri->string (http-get-error-uri c))
                                         (http-get-error-code c)
                                         (http-get-error-reason c)) #f))
    (define module (lookup-go-module name version))
    (and module (values (make-go-module-sexp
                         #:name (go-module-name module)
                         #:version (go-module-version->package-version (go-module-version module))
                         #:go-module-inputs (sort (map
                                                   go-module-dependency->go-module-name+version
                                                   (go-module-dependencies module))
                                                  string-ci<?)
                         #:home-page (go-module-home-page module)
                         #:synopsis (go-module-description module)
                         #:description (go-module-description module)
                         #:license (and=> (go-module-licenses module)
                                          list->licenses))
                        (append (map go-module-dependency->go-module-name+version
                                     (go-module-dependencies module)))))))


(define* (go-module-recursive-import name #:optional version)
  "Given a module NAME and, optionally its VERSION, produce package definitions
for itself and every dependency, recursively."
  (recursive-import (if version (string-append name "@" version) name) #f
                    #:repo->guix-package
                    (lambda (name repo)
                      (let-values (((name version) (package-name->name+version name)))
                            (go-module->guix-package name version))
                        )
                    #:guix-name go-module-name+version->package-name+version))

(define (list->licenses licenses)
  "Given a list of LICENSES following the SPDX conventions, return the
corresponding Guix license or 'unknown-license!"
  (filter-map (lambda (license)
                (and (not (string-null? license))
                     (not (any (lambda (elem) (string=? elem license))
                               '("AND" "OR" "WITH")))
                     ;; HACK rename https://github.com/google/licensecheck
                     ;; license types to the corresponding SPDX identifier
                     ;; if they differ. @https://github.com/golang/pkgsite
                     ;; internal/licenses/licenses.go#L174
                     (or (spdx-string->license (match license
                                                 ("BlueOak-1.0" "BlueOak-1.0.0")
                                                 ("BSD-0-Clause" "0BSD")
                                                 ("BSD-2-Clause" "BSD-2-Clause-FreeBSD")
                                                 ("GPL2" "GPL-2.0")
                                                 ("GPL3" "GPL-3.0")
                                                 ("NIST" "NIST-PD")
                                                 (_ license)))
                         'unknown-license!)))
              licenses))

(define* (maybe-go-module-inputs go-module-inputs)
  "Given a list of ARGUMENTS containing strings of module names, returns
the same ARGUMENTS in a wrapping quasiquote structure for use with
PACKAGE PROPAGATED-INPUTS."
  (match go-module-inputs
    (()
     '())
    ((args ...)
     `(#:go-module-inputs ,args))))

(define* (maybe-arguments arguments)
  "Given a list of ARGUMENTS, returns the same ARGUMENTS in a wrapping
quasiquote structure for use with PACKAGE PROPAGATED-INPUTS."
  (match arguments
    (()
     '())
    ((args ...)
     `((arguments (,'quasiquote ,args))))))

(define* (go-module-dependency->go-module-name+version dependency)
  "Given a <GO-MODULE-DEPENDENCY> returns its corresponding name and version
separated by @ in its native format."
  (string-append
   (go-module-dependency-name dependency) "@"
   (go-module-dependency-version dependency)))

(define* (go-module-version->package-version version)
  "Given a Go-native package VERSION, return its value without the leading v
character. Exempli gratia: v1.0.0-test => 1.0.0-test"
  (string-trim version #\v))

(define* (go-module-name+version->package-name+version name+version)
  "Given a Go Module NAME+VERSION or package specification (spec), returns a
normalized package name. Exempli gratia: golang.org/x/sys@v1.0.0-test =>
go-golang-org-x-sys-1.0.0-test"
  (let-values (((name version) (package-name->name+version name+version)))
    (string-downcase
     (string-append "go-" (regexp-substitute/global #f
                                                    "[^a-zA-Z0-9]+" name
                                                    'pre "-" 'post)
                    (if version (string-append "-"
                                               (go-module-version->package-version version)) "")))))

(define* (module-name+versions->package-inputs names #:optional (output #f))
  "Given a list of PACKAGE-NAMES, and an optional OUTPUT, tries to generate a
quoted list of inputs, as suitable to use in an 'inputs' field of a package
definition."
  (map (lambda (input) (cons*

                        (go-module-name+version->package-name+version
                         input)
                        (list 'unquote (string->symbol
                                        (go-module-name+version->package-name+version input)))
                        (or (and output (list output)) '())))
       names))

;;;
;;; Updater
;;;
;; FIXME: there is no updater because package names have a fixed version.
;; If we want to enable the updater, we'll need to create generic package
;; variables without including the version name.
