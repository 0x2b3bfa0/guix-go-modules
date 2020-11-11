;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
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

(define-module (guix build-system go)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%go-build-system-modules
            go-build
            go-build-system
            %go-proxy-url
            go-proxy-url?
            go-proxy-uri
            go-path-escape))

;; Commentary:
;;
;; Standard build procedure for packages using the Go build system.  It is
;; implemented as an extension of 'gnu-build-system'.
;;
;; Code:

;; Default module proxy server. Should point to a server that complies with the
;;module proxy protocol (https://golang.org/cmd/go/#hdr-Module_proxy_protocol),
;;allowing us to pull the source code for any module without dealing with every
;;version control protocol in the universe.
(define %go-proxy-url
  (make-parameter "https://proxy.golang.org"))

;; "Check if a given string is a URL that points to %go-proxy-url."
(define go-proxy-url?
  (cut string-prefix? %go-proxy-url <>))

;; "Return a URI pointing to the source hosted at proxy.golang.org for the
;; Go module corresponding to NAME and VERSION."
(define (go-proxy-uri name version)
  (string-append (%go-proxy-url) "/" (go-path-escape name)
                 "/@v/v" version ".zip"))

;; "Escape a module path by replacing every uppercase letter with an exclamation
;; mark followed with its lowercase equivalent, as per the module Escaped Paths
;; specification. https://godoc.org/golang.org/x/mod/module#hdr-Escaped_Paths"
(define (go-path-escape path)
  (define escape (lambda (occurrence)
    (string-append "!" (string-downcase (match:substring occurrence)))))
  (regexp-substitute/global #f "[A-Z]" path 'pre escape 'post))

(define %go-build-system-modules
  ;; Build-side modules imported and used by default.
  `((guix build go-build-system)
    (guix build union)
    ,@%gnu-build-system-modules))

(define (default-go)
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((go (resolve-interface '(gnu packages golang))))
    (module-ref go 'go)))

(define (package-go-inputs p)
  (apply
    (lambda* (#:key (go-module-inputs '()) #:allow-other-keys)
      go-module-inputs)
    (package-arguments p)))

(define (go-module-closure inputs)
  "Return the closure of INPUTS when considering the 'cargo-inputs' and
'cargod-dev-deps' edges.  Omit duplicate inputs, except for those
already present in INPUTS itself.

This is implemented as a breadth-first traversal such that INPUTS is
preserved, and only duplicate extracted inputs are removed.

Forked from ((guix packages) transitive-inputs) since this extraction
uses slightly different rules compared to the rest of Guix (i.e. we
do not extract the conventional inputs)."
  (define (seen? seen item)
    ;; FIXME: We're using pointer identity here, which is extremely sensitive
    ;; to memoization in package-producing procedures; see
    ;; <https://bugs.gnu.org/30155>.
    (vhash-assq item seen))

  (let loop ((inputs     inputs)
             (result     '())
             (propagated '())
             (first?     #t)
             (seen       vlist-null))
    (match inputs
      (()
       (if (null? propagated)
           (reverse result)
           (loop (reverse (concatenate propagated)) result '() #f seen)))
      (((and input (label (? package? package))) rest ...)
       (if (and (not first?) (seen? seen package))
           (loop rest result propagated first? seen)
           (loop rest
                 (cons input result)
                 (cons (package-go-inputs package)
                       propagated)
                 first?
                 (vhash-consq package package seen))))
      ((input rest ...)
       (loop rest (cons input result) propagated first? seen)))))

(define (expand-go-sources go-module-inputs)
  "Extract all transitive sources for CARGO-INPUTS and CARGO-DEVELOPMENT-INPUTS
along their 'cargo-inputs' edges.

Cargo requires all transitive crate dependencies' sources to be available
in its index, even if they are optional (this is so it can generate
deterministic Cargo.lock files regardless of the target platform or enabled
features). Thus we need all transitive crate dependencies for any cargo
dev-dependencies, but this is only needed when building/testing a crate directly
(i.e. we will never need transitive dev-dependencies for any dependency crates).

Another complication arises due potential dependency cycles from Guix's
perspective: Although cargo does not permit cyclic dependencies between crates,
however, it permits cycles to occur via dev-dependencies. For example, if crate
X depends on crate Y, crate Y's tests could pull in crate X to to verify
everything builds properly (this is a rare scenario, but it it happens for
example with the `proc-macro2` and `quote` crates). This is allowed by cargo
because tests are built as a pseudo-crate which happens to depend on the
X and Y crates, forming an acyclic graph.

We can side step this problem by only considering regular cargo dependencies
since they are guaranteed to not have cycles. We can further resolve any
potential dev-dependency cycles by extracting package sources (which never have
any dependencies and thus no cycles can exist).

There are several implications of this decision:
* Building a package definition does not require actually building/checking
any dependent crates. This can be a benefits:
 - For example, sometimes a crate may have an optional dependency on some OS
 specific package which cannot be built or run on the current system. This
 approach means that the build will not fail if cargo ends up internally ignoring
 the dependency.
 - It avoids waiting for quadratic builds from source: cargo always builds
 dependencies within the current workspace. This is largely due to Rust not
 having a stable ABI and other resolutions that cargo applies. This means that
 if we have a depencency chain of X -> Y -> Z and we build each definition
 independently the following will happen:
  * Cargo will build and test crate Z
  * Cargo will build crate Z in Y's workspace, then build and test Y
  * Cargo will build crates Y and Z in X's workspace, then build and test X
* But there are also some downsides with this approach:
  - If a dependent crate is subtly broken on the system (i.e. it builds but its
  tests fail) the consuming crates may build and test successfully but
  actually fail during normal usage (however, the CI will still build all
  packages which will give visibility in case packages suddenly break).
  - Because crates aren't declared as regular inputs, other Guix facilities
  such as tracking package graphs may not work by default (however, this is
  something that can always be extended or reworked in the future)."
  (filter-map
    (match-lambda
      ((label (? package? p))
       (list label (package-source p)))
      ((label input)
       (list label input)))
    (go-module-closure go-module-inputs)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (go (default-go))
                (go-module-inputs '())
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:go #:inputs #:native-inputs #:go-module-inputs))
  (define unzip
    (module-ref (resolve-interface '(gnu packages compression)) 'unzip))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("go" ,go)
                         ("unzip" ,unzip)
                         ,@(expand-go-sources go-module-inputs)
                         ; ,@(go-module-closure go-module-inputs)
                         ,@native-inputs))
         (outputs outputs)
         (build go-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (go-build store name inputs
                   #:key
                   (phases '(@ (guix build go-build-system)
                               %standard-phases))
                   (outputs '("out"))
                   (search-paths '())
                   (install-source? #t)
                   (import-path "")
                   (unpack-path "")
                   (build-flags ''())
                   (tests? #t)
                   (module? #f)
                   (build? #t)
                   (allow-go-reference? #f)
                   (system (%current-system))
                   (guile #f)
                   (imported-modules %go-build-system-modules)
                   (go-module-inputs '())
                   (modules '((guix build go-build-system)
                              (guix build union)
                              (guix build utils))))
  (define builder
   `(begin
      (use-modules ,@modules)
      (go-build #:name ,name
                #:source ,(match (assoc-ref inputs "source")
                                 (((? derivation? source))
                                  (derivation->output-path source))
                                 ((source)
                                  source)
                                 (source
                                  source))
                #:system ,system
                #:phases ,phases
                #:outputs %outputs
                #:search-paths ',(map search-path-specification->sexp
                                      search-paths)
                #:install-source? ,install-source?
                #:import-path ,import-path
                #:unpack-path ,unpack-path
                #:build-flags ,build-flags
                #:tests? ,tests?
                #:module? ,module?
                #:build? ,build?
                #:allow-go-reference? ,allow-go-reference?
                #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system
                             #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))

(define go-build-system
  (build-system
    (name 'go)
    (description
     "Build system for Go programs")
    (lower lower)))
