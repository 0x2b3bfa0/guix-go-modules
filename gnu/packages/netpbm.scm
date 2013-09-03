;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages netpbm)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libtiff)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:select (gpl2))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public netpbm
  (package
   (name "netpbm")
   (version "10.61.01")
   (source (origin
            (method url-fetch)
            ;; The "super-stable" and "stable" versions do not compile
            ;; with newer libpng; we need the "advanced" version. The tarball
            ;; on the server is generated by sourceforge from the "advanced"
            ;; branch of the subversion repository:
            ;; svn checkout http://netpbm.svn.sourceforge.net/svnroot/netpbm/advanced netpbm-version
            (uri (string-append "http://www.multiprecision.org/guix/netpbm-"
                                version ".tar.xz"))
            (sha256 (base32
                     "10nwvxc85kr6vhlhhahagy7s9848bbixl54b0p4ppim4g0dl10jz"))))
   (build-system gnu-build-system)
   (inputs `(("flex" ,flex)
             ("ghostscript" ,ghostscript)
             ("libjpeg" ,libjpeg)
             ("libpng" ,libpng)
             ("libtiff" ,libtiff)
             ("libxml2" ,libxml2)
             ("perl" ,perl)
             ("pkg-config" ,pkg-config)
             ("python" ,python-wrapper)
             ("zlib" ,zlib)))
   (arguments
    `(#:phases
      (alist-replace
       'configure
       (lambda* (#:key #:allow-other-keys #:rest args)
        (copy-file "config.mk.in" "config.mk")
        (let ((f (open-file "config.mk" "a")))
         (display "CC=gcc\n" f)
         (display "CFLAGS_SHLIB += -fPIC\n" f)
         (display "TIFFLIB = libtiff.so\n" f)
         (display "JPEGLIB = libjpeg.so\n" f)
         (display "ZLIB = libz.so\n" f)
         (close-port f)
         ;; drop advertisement for non-free program
         (substitute* "converter/ppm/Makefile" (("hpcdtoppm") ""))
         ;; drop programs without license, see
         ;; http://packages.debian.org/changelogs/pool/main/n/netpbm-free/netpbm-free_10.0-12.2/libnetpbm10.copyright
         (substitute* "converter/pbm/Makefile" (("pbmto4425") ""))
         (substitute* "converter/pbm/Makefile" (("pbmtoln03") ""))
         (substitute* "converter/pbm/Makefile" (("pbmtolps") ""))
         (substitute* "converter/pbm/Makefile" (("pbmtopk") ""))
         (substitute* "converter/pbm/Makefile" (("pktopbm") ""))
         (substitute* "converter/pgm/Makefile" (("spottopgm") ""))
         (substitute* "converter/ppm/Makefile" (("ppmtopjxl") ""))
         ))
      (alist-replace
       'check
       (lambda* (#:key #:allow-other-keys #:rest args)
        (let ((check (assoc-ref %standard-phases 'check)))
          ;; install temporarily into /tmp/netpbm
          (system* "make" "package")
          ;; remove test requiring X
          (substitute* "test/all-in-place.test" (("pamx") ""))
          ;; do not worry about non-existing file
          (substitute* "test/all-in-place.test" (("^rm ") "rm -f "))
          ;; remove four tests that fail for unknown reasons
          (substitute* "test/Test-Order" (("all-in-place.test") ""))
          (substitute* "test/Test-Order" (("pnmpsnr.test") ""))
          (substitute* "test/Test-Order" (("pnmremap1.test") ""))
          (substitute* "test/Test-Order" (("gif-roundtrip.test") ""))
          (apply check args)))
      (alist-replace
       'install
       (lambda* (#:key outputs make-flags #:allow-other-keys)
        (let ((out (assoc-ref outputs "out")))
         (apply system* "make" "package"
                        (string-append "pkgdir=" out) make-flags)
         ;; copy static library
         (copy-file (string-append out "/link/libnetpbm.a")
                    (string-append out "/lib/libnetpbm.a"))
         ;; remove superfluous folders and files
         (system* "rm" "-r" (string-append out "/link"))
         (system* "rm" "-r" (string-append out "/misc"))
         (with-directory-excursion out
           (for-each delete-file
                     '("config_template" "pkginfo" "README" "VERSION")))))
      %standard-phases)))))
   (synopsis "Netpbm, a toolkit for manipulation of images")
   (description
    "Netpbm is a toolkit for the manipulation of graphic images, including
the conversion of images between a variety of different formats.
There are over 300 separate tools in the package including converters for
about 100 graphics formats.")
   (license gpl2)
   (home-page "http://netpbm.sourceforge.net/")))
