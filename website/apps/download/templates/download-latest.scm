;;; GNU Guix web site
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.
;;;
;;; This file is part of the GNU Guix web site.
;;;
;;; The GNU Guix web site is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; The GNU Guix web site is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with the GNU Guix web site.  If not, see <http://www.gnu.org/licenses/>.

(define-module (apps download templates download-latest)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps download templates components)
  #:use-module (apps i18n)
  #:use-module (guix ci)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (download-latest-t))

(define ci-url "https://ci.guix.gnu.org")
(define images-spec "images")
(define tarball-spec "tarball")
(define default-system "x86_64-linux")

(define-record-type <image>
  (make-image title description logo job spec labels systems type)
  image?
  (title       image-title)         ;string
  (description image-description)   ;string
  (logo        image-logo)          ;string
  (job         image-job)           ;string
  (spec        image-spec)          ;string
  (labels      image-labels)        ;list of strings
  (systems     image-systems)       ;list of strings
  (type        image-type))         ;string

(define* (build-query job system
                      #:key
                      (spec images-spec))
  (format #f "query=spec:~a+status:success+system:~a+~a"
          spec system job))

(define (build-detail-url job spec system)
  "Return the detail page for BUILD hosted on CI server at URL."
  (format #f  "~a/search/latest?~a"
          ci-url (build-query job system #:spec spec)))

(define (build-product-download-url job spec system type)
  "Return a download URL for BUILD-PRODUCT hosted on CI server at URL."
  (format #f  "~a/search/latest/~a?~a"
          ci-url type (build-query job system #:spec spec)))

(define images
  (list (make-image
         (C_ "download page title" "GNU Guix System on Linux")
         (G_ "USB/DVD ISO installer of the standalone Guix System on Linux.")
         (guix-url "static/base/img/GuixSD-package.png")
         "image.iso"
         images-spec
         (list default-system)
         (list default-system)
         "ISO-9660")
        (make-image
         (C_ "download page title" "GNU Guix System on GNU Hurd")
         (G_ "Virtual machine image of the standalone Guix System on GNU Hurd.")
         (guix-url "static/base/img/hurd.png")
         "hurd-barebones.qcow2"
         images-spec
         (list "qcow2")
         (list default-system)
         "image")
        (make-image
         (C_ "download page title" "GNU Guix binary")
         (G_ "Self-contained tarball providing binaries for Guix and its
       dependencies, to be installed on top of your Linux-based system.")
         (guix-url "static/base/img/Guix-package.png")
         "guix-binary.tar.xz"
         tarball-spec
         (list default-system)
         (list default-system)
         "archive")
       (make-image
         (C_ "download page title" "GNU Guix System on Linux for Pinebook Pro")
         (G_ "Guix System on Linux barebones bootable raw image for Pinebook Pro.")
         (guix-url "static/base/img/pine.png")
         "pinebook-pro-barebones-raw-image"
         images-spec
         (list "raw")
         (list default-system)
         "image")))

(define (image-download image)
  "Return as an HTML table row, the representation of IMAGE."
  (let* ((title (image-title image))
         (description (image-description image))
         (job (image-job image))
         (spec (image-spec image))
         (labels (image-labels image))
         (systems (image-systems image))
         (type (image-type image))
         (logo (image-logo image)))
    `(div
      (@ (class "download-box"))
      (img (@ (src ,logo) (alt "")))
      (h3 ,title)
      ,description
      ,(G_ `(p "Download options:"))
      ,@(map (lambda (system label)
               `(a
                 (@ (class "download-btn")
                    (download "")
                    (href
                     ,(build-product-download-url job spec system type)))
                 ,label
                 " ")) ; Force a space for readability in non-CSS browsers.
             systems labels)
      ,(G_
        `(p "Build details: "
            ,@(map (lambda (system label)
                     `(a
                       (@ (class "detail-btn")
                          (download "")
                          (href
                           ,(build-detail-url job spec system)))
                       ,label
                       " ")) ; Force a space for readability in non-CSS
                             ; browsers.
                   systems labels))))))

(define (download-latest-t)
  "Return the Download latest page in SHTML."
  (theme
   #:title (C_ "webpage title" '("Download latest"))
   #:description
   (G_ "Download latest development GNU Guix System images built
by the Cuirass continuous integration system.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|Installer|Source code|\
Package manager") #\|)
   #:active-menu-item (C_ "website menu" "Download")
   #:css (list
          (guix-url "static/base/css/page.css")
          (guix-url "static/base/css/download.css"))
   #:crumbs
   (list (crumb (C_ "website menu" "Download") (guix-url "download/"))
         (crumb (C_ "website menu" "Latest") "./"))
   #:content
   `(main
     (section
      (@ (class "page"))
      ,(G_ `(h2 "Download latest development images"))
      ;; TRANSLATORS: Continuous Integration is a section name
      ;; in the English (en) manual.
      ,(G_
        `(p
          (@ (class "centered-block limit-width"))
          "Download latest GNU Guix System images built by the "
          ,(G_ (manual-href "Cuirass"  (G_ "en")
                            (G_ "Continuous-Integration.html")))
          " continuous integration system at "
          (a (@ (href ,ci-url)) "ci.guix.gnu.org")
          ". These images are " ,(G_ `(b "development snapshots"))
          ", you might prefer to use well-tested released images
that can be found "
          ,(G_ `(a (@ (href ,(guix-url "download/"))) "here."))))
      (div
       (@ (class "centered-block limit-width"))
       ,(map image-download images))))))
