;;; GNU Guix web site
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
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps download templates components)
  #:use-module (guix ci)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (download-latest-t))

(define ci-url "https://ci.guix.gnu.org")
(define default-spec "guix-master")

(define-record-type <image>
  (make-image title description logo job systems type)
  image?
  (title       image-title)         ;string
  (description image-description)   ;string
  (logo        image-logo)          ;string
  (job         image-job)           ;string
  (systems     image-systems)       ;list of strings
  (type        image-type))         ;string

(define images
  (list (make-image
         "GNU Guix System"
         "USB/DVD ISO installer of the standalone Guix System."
         (guix-url "static/base/img/GuixSD-package.png")
         "iso9660-image"
         (list "x86_64-linux")
         "ISO-9660")))

(define (build-query job system)
  (format #f "query=spec:~a+status:success+system:~a+~a"
          default-spec system job))

(define (build-detail-url job system)
  "Return the detail page for BUILD hosted on CI server at URL."
  (format #f  "~a/search/latest?~a" ci-url (build-query job system)))

(define (build-product-download-url job system type)
  "Return a download URL for BUILD-PRODUCT hosted on CI server at URL."
  (format #f  "~a/search/latest/~a?~a"
          ci-url type (build-query job system)))

(define (image-download image)
  "Return as an HTML table row, the representation of IMAGE."
  (let* ((title (image-title image))
         (description (image-description image))
         (job (image-job image))
         (systems (image-systems image))
         (type (image-type image))
         (logo (image-logo image)))
    `(div
      (@ (class "download-box"))
      (img (@ (src ,logo) (alt "")))
      (h3 ,title)
      ,description
      (p "Download options:")
      ,@(map (lambda (system)
               `(a
                 (@ (class "download-btn")
                    (download "")
                    (href ,(build-product-download-url job system type)))
                 ,system
                 " ")) ; Force a space for readability in non-CSS browsers.
             systems)
      (p "Build details: "
         ,@(map (lambda (system)
                  `(a
                    (@ (class "detail-btn")
                       (download "")
                       (href ,(build-detail-url job system)))
                    ,system
                    " ")) ; Force a space for readability in non-CSS browsers.
                systems)))))

(define (download-latest-t)
  "Return the Download latest page in SHTML."
  (theme
   #:title '("Download latest")
   #:description
   "Download latest GNU Guix System images built by the Cuirass continuous
integration system."
   #:keywords
   '("GNU" "Linux" "Unix" "Free software" "Libre software"
     "Operating system" "GNU Hurd" "GNU Guix package manager"
     "Installer" "Source code" "Package manager")
   #:active-menu-item "Download"
   #:css (list
          (guix-url "static/base/css/page.css")
          (guix-url "static/base/css/download.css"))
   #:crumbs (list (crumb "Download" (guix-url "download/"))
                  (crumb "Latest" "./"))
   #:content
   `(main
     (section
      (@ (class "page"))
      (h2 "Download latest images")
      (p
       (@ (class "centered-block limit-width"))
       "Download latest GNU Guix System images built by the "
       (a (@ (href ,(manual-url "Continuous-Integration.html"))) "Cuirass")
       " continuous integration system at "
       (a (@ (href ,ci-url)) "ci.guix.gnu.org")
       ". These images are " (b "development snapshots")
       ", you might prefer to use stable images that can be found "
       (a (@ (href ,(guix-url "download/"))) "here."))
      (div
       (@ (class "centered-block limit-width"))
       ,(map image-download images))))))
