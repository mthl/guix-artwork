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

(define-record-type <image>
  (make-image description logo job type)
  image?
  (description image-description)   ;string
  (logo        image-logo)          ;string
  (job         image-job)           ;string
  (type        image-type))         ;string

(define images
  (list (make-image
         "GNU Guix System ISO-9660 image for x86_64"
         (guix-url "static/base/img/GuixSD-package.png")
         "iso9660-image.x86_64-linux"
         "ISO-9660")))

(define (build-detail-url url build)
  "Return the detail page for BUILD hosted on CI server at URL."
  (format #f "~a/build/~a/details" url (build-id build)))

(define (build-product-download-url url build-product)
  "Return a download URL for BUILD-PRODUCT hosted on CI server at URL."
  (string-append url "/download/" (number->string
                                   (build-product-id build-product))))
(define* (products-latest-urls job type
                               #:optional (limit 15)
                               #:key url)
  "Fetch the latest LIMIT jobs from URL matching the given JOB. Then, for the
first job with a build output of the given TYPE, return '(DETAIL-URL
. DOWNLOAD-URL), where DETAIL-URL is the URL describing the matching build in
the CI, and DOWNLOAD-URL is the URL to download the build output. If no
matching jobs are found, return an empty list."
  ;; See build-status enumeration in (cuirass database).
  (define build-status-success 0)

  (define (find-product-by-type build-products type)
    (find (lambda (build-product)
            (string=? (build-product-type build-product) type))
          build-products))

  (define (latest-build-products)
    (let ((builds
           (latest-builds url limit
                          #:job job
                          #:status build-status-success)))
      (filter-map
       (lambda (build)
         (let ((products (build-products build)))
           (match products
             (() #f)
             (x (let ((product
                       (find-product-by-type products type)))
                  (and product
                       (cons build product)))))))
       builds)))

  (match (latest-build-products)
    (((build . product) . rest)
     (cons
      (build-detail-url url build)
      (build-product-download-url url product)))
    (_ '())))

(define (image-table-row image)
  "Return as an HTML table row, the representation of IMAGE."
  (let* ((description (image-description image))
         (job (image-job image))
         (type (image-type image))
         (logo (image-logo image))
         (urls (products-latest-urls job type #:url ci-url)))
    `(tr
      (td
       (table
        (@ (class "download-table-box"))
        (tbody
         (tr
          (td
           (@ (class "download-table-box"))
           (img (@ (src ,logo) (alt ""))))
          (td
           (@ (class "download-table-box"))
           (h3 ,description))))))
      ,(if (null? urls)
           '(td "No available image")
           (match urls
             ((detail-url . download-url)
              `(td
                (a (@ (href ,download-url)) "Download")
                " "
                (a (@ (href ,detail-url)) "(details)"))))))))

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
       (@ (class "centered-block limit-width table-box"))
       (table
        (thread
         (tr (th "image")
             (th "download")))
        (tbody
         ,(map image-table-row images))))))))
