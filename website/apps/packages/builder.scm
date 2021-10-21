;;; GNU Guix web site
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020, 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;;
;;; Initially written by sirgazil
;;; who waives all copyright interest on this file.
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

(define-module (apps packages builder)
  #:use-module (apps aux lists)
  #:use-module (apps aux system)
  #:use-module (apps base utils)
  #:use-module (apps packages data)
  #:use-module (apps packages templates detailed-index)
  #:use-module (apps packages templates index)
  #:use-module (apps packages templates detailed-package-list)
  #:use-module (apps packages templates package)
  #:use-module (apps packages templates package-list)
  #:use-module (apps packages types)
  #:use-module (apps packages utils)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (haunt utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)                       ;location
  #:use-module ((guix build download) #:select (maybe-expand-mirrors))
  #:use-module ((guix base64) #:select (base64-encode))
  #:use-module ((guix describe) #:select (current-profile))
  #:use-module ((guix config) #:select (%guix-version))
  #:use-module (guix gexp)
  #:use-module (json)
  #:use-module (ice-9 match)
  #:use-module ((web uri) #:select (string->uri uri->string))
  #:export (builder))

;;; Required by 'origin->json' for 'computed-origin-method' corner cases
(define gexp-references (@@ (guix gexp) gexp-references))

;;;
;;; Application builder.
;;;

(define (builder site posts)
  "Return the list of web resources that compose the app.

   This procedure is a Haunt builder procedure.

   SITE (<site>)
     A site object that defines all the properties of the website. See
     Haunt <site> objects for more information.

   POSTS (list of <post>)
     A list of post objects that represent articles from the blog. See
     Haunt <post> objects for more information.

   RETURN (list of <page>)
     A list of page objects that represent the web resources of the
     application. See Haunt <page> objects for more information."
  (flatten
   (list
    (index-builder)
    (sources-json-builder)
    (packages-json-builder)
    (packages-builder)
    (package-list-builder))))



;;;
;;; Helper builders.
;;;

(define %max-packages-on-index
  ;; Maximum number of packages shown on /packages.
  30)

(define (origin->json origin)
  "Return a JSON representation (an alist) of ORIGIN."
  (define method
    (origin-method origin))

  (define uri
    (origin-uri origin))

  (define (resolve urls)
    (map uri->string
         (append-map (cut maybe-expand-mirrors <> %mirrors)
                     (map string->uri urls))))

  (if (eq? method (@@ (guix packages) computed-origin-method))
      ;; Packages in gnu/packages/gnuzilla.scm and gnu/packages/linux.scm
      ;; represent their 'uri' as 'promise'.
      (match uri
        ((? promise? promise)
         (match (force promise)
           ((? gexp? g)
            (append-map origin->json
                        (filter-map (match-lambda
                                      ((? gexp-input? thing)
                                       (match (gexp-input-thing thing)
                                         ((? origin? o) o)
                                         (_ #f)))
                                      (_ #f))
                                    (gexp-references g))))
           (_ `((type . #nil))))))
      ;;Regular packages represent 'uri' as string.
      `(((type . ,(cond ((or (eq? url-fetch method)
                              (eq? url-fetch/tarbomb method)
                              (eq? url-fetch/zipbomb method)) 'url)
                         ((eq? git-fetch method) 'git)
                         ((or (eq? svn-fetch method)
                              (eq? svn-multi-fetch method)) 'svn)
                         ((eq? hg-fetch method) 'hg)
                         (else                   #nil)))
          ,@(cond ((or (eq? url-fetch method)
                       (eq? url-fetch/tarbomb method)
                       (eq? url-fetch/zipbomb method))
                   `(("urls" . ,(list->vector
                                 (resolve
                                  (match uri
                                    ((? string? url) (list url))
                                    ((urls ...) urls)))))))
                  ((eq? git-fetch method)
                   `(("git_url" . ,(git-reference-url uri))))
                  ((eq? svn-fetch method)
                   `(("svn_url" . ,(svn-reference-url uri))))
                  ((eq? svn-multi-fetch method)
                   `(("svn_url" . ,(svn-multi-reference-url uri))))
                  ((eq? hg-fetch method)
                   `(("hg_url" . ,(hg-reference-url uri))))
                  (else '()))
          ,@(if (or (eq? url-fetch method)
                    (eq? url-fetch/tarbomb method)
                    (eq? url-fetch/zipbomb method))
                (let* ((content-hash (origin-hash origin))
                       (hash-value (content-hash-value content-hash))
                       (hash-algorithm (content-hash-algorithm content-hash))
                       (algorithm-string (symbol->string hash-algorithm)))
                  `(("integrity" . ,(string-append algorithm-string "-"
                                                   (base64-encode hash-value)))))
                '())
          ,@(if (eq? method git-fetch)
                `(("git_ref" . ,(git-reference-commit uri)))
                '())
          ,@(if (eq? method svn-fetch)
                `(("svn_revision" . ,(svn-reference-revision uri)))
                '())
          ,@(if (eq? method svn-multi-fetch)
                `(("svn_revision" . ,(svn-multi-reference-revision uri)))
                '())
          ,@(if (eq? method hg-fetch)
                `(("hg_changeset" . ,(hg-reference-changeset uri)))
                '())))))

(define (packages-json-builder)
  "Return a JSON page listing all packages."
  (define (package->json package)
    (define cpe-name
      (assoc-ref (package-properties package) 'cpe-name))
    (define cpe-version
      (assoc-ref (package-properties package) 'cpe-version))

    `(("name"     . ,(package-name package))
      ("version"  . ,(package-version package))
      ,@(if cpe-name `(("cpe_name" . ,cpe-name)) '())
      ,@(if cpe-version `(("cpe_version" . ,cpe-version)) '())
      ,@(if (origin? (package-source package))
            `(("source" . ,(list->vector
                            (origin->json (package-source package)))))
            '())
      ("synopsis" . ,(package-synopsis package))
      ,@(if (package-home-page package)
            `(("homepage" . ,(package-home-page package)))
            '())
      ,@(match (package-location package)
          ((? location? location)
           `(("location"
              . ,(string-append (location-file location) ":"
                                (number->string
                                 (+ 1 (location-line location)))))))
          (#f
           '()))))

  (make-page "packages.json"
	     (list->vector (map package->json (all-packages)))
             scm->json))

(define (sources-json-builder)
  "Return a JSON page listing all the sources."
  ;; The Software Heritage format is described here:
  ;; https://forge.softwareheritage.org/source/swh-loader-core/browse/master/swh/loader/package/nixguix/tests/data/https_nix-community.github.io/nixpkgs-swh_sources.json
  ;; And the loader is implemented here:
  ;; https://forge.softwareheritage.org/source/swh-loader-core/browse/master/swh/loader/package/nixguix/
  (define (package->json package)
    `(,@(if (origin? (package-source package))
            (origin->json (package-source package))
            `(((type . "no-origin")
                ("name" . ,(package-name package)))))))

  (make-page "sources.json"
             `(("sources" . ,(list->vector (append-map package->json (all-packages))))
               ("version" . "1")
               ("revision" .
                ,(match (current-profile)
                   (#f %guix-version)   ;for lack of a better ID
                   (profile
                    (let ((channel (find guix-channel? (profile-channels profile))))
                      (channel-commit channel))))))
             scm->json))

(define (index-builder)
  "Return a Haunt page listing some random packages."
  (define (sample n from)
    (map (lambda (id) (list-ref from id))
         (list-tabulate n (lambda _ (random (length from))))))
  (let ((context (list (cons "packages"
                             (sample %max-packages-on-index
                                     (all-packages)))
		       (cons "total"
			     (length (all-packages))))))
    (make-page "packages/index.html" (index-t context) sxml->html)))


(define (detailed-index-builder)
  "Return a Haunt page listing some random packages."
  ;; TODO: Pass ~30 random Guix packages.
  (let ((context (list (cons "packages"
                             (take-at-most (all-packages)
                                           %max-packages-on-index)))))
    (make-page "packages/index.html"
               (detailed-index-t context (length (all-packages)))
               sxml->html)))


(define (detailed-package-list-builder)
  "Return a list of grouped Haunt pages listing Guix packages.

   Each group is a list of page objects corresponding to paginated
   packages starting with a specific letter."
  (let ((package-groups (packages/group-by-letter (all-packages))))
    (map
     (lambda (package-group)
       (let* ((letter (car package-group))
	      (context
	       (list
		(cons "letter" letter))))
	 (paginate #:dataset (cdr package-group)
		   #:limit 100
		   #:base-path (path-join "packages" letter)
		   #:template detailed-package-list-t
		   #:context context
		   #:writer sxml->html)))
     package-groups)))


(define (packages-builder)
  "Return a list of Haunt pages for each Guix package."
  (map
   (lambda (package)
     (let ((context (list (cons "package" package))))
       (make-page
	(path-join (package-url-path package) "index.html")
	(package-t context)
	sxml->html)))
   (all-packages)))


(define (package-list-builder)
  "Return a list of grouped Haunt pages listing Guix packages.

   Each group is a list of page objects corresponding to paginated
   packages starting with a specific letter."
  (let ((package-groups (packages/group-by-letter (all-packages))))
    (map
     (lambda (package-group)
       (let* ((letter (car package-group))
	      (context
	       (list
		(cons "letter" letter))))
	 (paginate #:dataset (cdr package-group)
		   #:limit 100
		   #:base-path (path-join "packages" letter)
		   #:template package-list-t
		   #:context context
		   #:writer sxml->html)))
     package-groups)))
