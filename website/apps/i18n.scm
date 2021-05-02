;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (apps i18n)
  #:use-module (haunt asset)
  #:use-module (haunt page)
  #:use-module (haunt utils)
  #:autoload   (haunt artifact) (artifact?        ;Haunt >= 0.2.5
                                 artifact-writer artifact-file-name
                                 make-artifact)
  #:use-module ((guix i18n) #:select (%package-text-domain))
  #:use-module (ice-9 match)
  #:use-module (sexp-xgettext)
  #:use-module (srfi srfi-1)
  #:export (G_
            N_
            C_
            NC_
            %current-ietf-tag
            %current-lang
            %current-lingua
            builder->localized-builder
            builders->localized-builders
            ietf-tags-file-contents
            localized-root-path))

(define %gettext-domain
  "guix-website")

(bindtextdomain %gettext-domain (getcwd))
(bind-textdomain-codeset %gettext-domain "UTF-8")
(textdomain %gettext-domain)
(bindtextdomain %package-text-domain (getcwd))
(bind-textdomain-codeset %package-text-domain "UTF-8")

;; NOTE: The sgettext macros have no hygiene because they use
;; datum->syntax and do not preserve the semantics of anything looking
;; like an sgettext macro.  This is an exceptional use case; do not
;; try this at home.

(define-syntax G_
  sgettext)

(set-simple-keywords! '(G_))

(define-syntax N_ ;like ngettext
  sngettext)

(define-syntax C_ ;like pgettext
  spgettext)

(define-syntax NC_ ;like npgettext
  snpgettext)

(set-complex-keywords! '(N_ C_ NC_))

(define %current-lingua
  ;; strip the character encoding:
  (car (string-split (setlocale LC_ALL) #\.)))

(define-syntax ietf-tags-file-contents
  (identifier-syntax
   (force (delay (call-with-input-file
                     "po/ietf-tags.scm"
                   (lambda (port) (read port)))))))


(define %current-ietf-tag
  (or (assoc-ref ietf-tags-file-contents %current-lingua)
      "en"))

(define %current-lang
  (car (string-split %current-ietf-tag #\-)))

(define* (localized-root-path url #:key (lingua %current-ietf-tag))
  "Given a URL as used in a href attribute, return the URL prefix
'builder->localized-builder' would use for the URL when called with
LINGUA."
  (if (or (string-suffix? ".html" url)
          (string-suffix? "/" url))
      (string-append lingua "/")
      ""))

(define (first-value arg)
  "For some reason the builder returned by static-directory returns
multiple values.  This procedure is used to retain only the first
return value.  TODO: This should not be necessary."
  arg)

(define (builder->localized-builder builder)
  "Return a Haunt builder procedure generated from an existing BUILDER
with translations for the current system locale coming from
sexp-xgettext."
  (compose
   (lambda (pages-and-assets)
     (map (match-lambda
            ((? page? page)                       ;Haunt < 0.2.5 (deprecated)
             (let ((new-name (string-append
                              (localized-root-path (page-file-name page))
                              (page-file-name page))))
               (make-page new-name (page-contents page)
                          (page-writer page))))
            ((? asset? asset)                     ;Haunt < 0.2.5 (deprecated)
             (let ((new-name (string-append
                              (localized-root-path (asset-target asset))
                              (asset-target asset))))
               (make-asset (asset-source asset) new-name)))
            ((? artifact? artifact)               ;Haunt >= 0.2.5
             (let ((new-name (string-append
                              (localized-root-path (artifact-file-name artifact))
                              (artifact-file-name artifact))))
               (make-artifact new-name
                              (artifact-writer artifact)))))
      pages-and-assets))
   (lambda (site posts)
     (first-value (builder site posts)))))

(define (builders->localized-builders builders)
  "Return a list of new Haunt builder procedures generated from
BUILDERS and localized via sexp-xgettext for the current system
locale."
  (flatten
   (map-in-order
    builder->localized-builder
    builders)))
