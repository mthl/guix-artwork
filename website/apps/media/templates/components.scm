;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

;;; This module defines HTML parts related to media.

(define-module (apps media templates components)
  #:use-module (apps aux lists)
  #:use-module (apps aux web)
  #:use-module (apps base utils)
  #:use-module (apps media types)
  #:export (screenshot->shtml
            screenshots-box))


;;;
;;; Components.
;;;

(define (screenshot->shtml shot)
  "Return an SHTML representation of the given screenshot object.

   SHOT (<screenshot>)
     A screenshot object as defined in (apps media types)."
  `(div
    (@ (class "screenshot-preview"))
    (a
     (@ (href ,(guix-url (url-path-join "screenshots"
                                        (screenshot-slug shot) ""))))
     (img
      (@ (class "responsive-image")
         (src ,(screenshot-preview shot))
         (alt "")))
     (span (@ (class "screenshot-inset-shadow")) ""))
    (p ,(screenshot-caption shot) (span (@ (class "hidden")) "."))))


(define* (screenshots-box screenshots #:optional (n 6) #:key shadow)
  "Return SHTML for a box displaying up to N many SCREENSHOTS randomly
chosen at build time.  If SHADOW is true, a shadow is displayed at the
top."
  `(div
    (@ (class ,(string-join `("screenshots-box"
                              ,@(if shadow
                                    '("top-shadow-bg")
                                    '())))))
    ,@(map screenshot->shtml (take-random screenshots n))))
