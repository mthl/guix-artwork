;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

;;; This module defines HTML parts related to media.

(define-module (apps media templates components)
  #:use-module (apps aux web)
  #:use-module (apps base utils)
  #:use-module (apps media types)
  #:export (screenshot->shtml))


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
