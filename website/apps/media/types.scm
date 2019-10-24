;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps media types)
  #:use-module (srfi srfi-9)
  #:export (screenshot
            screenshot?
            screenshot-caption
            screenshot-image
            screenshot-preview
            screenshot-slug
            screenshot-title))


;;;
;;; Data types.
;;;

;;; Screenshot (record type)
;;; ------------------------
;;;
;;; A screenshot object represents an image of a software view seen
;;; on a screen.
;;;
;;; Objects of this type can be created with the "screenshot"
;;; procedure (see Helper procedures below).
;;;
;;; Fields:
;;;
;;; title (string)
;;;   A title for the screenshot.
;;;
;;; slug (string)
;;;     Slug-like URL name for the screenshot. For example:
;;;     gnome-3-desktop.
;;;
;;; image (string)
;;;   A URL to the full size image of the screenshot.
;;;
;;; preview (string)
;;;   A URL to a small size image of the screenshot.
;;;
;;; caption (string)
;;;   A short text describing the screenshot.
;;;
(define-record-type <screenshot>
  (make-screenshot title slug image preview caption)
  screenshot?
  (title screenshot-title)
  (slug screenshot-slug)
  (image screenshot-image)
  (preview screenshot-preview)
  (caption screenshot-caption))

;;; Helper procedures.

(define* (screenshot #:key title slug image preview caption)
  "Return a <screenshot> object with the given attributes."
  (make-screenshot title slug image preview caption))
