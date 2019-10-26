;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media builder)
  #:use-module (apps aux system)
  #:use-module (apps media data)
  #:use-module (apps media templates screenshot)
  #:use-module (apps media templates screenshots-overview)
  #:use-module (apps media templates video)
  #:use-module (apps media templates video-list)
  #:use-module (apps media types)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (haunt utils)
  #:use-module (apps aux web)
  #:use-module (apps media utils)
  #:use-module (srfi srfi-1)
  #:export (builder))


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
   (list (screenshots-overview-builder)
         (screenshots-builder)
         (videos-builder)
         (video-list-builder))))


;;;
;;; Helper builders.
;;;

(define (screenshots-builder)
  "Return a list of Haunt pages representing screenshot pages."
  (map
   (lambda (shot)
     (let ((context
            (list (cons "screenshot" shot)
                  (cons "screenshots" screenshots))))
       (make-page (path-join "screenshots"
                             (screenshot-slug shot)
                             "index.html")
                  (screenshot-t context)
                  sxml->html)))
   screenshots))


(define (screenshots-overview-builder)
  "Return a Haunt page representing the screenshots overview page."
  (make-page "screenshots/index.html"
             (screenshots-overview-t screenshots)
             sxml->html))


(define (videos-builder)
  "Return a list of Haunt pages representing videos."
  (map-in-order
   (lambda (playlist)
     (map-in-order
      (lambda (previous video next)
        (make-page (video->url video)
                   (video-t previous video next)
                   sxml->html))
      (cons #f (drop-right playlist 1))
      playlist
      (append (cdr playlist) '(#f))))
   playlists))


(define (video-list-builder)
  "Return a Haunt page displaying all videos."
  (make-page (url-path-join "videos" "index.html")
             (video-list-t)
             sxml->html))
