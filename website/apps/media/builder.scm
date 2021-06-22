;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media builder)
  #:use-module (apps aux system)
  #:use-module (apps base utils)
  #:use-module (apps media data)
  #:use-module (apps media templates publication-list)
  #:use-module (apps media templates screenshot)
  #:use-module (apps media templates screenshots-overview)
  #:use-module (apps media templates video)
  #:use-module (apps media templates video-list)
  #:use-module (apps media types)
  #:use-module (haunt artifact)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (haunt utils)
  #:use-module (apps aux web)
  #:use-module (apps media utils)
  #:use-module (srfi srfi-19)
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

   RETURN (list of <artifact> and <page>)
     A list of objects that represent the web resources of the
     application. See Haunt <artifact> and <page> objects for more
     information."
  (flatten
   (list (publication-list-builder)
         (screenshots-overview-builder)
         (screenshots-builder)
         (videos-builder)
         (video-list-builder))))


;;;
;;; Helper builders.
;;;

(define (publication-list-builder)
  "Return a Haunt artifact representing the publications page."
  (serialized-artifact (url-path-join "publications" "index.html")
                       (publication-list-t publications)
                       sxml->html))


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
  "Return a list whose elements can be single Haunt artifacts or lists
   of Haunt artifacts, where each artifact represents a page of a
   video."
  (define* (video-builder video #:optional (playlist #false))
    "Return a Haunt artifact for the video."
    (let ((year (date-year (video-date video)))
          (slug (video-slug video)))

      (serialized-artifact (path-join "videos"
                                      (number->string year)
                                      slug
                                      "index.html")
                           (video-t video playlist)
                           sxml->html)))

  (define (playlist-builder playlist)
    "Return a list of Haunt artifacts for the videos in the playlist."
    (map
     (lambda (video)
       (video-builder video playlist))
     (playlist-videos playlist)))

  (map
   (lambda (item)
     (cond ((video? item) (video-builder item))
           ((playlist? item) (playlist-builder item))))
   videos))


(define (video-list-builder)
  "Return a list of Haunt pages representing a paginated catalog of
   videos."
  (let ((sorted-videos (reverse videos)))

    (paginate #:dataset sorted-videos
	            #:base-path "videos"
	            #:template video-list-t
	            #:writer sxml->html)))
