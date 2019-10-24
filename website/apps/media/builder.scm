;;; GNU Guix web site
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media builder)
  #:use-module (apps aux system)
  #:use-module (apps media data)
  #:use-module (apps media templates screenshot)
  #:use-module (apps media types)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (haunt utils)
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
   (list (screenshots-builder))))


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
