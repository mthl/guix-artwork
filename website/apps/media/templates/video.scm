;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media templates video)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps media templates components)
  #:use-module (apps media types)
  #:use-module (apps media utils)
  #:export (video-t))


(define (video-t previous video next)
  "Return a page in SHTML for the given VIDEO.  If true, links to the
PREVIOUS and NEXT videos are added."
  (theme
   #:title (list "Video" (video-title video))
   #:description
   "Video about GNU Guix."
   #:keywords
   '("GNU" "Linux" "Unix" "Free software" "Libre software"
     "Operating system" "GNU Hurd" "GNU Guix package manager"
     "Help resources" "Videos")
   #:active-menu-item "Videos"
   #:css (list
          (guix-url "static/base/css/page.css")
          (guix-url "static/base/css/index.css"))
   #:crumbs (list (crumb "Videos" (guix-url "videos/"))
                  (crumb (video-title video) "./"))
   #:content
   `(main
     (@ (class "page centered-block limit-width"))
     (h2 ,(video-title video))
     ,(video-content video)
     ,(if (or previous next)
          `(div
            (@ (class "fields-box"))
            ,(if previous
                 (button-big
                  #:label "← Previous"
                  #:url (guix-url (video->url previous)))
                 "")
            ,(if next
                 (button-big
                  #:label "Next →"
                  #:url (guix-url (video->url next)))
                 ""))
          ""))))
