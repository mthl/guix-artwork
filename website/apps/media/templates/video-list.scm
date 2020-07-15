;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media templates video-list)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:use-module (apps media data)
  #:use-module (apps media templates components)
  #:use-module (apps media types)
  #:use-module (apps media utils)
  #:export (video-list-t))


(define (video-list-t)
  "Return a list of videos in SHTML."
  (theme
   #:title (C_ "webpage title" '("Videos"))
   #:description
   (G_ "Video about GNU Guix.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|Help resources|Videos") #\|)
   #:active-menu-item (C_ "website menu" "Media")
   #:css (list
          (guix-url "static/base/css/page.css")
          (guix-url "static/base/css/index.css"))
   #:crumbs (list (crumb (C_ "website menu" "Videos") (guix-url "videos/")))
   #:content
   `(main
     (@ (class "page centered-block limit-width"))
     ,(map-in-order
       (lambda (playlist)
         `(,(map-in-order
             (lambda (video)
               `((h2 ,(link-subtle
                       #:label (video-title video)
                       #:url (guix-url (video->url video))))
                 ,(video-content video)))
             playlist)
           ,(horizontal-separator)))
       playlists))))
