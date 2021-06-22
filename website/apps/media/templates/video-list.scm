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
  #:use-module (apps media templates components)
  #:use-module (apps media types)
  #:use-module (apps media utils)
  #:export (video-list-t))


(define (video-list-t context)
  "Return an SHTML page listing all videos in the CONTEXT."
  (let ((items (context-datum context "items"))
        (page-number
	 (number->string (context-datum context "page-number")))
	(total-pages
	 (number->string (context-datum context "total-pages"))))

    (theme
     #:title (C_ "webpage title" '("Videos"))
     #:description
     (G_ "Videos about GNU Guix.")
     #:keywords
     (string-split   ;TRANSLATORS: |-separated list of webpage keywords
      (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|Help resources|Videos") #\|)
     #:active-menu-item (C_ "website menu" "Videos")
     #:css (list
            (guix-url "static/media/css/video-list.css")
            (guix-url "static/media/css/video-preview.css"))
     #:crumbs (list (crumb (C_ "website menu" "Videos") (guix-url "videos/")))
     #:content
     `(main
       (section
        (@ (class "light-text centered-text"))

        ,(G_ `(h2 "Videos"))

        ,(if (string=? page-number "1")
             (G_
              `(p
                (@ (class "limit-width centered-block"))
                "The following is a list of videos introducing GNU Guix and its many features as well as showing how it is used in different fields. These videos come from different sources; some come from the project itself, some from the community of users, and some from independent reviewers and organizations."))
             "")

        ,(page-indicator (string->number page-number)
			     (string->number total-pages))

        (div
         (@ (class "centered-text"))

         ,@(map
            (lambda (item)
              (cond ((video? item) (video-preview item))
                    (else (playlist-preview item))))
            items)

         ,(page-selector (string->number total-pages)
			 (string->number page-number)
			 (guix-url "videos"))))))))
