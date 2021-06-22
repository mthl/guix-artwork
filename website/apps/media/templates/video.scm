;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media templates video)
  #:use-module (apps aux web)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:use-module (apps media templates components)
  #:use-module (apps media types)
  #:use-module (apps media utils)
  #:use-module (srfi srfi-19)
  #:export (video-t))


(define (video-t video playlist)
  "Return a page in SHTML for the given VIDEO.

   VIDEO (Video)
     A video object as defined in the (apps media types) module.

   PLAYLIST (Playlist or #false)
     The playlist the video is part of, if any. Otherwise, #false.

     See the (apps media types) module for the definition of Playlist."
  (let* ((date
          (date->string (video-date video)
                        (C_ "SRFI-19 date->string format" "~b ~d, ~Y")))
         (year (date-year (video-date video)))
         (poster-name (string-append (video-slug video) ".webp"))
         (poster-path (guix-url (url-path-join "static" "media" "videos"
                                               (number->string year)
                                               poster-name))))

    (theme
     #:title (list (video-title video) (G_ "Videos"))
     #:description (video-description video)
     #:keywords
     (string-split ;TRANSLATORS: |-separated list of webpage keywords
      (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|Help resources|Videos") #\|)
     #:active-menu-item (C_ "website menu" "Videos")
     #:css (list
            (guix-url "static/media/css/video.css")
            (guix-url "static/media/css/video-preview.css"))
     #:crumbs (list (crumb (C_ "website menu" "Videos") (guix-url "videos/"))
                    (crumb (video-title video) "./"))
     #:content
     `(main
       (@ (class "light-text centered-text noise-bg"))

       (div
        (@ (class "video-box"))

        (h2 ,(video-title video))

        (div
         (@ (class "centered-block limit-width"))

         (video
          (@ (src ,(video-url video))
             (poster ,poster-path)
	     (controls "controls"))

          ,@(map
             (lambda (track)
               (video-track track video))
             (video-tracks video))

          ,(G_ "It seems your browser can't read this video.")
          " "
          (a (@ (href ,(video-url video))) ,(G_ "Download video"))
          "."
          )))

       (div
        (@ (class "video-description top-shadow-bg centered-text"))

        (p
         (span
          (@ (class "video-date video-authors"))
          ;; TRANSLATORS: <1/> is a date, <2/> is a list of authors.
          ,(G_ `("Published " ,date " by " ,(video-authors video)
                 ". ")))
         ,(video-description video)))

       ,(if playlist
            `(div
              (@ (class "centered-text"))

              ,@(map
                 (lambda (item)
                   (if (string=? (video-slug video) (video-slug item))
                       (video-preview item #:playing? #true)
                       (video-preview item)))
                 (playlist-videos playlist)))
            "")))))
