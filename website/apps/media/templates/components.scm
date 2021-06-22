;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

;;; This module defines HTML parts related to media.

(define-module (apps media templates components)
  #:use-module (apps aux lists)
  #:use-module (apps aux web)
  #:use-module (apps base templates components)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:use-module (apps media types)
  #:use-module (apps media utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (playlist-preview
            publication->shtml
            screenshot->shtml
            screenshots-box
            video-preview
            video-track))


;;;
;;; Components.
;;;

(define (playlist-preview playlist)
  "Return an SHTML representation of the given playlist.

   PLAYLIST (<playlist>)
     A playlist object as defined in (apps media types)."
  (let* ((date
          (date->string (playlist-date playlist)
                        (C_ "SRFI-19 date->string format" "~b ~d, ~Y")))
         (video (first (playlist-videos playlist)))
         (year (date-year (video-date video)))
         (poster-name (string-append (video-slug video) ".mini.webp"))
         (poster-path (guix-url (url-path-join "static" "media" "videos"
                                               (number->string year)
                                               poster-name)))
         (video-path (guix-url (url-path-join "videos"
                                              (number->string year)
                                              (video-slug video)
                                              ""))))

    `(a
      (@ (class "playlist-preview")
         (href ,video-path))

      (div
       (@ (class "poster-box"))

       (img
        (@ (class "poster") (src ,poster-path) (alt "")))
       (div (@ (class "poster-shadow")) ""))

      (div
       (@ (class "playlist-title"))

       (img (@ (class "playlist-icon")
               (src ,(guix-url "static/media/img/playlist-icon.svg"))
               (alt ,(G_ "Playlist: "))
               (title ,(G_ "Playlist"))))
       " "
       ,(playlist-title playlist))

      (div
       (@ (class "playlist-info"))
       ;; TRANSLATORS: <1/> is a date, <2/> is a list of authors.
       ,(G_
         `("Published " ,date
           " by " ,(playlist-authors playlist) ""))))))


(define (publication->shtml publication)
  "Return an SHTML representation of the given publication object.

   PUBLICATION (<publication>)
     A publication object as defined in (apps media types)."
  (let ((date
         (date->string (publication-date publication)
                       (C_ "SRFI-19 date->string format" "~b ~d, ~Y"))))

    `(a
      (@ (class "publication-preview")
         (href ,(publication-url publication)))

      (h3
       (@ (lang ,(publication-language publication))
          (class "publication-title"))
       ,(if (publication-scientific? publication)
            `((img
               (@ (class "scientific-mark")
                  (src ,(guix-url "static/media/img/scientific-mark.svg"))
                  ;; TRANSLATORS: This is a tag that indicates a
                  ;; publication is scientific.
                  (alt ,(G_ "[Scientific]"))
                  ;; TRANSLATORS: This is a help text indicating a
                  ;; publication is scientific.
                  (title ,(G_ "Scientific"))))
              " ")
            "")
       ,(publication-title publication))

      (p
       (@ (class "publication-info"))
       ;; TRANSLATORS: <1/> is a publication type, <2/> is a date, and
       ;; <3/> is a list of authors.
       ,(G_
         `("" ,(publication-type publication) ". Published " ,date
           " by " ,(publication-authors publication) ". "))))))


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


(define* (video-preview video #:key (playing? #false))
  "Return an SHTML representation of the given video.

   VIDEO (<video>)
     A video object as defined in (apps media types).

   PLAYING? (boolean)
     Indicate whether the video preview should display an icon
     indicating that the video is currently playing."
  (let* ((date
          (date->string (video-date video)
                        (C_ "SRFI-19 date->string format" "~b ~d, ~Y")))
         (year (date-year (video-date video)))
         (poster-name (string-append (video-slug video) ".mini.webp"))
         (poster-path (guix-url (url-path-join "static" "media" "videos"
                                               (number->string year)
                                               poster-name)))
         (video-path (guix-url (url-path-join "videos"
                                              (number->string year)
                                              (video-slug video)
                                              ""))))

    `(a
      (@ (class "video-preview")
         (href ,video-path))

      (div
       (@ (class "poster-box"))

       (img
        (@ (class "poster") (src ,poster-path) (alt "")))
       (div (@ (class "poster-shadow")) ""))

      (div
       (@ (class "video-title"))

       ,(if playing?
            `((img (@ (class "playing-icon")
                      (src ,(guix-url "static/media/img/playing-icon.svg"))

                      (alt
                       ;; TRANSLATORS: Alternative text indicating that
                       ;; a video is playing.
                       ,(G_ "Playing: "))
                      (title
                       ;; TRANSLATORS: Tool tip indicating that a video
                       ;; is playing.
                       ,(G_ "Playing"))))
              " ")
            "")

       ,(video-title video))

      (div
       (@ (class "video-info"))
       ;; TRANSLATORS: <1/> is a date, <2/> is a list of authors.
       ,(G_
         `("Published " ,date
           " by " ,(video-authors video) ""))))))


(define (video-track track video)
  "Return an SHTML track element representing the given track.

   TRACK (<track>)
     A track object as defined in the (apps media types) module.

   VIDEO (<video>)
     The video the track is related to. See (apps media types) for
     details on the Video type."
  (let* ((year (date-year (video-date video)))
         (kind (track-kind track))
         (lang (track-lang track))
         (track-name (string-append (video-slug video) "-" kind "."
                                    lang ".vtt"))
         (track-path (guix-url (url-path-join "static" "media" "videos"
                                              (number->string year)
                                              track-name)))
         (url (if (string-null? (track-url track))
                  track-path
                  (track-url track))))

    `(track
      (@ (label ,(track-label track))
         (kind ,kind)
         (srclang ,lang)
         (src ,url)))))
