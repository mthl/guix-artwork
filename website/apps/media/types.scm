;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media types)
  #:use-module (srfi srfi-9)
  #:export (playlist
            playlist?
            playlist-authors
            playlist-date
            playlist-title
            playlist-videos
            publication
            publication?
            publication-authors
            publication-scientific?
            publication-date
            publication-language
            publication-title
            publication-type
            publication-url
            screenshot
            screenshot?
            screenshot-caption
            screenshot-image
            screenshot-preview
            screenshot-slug
            screenshot-title
            track
            track?
            track-label
            track-kind
            track-lang
            track-url
            video
            video?
            video-authors
            video-date
            video-description
            video-slug
            video-title
            video-tracks
            video-url))


;;;
;;; Data types.
;;;

;;; Playlist (record type)
;;; ----------------------
;;;
;;; A playlist object represents a series of related videos that talk
;;; about GNU Guix.
;;;
;;; Create new objects of this type using the "playlist" constructor
;;; procedure (see below). You can find examples of its usage in the
;;; (apps media data) module.
;;;
;;; Fields:
;;;
;;; title (string)
;;;   The title of the playlist.
;;;
;;; authors (string)
;;;   The name(s) of the author(s) of the playlist.
;;;
;;; date (date)
;;;   The date of publication.
;;;
;;; videos (list of <video>)
;;;   See the Video data type in this same module.
;;;
(define-record-type <playlist>
  (make-playlist title authors date videos)
  playlist?
  (title playlist-title)
  (authors playlist-authors)
  (date playlist-date)
  (videos playlist-videos))

;;; Constructor.

(define* (playlist #:key title authors date videos)
  "Return a <playlist> object with the given attributes."
  (make-playlist title authors date videos))


;;; Publication (record type)
;;; -------------------------
;;;
;;; A publication object represents a written material that talks about
;;; GNU Guix.
;;;
;;; Objects of this type can be created with the "publication" procedure
;;; (see Helper procedures below).
;;;
;;; Fields:
;;;
;;; title (string)
;;;   The title of the publication.
;;;
;;; url (string)
;;;   A URL to the publication.
;;;
;;; authors (string)
;;;   The names of the authors.
;;;
;;; date (date)
;;;   The date of publication.
;;;
;;; language (string)
;;;   IETF language tag corresponding to the language in which the
;;;   publication is written.
;;;
;;; type (string)
;;;   The kind of publication. See the list of publications in the
;;;   (apps media data) module for examples.
;;;
;;; scientific? (boolean)
;;;   Whether the publication is scientific or not.
;;;
;;;
(define-record-type <publication>
  (make-publication title url authors date language type scientific?)
  publication?
  (title publication-title)
  (url publication-url)
  (authors publication-authors)
  (date publication-date)
  (language publication-language)
  (type publication-type)
  (scientific? publication-scientific?))

;;; Helper procedures.

(define* (publication #:key title url authors date (language "en") type
                      (scientific? #true))
  "Return a <publication> object with the given attributes."
  (make-publication title url authors date language type scientific?))


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


;;; Track (record type)
;;; -------------------
;;;
;;; A track object represents subtitles, closed captions or similar
;;; text tracks for HTML video. Track objects are mapped to HTML
;;; <track> elements.
;;;
;;; Create new objects of this type using the "track" constructor
;;; procedure (see below). You can find examples of its usage in the
;;; (apps media data) module.
;;;
;;; Fields:
;;;
;;; label (string)
;;;   The title of the track. For example: "English"
;;;
;;; kind (string)
;;;   The kind of track. It can be one of: "subtitles" or "captions".
;;;
;;; lang (string)
;;;   IETF language code. For example: "en-CA", "es", "ja".
;;;
;;; url (string)
;;;   Optional URL to the track file. If not provided, the track file is
;;;   expected to be located at
;;;
;;;   /static/media/videos/YEAR/SLUG-KIND.LANG.vtt
;;;
;;;   Where YEAR is the year of publication of the video the track is
;;;   related to, SLUG is the slug of the same video, and KIND and LANG
;;;   are the values of those attributes in the track.
;;;
(define-record-type <track>
  (make-track label kind lang url)
  track?
  (label track-label)
  (kind track-kind)
  (lang track-lang)
  (url track-url))

;;; Constructor.

(define* (track #:key label kind lang (url ""))
  "Return a <track> object with the given attributes."
  (make-track label kind lang url))


;;; Video (record type)
;;; -------------------
;;;
;;; A video object represents something viewable in an HTML video
;;; element and accessible from the videos list on the website.
;;;
;;; Create new objects of this type using the "video" constructor
;;; procedure (see below). You can find examples of its usage in the
;;; (apps media data) module.
;;;
;;; Fields:
;;;
;;; title (string)
;;;   The full name of the video.  For example:
;;;   "Everyday use of GNU Guix, Part One".
;;;
;;; authors (string)
;;;   The name(s) of the author(s) of the video.
;;;
;;; description (string)
;;;   Optional description of the video. If not provided, it defaults
;;;   to an empty string.
;;;
;;; url (string)
;;;   A URL to the video file.
;;;
;;; slug (string)
;;;   A unique subpath for the webpage of the video.  It should
;;;   correspond to the English video title converted to lower case with
;;;   spaces replaced by hyphens.  For example: 'installing-gnu-guix'.
;;;
;;;   The slug must be unique among the slugs of the videos published in
;;;   the same year. So, two videos titled 'Installing GNU Guix' and
;;;   published in 2021 can't use the same slug.
;;;
;;; tracks (list of <track>)
;;;   Optional list of subtitles and captions. See the Track data type
;;;   in this same module.
;;;
;;;   If not provided, it defaults to an empty list.
;;;
;;; date (date)
;;;   SRFI-19 date of the time the video was published.
;;;
(define-record-type <video>
  (make-video title authors description url slug tracks date)
  video?
  (title video-title)
  (authors video-authors)
  (description video-description)
  (url video-url)
  (slug video-slug)
  (tracks video-tracks)
  (date video-date))

;;; Constructor.

(define* (video #:key title authors (description "") url slug
                (tracks '()) date)
  "Return a <video> object with the given attributes."
  (make-video title authors description url slug tracks date))
