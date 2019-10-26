;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media utils)
  #:use-module (apps aux web)
  #:use-module (apps media types)
  #:use-module (ice-9 regex)
  #:export (video->url))


(define (video->url video)
  (url-path-join
   "videos"
   (string-downcase
    (regexp-substitute/global #f "[ \t]+" (video-title video)
                              'pre "-" 'post))
   "index.html"))
