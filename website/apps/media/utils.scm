;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media utils)
  #:use-module (apps aux web)
  #:use-module (apps media types)
  #:export (video->url))


(define (video->url video)
  (url-path-join
   "videos"
   (video-page-subpath video)
   "index.html"))
