;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media templates screenshots-overview)
  #:use-module (apps base templates theme)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:use-module (apps media templates components)
  #:export (screenshots-overview-t))


(define (screenshots-overview-t screenshots)
  "Return an SHTML page for all SCREENSHOTS."
  (theme
   #:title (C_ "webpage title" '("Screenshots"))
   #:description (G_ "Overview of all screenshots.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|GNU Guile|Guile \
Scheme|Transactional upgrades|Functional package \
management|Reproducibility") #\|)
   #:active-menu-item (C_ "website menu" "Media")
   #:css (list (guix-url "static/base/css/index.css"))
   #:content
   `(main
     (section
      (@ (class "light-text centered-text noise-bg"))
      ,(screenshots-box screenshots (length screenshots) #:shadow #t)))))
