;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps media templates screenshots-overview)
  #:use-module (apps base templates theme)
  #:use-module (apps base utils)
  #:use-module (apps media templates components)
  #:export (screenshots-overview-t))


(define (screenshots-overview-t screenshots)
  "Return an SHTML page for all SCREENSHOTS."
  (theme
   #:title '("Screenshots")
   #:description "Overview of all screenshots."
   #:keywords
   '("GNU" "Linux" "Unix" "Free software" "Libre software"
     "Operating system" "GNU Hurd" "GNU Guix package manager"
     "GNU Guile" "Guile Scheme" "Transactional upgrades"
     "Functional package management" "Reproducibility")
   #:active-menu-item "Media"
   #:css (list (guix-url "static/base/css/index.css")
               (guix-url "static/base/css/screenshots.css"))
   #:content
   `(main
     (section
      (@ (class "light-text centered-text noise-bg"))
      ,(screenshots-box screenshots (length screenshots) #:shadow #t)))))
