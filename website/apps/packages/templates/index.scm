;;; GNU Guix web site
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps packages templates index)
  #:use-module (apps aux web)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:use-module (apps packages templates components)
  #:use-module (srfi srfi-19)
  #:export (index-t))


(define (index-t context)
  "Return an SHTML representation of the index page."
  (let ((packages (context-datum context "packages"))
	(total    (context-datum context "total")))
    (theme
     #:title (C_ "webpage title" (list "Packages"))
     #:description
     (G_ "List of packages available through GNU Guix.")
     #:keywords
     (string-split ;TRANSLATORS: |-separated list of webpage keywords
      (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|GNU Guile|Guile \
Scheme|Transactional upgrades|Functional package \
management|Reproducibility") #\|)
     #:active-menu-item (C_ "website menu" "Packages")
     #:css
     (list (guix-url "static/base/css/page.css")
	   (guix-url "static/base/css/item-preview.css")
	   (guix-url "static/packages/css/letter-selector.css"))
     #:crumbs
     (list (crumb (C_ "website menu" "Packages") (guix-url "packages/")))
     #:content
     `(main
       (section
	(@ (class "page centered-text"))
        ,(G_ `(h2 "Packages"))

        ,(G_
          `(p
            (@ (class "limit-width centered-block"))
            "GNU Guix provides " ,(number* total) " packages transparently "
            ,(G_
              `(a (@ (href "https://www.gnu.org/software/guix/manual/en/html_node/Substitutes.html"))
                  "available as pre-built binaries"))
            ". These pages provide a complete list of the packages.  Our "
            ,(G_
              `(a (@ (href "https://ci.guix.gnu.org/jobset/master"))
                  "continuous integration system"))
            " shows their current build status "
            "(updated " ,(date->string (current-date)
                                       (C_ "SRFI-19 date->string format"
                                           "~B ~e, ~Y")) ")."))

	(div
	 (@ (class "sheet"))
	 ,(letter-selector)
	 ,@(map package-preview packages)
	 ,(letter-selector)))))))
