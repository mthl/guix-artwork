;;; GNU Guix web site
;;; Public domain 2021 Luis Felipe López Acevedo

(define-module (apps media templates publication-list)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:use-module ((apps media types) #:select (publication-date))
  #:use-module (apps media templates components)
  #:use-module (srfi srfi-19)
  #:export (publication-list-t))


(define (publication-list-t publications)
  "Return the Publication list page in SHTML.

   PUBLICATIONS (list of <publication>)
     See the (apps media types) module for information on the
     <publication> type."
  (theme
   #:title (C_ "webpage title" '("Publications"))
   #:description
   (G_ "A list of written publications about GNU Guix.")
   #:keywords
   ;; TRANSLATORS: |-separated list of webpage keywords.
   (string-split (G_ "Publications|Papers") #\|)
   #:active-menu-item (C_ "website menu" "Publications")
   #:css (list
	  (guix-url "static/base/css/page.css")
	  (guix-url "static/media/css/publications.css"))
   #:crumbs (list (crumb (C_ "website menu" "Publications") "./"))
   #:content
   `(main
     (section
      (@ (class "page"))
      ,(G_ `(h2 "Publications"))

      ,(G_
        `(p
          (@ (class "centered-block limit-width"))

          "The following is a list of written materials that talk about GNU Guix. It is a diverse collection of writings, from blog posts to grey literature to academic and research papers."))

      (div
       (@ (class "publication-list centered-block limit-width"))

       ,@(map publication->shtml
              ;; Show newest publications first.
              (sort publications
                    (lambda (p1 p2)
                      (time<? (date->time-utc (publication-date p2))
                              (date->time-utc (publication-date p1)))))))))))
