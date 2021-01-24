;;; GNU Guix web site
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps base templates cuirass)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (cuirass-t))

(define (cuirass-t)
  "Return the Cuirass page in SHTML."
  (theme
   #:title (C_ "webpage title" '("Cuirass"))
   #:description
   (G_ "GNU Guix continuous integration software.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Guix package manager|Cuirass|CI") #\|)
   #:css (list
          (guix-url "static/base/css/page.css"))
   #:content
   `(main
     (section
      (@ (class "page centered-block limit-width"))
      (h2 (img (@ (src ,(guix-url "static/base/img/cuirass.png"))
                  (alt "Cuirass"))))
      ,(G_ `(p "Cuirass is the GNU Guix continuous
integration software. It's a general purpose build automation server written
in GNU Guile that checks out sources from VCS repositories, execute build jobs
and store build results in a database. Cuirass also provides a web interface
to monitor the build results."))

      ,(G_ `(p "Cuirass is running on GNU Guix build farm at "
               `(a (@ (href "https://ci.guix.gnu.org"))
                   "https://ci.guix.gnu.org")
               "."))
      ,(G_ `(h3 "Features"))
      (ul
       (li
        ,(G_ "Poll sources from multiple Git repositories."))
       (li
        ,(G_ "Execute builds on local or remote Guix daemons."))
       (li
        ,(G_ "Store build products or artifacts."))
       (li
        ,(G_ "Collect build logs and metrics."))
       (li
        ,(G_ "Provides a REST API."))
       (li
        ,(G_ "High level of concurrency using ")
        (a (@ (href "https://github.com/wingo/fibers"))
           "Guile-Fibers")
        ,(G_ " asynchronous library.")))
      ,(G_ `(h3 "Documentation"))
      ,(G_ `(p "Cuirass documentation is accessible "
               ,(G_ `(a (@ (href ,(manual-url "Continuous-Integration.html")))
                        "here"))
               "."))
      ,(G_ `(h3 "Project repository"))
      ,(G_ `(p "Cuirass source code is hosted at "
               ,(G_ `(a (@ (href "//git.savannah.gnu.org/cgit/guix/guix-cuirass.git"))
                        "git://git.sv.gnu.org/guix/guix-cuirass.git"))
               "."))
      ))))
