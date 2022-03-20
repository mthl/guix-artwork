;;; GNU Guix web site
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps base templates contact)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (contact-t))


(define (contact-t context)
  "Return the Contact page in SHTML with the data in CONTEXT."
  (theme
   #:title (C_ "webpage title" '("Contact"))
   #:description
   (G_ "A list of channels to communicate with GNU Guix users
   and developers about anything you want.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|Community|Mailing lists|IRC \
channels|Bug reports|Help") #\|)
   #:active-menu-item (C_ "website menu" "About")
   #:css (list
	  (guix-url "static/base/css/page.css")
          (guix-url "static/base/css/buttons.css")
	  (guix-url "static/base/css/contact.css"))
   #:crumbs (list (crumb (C_ "website menu" "Contact") "./"))
   #:content
   `(main
     (section
      (@ (class "page centered-block limit-width"))
      ,(G_ `(h2 "Contact"))

      ,(G_
        `(p
          "We want to provide a warm, friendly, and harassment-free environment,
           so that anyone can contribute to the best of their abilities.  To
           this end our project uses a “Contributor Covenant”, which was adapted
           from "
          ,(G_ ((lambda (url)
                  `(a (@ (href ,url)) ,url))
                "https://contributor-covenant.org/"))
          ".  You can find the full pledge in the "
          ,(G_
            `(a (@ (href "//git.savannah.gnu.org/cgit/guix.git/tree/CODE-OF-CONDUCT")
                   (class "mono"))
                "CODE-OF-CONDUCT"))
          " file."))

      ,(G_
        `(p "Participation to the project communication channels listed below
            is subject to this code of conduct."))

      ,@(map
	 contact->shtml
	 (context-datum context "contact-media"))))))
