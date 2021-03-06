;;; GNU Guix web site
;;; Public domain 2021 Luis Felipe López Acevedo
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps base templates help)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (help-t))


(define (help-t)
  "Return the Help page in SHTML."
  (theme
   #:title (C_ "webpage title" '("Help"))
   #:description
   (G_ "A list of resources about how to use GNU Guix, plus
   information about getting help from the community of users and
   developers.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|Help resources") #\|)
   #:active-menu-item (C_ "website menu" "Help")
   #:css (list
	  (guix-url "static/base/css/page.css")
	  (guix-url "static/base/css/item-preview.css"))
   #:crumbs (list (crumb (C_ "website menu" "Help") "./"))
   #:content
   `(main
     (section
      (@ (class "page"))
      ,(G_ `(h2 "Help"))

      (div
       (@ (class "centered-text"))


       (div
	(@ (class "summary-box"))
	(img
	 (@ (src ,(guix-url "static/base/img/manual-icon.png"))
	    (alt "")))
        ,(G_ `(h3 "GNU Guix Manual " ,(latest-guix-version) ""))
        ,(G_
          `(p
            "Documentation for GNU Guix is available
            online.  You may also find more information about Guix by running "
            ,(G_ `(code "info guix")) "."))
        (p
         ,(link-more #:label (G_ (string-append "Read manual "
                                                (latest-guix-version) ""))
                     #:url (guix-url "manual/en" #:localize #f)))
        (p
         (a (@ (href ,(guix-url "manual/de" #:localize #f))) "Deutsch") " | "
         (a (@ (href ,(guix-url "manual/en" #:localize #f))) "English") " | "
         (a (@ (href ,(guix-url "manual/es" #:localize #f))) "español") " | "
         (a (@ (href ,(guix-url "manual/fr" #:localize #f))) "français") " | "
         (a (@ (href ,(guix-url "manual/ru" #:localize #f))) "русский")  " | "
         (a (@ (href ,(guix-url "manual/zh-cn" #:localize #f))) "简体中文"))

        ,(link-more
          #:label (G_ "Get Guix reference card")
	  #:url (guix-url "guix-refcard.pdf")))


       (div
	(@ (class "summary-box"))
	(img
	 (@ (src ,(guix-url "static/base/img/manual-latest-icon.png"))
	    (alt "")))
        ,(G_ `(h3 "GNU Guix Manual (Latest)"))
        ,(G_
          `(p
            "This version of the manual is updated frequently to
            include the latest changes from Guix's source files. It is
            more up-to-date than the manual for the release of Guix."))
        (p
         ,(link-more #:label (G_ "Read latest manual")
                     #:url (guix-url "manual/devel/" #:localize #f))))


       (div
        (@ (class "summary-box"))
        (img (@ (src ,(guix-url "static/base/img/videos-icon.png"))
                (alt "")))
        ,(G_ `(h3 "Videos"))
        ,(G_
          `(p
            "The collection of videos includes instructional material
            to help you get started with every day use of GNU Guix as
            well as other topics that present advanced features of the
            system."))
        (p
         ,(link-more
           #:label (G_ "Browse all videos")
           #:url (guix-url "videos/"))))


       (div
        (@ (class "summary-box"))
        (img (@ (src ,(guix-url "static/base/img/cookbook-icon.png"))
                (alt "")))
        ,(G_ `(h3 "Cookbook"))
        ,(G_
          `(p
            "Tutorials, how-to guides and examples contributed by the
            Guix community which show you how to use the system and its
            collection of packages to achieve common and not-so-common
            goals users may have."))
        (p
         ,(link-more
           #:label (G_ "Browse the recipes")
           #:url (guix-url "cookbook/" #:localize #f))))


       (div
	(@ (class "summary-box"))
	(img (@ (src ,(guix-url "static/base/img/library-icon.png"))
		(alt "")))
        ,(G_ `(h3 "GNU Manuals"))
        ,(G_
          `(p
            "Guix is a distribution of the "
            ,(G_ `(a (@ (href ,(gnu-url))) "GNU operating system"))
            ".  Documentation for GNU packages is
            available online in various formats. "))
	(p
	 ,(link-more
           #:label (G_ "Browse GNU manuals")
	   #:url (gnu-url "manual"))))


       (div
	(@ (class "summary-box"))
	(img (@ (src ,(guix-url "static/base/img/wiki-icon.png"))
		(alt "")))
        ,(G_ `(h3 "Wiki"))
        ,(G_
          `(p
            "The LibrePlanet Wiki provides a collaborative space for "
            "people to share additional information about the Guix "
            "project and its subprojects. It may contain help "
            "information, feature proposals, and notes about work in "
            "progress (among other things)."))
	(p
	 ,(link-more
           #:label (G_ "Browse the wiki")
	   #:url "https://libreplanet.org/wiki/Group:Guix")))


       (div
	(@ (class "summary-box"))
	(img (@ (src ,(guix-url "static/base/img/chat-icon.png"))
		(alt "")))
        ,(G_ `(h3 "IRC Chat"))
        ,(G_
          `(p
            "For real-time support from the community, you can connect
            to the " (code "#guix") " channel on irc.libera.chat. There
            you can get help about anything related to GNU Guix."))
        ,(G_
          `(p
            "The " (code "#guix") " channel is logged. Previous
            conversations can be browsed online. See the "
            ,(G_ `(a (@ (href ,guix-irc-log-url)) "channel logs")) ". "))
	(p
	 ,(link-more
           #:label (G_ "Connect")
	   #:url (guix-url "contact/irc/"))))


       (div
	(@ (class "summary-box"))
	(img (@ (src ,(guix-url "static/base/img/email-icon.png"))
		(alt "")))
        ,(G_ `(h3 "Mailing lists"))
        ,(G_
          `(p
            "Email support from the community is also available through
            several mailing list. The messages sent to the lists are
            public and archived online."))

	(p
	 ,(link-more
           #:label (G_ "See all lists")
	   #:url (guix-url "contact/")))))))))
