;;; GNU Guix web site
;;; Public domain 2020 Luis Felipe López Acevedo
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps base templates theme)
  #:use-module (apps base templates components)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (theme))


(define* (theme #:key
                (lang-tag %current-ietf-tag)
		(title '())
		(description "")
		(keywords '())
                (index? #true)
                (active-menu-item (C_ "website menu" "About"))
		(css '())
		(scripts '())
		(crumbs '())
		(content '(div "")))
  "Return an SHTML document using the website's theme.

   LANG-TAG (string)
     IETF language tag. This is used to specify the language of the
     document. For example: en, en-CA. If not provided, the value
     defaults to the currently built language, i.e. the
     %current-ietf-tag from (apps i18n).

   TITLE (list)
     A list of strings to form the value of the title element of the
     document. The elements of the list are joined together with em
     dashes as separators between them. For example, a list with two
     strings like 'Hello', and 'Blog' will result in a title like
     'Hello — Blog — Guix'.

   DESCRIPTION (string)
     The description of the document. This is the value used for the
     description meta element.

   KEYWORDS (list)
     A list of keyword strings that will be used as the value for
     the keywords meta element of the document.

   INDEX? (boolean)
     Indicate whether the page should be indexed by Internet robots,
     such as search engine robots. If not provided, it defaults to
     true.

   ACTIVE-MENU-ITEM (string)
     The label of the menu item in the navigation bar that should be
     highlighted to indicate the current section of the website that
     is being browsed. If not provided, the value defaults to 'About'.

   CSS (list)
     A list of strings that represent absolute URL paths to additional
     style sheets. For example: '/static/app/css/style.css'. If not
     provided, the value defaults to an empty list.

   SCRIPTS (list)
     A list of strings that represent absolute URL paths to additional
     script files. For example: '/static/app/js/builds.js'. If not
     provided, the value defaults to an empty list.

   CRUMBS (list)
     A list of <crumb> objects as defined in (apps base types). This
     objects are used to form the breadcrumbs of the website.

   CONTENT (SHTML)
     A main element with the content of the page. For example:
     '(main (h2 'Hello World!') (p 'Once upon a time...'))."
  `((doctype "html")

    (html
     (@ (lang ,lang-tag))

     (head
      ,(if (null? title)
           `(title ,(C_ "webpage title" "GNU Guix"))
           `(title ,(string-join (append title
                                         (C_ "webpage title" '("GNU Guix")))
                                 " — ")))
      (meta (@ (charset "UTF-8")))
      (meta (@ (name "keywords") (content ,(string-join keywords ", "))))
      (meta (@ (name "description") (content ,description)))
      (meta (@ (name "viewport") (content "width=device-width, initial-scale=1.0")))
      ;; Info for Internet robots.
      ,(if index?
           ""
           '(meta (@ (name "robots") (content "noindex"))))
      ;; Menu prefetch.
      (link (@ (rel "prefetch") (href ,(guix-url "menu/index.html"))))
      ;; Base CSS.
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/elements.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/common.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/messages.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/navbar.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/breadcrumbs.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/buttons.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/footer.css"))))
      ;; Additional CSS.
      ,@(map (lambda (style-sheet)
	       `(link (@ (rel "stylesheet") (href ,style-sheet))))
	     css)
      ;; Feeds.
      (link (@ (type "application/atom+xml") (rel "alternate")
               (title ,(C_ "webpage title" "GNU Guix — Activity Feed"))
	       (href ,(guix-url "feeds/blog.atom"))))
      (link (@ (rel "icon") (type "image/png")
	       (href ,(guix-url "static/base/img/icon.png"))))
      (link (@ (rel "icon") (type "image/svg+xml") (sizes "any")
	       (href ,(guix-url "static/base/img/icon.svg"))))
      ;; Additional scripts.
      ,@(map (lambda (script)
	       `(script (@ (src ,script)) ""))
	     scripts))

     (body
      ,(navbar #:active-item active-menu-item)

      ;; NOTE: Comment these messages out when they are not needed anymore.
      ;(div
      ; (@ (class "message-box msg-info"))
      ; (p ,(G_ `("Online conference February 19-20. "
      ;           ,(G_ `(a
      ;                  (@ (href "/blog/2022/online-guix-days-2022-announcement-1/"))
      ;                  "Learn more"))
      ;           "!"))))
      ;(div
      ; (@ (class "message-box msg-info"))
      ; (p ,(G_ `("Online conference February 19-20. "
      ;           "Watch the "
      ;           ,(G_ `(a
      ;      (@ (href "https://xana.lepiller.eu/guix-days-2022/"))
      ;      "pre-recorded talks"))
      ;           "."
      ;           ,(G_ `(a
      ;      (@ (href "https://meet.univ-grenoble-alpes.fr/b/pie-uia-2a2-wzl"))
      ;      "Join us"))
      ;           "! Learn "
      ;           ,(G_ `(a
      ;      (@ (href ,(guix-url "blog/2022/online-guix-days-2022-announcement-2/")))
      ;      "more"))
      ;           "!"))))

      ,(if (null? crumbs) "" (breadcrumbs crumbs))

      ,content
      ,(G_
        `(footer
          "Made with " ,(G_ `(span (@ (class "metta")) "♥"))
          " by humans and powered by "
          ,(G_ `(a
                 (@ (class "link-yellow")
                    (href ,(gnu-url "software/guile/")))
                 "GNU Guile"))
          ".  "
          ,(G_ `(a
                 (@ (class "link-yellow")
                    (href "//git.savannah.gnu.org/cgit/guix/guix-artwork.git/tree/website"))
                 "Source code"))
          " under the "
          ,(G_ `(a
                 (@ (class "link-yellow")
                    (href ,(gnu-url "licenses/agpl-3.0.html")))
                 "GNU AGPL"))
          "."))))))
