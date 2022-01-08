;;; GNU Guix web site
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps base templates donate)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (donate-t))


(define (donate-t)
  "Return the Donate page in SHTML."
  (theme
   #:title (C_ "webpage title" '("Donate"))
   #:description
   (G_ "We are looking for donations of hardware and optionally
   hosting for machines (they should be usable with exclusively
   free software).")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|Donations") #\|)
   #:active-menu-item (C_ "website menu" "Donate")
   #:css (list
	  (guix-url "static/base/css/page.css"))
   #:crumbs (list (crumb (C_ "website menu" "Donate") "./"))
   #:content
   `(main
     (section
      (@ (class "page centered-block limit-width"))
      ,(G_ `(h2 "Donate"))

      ,(G_
        `(p
          "The "
          ,(G_
            `(a (@ (href "https://ci.guix.gnu.org"))
                "build farm"))
          " of Guix runs on donated hardware and"
          " hosting. As the distribution grows (see the "
          ,(G_ `(a (@ (href ,(guix-url "packages/"))) "package list"))
          "), so do the computing and storage needs."))

      ,(G_
        `(p
          "Back in 2015 we "
          ,(G_ `(a (@ (href ,(guix-url "blog/2015/guix-starts-fundraising-campaign-with-support-from-the-fsf/")))
                   "ran a fundraising campaign"))
          " to strengthen our build farm, with "
          ,(G_
            `(a (@ (href "https://www.fsf.org/blogs/community/fsf-announces-support-for-gnu-guix"))
                "support from the Free Software Foundation (FSF)"))
          ".  The Guix project can always use financial support to further its "
          "mission.  Please consider helping out by making a donation on this
          FSF-hosted page:"))

      (p
       (@ (class "centered-text"))
       ,(button-big
         #:label (C_ "button" "♥ DONATE!")
	 #:url "https://my.fsf.org/civicrm/contribute/transact?reset=1&id=50"))

      ,(G_
        `(h3
          (@ (id "hardware-and-hosting"))
          "Hardware and Hosting"))

      ,(G_
        `(p
          "We are also looking for donations of hardware and optionally
           hosting for the following kinds of machines (they should be
           usable with exclusively free software): "))

      (ul
       ,(G_
         `(li "x86_64 machines, with on the order of 1\xa0TiB of storage
               and 4\xa0GiB of RAM;"))
       ,(G_
         `(li "armv7 machines (such as the Novena) to more quickly test
               and provide binaries for the armhf-linux port;"))
       ,(G_
         `(li "armv8 machines to more quickly test and provide binaries
              for the aarch64-linux port;"))
       ,(G_
         `(li "mips64el machines to strengthen this port.")))

      ,(G_
        `(p
          "Please get in touch with us through the "
          ,(G_ `(a (@ (href ,(guix-url "contact/"))) "usual channels"))
          " or using the " (b "guix-hardware@gnu.org") " private alias to
           discuss any opportunities. "))


      ,(G_
        `(h3
          (@ (id "hardware-donors"))
          "Thanks to the donors!"))

      ,(G_
        `(p
          "The table below summarizes hardware and hosting donations that
           make the " ,(G_ `(a (@ (href "https://ci.guix.gnu.org")) "build farm"))
           " for the Guix System Distribution a reality."))

      (div
       (@ (class "table-box"))
       (table
	(thead
         ,(G_ `(tr ,(G_ `(th "machine"))
                   ,(G_ `(th "system"))
                   ,(G_ `(th "donors")))))
	(tbody

         ,(G_ `(tr
                ,(G_ `(td "berlin.guix.gnu.org"))
                ,(G_ `(td "build farm with 25 build nodes for x86_64-linux and
i686-linux, and dedicated storage"))
                ,(G_ ((lambda content
                        `(td
                          (ul
                           (li
                            ,@content))))
                      (G_ `(a (@ (href "https://www.mdc-berlin.de/"))
                              "Max Delbrück Center for Molecular Medicine"))
                      " (hardware and hosting)"))))
         ,(G_ `(tr
                ,(G_ `(td "overdrive1"))
                ,(G_ `(td "aarch64-linux"))
                ,(G_ ((lambda content
                        `(td
                          (ul
                           (li
                            ,@content))))
                      (G_ `(a (@ (href ,(guix-url "blog/2018/aarch64-build-machines-donated/")))
                              "ARM Holdings") " (hardware)")))))
         ,(G_
           `(tr
             ,(G_ `(td "bayfront"))
             ,(G_ `(td "new build farm front-end (WIP)"))
             ,(G_ ((lambda (content)
                        `(td
                          (ul
                           (li
                            (a
                             (@ (href ,(guix-url "blog/2016/growing-our-build-farm/")))
                             ,content)))))
                   "Igalia"))))
         ,(G_
           `(tr
             ,(G_ `(td "fosshost1.guix.gnu.org, fosshost2.guix.gnu.org"))
             ,(G_ `(td "x86_64-linux, i686-linux"))
             ,(G_ ((lambda (content)
                     `(td
                       (ul
                        (li
                         (a
                          (@ (href "https://fosshost.org/"))
                          ,content)))))
                   "Fosshost"))))
         ,(G_ `(tr
                ,(G_ `(td "guix-x15.sjd.se, guix-x15b.sjd.se"))
                ,(G_ `(td "armhf-linux"))
                ,(G_ ((lambda (content)
                        `(td
                          (ul
                           (li
                            (a (@ (href "https://blog.josefsson.org/"))
                               ,content)))))
                      "Simon Josefsson"))))
         ,(G_ `(tr
                ,(G_ `(td "hydra-slave1"))
                ,(G_ `(td "armhf-linux"))
                ,(G_ ((lambda content
                        `(td
                          (ul
                           ,@content)))
                      (G_ `(li "Steve Sprang (hardware)"))
                      ;; XXX: Eventually move to the FSF?
                      (G_ `(li "Mark H Weaver (hosting)"))))))
         ,(G_ `(tr
                ,(G_ `(td "hydra-slave2"))
                ,(G_ `(td "armhf-linux"))
                ,(G_ ((lambda content
                        `(td
                          (ul
                           ,@content)))
                      (G_ `(li
                            ,(G_ `(a (@ (href "http://harmoninstruments.com/"))
                                     "Harmon Instruments"))
                            " (hardware)"))
                      ;; XXX: Eventually move to the FSF?
                      (G_ `(li "Mark H Weaver (hosting)"))))))
         ,(G_
           `(tr
             ,(G_ `(td "hydra-slave3"))
             ,(G_ `(td "armhf-linux"))
             ,(G_ ((lambda content
                     `(td
                       (ul
                        ,@content)))
                   (G_ `(li
                    ,(G_ `(a (@ (href "http://www.kosagi.com/w/index.php?title=Novena_Main_Page"))
                             "Kosagi (Sutajio Ko-Usagi Pte Ltd)"))
                    " (hardware)"))
                   (G_ `(li "Mark H Weaver (hosting)"))))))
         ,(G_
           `(tr
             ,(G_ `(td "redhill"))
             ,(G_ `(td "armhf-linux"))
             ,(G_ ((lambda content
                     `(td
                       (ul
                        ,@content)))
                   (G_ `(li
                    ,(G_ `(a (@ (href "http://www.kosagi.com/w/index.php?title=Novena_Main_Page"))
                             "Kosagi (Sutajio Ko-Usagi Pte Ltd)"))
                    " (hardware)"))
                   (G_ `(li "Andreas Enge (hosting)")))))))))

      ,(G_
        `(p "Other organizations and individuals helped Guix with hardware and
hosting in the past and we thank them: "
            ,(G_ `(a (@ (href "https://www.fsf.org")) "Free Software Foundation")) ", "
            "GNU España, "
            ,(G_ `(a (@ (href "https://fsffrance.org/index.en.html")) "FSF France")) ", "
            "and the Free Secure Network Systems Group at the "
            ,(G_ `(a (@ (href "https://www.tum.de/")) "Technische Universität München"))
            "."))))))
