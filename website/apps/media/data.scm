;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media data)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:use-module (apps media types)
  #:use-module (srfi srfi-19)
  #:export (playlists
            publications
            screenshots))


;;;
;;; Data.
;;;


(define playlists
  ;; List of "playlists" of related videos in proper order.
  (list
   (list
    (video
     #:title (C_ "video title" "Installation from Script")
     #:page-subpath "installation-from-script"
     #:description
     (G_ '(p "Explains how to install Guix on distributions not running
GNU Guix."))
     #:url "https://guix.gnu.org/guix-videos/01-installation-from-script.webm"
     #:poster (guix-url "static/videos/img/installation-from-script.png")
     #:last-updated (string->date "2020-03-28T16:00:00" "~Y-~m-~dT~H:~M:~S")))

   (list
    (video
     #:title (C_ "video title" "Everyday use of GNU Guix, Part One")
     #:page-subpath "everyday-use-of-gnu-guix,-part-one"
     #:description
     (G_ '(p "How to install packages and how to manage software package
generations."))
     #:url "https://guix.gnu.org/guix-videos/02-everyday-use-part-one.webm"
     #:poster (guix-url "static/videos/img/everyday-use-01.png")
     #:last-updated (string->date "2020-03-28T16:00:00" "~Y-~m-~dT~H:~M:~S"))
    (video
     #:title (C_ "video title" "Everyday use of GNU Guix, Part Two")
     #:page-subpath "everyday-use-of-gnu-guix,-part-two"
     #:description
     (G_ '(p "How to upgrade software and how to reclaim storage space."))
     #:url "https://guix.gnu.org/guix-videos/02-everyday-use-part-two.webm"
     #:poster (guix-url "static/videos/img/everyday-use-02.png")
     #:last-updated (string->date "2020-03-28T16:00:00" "~Y-~m-~dT~H:~M:~S")))

   (list
    (video
     #:title (C_ "video title"
                 "Demonstration of the Guix System graphical installer")
     #:page-subpath "system-graphical-installer"
     #:description
     (G_ '(p "Walks you through the graphical installer of GNU Guix System."))
     #:url "https://guix.gnu.org/guix-videos/guix-system-install-1.1.0.webm"
     #:poster (guix-url "static/videos/img/guix-system-install.png")
     #:last-updated (string->date "2020-04-15T16:00:00" "~Y-~m-~dT~H:~M:~S")))

   (list
    (video
     #:title (C_ "video title" "Asking for help")
     #:page-subpath "asking-for-help"
     #:description
     (G_ '(p "How to get help from the Guix community."))
     #:url "https://guix.gnu.org/guix-videos/03-help.webm"
     #:poster (guix-url "static/videos/img/help.png")
     #:last-updated (string->date "2020-03-28T16:00:00" "~Y-~m-~dT~H:~M:~S")))
   (list
    (video
     #:title (C_ "video title" "Packaging, Part One")
     #:page-subpath "packaging,-part-one"
     #:description
     (G_ '(p "How to set up a development environment for GNU Guix."))
     #:url "https://guix.gnu.org/guix-videos/04-packaging-part-one.webm"
     #:poster (guix-url "static/videos/img/packaging-01.png")
     #:last-updated (string->date "2020-03-28T16:00:00" "~Y-~m-~dT~H:~M:~S"))
    (video
     #:title (C_ "video title" "Packaging, Part Two")
     #:page-subpath "packaging,-part-two"
     #:description
     (G_ '(p "How to create a package recipe for not yet packaged software."))
     #:url "https://guix.gnu.org/guix-videos/04-packaging-part-two.webm"
     #:poster (guix-url "static/videos/img/packaging-02.png")
     #:last-updated (string->date "2020-03-28T16:00:00" "~Y-~m-~dT~H:~M:~S"))
                    (video
     #:title (C_ "video title" "Packaging, Part Three")
     #:page-subpath "packaging,-part-three"
     #:description
     (G_ '(p "How to submit a package for inclusion in the GNU Guix
distribution."))
     #:url "https://guix.gnu.org/guix-videos/04-packaging-part-three.webm"
     #:poster (guix-url "static/videos/img/packaging-03.png")
     #:last-updated (string->date "2020-03-28T16:00:00" "~Y-~m-~dT~H:~M:~S")))))


(define publications
  (list
   (publication
    #:title "Functional Package Management with Guix"
    #:url "https://arxiv.org/abs/1305.4584"
    #:authors "Ludovic Courtès"
    #:date (string->date "2013-05-20" "~Y-~m-~d")
    #:type (C_ "publication type" "Conference paper"))
   (publication
    #:title "Reproducible and User-Controlled Software Environments in HPC with Guix"
    #:url "https://hal.inria.fr/hal-01161771/en"
    #:authors (G_ "Ludovic Courtès, Ricardo Wurmus")
    #:date (string->date "2015-07-25" "~Y-~m-~d")
    #:type (C_ "publication type" "Conference paper"))
   (publication
    #:title "Code Staging in GNU Guix"
    #:url "https://arxiv.org/abs/1709.00833"
    #:authors "Ludovic Courtès"
    #:date (string->date "2017-09-04" "~Y-~m-~d")
    #:type (C_ "publication type" "Conference paper"))
   (publication
    #:title "Scientific Data Analysis Pipelines and Reproducibility"
    #:authors "Altuna Akalin"
    #:url "https://towardsdatascience.com/scientific-data-analysis-pipelines-and-reproducibility-75ff9df5b4c5"
    #:date (string->date "2018-10-01" "~Y-~m-~d")
    #:type (C_ "publication type" "Article"))
   (publication
    #:title "Reproducible Genomics Analysis Pipelines with GNU Guix"
    #:url "http://dx.doi.org/10.1093/gigascience/giy123"
    #:authors "Ricardo Wurmus, Bora Uyar, Brendan Osberg, Vedran Franke, \
Alexander Gosdschan, Katarzyna Wreczycka, Jonathan Ronen, Altuna Akalin"
    #:date (string->date "2018-10-02" "~Y-~m-~d")
    #:type (C_ "publication type" "Journal article"))
   (publication
    #:title "Guix: A most advanced operating system"
    #:url "https://ambrevar.xyz/guix-advance/index.html"
    #:authors "Pierre Neidhardt"
    #:date (string->date "2019-01-14" "~Y-~m-~d")
    #:type (C_ "publication type" "Article"))
   (publication
    #:title "Scalable Workflows and Reproducible Data Analysis for Genomics"
    #:url "https://link.springer.com/protocol/10.1007%2F978-1-4939-9074-0_24"
    #:authors "Francesco Strozzi, Roel Janssen, Ricardo Wurmus, \
Michael R. Crusoe, George Githinji, Paolo Di Tommaso, Dominique Belhachemi, \
Steffen Möller, Geert Smant, Joep de Ligt, Pjotr Prins"
    #:date (string->date "2019-07-06" "~Y-~m-~d")
    #:type (C_ "publication type" "Book chapter"))))


(define screenshots
  (list
   (screenshot
    #:title (C_ "screenshot title" "Graphical log-in")
    #:slug "slim"
    #:image (guix-url "static/media/img/gdm-sessions.png")
    #:preview (guix-url "static/media/img/gdm-sessions.mini.png")
    #:caption (G_ "Graphical log-in screen"))

   (screenshot
    #:title (C_ "screenshot title" "GNOME")
    #:slug "gnome"
    #:image (guix-url "static/media/img/gnome.png")
    #:preview (guix-url "static/media/img/gnome.mini.png")
    #:caption (G_ "GNOME desktop environment"))

   (screenshot
    #:title (C_ "screenshot title" "Xfce")
    #:slug "xfce"
    #:image (guix-url "static/media/img/xfce.png")
    #:preview (guix-url "static/media/img/xfce.mini.png")
    #:caption (G_ "Xfce desktop environment"))

   (screenshot
    #:title (C_ "screenshot title" "Virtual machine")
    #:slug "virtual-machine"
    #:image (guix-url "static/media/img/guix-system-vm.png")
    #:preview (guix-url "static/media/img/guix-system-vm.mini.png")
    #:caption (G_ "Virtual machine started with 'guix system vm'"))

   (screenshot
    #:title (C_ "screenshot title" "Sway")
    #:slug "sway"
    #:image (guix-url "static/media/img/sway.png")
    #:preview (guix-url "static/media/img/sway.mini.png")
    #:caption (G_ "Sway window manager running wayland"))

   (screenshot
    #:title (C_ "screenshot title" "Enlightenment")
    #:slug "enlightenment"
    #:image (guix-url "static/media/img/enlightenment-inkscape.png")
    #:preview (guix-url "static/media/img/enlightenment-inkscape.mini.png")
    #:caption (G_ "Enlightenment, Inkscape, and Serbian text"))))
