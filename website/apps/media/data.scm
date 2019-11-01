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
