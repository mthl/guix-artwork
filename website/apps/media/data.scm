;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps media data)
  #:use-module (apps base utils)
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
     #:title "Installation from Script"
     #:description
     '(p "Explains how to install Guix on distributions not running
GNU Guix.")
     #:url "https://archive.org/download/guix-videos/01-installation-from-script.webm"
     #:poster (guix-url "static/videos/img/installation-from-script.png")
     #:last-updated (string->date "2019-10-25T20:00:00" "~Y-~m-~dT~H:~M:~S")))
   (list
    (video
     #:title "Everyday use of GNU Guix, Part One"
     #:description
     '(p "How to install packages and how to manage software package
generations.")
     #:url "https://archive.org/download/guix-videos/02-everyday-use-part-one.webm"
     #:poster (guix-url "static/videos/img/everyday-use-01.png")
     #:last-updated (string->date "2019-10-25T20:00:00" "~Y-~m-~dT~H:~M:~S"))
    (video
     #:title "Everyday use of GNU Guix, Part Two"
     #:description
     '(p "How to upgrade software and how to reclaim storage space.")
     #:url "https://archive.org/download/guix-videos/02-everyday-use-part-two.webm"
     #:poster (guix-url "static/videos/img/everyday-use-02.png")
     #:last-updated (string->date "2019-10-25T20:00:00" "~Y-~m-~dT~H:~M:~S")))
   (list
    (video
     #:title "Asking for help"
     #:description
     '(p "How to get help from the Guix community.")
     #:url "https://archive.org/download/guix-videos/03-help-new-version.webm"
     #:poster (guix-url "static/videos/img/help.png")
     #:last-updated (string->date "2019-10-25T20:00:00" "~Y-~m-~dT~H:~M:~S")))
   (list
    (video
     #:title "Packaging, Part One"
     #:description
     '(p "How to set up a development environment for GNU Guix.")
     #:url "https://archive.org/download/guix-videos/04-packaging-part-one.webm"
     #:poster (guix-url "static/videos/img/packaging-01.png")
     #:last-updated (string->date "2019-10-25T20:00:00" "~Y-~m-~dT~H:~M:~S"))
                (video
     #:title "Packaging, Part Two"
     #:description
     '(p "How to create a package recipe for not yet packaged software.")
     #:url "https://archive.org/download/guix-videos/04-packaging-part-two.webm"
     #:poster (guix-url "static/videos/img/packaging-02.png")
     #:last-updated (string->date "2019-10-25T20:00:00" "~Y-~m-~dT~H:~M:~S"))
                    (video
     #:title "Packaging, Part Three"
     #:description
     '(p "How to submit a package for inclusion in the GNU Guix
distribution.")
     #:url "https://archive.org/download/guix-videos/04-packaging-part-three.webm"
     #:poster (guix-url "static/videos/img/packaging-03.png")
     #:last-updated (string->date "2019-10-25T20:00:00" "~Y-~m-~dT~H:~M:~S")))))


(define screenshots
  (list
   (screenshot
    #:title "Graphical log-in"
    #:slug "slim"
    #:image (guix-url "static/media/img/gdm-sessions.png")
    #:preview (guix-url "static/media/img/gdm-sessions.mini.png")
    #:caption "Graphical log-in screen")

   (screenshot
    #:title "GNOME"
    #:slug "gnome"
    #:image (guix-url "static/media/img/gnome-totem-epiphany.png")
    #:preview (guix-url "static/media/img/gnome-totem-epiphany.mini.png")
    #:caption "Control your computer with the GNOME desktop environment")

   (screenshot
    #:title "Xfce"
    #:slug "xfce"
    #:image (guix-url "static/media/img/guixsd-xfce-icecat-emacs.png")
    #:preview (guix-url "static/media/img/guixsd-xfce-icecat-emacs.mini.png")
    #:caption "The Xfce desktop environment with GNU Emacs and IceCat")

   (screenshot
    #:title "Virtual machine"
    #:slug "virtual-machine"
    #:image (guix-url "static/media/img/guix-system-vm.png")
    #:preview (guix-url "static/media/img/guix-system-vm.mini.png")
    #:caption "Virtual machine started with 'guix system vm'")

   (screenshot
    #:title "Sway"
    #:slug "sway"
    #:image (guix-url "static/media/img/sway.png")
    #:preview (guix-url "static/media/img/sway.mini.png")
    #:caption "Sway window manager running wayland")

   (screenshot
    #:title "Enlightenment"
    #:slug "enlightenment"
    #:image (guix-url "static/media/img/enlightenment-inkscape.png")
    #:preview (guix-url "static/media/img/enlightenment-inkscape.mini.png")
    #:caption "Enlightenment, Inkscape, and Serbian text")))
