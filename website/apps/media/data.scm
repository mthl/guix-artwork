;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps media data)
  #:use-module (apps base utils)
  #:use-module (apps media types)
  #:export (screenshots))


;;;
;;; Data.
;;;

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
