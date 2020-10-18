(use-modules (guix packages)
             ((gnu packages package-management) #:select (guix))
             ((gnu packages guile-xyz)          #:select (haunt))
             (srfi srfi-1))

(define the-good-guile
  (car (assoc-ref (package-native-inputs guix) "guile")))

(define haunt-the-ghost
  (package
    (inherit haunt)
    (name "haunt-for-guix-website")
    (inputs
     `(("guile" ,the-good-guile)
       ,@(alist-delete "guile" (package-inputs haunt))))))

(packages->manifest
 (append
  ;; Guile needs to be compatible
  (list
   guix
   the-good-guile
   haunt-the-ghost)

  ;; Other packages
  (map specification->package
       (list
        "glibc-locales"
        "git"
        "guile-syntax-highlight"))))
