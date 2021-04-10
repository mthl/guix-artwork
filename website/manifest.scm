(use-modules (guix packages)
             ((gnu packages package-management) #:select (guix))
             ((gnu packages guile-xyz)          #:select (haunt))
             (gnu system locale)
             (ice-9 rdelim)
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

(define locales
  (locale-directory
   (call-with-input-file "po/LINGUAS"
     (lambda (port)
       (let loop ((line (read-line port))
                  (locales '()))
         (if (eof-object? line)
             locales
             (if (equal? (string-ref line 0) #\#)
                 (loop (read-line port) locales)
                 (loop (read-line port)
                       (cons
                        (locale-definition
                         (name (string-append line ".utf8"))
                         (source line))
                        locales)))))))
   #:libcs
   (list glibc)))

(manifest
 (cons
  (manifest-entry
    (name "locales")
    (version "0")
    (item
     (computed-file "locales"
                    (with-imported-modules '((guix build utils))
                      #~(let ((out (string-append #$output "/lib/locale")))
                          (use-modules (guix build utils))
                          (mkdir-p out)
                          (copy-recursively #$locales out)))))
    (search-paths
     (list (search-path-specification
            (variable "GUIX_LOCPATH")
            (files '("lib/locale"))))))
  (manifest-entries
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
           "git"
           "guile-syntax-highlight")))))))
