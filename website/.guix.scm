;;; GNU Guix web site
;;; Copyright © 2017, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of the GNU Guix web site.
;;;
;;; The GNU Guix web site is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; The GNU Guix web site is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with the GNU Guix web site.  If not, see <http://www.gnu.org/licenses/>.

;; Run 'guix build -f .guix.scm' to build the web site.

(use-modules (guix) (gnu)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (guix modules)
             (guix git-download)
             (guix gexp)
             (guix channels)
             (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 match))

(define this-directory
  (dirname (current-filename)))

(define source
  (local-file this-directory "guix-web-site"
              #:recursive? #t
              #:select? (git-predicate this-directory)))

(define (package+propagated-inputs package)
  (match (package-transitive-propagated-inputs package)
    (((labels packages) ...)
     (cons package packages))))

;; Representation of the latest channels.  This type exists just so we can
;; refer to such records in a gexp.
(define-record-type <latest-channels>
  (latest-channels channels)
  latest-channels?
  (channels latest-channels-channels))

(define-gexp-compiler (latest-channels-compiler (latest <latest-channels>)
                                                system target)
  (match latest
    (($ <latest-channels> channels)
     (latest-channel-derivation channels))))

(define latest-guix
  ;; The latest Guix.  Using it rather than the 'guix' package ensures we
  ;; build the latest package list.
  (latest-channels %default-channels))

;; Remove on the next rebuild cycle when Haunt will be built with Guile
;; 3.0.3. Until then, this is needed to avoid a discrepancy when running Haunt
;; with Guile 3.0.2 but loading Guix modules built for Guile 3.0.3.
(define haunt-with-guile-3.0.3
  (package
    (inherit haunt)
    (inputs
     `(("guile" ,guile-3.0/libgc-7)
       ,@(alist-delete "guile" (package-inputs haunt))))))

(define build
  ;; We need Guile-JSON for 'packages-json-builder'.
  (with-extensions (append (package+propagated-inputs
                            (specification->package "guile-json"))

                           (package+propagated-inputs
                            (specification->package "guile-syntax-highlight")))
    (with-imported-modules (source-module-closure
                            '((guix build utils)))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 popen)
                       (ice-9 match))

          (setvbuf (current-output-port) 'line)
          (setvbuf (current-error-port) 'line)

          (copy-recursively #$source ".")

          ;; Set 'GUILE_LOAD_PATH' so that Haunt find the Guix modules and
          ;; its dependencies.  To find out the load path of Guix and its
          ;; dependencies, fetch its value over 'guix repl'.
          (let ((pipe (open-pipe* OPEN_BOTH
                                  #+(file-append latest-guix "/bin/guix")
                                  "repl" "-t" "machine")))
            (pk 'repl-version (read pipe))
            (write '(list %load-path %load-compiled-path) pipe)
            (force-output pipe)
            (match (read pipe)
              (('values ('value ((load-path ...) (compiled-path ...))))
               (setenv "GUILE_LOAD_PATH" (string-join
                                          (append load-path %load-path)
                                          ":"))
               (setenv "GUILE_LOAD_COMPILED_PATH"
                       (string-join (append compiled-path
                                            %load-compiled-path)
                                    ":"))))
            (close-pipe pipe))

          ;; So we can read/write UTF-8 files.
          (setenv "GUIX_LOCPATH"
                  #+(file-append (specification->package "glibc-utf8-locales")
                                 "/lib/locale"))
          (setenv "LC_ALL" "en_US.utf8")

          ;; Use a sane default.
          (setenv "XDG_CACHE_HOME" "/tmp/.cache")

          (format #t "Running 'haunt build'...~%")
          (invoke #+(file-append haunt-with-guile-3.0.3 "/bin/haunt")
                  "build")

          (mkdir-p #$output)
          (copy-recursively "/tmp/gnu.org/software/guix" #$output
                            #:log (%make-void-port "w"))
          (symlink "guix.html" (string-append #$output "/index.html"))))))

(computed-file "guix-web-site" build
               #:guile (specification->package "guile")
               #:options '(#:effective-version "3.0"))

;; Local Variables:
;; eval: (put 'let-package 'scheme-indent-function 1)
;; End:
