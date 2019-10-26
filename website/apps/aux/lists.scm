;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps aux lists)
  #:use-module (apps aux numbers)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-27)
  #:export (list-group
	    list-slice
	    rest
            separate
            take-random))


(define (list-group los limit)
  (map (lambda (index)
	 (list-slice los index (+ index limit)))
       ;; TODO: Use a skip-count procedure instead of iota.
       (iota (ceiling (/ (length los) limit)) 0 limit)))


(define* (list-slice los index-a #:optional (index-b #false))
  (let ((start index-a)
	(end (if (or (not index-b) (> index-b (length los)))
		 (- (length los) 1)
		 (- index-b 1))))
    (map (lambda (index)
	   (list-ref los index))
	 (range index-a end))))


(define (rest los)
  (cond ((<= (length los) 1) (list))
	(else (list-tail los 1))))


(define (separate los separator)
  "Return a list with the elements of LOS separated by SEPARATOR.

   LOS (list)
     A list of s-expressions.

   SEPARATOR (s-expression)
     Any s-expression that will be added between the elements of the
     given list.

   RETURN VALUE (list)
     A list of s-expressions."
  (cond ((or (null? los) (= (length los) 1)) los)
	(else
	 (cons (first los)
	       (cons separator (separate (rest los) separator))))))

(define (take-random list n)
  "Return a list containing N elements from LIST, if possible, chosen
randomly and evenly distributed.  If LIST has less than N elements,
the result is a permutation of LIST."
  (let loop ((list list)
             (n n)
             (len (length list)))
    (if (<= (min n len) 0)
        '()
        (let ((r (random-integer len)))
          (cons (list-ref list r)
                (loop (append (take list r)
                              (drop list (1+ r)))
                      (- len 1)
                      (- n 1)))))))
