#!/usr/local/bin/guile -s
!#
;;; Copyright 2018 Alejandro Sanchez
;;;
;;; This file is part of msgpack-guile.
;;; 
;;; Msgpack-guile is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; Msgpack-guile is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with msgpack-guile.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (srfi srfi-64)
             (test unpack utility test-cases))


(define (hash-table-equal? h1 h2)
  "- Scheme procedure: hash-table-equal? h1 h2
     Compare two hash tables for entry-wise equality.
     
     The arguments 'h1' and 'h2' are not equal if and only if either:
       • One of them is not a hash table
       • They do not have the same number of entries
       • Values for the same key are not 'equal?' or 'hash-equal?'

     Two hash tables are always 'hash-equal?' if they are 'eq?'."
  ;; First take care of the trivial cases. Then compare the length of the
  ;; tables; if the lengths don't match the tables not equal. Finally, loop
  ;; over the keys of one table and compare the values with those in the other
  ;; table.
  ;;
  ;; This algorithm traverses the first tables thrice and the second table
  ;; twice in the worst case: Once each to count the keys, once the first one
  ;; to collect the keys, and once both to compare all values. The loop will
  ;; terminate the instant a mismatch is found.
  ;;
  ;; If the first value is a hash table we call this function recursively
  ;; because hash tables are never 'equal?' (unless they are 'eq?')
  (cond
    ((not (hash-table? h1)) #f)
    ((not (hash-table? h2)) #f)
    ((eq? h1 h2) #t)
    (else
      (let ((n-keys1 (hash-fold (λ (k v keys) (1+ keys)) 0 h1))
            (n-keys2 (hash-fold (λ (k v keys) (1+ keys)) 0 h2)))
        (if (not (= n-keys1 n-keys2))
          #f
          (do ((keys (hash-fold (λ (k v keys) (cons k keys)) '() h1) (cdr keys))
               (same? #t))
              ((or (not same?) (null? keys))
               same?)
            (let* ((key (car keys))
                   (v1 (hash-ref h1 key))
                   (v2 (hash-get-handle h2 key)))
              (cond
                ((not v2)                   (set! same? #f))
                ((hash-table? v1)           (set! same? (hash-table-equal? v1 v2)))
                ((not (equal? v1 (cdr v2))) (set! same? #f))))))))))


(test-begin "Maps")
(test-cases "Empty map" hash-table-equal?
  ((make-hash-table 0) (#b10000000))
  ((make-hash-table 0) (#xDE #x00 #x00))
  ((make-hash-table 0) (#xDF #x00 #x00 #x00 #x00)))
(let ((h (make-hash-table 3)))
  (hash-set! h "a" 1)
  (hash-set! h "b" 2)
  (hash-set! h "c" 3)
  (test-cases "Map with contents" hash-table-equal?
    (h (#b10000011               #xA1 #x61 #x01 #xA1 #x62 #x02 #xA1 #x63 #x03))
    (h (#xDE           #x00 #x03 #xA1 #x61 #x01 #xA1 #x62 #x02 #xA1 #x63 #x03))
    (h (#xDF #x00 #x00 #x00 #x03 #xA1 #x61 #x01 #xA1 #x62 #x02 #xA1 #x63 #x03))))
(test-end "Maps")
