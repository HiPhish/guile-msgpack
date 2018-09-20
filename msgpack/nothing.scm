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

(define-module (msgpack nothing)
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:export (nothing? nothing))

;;; MessagePack defines a type "nil" for nothingness, but Guile has no
;;; dedicated value for nothing. We could use the empty list "'()" or "#f", but
;;; then "nothing" would be be indistinguishable from those two. Instead we
;;; define a singleton instance of a new "nothing" type. Users of this library
;;; can only use the "nothing" function which returns the one instance of the
;;; object and the "nothing?" predicate.

(define <nothing>
  (make-record-type "<nothing>" '() (λ (rec out) (display "#<nothing>" out))))

(define nothing
  (let ((the-nothing ((record-constructor <nothing>))))
    (λ ()
      "- Scheme Procedure: nothing
     Returns the singleton instance of the MessagePack nothingness object."
      the-nothing)))

(define (nothing? object)
  "- Scheme Procedure: nothing? object
     Return `#t' if OBJECT is 'eq?' to the MessagePack nothingness object,
     else return `#f'."
  (eq? object (nothing)))
