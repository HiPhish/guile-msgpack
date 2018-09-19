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

(define-module (test pack utility test-cases)
  #:use-module ((msgpack pack)     #:select (pack))
  #:use-module ((srfi srfi-64)     #:select (test-begin test-end test-assert))
  #:use-module ((srfi srfi-28)     #:select (format))
  #:use-module ((rnrs bytevectors) #:select (u8-list->bytevector bytevector=?))
  #:export (test-cases test-case))

(define-syntax test-cases
  (syntax-rules ()
    ((_ title
       (given (byte byte* ...))
       ...)
     (begin
       (test-begin title)
       (test-case given (byte byte* ...))
       ...
       (test-end title)))))

(define-syntax test-case
  (syntax-rules ()
    ((_ given (byte byte* ...))
     (test-bytevector= (pack given)
                       (u8-list->bytevector '(byte byte* ...))))))

(define (test-bytevector= bv1 bv2)
  (define equivalent? (bytevector=? bv1 bv2))
  (unless equivalent?
    (display (format "~a is not ~a~%" bv1 bv2)))
  (test-assert equivalent?))
