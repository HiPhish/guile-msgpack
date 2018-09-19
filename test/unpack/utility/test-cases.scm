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

(define-module (test unpack utility test-cases)
  #:use-module ((rnrs bytevectors)
                #:select (u8-list->bytevector))
  #:use-module ((srfi srfi-64)
                #:select (test-begin test-end test-assert))
  #:export (test-cases test-case))


;; Define a test-case specification language; every specification consists of a
;; title followed by a list of cases, each of which is the expected result and
;; a list of input bytes to unpack.
(define-syntax test-cases
  (syntax-rules ()
    ((_ title pred
       (expected (byte byte* ...))
       ...)
     (begin
       (test-begin title)
       (test-case pred expected (byte byte* ...))
       ...
       (test-end title)))))

(define-syntax test-case
  (syntax-rules ()
    ((_ pred expected (byte byte* ...))
     (test-assert (pred expected
                        (unpack (u8-list->bytevector '(byte byte* ...))))))))
