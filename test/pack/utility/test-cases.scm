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
  #:use-module ((srfi srfi-1)      #:select (append-map))
  #:use-module ((srfi srfi-64)     #:select (test-begin test-end test-assert))
  #:use-module ((srfi srfi-28)     #:select (format))
  #:use-module ((ice-9 match)      #:select (match-lambda))
  #:use-module ((rnrs bytevectors) #:select (u8-list->bytevector bytevector=?))
  #:export (test-cases test-case))

;; Test cases are specified as (test-case given expected), where 'given' is the
;; bytevector of packed data and 'expected' is a specification of expected
;; bytes.
;;
;; expected ::= (spec ...)
;; spec     ::= byte
;;              (count byte ...)
;;
;; A single byte is take as the literal byte, a (count byte ...) is taken as
;; 'count' many repetitions of the following bytes.
;;
;; Example: (#x00 #x01 (3 #x02) #(2 #x03 #x04)) is the same as
;;          (#x00 #x01 #x02 #x02 #x02 #x03 #x04 #x03 #x04)

(define-syntax test-cases
  (syntax-rules ()
    ((_ title
        (given (spec ...))
        ...)
     (begin
       (test-begin title)
       (test-case given (spec ...))
       ...
       (test-end title)))))

(define-syntax test-case
  (syntax-rules ()
    ((_ given (spec ...))
     (test-bytevector= (pack given)
                       (compact-bytevector (syntax->datum #'(spec ...)))))))

(define (compact-bytevector specs)
  "Convert a series of of (count byte ...) or single byte specifications into a
  bytevector."
  (u8-list->bytevector
    (append-map (match-lambda
                  ((count byte ...)
                   (apply append (make-list count byte)))
                  (byte
                   (list byte)))
                specs)))

(define (test-bytevector= bv1 bv2)
  (define equivalent? (bytevector=? bv1 bv2))
  (unless equivalent?
    (display (format "~a is not ~a~%" bv1 bv2)))
  (test-assert equivalent?))
