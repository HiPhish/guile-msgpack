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
  #:use-module ((msgpack)          #:select (unpack))
  #:use-module ((rnrs bytevectors) #:select (u8-list->bytevector))
  #:use-module ((srfi srfi-1)      #:select (append-map))
  #:use-module ((srfi srfi-64)     #:select (test-begin test-end test-assert))
  #:use-module ((ice-9 match)      #:select (match-lambda))
  #:export (test-cases test-case test-cases-strdec test-case-strdec))

;; Test cases are specified as (test-case expected given), where 'expected' is
;; the unpacked object we expect, and 'given' is a specification of bytes to
;; unpack.
;;
;; given ::= (spec ...)
;; spec  ::= byte
;;           (count byte ...)
;;
;; A single byte is take as the literal byte, a (count byte ...) is taken as
;; 'count' many repetitions of the following bytes.
;;
;; Example: (#x00 #x01 (3 #x02) #(2 #x03 #x04)) is the same as
;;          (#x00 #x01 #x02 #x02 #x02 #x03 #x04 #x03 #x04)

(define-syntax test-cases
  (syntax-rules ()
    ((_ title pred
       (expected (spec ...))
       ...)
     (begin
       (test-begin title)
       (test-case pred expected (spec ...))
       ...
       (test-end title)))))

(define-syntax test-case
  (syntax-rules ()
    ((_ pred expected (spec ...))
     (test-assert (pred expected
                        (unpack (compact-bytevector (syntax->datum #'(spec ...)))))))))

(define-syntax test-cases-strdec
  (syntax-rules ()
    ((_ title pred dec
       (expected (spec ...))
       ...)
     (begin
       (test-begin title)
       (test-case-strdec pred dec expected (spec ...))
       ...
       (test-end title)))))

(define-syntax test-case-strdec
  (syntax-rules ()
    ((_ pred string-decoder expected (spec ...))
     (test-assert (pred expected
                        (unpack (compact-bytevector (syntax->datum #'(spec ...)))
                          #:string-decoder string-decoder))))))

(define (compact-bytevector specs)
  "Convert a series of of (count byte ...) or single byte specifications into a
  bytevector."
  (u8-list->bytevector
    (append-map (match-lambda
                  (byte
                   (list byte))
                  ((count byte ...)
                   (apply append (make-list count byte))))
                specs)))
