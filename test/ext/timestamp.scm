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

(use-modules ((msgpack ext)
              #:select (ext ext-type ext-data))
             (msgpack ext timestamp)
             ((srfi srfi-19)
              #:select (make-time time-utc))
             ((rnrs bytevectors) #:select (u8-list->bytevector))
             (srfi srfi-64))

(define-syntax test-case
  (syntax-rules ()
    ((_ nanosecond second
        (byte byte* ...))
     (test-assert
       (equal? (time->ext (make-time time-utc nanosecond second))
               (ext -1 (u8-list->bytevector '(byte byte* ...))))))))

(test-begin "Timestamp extension")
(test-case 0 0
  (#x00 #x00 #x00 #x00))
(test-case 0 (1- (expt 2 32))
  (#xFF #xFF #xFF #xFF))

(test-case 1 0
  (#x00 #x00 #x00 #x04 #x00 #x00 #x00 #x00))
(test-case (1- (expt 2 30))
           (1- (expt 2 34))
  (#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF))

(test-case 1 #x1000000000000000
  (#x00 #x00 #x00 #x01 #x10 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
(test-case (1- (expt 2 32))
           (1- (expt 2 64))
  (#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF))
(test-end "Timestamp extension")
