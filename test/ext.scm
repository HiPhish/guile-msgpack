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

(use-modules (msgpack ext)
             ((rnrs bytevectors)
              #:select (u8-list->bytevector bytevector=?))
             (srfi srfi-64))

(define-syntax test-case
  (syntax-rules ()
    ((_ type (byte ...))
     (let* ((data (u8-list->bytevector '(byte ...)))
            (e (make-ext type data)))
       (test-assert (ext? e))
       (test-assert (= (ext-type e) type))
       (test-assert (bytevector=? (ext-data e) data))))))

(test-begin "Extension data type")
(test-case 0 ())
(test-case 0 (#x00 #x01 #x02))
(test-end "Extension data type")
