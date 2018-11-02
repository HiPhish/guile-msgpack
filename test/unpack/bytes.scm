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
             (test unpack utility test-cases)
             ((rnrs bytevectors)
              #:select (bytevector=?)))


(test-begin "Byte strings")
(test-cases "Empty string" bytevector=?
  (#vu8() (#xC4 #x00))
  (#vu8() (#xC5 #x00 #x00))
  (#vu8() (#xC6 #x00 #x00 #x00 #x00)))
;;; The bytes spell out "Test" in ASCII
(test-cases "Filled strings" bytevector=?
  (#vu8(#x54 #x65 #x73 #x74) (#xC4                #x04 #x54 #x65 #x73 #x74))
  (#vu8(#x54 #x65 #x73 #x74) (#xC5           #x00 #x04 #x54 #x65 #x73 #x74))
  (#vu8(#x54 #x65 #x73 #x74) (#xC6 #x00 #x00 #x00 #x04 #x54 #x65 #x73 #x74)))
(test-end "Byte strings")
