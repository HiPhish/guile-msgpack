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
             (test pack utility test-cases))


(test-begin "Arrays")
(test-cases "Fixed-size arrays"
  ((make-vector  0 0) (#b10010000 ( 0 #x00)))
  ((make-vector  1 0) (#b10010001 ( 1 #x00)))
  ((make-vector  2 0) (#b10010010 ( 2 #x00)))
  ((make-vector 15 0) (#b10011111 (15 #x00))))
(test-cases "16-bit arrays"
  ((make-vector (expt 2 4) 0) (#xDC #x00 #x10 (16 #x00))))
;;; FIXME: 32-bit arrays are too large to write down the bytes for
(test-end "Arrays")
