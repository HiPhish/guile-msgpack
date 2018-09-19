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

(use-modules (msgpack pack)
             (srfi srfi-64)
             (test pack utility test-cases))

(test-begin "Floating point numbers")
;;; Bit patterns: (S sign, E exponent, F fraction)

;;; S.EEEEEEEE.FFFFFFFFFFFFFFFFFFFFFFF
(parameterize ((float-precision 'single))
  (test-cases "Single precision floating point numbers"
    (+0.0                (#xCA #b00000000 #b00000000 #b00000000 #b00000000))
    (-0.0                (#xCA #b10000000 #b00000000 #b00000000 #b00000000))
    (+inf.0              (#xCA #b01111111 #b10000000 #b00000000 #b00000000))
    (-inf.0              (#xCA #b11111111 #b10000000 #b00000000 #b00000000))
    ;; Guile encodes +NaN and -NaN the same
    (+nan.0              (#xCA #b01111111 #b11000000 #b00000000 #b00000000))
    (-nan.0              (#xCA #b01111111 #b11000000 #b00000000 #b00000000))
    (+3.1415927410125732 (#xCA #b01000000 #b01001001 #b00001111 #b11011011))
    (-3.1415927410125732 (#xCA #b11000000 #b01001001 #b00001111 #b11011011))))

;;; S.EEEEEEEEEEE.FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
(test-cases "Double precision floating point numbers"
  (+0.0   (#xCB #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000))
  (-0.0   (#xCB #b10000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000))
  (+inf.0 (#xCB #b01111111 #b11110000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000))
  (-inf.0 (#xCB #b11111111 #b11110000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000))
  ;; Guile encodes +NaN and -NaN the same
  (+nan.0 (#xCB #b01111111 #b11111000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000))
  (-nan.0 (#xCB #b01111111 #b11111000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000))
  (+3.141592653589793
   (#xCB #b01000000 #b00001001 #b00100001 #b11111011 #b01010100 #b01000100 #b00101101 #b00011000))
  (-3.141592653589793
   (#xCB #b11000000 #b00001001 #b00100001 #b11111011 #b01010100 #b01000100 #b00101101 #b00011000)))

(test-end "Floating point numbers")
