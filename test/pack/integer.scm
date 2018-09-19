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

(test-begin "Integer")

(test-cases "Fixed positive integers"
  (  0 (#b00000000))
  (127 (#b01111111)))

(test-cases "Fixed negative integers"
  (-01 (#b11111111))
  (-02 (#b11111110))
  (-04 (#b11111100))
  (-08 (#b11111000))
  (-16 (#b11110000))
  (-32 (#b11100000)))

(test-cases "Unsigned 8-bit integers"
  (#x80 (#xCC #x80))
  (#xFF (#xCC #xFF)))

(test-cases "Unsigned 16-bit integers"
  (   (expt 2  8)    (#xCD #x01 #x00))
  ((- (expt 2 16) 1) (#xCD #xFF #xFF)))

(test-cases "Unsigned 32-bit integers"
  (   (expt 2 16)    (#xCE #x00 #x01 #x00 #x00))
  ((- (expt 2 32) 1) (#xCE #xFF #xFF #xFF #xFF)))

(test-cases "Unsigned 64-bit integers"
  (   (expt 2 32)    (#xCF #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x00))
  ((- (expt 2 64) 1) (#xCF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)))

(test-cases "Signed 8-bit integers"
  (-033 (#xD0 #xDF))
  (-128 (#xD0 #x80)))

(test-cases "Signed 16-bit integers"
  (-129            (#xD1 #xFF #x7F))
  ((- (expt 2 15)) (#xD1 #x80 #x00)))

(test-cases "Signed 32-bit integers"
  ((- 0 (expt 2 15) 1) (#xD2 #xFF #xFF #x7F #xFF))
  ((-   (expt 2 31)  ) (#xD2 #x80 #x00 #x00 #x00)))

(test-cases "Signed 64-bit integers"
  ((- 0 (expt 2 31) 1) (#xD3 #xFF #xFF #xFF #xFF #x7F #xFF #xFF #xFF))
  ((-   (expt 2 63)  ) (#xD3 #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))

(test-end "Integer")
