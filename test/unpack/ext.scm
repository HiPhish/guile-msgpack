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

(use-modules ((msgpack ext) #:select (ext))
             (srfi srfi-64)
             (test unpack utility test-cases))


(test-begin "Exentsions")
(test-cases "Fixed-length extensions" equal?
  ((ext 1 #vu8(#x00)) (#xD4 #x01 #x00))
  ((ext 1 #vu8(#x00 #x01))
   (#xD5 #x01 #x00 #x01))
  ((ext 1 #vu8(#x00 #x01 #x02 #x03))
   (#xD6 #x01 #x00 #x01 #x02 #x03))
  ((ext 1 #vu8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07))
   (#xD7 #x01 #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07))
  ((ext 1 #vu8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F))
   (#xD8 #x01 #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F)))
(test-cases "Variable-length extensions" equal?
  ((ext 1 #vu8(#x00)) (#xC7                #x01 #x01 #x00))
  ((ext 1 #vu8(#x00)) (#xC8           #x00 #x01 #x01 #x00))
  ((ext 1 #vu8(#x00)) (#xC9 #x00 #x00 #x00 #x01 #x01 #x00)))
(test-end "Exentsions")
