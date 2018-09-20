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


(test-begin "Text strings")
(test-cases "Empty string"
  ("" (#b10100000)))

(test-cases "Fixed-length strings"
  ("Unicode" (#b10100111 #x55 #x6E #x69 #x63 #x6F #x64 #x65))
  ("Юникод"  (#b10101100 #xD0 #xAE #xD0 #xBD #xD0 #xB8 #xD0 #xBA #xD0 #xBE #xD0 #xB4)))

(test-cases "8-bit strings"
  ((make-string (expt 2 5) #\x)
   (#xD9 #x20 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
              #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
              #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78))
  ;; Cyrillic letter #\Б (the "B" equivalent) is encoded as the two bytes #xD0 #x91
  ((make-string (expt 2 4) #\Б)
   (#xD9 #x20 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
              #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
              #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91)))

(test-cases "16-bit strings"
  ((make-string (expt 2 8) #\x)
   (#xDA #x01 #x00 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78 #x78
                   #x78 #x78 #x78 #x78))
  ((make-string (expt 2 7) #\Б)
   (#xDA #x01 #x00 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91 #xD0 #x91
                   #xD0 #x91 #xD0 #x91)))

;;; I'm skipping this size because it's too large to handle
(test-cases "32-bit strings")

(test-end "Text strings")
