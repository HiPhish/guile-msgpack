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
             (test unpack utility test-cases))


(test-begin "Text Strings")
(test-cases "Empty string" string=?
  ("" (#b10100000))
  ("" (#xD9 #x00))
  ("" (#xDA #x00 #x00))
  ("" (#xDB #x00 #x00 #x00 #x00)))
;; We test first an ASCII string, then a UTF-8 encoded Unicode string. The
;; Unicode text says "Unicode" in Cyrillic; feel free to add more alphabets as
;; additional test cases if needed.
(test-cases "Fixed-length string" string=?
  ("Unicode" (#b10100111 #x55 #x6E #x69 #x63 #x6F #x64 #x65))
  ("Юникод"  (#b10101100 #xD0 #xAE #xD0 #xBD #xD0 #xB8 #xD0 #xBA #xD0 #xBE #xD0 #xB4)))
(test-cases "8-bit string" string=?
  ("Unicode" (#xD9 #x07 #x55 #x6E #x69 #x63 #x6F #x64 #x65))
  ("Юникод"  (#xD9 #x0C #xD0 #xAE #xD0 #xBD #xD0 #xB8 #xD0 #xBA #xD0 #xBE #xD0 #xB4)))
(test-cases "16-bit string" string=?
  ("Unicode" (#xDA #x00 #x07 #x55 #x6E #x69 #x63 #x6F #x64 #x65))
  ("Юникод"  (#xDA #x00 #x0C #xD0 #xAE #xD0 #xBD #xD0 #xB8 #xD0 #xBA #xD0 #xBE #xD0 #xB4)))
(test-cases "32-bit string" string=?
  ("Unicode" (#xDB #x00 #x00 #x00 #x07 #x55 #x6E #x69 #x63 #x6F #x64 #x65))
  ("Юникод"  (#xDB #x00 #x00 #x00 #x0C #xD0 #xAE #xD0 #xBD #xD0 #xB8 #xD0 #xBA #xD0 #xBE #xD0 #xB4)))
(test-end "Text Strings")
