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

(use-modules (msgpack unpack)
             (srfi srfi-64)
             (test unpack utility test-cases))


(test-begin "Integers")
(test-cases "Positive fixnum" =
  (#x00 (#x00))
  (#x7F (#x7F)))
(test-cases "Unsigned 8-bit integer" =
  (#x80 (#xCC #x80))
  (#xFF (#xCC #xFF)))
(test-cases "Unsigned 16-bit integer" =
  (#x0100 (#xCD #x01 #x00))
  (#xFFFF (#xCD #xFF #xFF)))
(test-cases "Unsigned 32-bit integer" =
  (#x01000000 (#xCE #x01 #x00 #x00 #x00))
  (#xFFFFFFFF (#xCE #xFF #xFF #xFF #xFF)))
(test-cases "Unsigned 64-bit integer" =
  (#x0100000000000000 (#xCF #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (#xFFFFFFFFFFFFFFFF (#xCF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)))
(test-cases "Negative fixint" =
  (#x-01 (#xFF))
  (#x-20 (#xE0)))
(test-cases "Signed 8-bit integer" =
  (#x+00 (#xD0 #x00))
  (#x+7F (#xD0 #x7F))
  (#x-01 (#xD0 #xFF))
  (#x-21 (#xD0 #xDF))
  (#x-80 (#xD0 #x80)))
(test-cases "Signed 16-bit integer" =
  (#x+0000 (#xD1 #x00 #x00))
  (#x+7FFF (#xD1 #x7F #xFF))
  (#x-0001 (#xD1 #xFF #xFF))
  (#x-8000 (#xD1 #x80 #x00)))
(test-cases "Signed 32-bit integer" =
  (#x+00000000 (#xD2 #x00 #x00 #x00 #x00))
  (#x+7FFFFFFF (#xD2 #x7F #xFF #xFF #xFF))
  (#x-00000001 (#xD2 #xFF #xFF #xFF #xFF))
  (#x-80000000 (#xD2 #x80 #x00 #x00 #x00)))
(test-cases "Signed 64-bit integer" =
  (#x+0000000000000000 (#xD3 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
  (#x+7FFFFFFFFFFFFFFF (#xD3 #x7F #xFF #xFF #xFF #xFF #xFF #xFF #xFF))
  (#x-0000000000000001 (#xD3 #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF))
  (#x-8000000000000000 (#xD3 #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))
(test-end "Integers")
