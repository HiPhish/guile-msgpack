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
             (test unpack utility test-cases)
             ((srfi srfi-19)
              #:select (make-time time-utc)))


(define (time nanosecond second)
  (make-time time-utc nanosecond second))

(test-begin "Timestamps")
(test-cases "Edge of the time ranges" equal?
  ((time 0 0)
   (#xD6 #xFF #x00 #x00 #x00 #x00))
  ((time 0 (1- (expt 2 32)))
   (#xD6 #xFF #xFF #xFF #xFF #xFF))
  ((time (1- (expt 2 30)) (1- (expt 2 34)))
   (#xD7 #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF))
  ((time (1- (expt 2 32)) (1- (expt 2 64)))
   (#xC7 #x0C #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)))
(test-end "Timestamps")