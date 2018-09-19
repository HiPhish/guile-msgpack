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

(define-module (msgpack ext)
  #:use-module ((srfi srfi-9)
                #:select (define-record-type))
  #:use-module ((srfi srfi-9 gnu)
                #:select (set-record-type-printer!))
  #:use-module ((srfi srfi-28)
                #:select (format))
  #:export (make-ext
            ext?
            ext-type
            ext-data))

(define-record-type ext
  (make-ext type data)
  ext?
  (type ext-type)
  (data ext-data))

(set-record-type-printer! ext
  (λ (record port)
    (display (format "#<ext ~a ~a>" (ext-type record) (ext-data record))
             port)))
