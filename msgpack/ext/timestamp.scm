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

(define-module (msgpack ext timestamp)
  #:use-module ((msgpack ext)
                #:select (ext ext-type ext-data))
  #:use-module ((srfi srfi-11)
                #:select (let-values))
  #:use-module ((srfi srfi-19)
                #:select (time-utc make-time))
  #:use-module ((rnrs bytevectors)
                #:select (make-bytevector bytevector-length bytevector-uint-ref bytevector-uint-set! endianness))
  #:use-module ((ice-9 match)
                #:select (match)))

(define (ext->time ext)
  "- Scheme Procedure: ext->time ext
     Return a SRFI 19 time object from an extension object of type -1 as
     defined by the MessagePack specification."
  (define type (ext-type ext))
  (define data (ext-data ext))
  (define (data->times ns-bytes s-bytes)
    "Disassemble the data into nanoseconds and seconds"
    (values (bytevector-uint-ref data        0 (endianness big) ns-bytes)
            (bytevector-uint-ref data ns-bytes (endianness big)  s-bytes)))

  (unless (= -1 (ext-type ext))
    (throw 'wrong-type-arg "Type must be -1" type))
  (let-values (((nanoseconds seconds)
                (match (bytevector-length data)
                  (32 (data->times  0 32))
                  (64 (data->times 30 34))
                  (96 (data->times 32 64))
                  (_ (throw 'wrong-type-arg "Invalid data for timestamp")))))
    (make-time time-utc nanoseconds seconds)))

(define (time->ext time)
  "- Scheme Procedure: time->ext time
     Return a MessagePack extension object of type -1 generated from the
     contents of the SRFI-19 time object TIME.
     
     As per MessagePack specifications, the number of nanoseconds may not be
     larger than an unsigned 32-bit integer, and the number of seconds may not
     be larger than a 64-bit unsigned integer."
  (define nanoseconds (time nanoseconds time))
  (define     seconds (time     seconds time))
  (define (time->bytes ns-bytes s-bytes)
    (let ((bv (make-bytevector (+ ns-bytes s-bytes) #x00)))
      (bytevector-uint-set! bv        0 nanoseconds (endianness big) ns-bytes)
      (bytevector-uint-set! bv ns-bytes     seconds (endianness big)  s-bytes)
      bv))
  (define data
    (cond
      ((and (zero? nanoseconds) (< seconds (expt 2 32)))
       (time->bytes 0 32))
      ((and (< nanoseconds (expt 2 30)) (< seconds (expt 2 34)))
       (time->bytes 30 34))
      ((and (< nanoseconds (expt 2 34)) (< seconds (expt 2 64)))
       (time->bytes 32 64))
      (else (throw 'value-out-of-range))))
  (ext -1 data))
