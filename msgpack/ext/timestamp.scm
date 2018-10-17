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
                #:select (time-utc make-time time-nanosecond time-second))
  #:use-module ((rnrs bytevectors)
                #:select (make-bytevector
                          bytevector-length
                          bytevector-uint-ref
                          bytevector-uint-set!
                          bytevector-u8-ref
                          bytevector-u8-set!
                          endianness))
  #:use-module ((ice-9 match)
                #:select (match))
  #:export (ext->time time->ext))


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
                  ( 4 (data->times 0 4))
                  ( 8 (data->times (/ 30 8) (/ 34 8)))  ; FIXME: this is totally fucked
                  (12 (data->times 4 8))
                  (_  (throw 'wrong-type-arg "Invalid data for timestamp")))))
    (make-time time-utc nanoseconds seconds)))

(define (time->ext time)
  "- Scheme Procedure: time->ext time
     Return a MessagePack extension object of type -1 generated from the
     contents of the SRFI-19 time object TIME.
     
     As per MessagePack specifications, the number of nanoseconds may not be
     larger than an unsigned 32-bit integer, and the number of seconds may not
     be larger than a 64-bit unsigned integer."
  (define 1mrd (expt 10 9))  ; One billion (milliard)
  (define nanosecond (time-nanosecond time))
  (define     second (time-second     time))
  ;; Implementation note: it is possible for the Guile implementation to divide
  ;; the nanoseconds by 10^9, add the result to the seconds and keep only the
  ;; remainder as nanoseconds. We can detect such cases, subtract some seconds
  ;; from the seconds and add them back to the nanoseconds. This extends our
  ;; range beyond the naive approach.
  (define (data nanosecond second)
    "Compute the data bytevector from the nanoseconds and seconds."
    ; (display (format "ns: ~a; s: ~a\n" nanosecond second))
    (cond
      ((and (zero? nanosecond) (< second (expt 2 32)))
       (let ((bv (make-bytevector (/ 32 8))))
         (bytevector-uint-set! bv 0 second (endianness big) (/ 32 8))
         bv))
      ((and (< nanosecond (expt 2 30)) (< second (expt 2 34)))
       (let ((bv (make-bytevector (/ (+ 30 34) 8)))
             (nanosecond (ash nanosecond 2))
             (second (logior second (ash (logand (ash nanosecond 2) #xFF) 32))))
         (bytevector-uint-set! bv 0 nanosecond (endianness big) 4)
         (bytevector-uint-set! bv 3 second     (endianness big) 5)
         bv))
      ;; We can fit a whole extra second into the nanoseconds under the
      ;; following condition
      ((and (< nanosecond (- (expt 2 30) 1mrd))
            (<     second (+ (expt 2 34)    1)))
       (data (+ nanosecond 1mrd) (1- second)))
      ((and (< nanosecond (expt 2 32)) (< second (expt 2 64)))
       (let ((bv (make-bytevector (/ (+ 32 64) 8))))
         (bytevector-uint-set! bv 0 nanosecond (endianness big) 4)
         (bytevector-uint-set! bv 4     second (endianness big) 8)
         bv))
      ;; We can fit four extra seconds into the nanoseconds under the following
      ;; condition
      ((and (< nanosecond (- (expt 2 32) (* 4 1mrd)))
            (<     second (+ (expt 2 64)          4)))
       (data (+ nanosecond (* 4 1mrd))
             (-     second          4)))
      (else (throw 'value-out-of-range))))
  (ext -1 (data nanosecond second)))
