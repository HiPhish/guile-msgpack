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
  #:use-module ((rnrs bytevectors)
                #:select (bytevector? bytevector-length bytevector-uint-ref endianness))
  #:use-module ((srfi srfi-9)
                #:select (define-record-type))
  #:use-module ((srfi srfi-9 gnu)
                #:select (set-record-type-printer!))
  #:use-module ((srfi srfi-11)
                #:select (let-values))
  #:use-module ((srfi srfi-19)
                #:select (time-utc make-time))
  #:use-module ((srfi srfi-28)
                #:select (format))
  #:use-module ((ice-9 match)
                #:select (match))
  #:export (ext
            ext?
            ext-type
            ext-data))

(define <ext>
  (make-record-type "<ext>" '(type data)))

(define (ext type data)
  "- Scheme Procedure: ext type data
     Return a new MessagePack extension object of type TYPE with payload DATA."
  (unless (and (integer? type) (exact? type) (<= -128 type 127))
    (throw 'wrong-type-arg "Type must be signed 8-bit integer"))
  (unless (bytevector? data)
    (throw 'wrong-type-arg "Data must be bytevector"))
  (unless (< (bytevector-length data) (expt 2 32))
    (throw 'out-of-range "Data sequence must be < 2^32 bytes long"))
  ((record-constructor <ext>) type data))

(define (ext? ext)
  "- Scheme Procedure: ext? ext
     Return `#t' if EXT is a MessagePack extension object, else return `#f'."
  ((record-predicate <ext>) ext))

(define (ext-type ext)
  "- Scheme Procedure: ext-type ext
     Return the type number of the MessagePack extension object."
  ((record-accessor <ext> 'type) ext))

(define (ext-data ext)
  "- Scheme Procedure: ext-data ext
     Return the data bytevector of the MessagePack extension object."
  ((record-accessor <ext> 'data) ext))

(set-record-type-printer! <ext>
  (λ (record port)
    (display (format "#<ext ~a ~a>" (ext-type record) (ext-data record))
             port)))

;; ---[ Conversions ]----------------------------------------------------------
;; The following conversion have been defined by MessagePack

(define (ext->time ext)
  "- Scheme Procedure: ext->timestamp ext
     Return a SRFI 19 time object from an extension object of type -1 as
     defined by the MessagePack specification."
  (define type (ext-type ext))
  (define data (ext-data ext))
  (define (data->times ns-byte s-bytes)
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
