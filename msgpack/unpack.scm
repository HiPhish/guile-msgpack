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

(define-module (msgpack unpack)
  #:export (unpack-from unpack))

(use-modules ((msgpack nothing)
              #:select (nothing))
             ((msgpack ext)
              #:select (ext? ext-type))
             ((msgpack ext timestamp)
              #:select (ext->time))
             ((ice-9 binary-ports)
              #:select (get-u8
                        get-bytevector-n))
             ((ice-9 iconv)
              #:select (bytevector->string))
             ((ice-9 match)
              #:select (match))
             ((rnrs io ports)
              #:select (open-bytevector-input-port))
             ((rnrs bytevectors)
              #:select (bytevector-uint-ref
                        bytevector-sint-ref
                        bytevector-s8-ref
                        utf8->string
                        bytevector-ieee-single-ref
                        bytevector-ieee-double-ref
                        endianness))
             ((msgpack ext)
              #:select (ext)))

;; ----------------------------------------------------------------------------
(define* (unpack-from in #:key (string-decoder utf8->string))
  "- Scheme Procedure: unpack-from in
     Unpack (de-serialise) one value from the open binary input port IN.

     If the port is empty (not a single byte could be read) an 'empty-port
     exception will be raised.

     To decode non UTF-8 strings pass the keyword argument STRING-DECODER,
     a procedure that decodes a byte vector."
  (define (primitive-unpack)
    "Unpack an object, but do not expand extension objects yet"
    (cond
      ((<= tag #x7F) tag)
      ((<= #x80 tag #x8F) (unpack-map    (logand tag #b00001111) in))
      ((<= #x90 tag #x9F) (unpack-array  (logand tag #b00001111) in))
      ((<= #xA0 tag #xBF) (unpack-string (logand tag #b00011111) in string-decoder))
      ((= tag #xC0) (nothing))
      ((= tag #xC1) (throw 'unpackable "#xC1 cannot be unpacked"))
      ((= tag #xC2) #f)
      ((= tag #xC3) #t)
      ((= tag #xC4) (unpack-bytes (unpack-integer 1 #f in) in))
      ((= tag #xC5) (unpack-bytes (unpack-integer 2 #f in) in))
      ((= tag #xC6) (unpack-bytes (unpack-integer 4 #f in) in))
      ((= tag #xC7) (unpack-ext   (unpack-integer 1 #f in) in))
      ((= tag #xC8) (unpack-ext   (unpack-integer 2 #f in) in))
      ((= tag #xC9) (unpack-ext   (unpack-integer 4 #f in) in))
      ((= tag #xCA) (unpack-float 'single in))
      ((= tag #xCB) (unpack-float 'double in))
      ((= tag #xCC) (unpack-integer 1 #f in))
      ((= tag #xCD) (unpack-integer 2 #f in))
      ((= tag #xCE) (unpack-integer 4 #f in))
      ((= tag #xCF) (unpack-integer 8 #f in))
      ((= tag #xD0) (unpack-integer 1 #t in))
      ((= tag #xD1) (unpack-integer 2 #t in))
      ((= tag #xD2) (unpack-integer 4 #t in))
      ((= tag #xD3) (unpack-integer 8 #t in))
      ((= tag #xD4) (unpack-ext  1 in))
      ((= tag #xD5) (unpack-ext  2 in))
      ((= tag #xD6) (unpack-ext  4 in))
      ((= tag #xD7) (unpack-ext  8 in))
      ((= tag #xD8) (unpack-ext 16 in))
      ((= tag #xD9) (unpack-string (unpack-integer 1 #f in) in string-decoder))
      ((= tag #xDA) (unpack-string (unpack-integer 2 #f in) in string-decoder))
      ((= tag #xDB) (unpack-string (unpack-integer 4 #f in) in string-decoder))
      ((= tag #xDC) (unpack-array  (unpack-integer 2 #f in) in))
      ((= tag #xDD) (unpack-array  (unpack-integer 4 #f in) in))
      ((= tag #xDE) (unpack-map    (unpack-integer 2 #f in) in))
      ((= tag #xDF) (unpack-map    (unpack-integer 4 #f in) in))
      (else (* -1 (- #x100 tag)))))  ; Negative fixint
  (define tag (get-u8 in))
  (when (eof-object? tag)
    (throw 'empty-port))
  (let ((primitive (primitive-unpack)))
    (if (not (ext? primitive))
      primitive
      ;; Add new official extensions to the matches
      (match (ext-type primitive)
        (-1 (ext->time primitive))
        (_  primitive)))))


(define* (unpack bytes #:key (string-decoder utf8->string))
  "- Scheme Procedure: unpack bytes
     Unpack (de-serialise) all values from the bytevector BYTES.
     
     This procedure returns as many values as there are objects packed in
     BYTES. If BYTES is empty, no values will be returned."
  (let ((in (open-bytevector-input-port bytes))
        (objs '()))
    ;; Keep unpacking values from the bytes "forever" and accumulate results in
    ;; a list; eventually an EOF exception will be thrown because we ran out of
    ;; bytes.
    (catch 'empty-port
      (λ ()
        (while #t
          (set! objs (cons (unpack-from in #:string-decoder string-decoder) objs))))
      (λ (key . args)
        (apply values (reverse! objs))))))

;; ----------------------------------------------------------------------------
(define (unpack-integer size signed? in)
  "- Scheme Procedure: unpack-integer size signed? in
     Unpack an integer from binary input port ‘in’ of ‘size’ bytes length."
  (define proc (if signed? bytevector-sint-ref bytevector-uint-ref))
  (define in-bytes (get-bytevector-n in size))
  (proc in-bytes 0 (endianness big) size))

(define (unpack-float precision in)
  "- Scheme Procedure: unpack-float precision in
     Unpack a ‘precision’ floating-point number from binary input port ‘in’."
  (define in-bytes (get-bytevector-n in (if (eq? precision 'single) 4 8)))
  (match precision
    ('single (bytevector-ieee-single-ref in-bytes 0 (endianness big)))
    ('double (bytevector-ieee-double-ref in-bytes 0 (endianness big)))
    (_ (throw 'wrong-type-arg "Either 'single or 'double" precision))))

(define (unpack-map size in)
  "- Scheme Procedure: unpack-map size in
     Unpack a hash table of size ‘size’ from binary input port ‘in’, using
     ‘equal?’ for key equality."
  (catch 'empty-port
    (λ ()
      (do ((hash (make-hash-table size))
           (i 0 (1+ i)))
          ((= i size)
           hash)
        (hash-set! hash (unpack-from in) (unpack-from in))))
    (λ (key . args)
      (throw 'unexpected-eof))))

(define (unpack-array size in)
  "- Scheme Procedure: unpack-array size in
     Unpack a vector of size ‘size’ from binary input port ‘in’."
  (catch 'empty-port
    (λ ()
      (do ((array (make-vector size))
           (i 0 (1+ i)))
          ((= i size)
           array)
        (vector-set! array i (unpack-from in))))
    (λ (key . args)
      (throw 'unexpected-eof))))


(define (unpack-string size in string-decoder)
  "- Scheme Procedure: unpack-string size in proc
     Unpack a string of length ‘size’ from binary input port ‘in’.

     If PROC is not #f, (‘PROC’ bytevector) is called to decode the string,
     otherwise it is decoded as UTF-8"
  (define bytes (get-bytevector-n in size))
  (string-decoder bytes))

(define (unpack-bytes size in)
  "- Scheme Procedure: unpack-bytes size in
     Unpack a bytevector of length ‘size’ from binary input port ‘in’."
  (define bytes (get-bytevector-n in size))
  bytes)

(define (unpack-ext size in)
  "- Scheme Procedure: unpack-ext size in
     Unpack a MessagePack extension object of ‘size’ bytes length from binary
     input port ‘in’."
  (define type (bytevector-s8-ref (get-bytevector-n in 1) 0))
  (define data (get-bytevector-n in size))
  (ext type data))
