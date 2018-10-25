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

(define-module (msgpack pack)
  #:export (pack-to pack packing-table float-precision))

(use-modules ((msgpack nothing)
              #:select (nothing?))
             ((msgpack ext)
              #:select (ext? ext-type ext-data))
             ((msgpack ext timestamp)
              #:select (time->ext))
             ((ice-9 match)
              #:select (match))
             ((ice-9 binary-ports)
              #:select (open-bytevector-output-port
                        put-u8
                        put-bytevector))
             ((rnrs bytevectors)
              #:select (bytevector?
                        make-bytevector
                        bytevector-length
                        bytevector-u8-set!
                        bytevector-s8-set!
                        bytevector-sint-set!
                        bytevector-uint-set!
                        bytevector-ieee-single-set!
                        bytevector-ieee-double-set!
                        string->utf8
                        endianness))
             ((srfi srfi-19)
              #:select (time?)))


;; ----------------------------------------------------------------------------
;; Used for packing float
(define float-precision
  (make-parameter 'double
    (λ (v)
      (match v
        ((or 'single 'double) v)
        (_ (throw 'wrong-type-arg "Must be either 'single or 'double"))))))


;; ----------------------------------------------------------------------------
(define (pack-nothing out datum)
  (put-u8 out #xC0))

(define (pack-bool out value)
  (put-u8 out (if value #xC3 #xC2)))

(define (pack-int out int)
  (define (write-int signed? size tag)
    (define bv (make-bytevector (+ size 1)))
    (bytevector-u8-set! bv 0 tag)
    ((if signed? bytevector-sint-set! bytevector-uint-set!)
       bv 1 int (endianness big) size)
    (put-bytevector out bv))
  (if (>= int 0)
    (cond
      ((< int (expt 2  7)) (put-u8 out int))
      ((< int (expt 2  8)) (write-int #f 1 #xCC))
      ((< int (expt 2 16)) (write-int #f 2 #xCD))
      ((< int (expt 2 32)) (write-int #f 4 #xCE))
      ((< int (expt 2 64)) (write-int #f 8 #xCF))
      (else (throw 'integer-overflow int)))
    (cond
      ((<= (- (expt 2  5)) int) (put-u8 out (bit-extract int 0 8)))
      ((<= (- (expt 2  7)) int) (write-int #t 1 #xD0))
      ((<= (- (expt 2 15)) int) (write-int #t 2 #xD1))
      ((<= (- (expt 2 31)) int) (write-int #t 4 #xD2))
      ((<= (- (expt 2 63)) int) (write-int #t 8 #xD3))
      (else (throw 'integer-underflow int)))))

(define (pack-float out float)
  (define-values (size tag proc)
    (match (float-precision)
      ('single (values 4 #xCA bytevector-ieee-single-set!))
      ('double (values 8 #xCB bytevector-ieee-double-set!))))
  (define bv (make-bytevector (1+ size)))
  (bytevector-u8-set! bv 0 tag)
  (proc bv 1 float (endianness big))
  (put-bytevector out bv))

(define (pack-string out string)
  (define size (string-utf8-length string))
  (define bv   (string->utf8 string))
  (define-values (size-field-length tag)
    (cond
      ((< size (expt 2  5)) (values 0 (logior #b10100000 size)))
      ((< size (expt 2  8)) (values 1 #xD9))
      ((< size (expt 2 16)) (values 2 #xDA))
      ((< size (expt 2 32)) (values 4 #xDB))
      (else (throw 'string-overflow string))))
  (put-u8 out tag)
  (unless (= size-field-length 0)
    (let ((size-field (make-bytevector size-field-length)))
      (bytevector-uint-set! size-field 0 size (endianness big) size-field-length)
      (put-bytevector out size-field)))
  (put-bytevector out bv))

(define (pack-symbol out sym)
  (pack-string out (symbol->string sym)))

(define (pack-bin out bin)
  (define size (bytevector-length bin))
  (define-values (tag size-length)
    (cond
      ((< size (expt 2  8)) (values #xC4 1))
      ((< size (expt 2 16)) (values #xC5 2))
      ((< size (expt 2 32)) (values #xC6 4))
      (else (throw 'binary-overflow bin))))
  (define bv (make-bytevector (+ 1 size-length)))
  (bytevector-u8-set! bv 0 tag)
  (bytevector-uint-set! bv 1 size (endianness big) size-length)
  (put-bytevector out  bv)
  (put-bytevector out bin))

(define (pack-vector out v)
  (define size (vector-length v))
  (define tag
    (cond
      ((< size (expt 2  4)) (logior #b10010000 size))
      ((< size (expt 2 16)) #xDC)
      ((< size (expt 2 32)) #xDD)
      (else (throw 'array-overflow v))))
  (put-u8 out tag)
  (when (>= size (expt 2 4))
    (let* ((nbytes (if (< size (expt 2 16)) 2 4))
           (bv (make-bytevector nbytes)))
      (bytevector-uint-set! bv 0 size (endianness big) nbytes)
      (put-bytevector out bv)))
  (do ((i 0 (1+ i)))
      ((= i size))
    (pack-to out (vector-ref v i))))

(define (pack-hash-table out table)
  (define (pack-entry key value)
    (pack-to out key)
    (pack-to out value))
  (define size (hash-count (const #t) table))
  (define tag
    (cond
      ((< size (expt 2  4)) (logior #b10000000 size))
      ((< size (expt 2 16)) #xDE)
      ((< size (expt 2 32)) #xDF)
      (else (throw 'map-overflow table))))
  (put-u8 out tag)
  (when (>= size (expt 2 4))
    (let* ((nbytes (if (< size (expt 2 16)) 2 4))
           (bv (make-bytevector nbytes)))
      (bytevector-uint-set! bv 0 size (endianness big) nbytes)
      (put-bytevector out bv)))
  (hash-for-each pack-entry table))

(define (pack-ext out e)
  (define type (ext-type e))
  (define data (ext-data e))
  (define size (bytevector-length data))
  (define-values (tag nbytes)  ; tag and number of bytes to write the size
    (cond
      ((= size  1)          (values #xD4 0))
      ((= size  2)          (values #xD5 0))
      ((= size  4)          (values #xD6 0))
      ((= size  8)          (values #xD7 0))
      ((= size 16)          (values #xD8 0))
      ((< size (expt 2  8)) (values #xC7 1))
      ((< size (expt 2 16)) (values #xC8 2))
      ((< size (expt 2 32)) (values #xC9 4))
      (else (throw 'ext-overflow e  "Data sequence too long"))))
  (put-u8 out tag)
  (when (< tag #xD4)
    (let ((bv (make-bytevector nbytes)))
      (bytevector-uint-set! bv 0 size (endianness big) nbytes)
      (put-bytevector out bv)))
  ;; The 'type' is an 8bit signed integer, so we need this round trip first
  (let ((bv (make-bytevector 1)))
    (bytevector-s8-set! bv 0 type)
    (put-bytevector out bv))
  (put-bytevector out data))


;; ----------------------------------------------------------------------------
(define packing-table
  (let ((int?   (λ (value) (and (integer? value) (exact?   value))))
        (float? (λ (value) (and (real?    value) (inexact? value)))))
    (make-parameter
      `((,nothing?    . ,pack-nothing)
        (,boolean?    . ,pack-bool)
        (,int?        . ,pack-int)
        (,float?      . ,pack-float)
        (,string?     . ,pack-string)
        (,symbol?     . ,pack-symbol)
        (,bytevector? . ,pack-bin)
        (,vector?     . ,pack-vector)
        ;; List
        (,hash-table? . ,pack-hash-table)
        (,ext?        . ,pack-ext)
        (,time?       . ,(λ (out t)
                           (pack-ext out (time->ext t))))))))


;; ----------------------------------------------------------------------------
(define (pack-to out . data)
  "- Scheme procedure: pack-to out [datum ...]
     Write each of the DATUM objects in serialised form to the binary output
     port OUT.
     
     DATUM must be an object of a type which can be serialised as a MessagePack
     object."
  (define (pack-datum datum)
    (do ((table (packing-table) (cdr table))
         (success? #f))
        ((or success? (null? table))
         (unless success?
           (throw 'unpackable datum)))
      (let ((pred (car (car table)))
            (proc (cdr (car table))))
        (when (pred datum)
          (proc out datum)
          (set! success? #t)))))
  (do ((rest data (cdr rest)))
      ((null? rest))
    (pack-datum (car rest))))

(define (pack . data)
  "- Scheme procedure: pack [datum ...]
     Return the bytevector that results from packing the DATUMs.
     
     Each DATUM must be an object of a type which can be serialised as a
     MessagePack object."
  (call-with-values (λ () (open-bytevector-output-port))
    (λ (out get-bv)
      (apply pack-to (cons out data))
      (get-bv))))
