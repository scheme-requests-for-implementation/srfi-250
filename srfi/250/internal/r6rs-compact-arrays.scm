; SPDX-FileCopyrightText: 2025 Daphne Preston-Kendal
;
; SPDX-License-Identifier: MIT

(define (make-compact-array size)
  (cond ((fx<? size #xFF)
         (let ((bv (make-bytevector (fx+ size 2) #xFF)))
           (bytevector-u8-set! bv 0 1)
           (bytevector-u8-set! bv 1 size)
           bv))
        ((fx<? size #xFFFF)
         (let ((bv (make-bytevector (fxarithmetic-shift-left (fx+ size 2) 1) #xFF)))
           (bytevector-u8-set! bv 0 2)
           (bytevector-u16-native-set! bv 2 size)
           bv))
        ((fx<? size #xFFFFFFFF)
         (let ((bv (make-bytevector (fxarithmetic-shift-left (fx+ size 2) 2) #xFF)))
           (bytevector-u8-set! bv 0 4)
           (bytevector-u32-native-set! bv 4 size)
           bv))
        (else
         (let ((bv (make-bytevector (fxarithmetic-shift-left (fx+ size 2) 3) #xFF)))
           (bytevector-u8-set! bv 0 8)
           (bytevector-u64-native-set! bv 8 size)
           bv))))

(define (compact-array-ref sa idx)
  (define (max-to x n) (if (eqv? x n) #f x))
  (case (bytevector-u8-ref sa 0)
    ((1) (max-to (bytevector-u8-ref sa (fx+ idx 2)) #xFF))
    ((2) (max-to (bytevector-u16-native-ref sa (fxarithmetic-shift-left (fx+ idx 2) 1)) #xFFFF))
    ((4) (max-to (bytevector-u32-native-ref sa (fxarithmetic-shift-left (fx+ idx 2) 2)) #xFFFFFFFF))
    ((8) (max-to (bytevector-u64-native-ref sa (fxarithmetic-shift-left (fx+ idx 2) 3)) #xFFFFFFFFFFFFFFFF))))

(define (compact-array-set? sa idx)
  (not (not (compact-array-ref sa idx))))

(define (compact-array-set! sa idx val)
  (case (bytevector-u8-ref sa 0)
    ((1) (bytevector-u8-set! sa (fx+ idx 2) val))
    ((2) (bytevector-u16-native-set! sa (fxarithmetic-shift-left (fx+ idx 2) 1) val))
    ((4) (bytevector-u32-native-set! sa (fxarithmetic-shift-left (fx+ idx 2) 2) val))
    ((8) (fxarithmetic-shift-left (fx+ idx 2) 3))))

(define (compact-array-delete! sa idx)
  (case (bytevector-u8-ref sa 0)
    ((1) (bytevector-u8-set! sa (fx+ idx 2) #xFF))
    ((2) (bytevector-u16-native-set! sa (fxarithmetic-shift-left (fx+ idx 2) 1) #xFFFF))
    ((4) (bytevector-u32-native-set! sa (fxarithmetic-shift-left (fx+ idx 2) 2) #xFFFFFFFF))
    ((8) (bytevector-u64-native-set! sa (fxarithmetic-shift-left (fx+ idx 2) 3) #xFFFFFFFFFFFFFFFF))))

(define (compact-array-clear! sa)
  (let ((elt-size (bytevector-u8-ref sa 0)))
    (bytevector-fill! sa #xFF)
    (bytevector-u8-set! sa 0 elt-size)
    (case elt-size
      ((1) (bytevector-u8-set! sa 1 (fx- (bytevector-length sa) 2)))
      ((2) (bytevector-u16-native-set! sa 2
                                       (fx- (fxarithmetic-shift-right (bytevector-length sa) 1) 2)))
      ((4) (bytevector-u32-native-set! sa 4
                                       (fx- (fxarithmetic-shift-right (bytevector-length sa) 2) 2)))
      ((8) (bytevector-u32-native-set! sa 8
                                       (fx- (fxarithmetic-shift-right (bytevector-length sa) 3) 2))))))

(define (compact-array-copy sa) (bytevector-copy sa))

(define (compact-array-length sa)
  (case (bytevector-u8-ref sa 0)
    ((1) (bytevector-u8-ref sa 1))
    ((2) (bytevector-u16-native-ref sa 2))
    ((4) (bytevector-u32-native-ref sa 4))
    ((8) (bytevector-u64-native-ref sa 8))))
