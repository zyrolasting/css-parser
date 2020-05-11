#lang racket/base

(require "../preprocess.rkt"
         "terms.rkt")

(module+ test
  (require rackunit)

  (define (check-consume-number str expected-type expected-value)
    (define-values (val type)
      (consume-number (open-input-string str)))
    (check-equal? type expected-type)

    (check-true
     (if (eq? type 'number)
         (< (abs (exact->inexact (- val expected-value))) 0.001)
         (equal? val expected-value))))

  (test-case "§4.3.12"
    (check-consume-number "100" 'integer 100)
    (check-consume-number "100.0" 'number 100)
    (check-consume-number "-100.91" 'number -100.91)
    (check-consume-number "9.7e-1" 'number #i9.7e-1)))

(define (sign-character? maybe-sign)
  (or (char=? maybe-sign #\u002B)
      (char=? maybe-sign #\u002D)))

(define (read-base10-digits in)
  (let loop ([next (peek-char in)] [accum null])
    (if (digit? next)
        (begin (read-char in)
               (loop (peek-char in) (cons next accum)))
        (values (string->number (apply string (reverse accum)))
                (length accum)))))

(define (exponent-indicator? maybe-e)
  (or (char=? maybe-e #\u0045)
      (char=? maybe-e #\u0065)))

; Implementation of §4.3.12
; Comments indicate steps followed from spec.
(define (consume-number in)
  ; 1., plus utilities
  (define repr null)
  (define type 'integer)

  (define (consume-char!)
    (set! repr (cons (read-char in) repr)))

  (define (consume-digits!)
    (let loop ([maybe-digit (peek-char/css in)])
      (when (digit? maybe-digit)
        (consume-char!)
        (loop (peek-char/css in)))))

  (define (maybe-consume-sign!)
    (when (sign-character? (peek-char/css in))
      (consume-char!)))

  ; 2.
  (maybe-consume-sign!)

  ; 3.
  (consume-digits!)

  ; 4.
  (define maybe-decimal-start (peek-char/css/multi in 2))
  (when (and (= (length maybe-decimal-start) 2)
             (char=? (car maybe-decimal-start) #\u002E)
             (digit? (cadr maybe-decimal-start)))
    (consume-char!)
    (consume-char!)
    (set! type 'number)
    (consume-digits!))

  ; 5.
  (define maybe-e-notation-start (peek-char/css/multi in 3))
  (define elen (length maybe-e-notation-start))
  (when (> elen 2)
    (let ([maybe-e (car maybe-e-notation-start)]
          [maybe-sign-or-digit (cadr maybe-e-notation-start)]
          [maybe-digit-or-other (if (= elen 3)
                                    (caddr maybe-e-notation-start)
                                    #f)])
      (when (and (or (char=? maybe-e #\u0045) (char=? maybe-e #\u0065))
                 (if (sign-character? maybe-sign-or-digit)
                     (digit? maybe-digit-or-other)
                     (digit? maybe-sign-or-digit)))
        (consume-char!)
        (maybe-consume-sign!)
        (consume-digits!))))

  (values (number-repr->racket-number (apply string (reverse repr)))
          type))

; §4.3.13
(define (number-repr->racket-number repr-string)
  (define in (open-input-string repr-string))

  ; 1. Read sign
  (define s
    (if (char=? (peek-char in) #\u002D)
        (begin (read-char in) -1)
        1))

  ; 2. Read integer part
  (define i
    (if (digit? (peek-char in))
        (let-values ([(i-val i-length)
                      (read-base10-digits in)])
          i-val)
        0))

  ; 3. Read decimal point
  (define decimal?
    (read-if in (λ (ch) (char=? ch #\u002E))))

  ; 4. Read fractional part
  (define-values (f d)
    (if decimal?
        (read-base10-digits in)
        (values 0 0)))

  ; 5. Read exponent indicator
  (define e-notation?
    (read-if in exponent-indicator?))

  ; 6. Exponent sign
  (define t
    (if e-notation?
        (if (read-if in (λ (ch) (char=? ch #\u002D)))
            -1 1)
        1))

  ; 7. Exponent
  (define e
    (if e-notation?
        (if (digit? (peek-char in))
            (let-values ([(e-val e-length)
                          (read-base10-digits in)])
              e-val)
            0)
        0))

  (* s (+ i (* f (expt 10 (- d)))) (expt 10 (* t e))))

(define (read-if in ?)
  (let ([ch (peek-char in)])
    (and (char? ch)
         (? ch)
         (read-char in))))
