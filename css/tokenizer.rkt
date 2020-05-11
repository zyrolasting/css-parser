#lang racket/base

(provide get-next-token
         tokenize)

(require "preprocess.rkt"
         "exn.rkt"
         "tokenizer/code-points.rkt"
         "tokenizer/terms.rkt"
         "tokenizer/tokens.rkt")

;=======================================================
; §4: Tokenization
;=======================================================

(define (tokenize in [out null])
  (let ([next (get-next-token in)])
    (if (eof-token? next)
        (reverse (cons next out))
        (tokenize in
                  (if (void? next)
                      out
                      (cons next out))))))


;=======================================================
; §4.3.1: Consume a token
;=======================================================

(define (get-next-token in)
  (let loop ()
    (when (starts-comment? in)
      (consume-comment in)
      (loop)))

  (define next (peek-char/css in))
  (define (? ch) (char=? next ch))

  (cond [(eof-object? next) (eof-token)]
        [(whitespace? next) (consume-whitespace-token in)]
        [(? QUOTATION-MARK) (on-string-start in)]
        [(? NUMBER-SIGN) (on-number-sign in)]
        [(? APOSTROPHE) (on-string-start in)]
        [(? LEFT-PARENTHESIS) (l-paren-token)]
        [(? RIGHT-PARENTHESIS) (r-paren-token)]
        [(? PLUS-SIGN) (make-numeric-or-delim-token in)]
        [(? COMMA) (comma-token)]
        [(? HYPHEN-MINUS) (on-hyphen-minus in)]
        [(? FULL-STOP) (on-full-stop in)]
        [(? COLON) (colon-token)]
        [(? SEMICOLON) (semicolon-token)]
        [(? LESS-THAN-SIGN) (on-less-than in)]
        [(? COMMERCIAL-AT) (on-commercial-at in)]
        [(? LEFT-SQUARE-BRACKET) (l-square-bracket-token)]
        [(? RIGHT-SQUARE-BRACKET) (r-square-bracket-token)]
        [(? LEFT-CURLY-BRACKET) (l-curly-bracket-token)]
        [(? RIGHT-CURLY-BRACKET) (r-curly-bracket-token)]
        [(digit? next) (consume-numeric-token in)]
        [(name-start-code-point? next) (consume-ident-like-token in)]
        [else (delim-token next)]))

(define (on-string-start in)
  (consume-string-token in (read-char in) null))

(define (on-hyphen-minus in)
  (void))

(define (on-commercial-at in)
  (void))

(define (on-full-stop in)
  (void))

(define (on-less-than in)
  (void))

(define (on-number-sign in)
  (read-char in) ; Discard #
  (if (or (name-code-point? (peek-char/css in))
          (valid-escape? in))
      (hash-token (if (starts-identifier? in)
                      "id"
                      "unrestricted")
                  (consume-name in))
      (delim-token (read-char/css in))))

(define (consume-whitespace-token in [next (peek-char/css in)])
  (if (whitespace? next)
      (consume-whitespace-token in (begin (read-char/css in)
                                          (peek-char/css in)))
      (whitespace-token)))

(define (make-numeric-or-delim-token in)
  (read-char in) ; Discard + or -
  (if (starts-number? in)
      (consume-numeric-token in)
      (delim-token (peek-char/css in))))


;=======================================================
; §4.3.2: Consume comments
;=======================================================

(define (starts-comment? in)
  (equal? (peek-char/css/multi in 2)
          '(#\u002F #\u002A)))

(define (ends-comment? in)
  (equal? (peek-char/css/multi in 2)
          '(#\u002A #\u002F)))

(define (consume-comment in)
  (define eof-err (make-parse-error in "Unexpected EOF in comment."))
  (read-char in)
  (read-char in)
  (let loop ([next (peek-char/css in)])
    (cond [(eof-object? next)
           (raise eof-err)]
          [(and (char=? next #\u002A)
                (ends-comment? in))
           (read-char in)
           (read-char in)]
          [else
           (read-char/css in)
           (loop (peek-char/css in))]))
  (void))


(module+ test
  (require racket/string
           rackunit)

  (test-case "Can see comment endpoints"
    (check-pred starts-comment? (open-input-string (string #\u002F #\u002A)))
    (check-pred ends-comment?   (open-input-string (string #\u002A #\u002F))))

  (test-case "§4.3.2: Consume comments"
    (define (run in)
      (check-pred void? (consume-comment in)))

    (test-case "Will eat up comments, and only comments"
      (define in (open-input-string "leading\n/* blippity bloppity */1/**//*a*/done"))
      (read-line in)
      (run in)
      (check-eq? (read-char in) #\1)
      (run in)
      (run in)
      (check-eq? (read in) 'done))

    (test-exn "Will complain about unexpected EOF."
              (λ (e)
                (and (exn:fail:css:parse? e)
                     (string-contains? (string-downcase (exn-message e))
                                       "eof")))
              (λ ()
                (consume-comment (open-input-string "/*  uh oh"))))))


;=======================================================
; §4.3.3: Consume a numeric token.
;=======================================================

(define (consume-numeric-token in)
  (define-values (number type) (consume-number in))
  (if (starts-identifier? in)
      (dimension-token type number (consume-name in))
      (if (char=? in PERCENTAGE-SIGN)
          (percentage-token number)
          (number-token type number))))


;=======================================================
; §4.3.4: Consume an ident-like token
;=======================================================

(define (consume-ident-like-token in)
  (void))


;=======================================================
; §4.3.5: Consume a string token
;=======================================================

(define (consume-string-token in ending-char [chars null])
  (let ([next (peek-char/css in)])
    (cond [(eof-object? next)
           (raise-parse-error in "Unexpected EOF when parsing string.")]

          [(char=? next ending-char)
           (read-char/css in) ; Consume closing quote.
           (string-token (apply string (reverse chars)))]

          [(char=? next #\u000A) ; Unescaped newline within string literal.
           (bad-string-token)]

          ; Process escaped characters
          [(char=? next #\u005C)
           (read-char/css in) ; Consume the '\'
           (let ([escaped (peek-char/css in)])
             (cond [(eof-object? escaped)
                    ; Lead into error case.
                    (consume-string-token in ending-char chars)]

                   [(char=? escaped #\u000A)
                    ; Escaped newline. Consume the newline and continue
                    ; as if the next line is part of the literal.
                    (read-char/css in)
                    (consume-string-token in ending-char chars)]

                   [else
                    (consume-string-token
                     in ending-char
                     (cons (read-escaped in) chars))]))]

          [else
           (consume-string-token
            in ending-char (cons (read-char/css in) chars))])))


; ======================================================
; §4.3.6: Consume a url token
; ======================================================

(define (consume-url-token in)
  (void))


; ======================================================
; §4.3.7: Consume an escaped code point.
; ======================================================

(define (read-escaped in)
  (let ([next (read-char/css in)])
    (cond [(eof-object? next)
           #\uFFFD]
          [(hex-digit? next)
           (define err (make-parse-error "Too many hex digits"))
           (define hex
             (string->number
              (let loop ([digits (list next)] [succ (peek-char in)])
                (if (> (length digits) 6)
                    (raise err)
                    (if (hex-digit? succ)
                        (begin
                          (read-char in)
                          (loop (cons succ digits)
                                (peek-char in)))
                        (apply string (reverse digits)))))))

           (when (char-whitespace? (peek-char/css in))
             (read-char/css in))

           (if (or (eq? hex 0)
                   (surrogate-code-point? hex)
                   (> hex #x10FFFF))
               #\uFFFD
               (integer->char hex))]

          [else next])))


; ======================================================
; §4.3.8: Check if two code points are a valid escape.
; ======================================================

(define valid-escape?
  (case-lambda
    [(in) (apply valid-escape?
                 (peek-char/css/multi in 2))]
    [(prev next)
     (and (char=? prev #\u005C)
          (not (char=? next #\u000A)))]))


;=======================================================
; §4.3.9: Check if three code points would start an
;         identifier.
;=======================================================

(define starts-identifier?
  (case-lambda
    [(in)
     (apply starts-identifier?
            (peek-char/css/multi in 3))]

    [(-1st -2nd -3rd)
     (cond [(char=? -1st #\u002D)
            (or (name-start-code-point? -2nd)
                (char=? -2nd #\u002D)
                (valid-escape? -2nd -3rd))]

           [(name-start-code-point? -1st) #t]

           [(char=? -1st #\u005C)
            (valid-escape? -1st -2nd)]

           [else #f])]))


;=======================================================
; §4.3.10: Check if three code points would start a
;          number.
;=======================================================

(define starts-number?
  (case-lambda
    [(in) (apply starts-number?
                 (peek-char/css/multi in 3))]
    [(-1st -2nd -3rd)
     (cond [(sign-character? -1st)
            (or (digit? -2nd)
                (and (char=? -2nd #\u002E)
                     (digit? -3rd)))]
           [(char=? -1st #\u002E)
            (digit? -2nd)]
           [else (digit? -1st)])]))


;=======================================================
; §4.3.11: Consume a name
;=======================================================

(define (consume-name in [out null])
  (let ([next (peek-char/css in)])
    (cond [(name-code-point? next)
           (consume-name in (cons (read-char/css in) out))]
          [(valid-escape? in)
           (consume-name in (cons (read-escaped in) out))]
          [else (apply string (reverse out))])))


;=======================================================
; §4.3.12: Consume a number.
;
; Comments indicate steps followed from spec.
;=======================================================

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


;=======================================================
; §4.3.13: Convert a string to a number
;=======================================================

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

(define (read-base10-digits in)
  (let loop ([next (peek-char in)] [accum null])
    (if (digit? next)
        (begin (read-char in)
               (loop (peek-char in) (cons next accum)))
        (values (string->number (apply string (reverse accum)))
                (length accum)))))


; ======================================================
; §4.3.14: Consume the remnants of a bad url.
; ======================================================

(define (consume-bad-url-remnants in)
  (void))
