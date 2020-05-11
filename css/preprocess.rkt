#lang racket/base

(require racket/port)

(provide peek-char/css
         read-char/css
         peek-char/css/multi
         peek-string/css)

; This module complies with
; https://www.w3.org/TR/css-syntax-3/#input-preprocessing

; Returns the next preprocessed character according to §3.3,
; and the number of characters to actually read to consider
; a character consumed from the tokenizer's perspective.
(define (peek-preprocessed-char in)
  (define next (peek-char in))
  (cond [(eof-object? next)
         (values next 1)]
        [(char=? next #\u000C)
         (values #\u000A 1)]
        [(char=? next #\u000D)
         (if (string=? (string #\u000D #\u000A) (peek-string 2 0 in))
             (values #\u000A 2)
             (values next 1))]
        [(char=? next #\nul)
         (values #\uFFFD 1)]
        [else (values next 1)]))

(define (peek-char/css in)
  (let-values ([(ch _) (peek-preprocessed-char in)]) ch))

(define (read-char/css in)
  (let-values ([(ch n) (peek-preprocessed-char in)])
    (for ([i (in-range n)]) (read-char in))
    ch))

(define (peek-char/css/multi in amt)
  (define pin (peeking-input-port in))
  (let loop ([accum null] [peeked 0])
    (define ch (read-char/css pin))
    (if (or (eof-object? ch)
            (= peeked amt))
        (reverse accum)
        (loop (cons ch accum)
              (add1 peeked)))))

(define (peek-string/css in amt)
  (apply string (peek-char/css/multi in amt)))

(module+ test
  (require rackunit)

  (test-equal? "Can peek into multiple characters, with preprocessing"
               (peek-char/css/multi (open-input-string (string #\u000D #\u000A #\u000C)) 2)
               '(#\u000A #\u000A))

  (test-case "Form feeds appear as line feeds"
    (define-values (next n)
      (peek-preprocessed-char (open-input-string (string #\u000C))))
    (check-eq? n 1)
    (check-equal? #\u000A next))

  (test-case "Carriage returns are preserved when not followed by a line feed."
    (define-values (next n)
      (peek-preprocessed-char (open-input-string (string #\u000D #\u000C))))
    (check-eq? n 1)
    (check-equal? #\u000D next))

  (test-case "Carriage returns are discarded when followed by a line feed."
    (define-values (next n)
      (peek-preprocessed-char (open-input-string (string #\u000D #\u000A))))
    (check-eq? n 2)
    (check-equal? #\u000A next))

  (test-case "Null characters appear as replacement chars."
    (define-values (next n)
      (peek-preprocessed-char (open-input-string (string #\nul))))
    (check-eq? n 1)
    (check-equal? #\uFFFD next))

  (test-case "Null characters appear as replacement chars."
    (define-values (next n)
      (peek-preprocessed-char (open-input-string (string #\nul))))
    (check-eq? n 1)
    (check-equal? #\uFFFD next)))
