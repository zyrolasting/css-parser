#lang racket/base

(provide get-next-token
         tokenize)

(require "preprocess.rkt"
         "exn.rkt"
         "tokenizer/code-points.rkt"
         "tokenizer/terms.rkt"
         "tokenizer/tokens.rkt"
         "tokenizer/identifiers.rkt"
         "tokenizer/names.rkt"
         "tokenizer/escapes.rkt"
         "tokenizer/numbers.rkt"
         "tokenizer/comments.rkt")

; §4
(define (tokenize in [out null])
  (let ([next (get-next-token in)])
    (if (eof-token? next)
        (reverse (cons next out))
        (tokenize in
                  (if (void? next)
                      out
                      (cons next out))))))

; §4.3.1
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

(define (consume-ident-like-token in)
  (void))

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


; §4.3.5
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
