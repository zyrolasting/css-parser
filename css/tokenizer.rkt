#lang racket/base

(provide get-next-token
         tokenize)

(require "preprocess.rkt"
         "exn.rkt"
         "tokenizer/terms.rkt"
         "tokenizer/tokens.rkt"
         "tokenizer/strings.rkt"
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
  (define next (peek-char/css in))
  (cond [(eof-object? next) (eof-token)]
        [(starts-comment? in) (consume-comment in)]
        [(starts-string? next) (make-string-token in (read-char in) null)]
        [(char=? next #\u0028) (l-paren-token)]
        [(char=? next #\u0029) (r-paren-token)]
        [(char=? next #\u002C) (comma-token)]
        [(char=? next #\u003A) (colon-token)]
        [(char=? next #\u003B) (semicolon-token)]
        [(char=? next #\u005B) (l-square-bracket-token)]
        [(char=? next #\u005D) (r-square-bracket-token)]
        [(char=? next #\u007B) (l-curly-bracket-token)]
        [(char=? next #\u007D) (r-curly-bracket-token)]
        [else (delim-token next)]))
