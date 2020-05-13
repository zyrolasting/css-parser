#lang racket/base

; Token definitions as seen in §4.

(provide (all-defined-out))
(require (for-syntax racket/base
                     racket/syntax))

(define current-line (make-thread-cell #f))
(define current-column  (make-thread-cell #f))

(define (capture-position! in)
  (define-values (line col pos)
    (port-next-location in))
  (thread-cell-set! current-line line)
  (thread-cell-set! current-column col))

(struct token (line column) #:transparent)

(define-syntax (token-struct stx)
  (syntax-case stx ()
    [(_ name (fields ...))
     (with-syntax ([ctor (format-id #'name "make-~a" #'name)])
       #'(begin (struct name token (fields ...) #:transparent)
                (define (ctor . args)
                  (apply name
                         (thread-cell-ref current-line)
                         (thread-cell-ref current-column)
                         args))))]))

; Keep alphabetized, plz.
(token-struct at-keyword-token (value))
(token-struct bad-string-token ())
(token-struct bad-url-token ())
(token-struct cdc-token ())
(token-struct cdo-token ())
(token-struct colon-token ())
(token-struct comma-token ())
(token-struct delim-token (value))
(token-struct dimension-token (type value unit))
(token-struct eof-token ())
(token-struct function-token (value))
(token-struct hash-token (type value))
(token-struct ident-token (value))
(token-struct l-curly-bracket-token ())
(token-struct l-paren-token ())
(token-struct l-square-bracket-token ())
(token-struct number-token (type value))
(token-struct percentage-token (value))
(token-struct r-curly-bracket-token ())
(token-struct r-paren-token ())
(token-struct r-square-bracket-token ())
(token-struct semicolon-token ())
(token-struct string-token (value))
(token-struct url-token (value))
(token-struct whitespace-token ())

(define (get-token-value t)
  (and (token? t)
       (cond [(at-keyword-token? t) (at-keyword-token-value t)]
             [(delim-token? t) (delim-token-value t)]
             [(dimension-token? t) (dimension-token-value t)]
             [(function-token? t) (function-token-value t)]
             [(hash-token? t) (hash-token-value t)]
             [(ident-token? t) (ident-token-value t)]
             [(number-token? t) (number-token-value t)]
             [(percentage-token? t) (percentage-token-value t)]
             [(string-token? t) (string-token-value t)]
             [(url-token? t) (url-token-value t)]
             [else #f])))
