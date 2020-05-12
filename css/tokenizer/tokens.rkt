#lang racket/base

; Token definitions as seen in §4.

(provide (all-defined-out))

(struct token () #:transparent)

; Keep alphabetized, plz.
(struct at-keyword-token token (value) #:transparent)
(struct bad-string-token token () #:transparent)
(struct bad-url-token token () #:transparent)
(struct cdc-token token () #:transparent)
(struct cdo-token token () #:transparent)
(struct colon-token token () #:transparent)
(struct comma-token token () #:transparent)
(struct delim-token token (value) #:transparent)
(struct dimension-token token (type value unit) #:transparent)
(struct eof-token token () #:transparent)
(struct function-token token (value) #:transparent)
(struct hash-token token (type value) #:transparent)
(struct ident-token token (value) #:transparent)
(struct l-curly-bracket-token token () #:transparent)
(struct l-paren-token token () #:transparent)
(struct l-square-bracket-token token () #:transparent)
(struct number-token token (type value) #:transparent)
(struct percentage-token token (value) #:transparent)
(struct r-curly-bracket-token token () #:transparent)
(struct r-paren-token token () #:transparent)
(struct r-square-bracket-token token () #:transparent)
(struct semicolon-token token () #:transparent)
(struct string-token token (value) #:transparent)
(struct url-token token (value) #:transparent)
(struct whitespace-token token () #:transparent)
