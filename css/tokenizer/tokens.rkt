#lang racket/base

; Token definitions as seen in §4.

(provide (all-defined-out))

(struct token ())

; Keep alphabetized, plz.
(struct at-keyword-token token ())
(struct bad-string-token token ())
(struct bad-url-token token ())
(struct cdc-token token ())
(struct cdo-token token ())
(struct colon-token token ())
(struct comma-token token ())
(struct delim-token token (value))
(struct dimension-token token ())
(struct eof-token token ())
(struct function-token token ())
(struct hash-token token (type value))
(struct ident-token token ())
(struct l-curly-bracket-token token ())
(struct l-paren-token token ())
(struct l-square-bracket-token token ())
(struct number-token token ())
(struct percentage-token token ())
(struct r-curly-bracket-token token ())
(struct r-paren-token token ())
(struct r-square-bracket-token token ())
(struct semicolon-token token ())
(struct string-token token (value))
(struct url-token token ())
(struct whitespace-token token ())
