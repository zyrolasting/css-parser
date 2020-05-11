#lang racket/base

; Token definitions as seen in §4.

(provide (all-defined-out))

(struct token ())
(struct delim-token token (value))
(struct hash-token token (type value))
(struct eof-token token ())
(struct l-paren-token token ())
(struct r-paren-token token ())
(struct comma-token token ())
(struct colon-token token ())
(struct semicolon-token token ())
(struct l-square-bracket-token token ())
(struct r-square-bracket-token token ())
(struct l-curly-bracket-token token ())
(struct r-curly-bracket-token token ())
(struct string-token token (value))
(struct bad-string-token token ())
