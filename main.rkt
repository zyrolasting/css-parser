#lang racket/base

(require "preprocess.rkt")

; Follows §3, except it assumes UTF-8.
(define (parse-css in)
  (construct-tree (tokenize-css in)))
