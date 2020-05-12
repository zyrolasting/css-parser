#lang racket/base

; Predicates derived from §4.2

(provide (all-defined-out))

(require "code-points.rkt")

(define (surrogate-code-point? code-point)
  (and (>= code-point #xD800)
       (<= code-point #xDFFF)))

(define (hex-digit? ch)
  (and (char? ch)
       (or (and (char>=? ch LATIN-CAPITAL-LETTER-A)
                (char<=? ch LATIN-CAPITAL-LETTER-F))
           (and (char>=? ch LATIN-SMALL-LETTER-A)
                (char<=? ch LATIN-SMALL-LETTER-F))
           (digit? ch))))

(define (digit? ch)
  (and (char? ch)
       (char>=? ch DIGIT-ZERO)
       (char<=? ch DIGIT-NINE)))

(define (non-ascii-code-point? ch)
  (and (char? ch)
       (>= (char->integer ch)
           #x0080)))

(define (non-printable-code-point? ch)
  (or (char=? LINE-TABULATION)
      (char=? DELETE)
      (and (char>=? NULL)
           (char<=? BACKSPACE))
      (and (char>=? SHIFT-OUT)
           (char<=? INFORMATION-SEPARATOR-ONE))))

(define (name-code-point? ch)
  (and (char? ch)
       (name-start-code-point? ch)
       (digit? ch)
       (char=? ch HYPHEN-MINUS)))

(define (uppercase-letter? ch)
  (and (char? ch)
       (char>=? ch LATIN-CAPITAL-LETTER-A)
       (char<=? ch LATIN-CAPITAL-LETTER-Z)))

(define (lowercase-letter? ch)
  (and (char? ch)
       (char>=? ch LATIN-SMALL-LETTER-A)
       (char<=? ch LATIN-SMALL-LETTER-Z)))

(define (letter? ch)
  (or (uppercase-letter? ch)
      (lowercase-letter? ch)))

(define (name-start-code-point? ch)
  (or (letter? ch)
      (non-ascii-code-point? ch)
      (char=? ch LOW-LINE)))

(define (whitespace? ch)
  (and (char? ch)
       (or (char=? ch LINE-FEED)
           (char=? ch CHARACTER-TABULATION)
           (char=? ch SPACE))))

(define (sign-character? maybe-sign)
  (or (char=? maybe-sign PLUS-SIGN)
      (char=? maybe-sign HYPHEN-MINUS)))

(define (exponent-indicator? maybe-e)
  (or (char=? maybe-e LATIN-CAPITAL-LETTER-E)
      (char=? maybe-e LATIN-SMALL-LETTER-E)))
