#lang racket/base

(provide starts-comment?
         consume-comment)

(require "terms.rkt"
         "../exn.rkt"
         "../preprocess.rkt")

; Comments (§4.3.2)
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
