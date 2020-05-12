#lang racket/base

(require racket/sequence
         "tokenizer/tokens.rkt"
         "tokenizer.rkt"
         "exn.rkt")

(struct exn:fail:css:syntax exn:fail (line column))

(define (make-syntax-error/css msg [tok (get-next-token)])
  (exn:fail:css:syntax msg
                       (current-continuation-marks)
                       (token-line tok)
                       (token-column tok)))

(define (raise-syntax-error/css msg [tok (get-next-token)])
  (raise (make-syntax-error/css msg tok)))

;=======================================================
; §5: Parse tree nodes and definitions
;=======================================================

(struct css-node (line col))
(struct at-rule css-node (name prelude block))
(struct qualified-rule css-node (prelude block))
(struct declaration (name value important))
(struct simple-block (token value))
(struct function (name value))
(struct stylesheet (rules))

(define top-level? (make-parameter #f))

(define (component-value? v)
  (or (component-value? v)
      (simple-block? v)
      (function? v)))

(define (preserved-token? tok)
  (and (token? tok)
       (not (function-token? tok))
       (not (l-curly-bracket-token? tok))
       (not (l-paren-token? tok))
       (not (l-square-bracket-token? tok))))



;=======================================================
; §5.2
;=======================================================

(define current-token (make-thread-cell #f))
(define next-token (make-thread-cell #f))
(define reconsume? (make-thread-cell #f))

(define (consume-next-token seq)
  (if (thread-cell-ref reconsume?)
      (thread-cell-set! reconsume? #f)
      (begin (thread-cell-set! current-token (thread-cell-ref next-token))
             (thread-cell-set! next-token (sequence-ref seq 0))))
  (thread-cell-ref current-token))

(define (get-current-token)
  (thread-cell-ref current-token))

(define (get-next-token)
  (thread-cell-ref next-token))

(define (reconsume-current-token seq)
  (thread-cell-set! reconsume? #t))


;=======================================================
; §5.3
;=======================================================

(define (normalize-argument in)
  (cond [(string? in)
         (tokenize (open-input-string in))]
        [(input-port? in)
         (tokenize in)]
        [else in]))


;=======================================================
; §5.3.2: Parse a stylesheet
;=======================================================

(define (parse-stylesheet tokens)
  (stylesheet (parameterize ([top-level? #t])
                (consume-rule-list tokens))))


;=======================================================
; §5.3.3: Parse a list of rules
;=======================================================

(define (parse-rule-list tokens)
  (parameterize ([top-level? #f])
    (consume-rule-list tokens)))


;=======================================================
; §5.3.4: Parse a rule
;=======================================================

(define (parse-rule tokens)
  (with-handlers ([exn:fail:css:syntax? values])
    (consume-leading-whitespace-tokens tokens)

    (define rule
      (cond [(eof-token? (get-next-token))
             (raise-syntax-error/css "Unexpected EOF when parsing rule")]
            [(at-keyword-token? (get-next-token))
             (consume-at-rule tokens)]
            [else (consume-qualified-rule)]))

    (unless rule
      (raise-syntax-error/css "Could not parse rule"))

    (consume-leading-whitespace-tokens tokens)
    (if (eof-token? (get-next-token))
        rule
        (make-syntax-error/css "Expected EOF after parsing rule"))))


;=======================================================
; §5.3.5: Parse a declaration, not an at-rule
;=======================================================

(define (parse-declaration tokens)
  (consume-leading-whitespace-tokens tokens)
  (cond [(not (ident-token? (get-next-token)))
         (make-syntax-error/css "Expected ident token"
                                (get-next-token))]
        [else
         (let* ([err (make-syntax-error/css
                      "Expected declaration"
                      (get-next-token))]
                [decl (consume-declaration tokens)])
           (or decl err))]))


;=======================================================
; §5.3.6: Parse a list of declarations (incl. at rules)
;=======================================================

(define (parse-declarations tokens)
  (consume-declaration-list tokens))


;=======================================================
; §5.3.7: Parse a component value
;=======================================================

(define (parse-component-value tokens [value #f])
  (consume-leading-whitespace-tokens tokens)
  (define next (get-next-token))
  (cond [(eof-token? next)
         (or value (make-syntax-error/css "Unexpected EOF when parsing component value" next))]
        [else (parse-component-value tokens (consume-component-value))]))


;=======================================================
; §5.3.8: Parse list of component values
;=======================================================

(define (parse-component-value-list tokens [out null])
  (define next (consume-component-value tokens))
  (if (eof-token? next)
      (reverse out)
      (parse-component-value-list tokens (cons next out))))


;=======================================================
; §5.3.9: Parse comma-separated list of component values
;=======================================================

(define (parse-comma-separated-component-value-list tokens [out null] [csv null])
  (define next (consume-component-value tokens))
  (cond [(eof-token? next)
         (reverse out)]
        [(comma-token? next)
         (parse-comma-separated-component-value-list tokens (cons csv out) null)]
        [else
         (parse-comma-separated-component-value-list tokens out (cons next csv))]))


;=======================================================
; §5.4: Parser algorithms
;=======================================================

;=======================================================
; §5.4.1: Consume a list of rules
;=======================================================

(define (consume-rule-list tokens [out null])
  (define current (consume-next-token tokens))
  (cond [(eof-token? current) (reverse out)]
        [(whitespace-token? current)
         (consume-rule-list tokens out)]
        [(or (cdo-token? current)
             (cdc-token? current))
         (if (top-level?)
             (consume-rule-list tokens out)
             (begin (reconsume-current-token)
                    (let ([rule (consume-qualified-rule tokens)])
                      (consume-rule-list tokens (if rule (cons rule out) out)))))]
        [(at-keyword-token? current)
         (reconsume-current-token)
         (consume-rule-list tokens (cons (consume-at-rule) out))]
        [else (reconsume-current-token)
              (let ([rule (consume-qualified-rule tokens)])
                (consume-rule-list tokens (if rule (cons rule out) out)))]))

;=======================================================
; §5.4.2: Consume an at-rule
;=======================================================

(define (consume-at-rule tokens)
  (consume-next-token)

  (define name (get-token-value (get-current-token)))

  (define (build current prelude block)
    (at-rule (token-line current)
             (token-column current)
             name
             prelude
             block))

  (let loop ([prelude null])
    (consume-next-token)
    (define current (get-current-token))
    (cond [(semicolon-token? current)
           (build current prelude #f)]
          [(eof-token? current)
           (maybe-raise
            (make-syntax-error/css "Unexpected EOF in at-rule"
                                   current))
           (build current null #f)]
          [(l-curly-bracket-token? current)
           (build current
                  prelude
                  (consume-simple-block tokens))]
          [else (reconsume-current-token)
                (loop (cons (consume-component-value tokens)
                            prelude))])))


;=======================================================
; §5.4.3: Consume a qualified rule
;=======================================================

(define (consume-qualified-rule tokens)
  (define start-tok (get-next-token))

  (define-values (line col)
    (values (token-line start-tok)
            (token-column start-tok)))

  (let loop ([prelude null])
    (define current (consume-next-token tokens))
    (cond [(eof-token? current)
           (maybe-raise (make-syntax-error/css
                         "Unexpected EOF in qualified rule"
                         start-tok))
           #f]
          [(l-curly-bracket-token? current)
           (qualified-rule line col
                           prelude
                           (consume-simple-block tokens))]
          [(and (simple-block? current)
                (l-curly-bracket-token? (simple-block-token current)))
           (qualified-rule line col
                           prelude
                           current)]
          [else (reconsume-current-token)
                (loop (cons (consume-component-value tokens) prelude))])))


;=======================================================
; §5.4.4: Consume a list of declarations
;=======================================================

(define (consume-declaration-list tokens [decls null])
  (define current (consume-next-token tokens))
  (cond [(or (whitespace-token? current)
             (semicolon-token? current))
         (consume-declaration-list tokens decls)]
        [(eof-token? current)
         (reverse decls)]
        [(at-keyword-token? current)
         (reconsume-current-token)
         (consume-declaration-list tokens
                                   (cons (consume-at-rule tokens)
                                         decls))]
        [(ident-token? current)
         (define temp-component-values
           (let loop ([tmp (list current)])
             (define next (get-next-token))
             (if (and (not (semicolon-token? next))
                      (not (eof-token? next)))
                 (loop (cons (consume-component-value tokens) tmp))
                 (reverse tmp))))
         (define maybe-decl
           (consume-declaration temp-component-values))

         (consume-declaration-list tokens (if maybe-decl
                                              (cons maybe-decl decls)
                                              decls))]

        [else
         (maybe-raise (make-syntax-error/css ""))
         (reconsume-current-token)
         (let loop ([tmp (list current)])
           (define next (get-next-token))
           (when (and (not (semicolon-token? next))
                      (not (eof-token? next)))
             (consume-component-value tokens)))]))


;=======================================================
; §5.4.5: Consume a declaration
;=======================================================

(define (consume-declaration tokens)
  (with-handlers ([(λ (x) (not (exn? x))) values])
    (define name (get-token-value (get-current-token)))
    (consume-leading-whitespace-tokens tokens)

    (if (colon-token? (get-next-token))
        (consume-next-token tokens)
        (begin (maybe-raise (make-syntax-error/css "Expected colon in declaration"
                                                   (get-next-token)))
               (raise #f)))

    (consume-leading-whitespace-tokens tokens)

    (define (trim-ws l)
      (if (null? l)
          l
          (if (whitespace-token? (car l))
              (trim-ws (cdr l))
              l)))

    (define raw-value
       (let loop ([wip null])
         (if (eof-token? (get-next-token))
             (trim-ws wip)
             (loop (cons (consume-next-token) wip)))))

    (define-values (important? final-values)
      (let* ([end (car raw-value)]
             [adj (cadr raw-value)]
             [imp (and (delim-token? adj)
                       (equal? (delim-token-value adj) "!")
                       (ident-token? end)
                       (equal? (string-downcase (ident-token-value end))
                               "important"))])
        (values imp (if imp
                        (trim-ws (cddr raw-value))
                        raw-value))))

    (declaration name
                 (reverse final-values)
                 important?)))



;=======================================================
; §5.4.6: Consume a component value
;=======================================================

(define (consume-component-value tokens)
  (define current (consume-next-token))
  (cond [(or (l-curly-bracket-token? current)
             (l-square-bracket-token? current)
             (l-paren-token? current))
         (consume-simple-block tokens)]
        [(function-token? current)
         (consume-function current)]
        [else current]))


;=======================================================
; §5.4.7: Consume a simple block
;=======================================================

(define (consume-simple-block tokens)
  (define starting-token (get-current-token))
  (define ending-token?
    (cond [(l-curly-bracket-token? starting-token)
           r-curly-bracket-token?]
          [(l-square-bracket-token? starting-token)
           r-square-bracket-token?]
          [(l-paren-token? starting-token)
           r-paren-token?]))

  (let loop ([value null])
    (define in-body (consume-next-token))
    (cond [(eof-token? in-body)
           (maybe-raise (make-syntax-error/css "Unexpected EOF in simple block"
                                               starting-token))
           (simple-block starting-token (reverse value))]
          [(ending-token? in-body)
           (simple-block starting-token (reverse value))]
          [else (reconsume-current-token)
                (loop (cons (consume-component-value tokens)
                            value))])))


;=======================================================
; §5.4.8: Consume a function
;=======================================================

(define (consume-function tokens)
  (define starting-token (get-current-token))
  (define name (get-token-value starting-token))
  (let loop ([value null])
    (define in-body (consume-next-token))
    (cond [(eof-token? in-body)
           (maybe-raise (make-syntax-error/css "Unexpected EOF in function"
                                               starting-token))
           (function name (reverse value))]
          [(l-paren-token? in-body)
           (function name (reverse value))]
          [else (reconsume-current-token)
                (loop (cons (consume-component-value tokens)
                            value))])))


;=======================================================
; Extras
;=======================================================

(define (consume-leading-whitespace-tokens tokens)
  (when (whitespace-token? (get-next-token))
    (consume-next-token tokens)
    (consume-leading-whitespace-tokens tokens)))
