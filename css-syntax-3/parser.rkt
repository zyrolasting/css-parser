#lang racket/base

(require racket/contract
         racket/generator
         "tokenizer/tokens.rkt"
         "tokenizer.rkt"
         "errors.rkt")

(module+ test
  (require rackunit))

;=======================================================
; §5: Parse tree nodes and definitions
;=======================================================

(provide (contract-out [component-value? predicate/c]
                       [preserved-token? predicate/c]
                       [parser-entry-input/c contract?])
         (struct-out css3-syntax-parse-node)
         (struct-out stylesheet))

(define-syntax-rule (parse-node id (fields ...))
  (begin (provide (struct-out id))
         (struct id css3-syntax-parse-node (fields ...))))

(define-syntax-rule (provide-entry-point id rng)
  (provide (contract-out [id (-> parser-entry-input/c rng)])))

(struct css3-syntax-parse-node (line col))
(parse-node stylesheet (rules))
(parse-node at-rule (name prelude block))
(parse-node qualified-rule (prelude block))
(parse-node declaration (name value important))
(parse-node simple-block (token value))
(parse-node function (name value))

(define parser-entry-input/c (or/c generator? string? input-port?))

(define top-level? (make-parameter #f))

(define (component-value? v)
  (or (preserved-token? v)
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

; Thread cells make reading from token sequences thread-safe.
(define current-token (make-thread-cell #f))
(define next-token (make-thread-cell #f))
(define reconsume? (make-thread-cell #f))

(define (consume-next-token gen)
  (if (thread-cell-ref reconsume?)
      (thread-cell-set! reconsume? #f)
      (begin (thread-cell-set! current-token (thread-cell-ref next-token))
             (thread-cell-set! next-token (gen))))
  (thread-cell-ref current-token))

(define (get-current-token)
  (thread-cell-ref current-token))

(define (get-next-token)
  (thread-cell-ref next-token))

(define (reconsume-current-token)
  (thread-cell-set! reconsume? #t))


;=======================================================
; §5.3
;=======================================================

(define (normalize-argument in [id 'normalize-argument])
  (define tokens
    (cond [(string? in)
           (tokenize (open-input-string in))]
          [(input-port? in)
           (tokenize in)]
          [(generator? in)
           in]
          [else (raise-argument-error id
                                      "A string, an input port with UTF-8 characters, or a generator."
                                      in)]))
  (thread-cell-set! next-token (tokens))
  tokens)


;=======================================================
; §5.3.1: Parse something according to a CSS grammar
;=======================================================

(provide
 (contract-out
  [parse-as (-> parser-entry-input/c
                (-> (listof component-value?))
                (or/c #f css3-syntax-parse-node? stylesheet?))]))

; Cheat by using a callback.
(define (parse-as in ?)
  (let ([comp-vals (parse-component-value-list in)])
    (and (? comp-vals)
         comp-vals)))

;=======================================================
; §5.3.2: Parse a stylesheet
;=======================================================

(provide-entry-point parse-stylesheet stylesheet?)

(define (parse-stylesheet in)
  (stylesheet #f #f
              (parameterize ([top-level? #t])
                (consume-rule-list (normalize-argument in)))))


;=======================================================
; §5.3.3: Parse a list of rules
;=======================================================

(provide-entry-point parse-rule-list (listof (or/c qualified-rule? at-rule?)))

(define (parse-rule-list in)
  (parameterize ([top-level? #f])
    (consume-rule-list (normalize-argument in))))


;=======================================================
; §5.3.4: Parse a rule
;=======================================================

(provide-entry-point parse-rule (or/c qualified-rule? at-rule?))

(define (parse-rule in)
  (define tokens (normalize-argument in))
  (with-handlers ([exn:fail:css3:syntax? (λ (e) (if (strict?) (raise e) e))])
    (consume-leading-whitespace-tokens tokens)

    (define has-rule-location (get-next-token))
    (define rule
      (cond [(eof-token? (get-next-token))
             (raise (make-css3-syntax-error has-rule-location
                                            "Unexpected EOF when parsing rule"))]
            [(at-keyword-token? (get-next-token))
             (consume-at-rule tokens)]
            [else (consume-qualified-rule tokens)]))

    (unless rule
      (maybe-raise-css3-syntax-error has-rule-location
                                     "Could not parse rule"))

    (consume-leading-whitespace-tokens tokens)
    (if (eof-token? (get-next-token))
        rule
        (make-css3-syntax-error (get-next-token)
                                "Expected EOF after parsing rule"))))


;=======================================================
; §5.3.5: Parse a declaration, not an at-rule
;=======================================================

(provide-entry-point parse-declaration declaration?)

(define (parse-declaration in)
  (define tokens (normalize-argument in))
  (consume-leading-whitespace-tokens tokens)
  (cond [(not (ident-token? (get-next-token)))
         (make-css3-syntax-error (get-next-token) "Expected ident token")]
        [else
         (let* ([err (make-css3-syntax-error (get-next-token) "Expected declaration")]
                [decl (consume-declaration tokens)])
           (or decl err))]))


;=======================================================
; §5.3.6: Parse a list of declarations (incl. at rules)
;=======================================================

(provide-entry-point parse-declaration-list (listof (or/c declaration? at-rule?)))

(define (parse-declaration-list in)
  (consume-declaration-list (normalize-argument in)))


;=======================================================
; §5.3.7: Parse a component value
;=======================================================

(provide-entry-point parse-component-value component-value?)

(define (parse-component-value in [value #f])
  (define tokens (normalize-argument in))
  (consume-leading-whitespace-tokens tokens)
  (define next (get-next-token))
  (cond [(eof-token? next)
         (or value (maybe-raise-css3-syntax-error next "Unexpected EOF when parsing component value"))]
        [else (parse-component-value tokens (consume-component-value tokens))]))


;=======================================================
; §5.3.8: Parse list of component values
;=======================================================

(provide-entry-point parse-component-value-list (listof component-value?))

(define (parse-component-value-list in [out null])
  (define tokens (normalize-argument in))
  (define next (consume-component-value tokens))
  (if (eof-token? next)
      (reverse out)
      (parse-component-value-list tokens (cons next out))))


;=======================================================
; §5.3.9: Parse comma-separated list of component values
;=======================================================

(provide-entry-point parse-comma-separated-component-value-list (listof component-value?))

(define (parse-comma-separated-component-value-list in [out null] [csv null])
  (define tokens (normalize-argument in))
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
  (consume-next-token tokens)

  (define name (get-token-value (get-current-token)))

  (define (build current prelude block)
    (at-rule (token-line current)
             (token-column current)
             name
             prelude
             block))

  (let loop ([prelude null])
    (consume-next-token tokens)
    (define current (get-current-token))
    (cond [(semicolon-token? current)
           (build current prelude #f)]
          [(eof-token? current)
           (maybe-raise-css3-syntax-error current "Unexpected EOF in at-rule")
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

  (define (build prelude v)
    (qualified-rule
     (token-line start-tok)
     (token-column start-tok)
     (reverse prelude)
     v))

  (let loop ([prelude null])
    (define current (consume-next-token tokens))
    (cond [(eof-token? current)
           (maybe-raise-css3-parse-error
            "Unexpected EOF in qualified rule"
            start-tok)
           #f]
          [(l-curly-bracket-token? current)
           (build prelude (consume-simple-block tokens))]
          [(and (simple-block? current)
                (l-curly-bracket-token? (simple-block-token current)))
           (build prelude current)]
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
         (maybe-raise-css3-syntax-error current "Unrecognized token in declaration")
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
        (begin (maybe-raise-css3-syntax-error (get-next-token) "Expected colon in declaration")
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
             (loop (cons (consume-next-token tokens) wip)))))

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
  (define current (consume-next-token tokens))
  (cond [(or (l-curly-bracket-token? current)
             (l-square-bracket-token? current)
             (l-paren-token? current))
         (consume-simple-block tokens)]
        [(function-token? current)
         (consume-function tokens)]
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

  (define (build v)
    (simple-block (token-line starting-token)
                  (token-column starting-token)
                  starting-token
                  (reverse v)))

  (let loop ([value null])
    (define in-body (consume-next-token tokens))
    (cond [(eof-token? in-body)
           (maybe-raise-css3-syntax-error starting-token "Unexpected EOF in simple block")
           (build value)]
          [(ending-token? in-body)
           (build value)]
          [else (reconsume-current-token)
                (loop (cons (consume-component-value tokens)
                            value))])))


;=======================================================
; §5.4.8: Consume a function
;=======================================================

(define (consume-function tokens)
  (define starting-token (get-current-token))
  (define name (get-token-value starting-token))
  (define (build v)
    (function (token-line starting-token)
              (token-column starting-token)
              name
              (reverse v)))

  (let loop ([value null])
    (define in-body (consume-next-token tokens))
    (cond [(eof-token? in-body)
           (maybe-raise-css3-syntax-error starting-token "Unexpected EOF in function")
           (build value)]
          [(r-paren-token? in-body)
           (build value)]
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
