#lang scribble/manual

@require[@for-label[
w3c/css-syntax/parser
w3c/css-syntax/tokenizer
w3c/css-syntax/errors
racket/base
racket/contract
racket/generator]
"toc.rkt"]

@title{W3C's CSS Syntax Module (Level 3)}
@author{Sage Gerard}

This package provides a CSS parser and tokenizer as defined in
the W3C's @hyperlink[spec-url]{CSS Syntax Module Level 3} (the
“spec”). The source code and this manual both reference specific
sections when relevant like so: @sl{4.1}.

The current implementation follows the 16 July 2019 version of the
spec.

While the library strives for compliance, it takes the following
liberties:

@itemlist[

@item{It assumes that all input character streams are encoded using UTF-8.}

@item{It uses @racket[#f] to represent “nothing” in some parse results.}

@item{It uses generators to represent token streams.}
]


@section{Parser}
@defmodule[w3c/css-syntax/parser]

This module provides the parser entry point procedures as defined in
@sl{5.3}, the struct definitions representing parsed CSS3 code, and
other utilities pertaining to @sl{5}.

@subsection[#:tag "parser-entry-points"]{Entry Points}
@margin-note{@sl{5.3.1}'s algorithm is supposed to accept a CSS
grammar specification or term. However, that specification/term has an
"unspecified structure." In a world with @racketmodname[racket/match],
it seems that you would be better off using that in @racket[match?]
instead of whatever I'd make up.}
@defproc[(parse-as [in parser-entry-input/c]
                   [match? (-> (listof component-value?) any/c)]) any/c]{
Parses the given input according to @sl{5.3.1}.

Returns @racket[(and (match? L) L)], where @racket[L] is
@racket[(parse-component-value-list L)].
}

@defproc[(parse-stylesheet [in parser-entry-input/c]) stylesheet?]{
Parses a @racket[stylesheet] according to @sl{5.3.2}, where
contained rules are considered top-level.
}

@defproc[(parse-rule-list [in parser-entry-input/c]) (listof (or/c exn:fail:css3:syntax? qualified-rule? at-rule?))]{
Parses a list of non-top-level qualified rules and at-rules according
to @sl{5.3.3}, using @racket[parse-rule].
}

@defproc[(parse-rule [in parser-entry-input/c]) (or/c qualified-rule? at-rule? exn:fail:css3:syntax?)]{
Parses a qualified rule or at-rule according to @sl{5.3.4}.

If @racket[(strict?)] is @racket[#f], the return value may be a syntax
error. This is consistent with the spec's instructions.

If @racket[(strict?)] is a true value, the error is raised instead of
returned.
}

@defproc[(parse-declaration [in parser-entry-input/c]) declaration?]{
Parse a declaration according to @sl{5.3.5}.
}

@defproc[(parse-declaration-list [in parser-entry-input/c]) (listof declaration? at-rule?)]{
Parse a declaration according to @sl{5.3.6}.

The spec warns that this algorithm may output at-rules, despite the
name.
}

@defproc[(parse-component-value [in parser-entry-input/c]) component-value?]{
Parse a component value according to @sl{5.3.7}.
}

@defproc[(parse-component-value-list [in parser-entry-input/c]) (listof component-value?)]{
Parse a list of component values according to @sl{5.3.8}.
}

@defproc[(parse-comma-separated-component-value-list [in parser-entry-input/c]) (listof component-value?)]{
Parse a list of comma-separated component values according to @sl{5.3.9}.
}


@subsection{Parsed Node Definitions}

@defstruct*[css3-syntax-parse-node ([line (or/c #f exact-nonnegative-integer?)]
                     [column (or/c #f exact-nonnegative-integer?)])]{
The superstructure for all parse output.

Each @racket[css3-syntax-parse-node] substructure type has a meaning derived from
@sl{5}.

The line and column match the line and column information for the
@italic{first} token used to produce a @racket[css3-syntax-parse-node] instance.
}

@defstruct*[(at-rule css3-syntax-parse-node)
           ([name string?]
            [prelude (listof css3-syntax-parse-node?)]
            [block (or/c simple-block? #f)])]{
Represents an at-rule like @litchar|{@import}| or @litchar|{@media}|.

The @racket[preamble] captures nodes before any defined
@racket[block]. The @racket[block] itself is optional.
}

@defstruct*[(qualified-rule css3-syntax-parse-node)
           ([prelude (listof css3-syntax-parse-node?)]
            [block simple-block?])]{
Represents a qualified rule, which is the general form used to apply
styles to a document.
}

@defstruct*[(declaration css3-syntax-parse-node) ([name string?] [value css3-syntax-parse-node?] [important boolean?])]{
Represents an individual style declaration, optionally marked
@racket[important].
}

@defstruct*[(simple-block css3-syntax-parse-node)
           ([token (or/c l-curly-bracket-token?
                         l-square-bracket-token?
                         l-paren-token?)]
            [value (listof css3-syntax-parse-node?)])]{
Represents a block of nodes with an associated @litchar|{(}|,
@litchar|{{}|, or @litchar|{[}| @racket[token] that represents the
opening brace.

The @racket[value] is a list of nodes contained within the block.
}

@defstruct*[(function css3-syntax-parse-node) ([name string?] [value (listof css3-syntax-parse-node?)])]{
Represents a function call like @litchar{box-shadow(2px 2px 2px black)}.
}

@defstruct*[(stylesheet css3-syntax-parse-node) ([rules (listof (or/c qualified-rule? at-rule?))])]{
Represents a list of qualified rules and at-rules.

@bold{Be warned that this is not equivalent to a CSS stylesheet object
as defined in @sl{9}.} This parser output follows @sl{5}, which only
organizes declarations and component values in blocks and rules.
The meaning and grammar of this object is @italic{entirely agnostic},
and does not understand CSS-specific concerns like selectors.
}

@subsection{Parser Utilities}
@defthing[parser-entry-input/c (or/c string? input-port? (-> (or/c char? token?)))]{
This contract captures a parser entry point's argument as defined in
@sl{5.3}. Each procedure defined by this module will first try to
transform its sole argument into a token stream using
@racket[tokenize] if necessary.

Currently, this library does @italic{not} decode input byte streams
as defined in @sl{3.2}. If you pass an input port, this library assumes
that the port produces UTF-8 encoded characters.
}

@defproc[(component-value? [v any/c]) boolean?]{
Checks if the given value complies with the @bold{component value}
definition in @sl{5}.
}

@defproc[(preserved-token? [v any/c]) boolean?]{
Checks if the given value complies with the @bold{preserved token}
definition in @sl{5}.
}


@section{Tokenizer}
@defmodule[w3c/css-syntax/tokenizer]

The tokenizer consumes characters from an input port to produce tokens
as defined in @secref{token-definitions}.

The tokenizer derives source location data from
@racket[port-next-location], but does not enable line counting for
you. So if you want location info from @racket[tokenize] or
@racket[get-next-token], be sure to use @racket[port-count-lines!] or
@racket[port-count-lines-enabled] first.

@racketinput[(require racket/generator)]
@racketinput[(define s (open-input-string "h1 { color: red }"))]
@racketinput[(port-count-lines! s)]
@racketinput[(for/list ([tok (in-generator (tokenize s))]) tok)]
@racketresult[(list
                (ident-token 1 0 "h1")
                (whitespace-token 1 2)
                (l-curly-bracket-token 1 3)
                (whitespace-token 1 4)
                (ident-token 1 5 "color")
                (colon-token 1 10)
                (whitespace-token 1 11)
                (ident-token 1 12 "red")
                (whitespace-token 1 15)
                (r-curly-bracket-token 1 16)
                (eof-token 1 17))]

@subsection[#:tag "tokenizer-entry-points"]{Entry Points}
@defproc[(get-next-token [in input-port?]) token?]{
Consumes text from the given port and returns the next token.

If this procedure encounters a parse error while @racket[(strict?)] is
@racket[#t], then it will raise a relevant @racket[exn:fail:css3:parse]
exception.
}

@defproc[(tokenize [in input-port?]) generator?]{
Returns a generator that lazily translates characters
from @racket[in] to tokens using @racket[get-next-token].
}

@subsection[#:tag "token-definitions"]{Token Definitions}
@deftogether[(
@defstruct*[token ([line (or/c #f exact-nonnegative-integer?)]
                   [column (or/c #f exact-nonnegative-integer?)])
                   #:transparent]
@defstruct*[(at-keyword-token token) ([value string?]) #:transparent]
@defstruct*[(bad-string-token token) () #:transparent]
@defstruct*[(bad-url-token token) () #:transparent]
@defstruct*[(cdc-token token) () #:transparent]
@defstruct*[(cdo-token token) () #:transparent]
@defstruct*[(colon-token token) () #:transparent]
@defstruct*[(comma-token token) () #:transparent]
@defstruct*[(delim-token token) ([value string?]) #:transparent]
@defstruct*[(dimension-token token) ([type (or/c "integer" "number")] [value string?] [unit string?]) #:transparent]
@defstruct*[(eof-token token) () #:transparent]
@defstruct*[(function-token token) ([value string?]) #:transparent]
@defstruct*[(hash-token token) ([type (or/c "id" "unrestricted")] [value any/c]) #:transparent]
@defstruct*[(ident-token token) ([value string?]) #:transparent]
@defstruct*[(l-curly-bracket-token token) () #:transparent]
@defstruct*[(l-paren-token token) () #:transparent]
@defstruct*[(l-square-bracket-token token) () #:transparent]
@defstruct*[(number-token token) ([type (or/c "integer" "number")] [value number?]) #:transparent]
@defstruct*[(percentage-token token) ([value number?]) #:transparent]
@defstruct*[(r-curly-bracket-token token) () #:transparent]
@defstruct*[(r-paren-token token) () #:transparent]
@defstruct*[(r-square-bracket-token token) () #:transparent]
@defstruct*[(semicolon-token token) () #:transparent]
@defstruct*[(string-token token) ([value string?]) #:transparent]
@defstruct*[(url-token token) ([value string?]) #:transparent]
@defstruct*[(whitespace-token token) () #:transparent]
)]{
These transparent structure types represent tokens from @sl{4}.

You should not need to construct instances of these yourself. They
are, however, useful for deriving source location information and
writing custom parsers.
}


@section{Errors}
@defmodule[w3c/css-syntax/errors]

The spec defines error recovery rules for user agents so that they can
be compatible with adjusted CSS grammars. This module defines global
error handling configuration and exceptions to that end.

@defthing[strict? (parameter/c boolean?) #:value #t]{
When @racket[#t], the parser or tokenizer will raise
@racket[exn:fail:css3:parse] or @racket[exn:fail:css3:syntax] on any
error it encounters while processing input.

Otherwise, both the parser and tokenizer will follow the spec's recovery
instructions to produce the best possible output.
}

@deftogether[(
@defstruct*[(exn:fail:css3:parse exn:fail)
            ([line (or/c #f exact-nonnegative-integer?)]
             [column (or/c #f exact-nonnegative-integer?)])]
@defstruct*[(exn:fail:css3:syntax exn:fail)
            ([line (or/c #f exact-nonnegative-integer?)]
             [column (or/c #f exact-nonnegative-integer?)])])]{
Represents parse and syntax errors.

The spec does not specify that parse and syntax errors must be
exceptions. In @sl{3}, a parse error is more like a @italic{condition}
than a value. It does not place any restrictions on what a parse error
or syntax error looks like, so here they are defined as exceptions
that hold source location information.

A @racket[exn:fail:css3:parse] instance represents a location where a
parse error (the condition) was discovered when attempting to consume
characters or tokens. The source location data for a parse exception
comes from a @racket[token] (during parsing) or
@racket[port-next-location] (during tokenization).

A @racket[exn:fail:css3:syntax] instance represents a failure to
construct a @racket[css3-syntax-parse-node] out of individually valid tokens (not
characters). A @racket[token] that starts a form donates its source
location data to a @racket[exn:fail:css3:syntax] instance.
}
