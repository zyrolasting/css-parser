#lang scribble/manual

@require[@for-label[css-parser racket/base]]

@title{CSS Parsing}
@author{Sage Gerard}

This package provides a tokenizer and parser for CSS, as defined by
the 16 July 2019 edition of @italic{CSS Syntax Module Level 3}.

While the library strives for compliance, it takes the following
liberties:

@itemlist[

@item{It assumes that all input streams are encoded using UTF-8.}

@item{It has zero-tolerance of parse errors. The first error
encountered aborts processing.}

@item{It does not encapsulate parser output. You will be given an
exact parse tree in the name of programmatic control.}

]


@section{Convenience Functions}
@defmodule[css]

@defproc[(port->stylesheet [in input-port?]) list?]{
Consumes all content in the input port to produce
a parsed stylesheet as a tree of rules.
}

@section{Tokenizer}
@defmodule[css/tokenizer]

The tokenizer consumes characters from an input port to produce tokens
as defined in section 4 of @italic{CSS Syntax Module Level 3}.

@defproc[(get-next-token [in input-port?]) token?]{
Consumes text from the given port and returns the next token.

Raises @racket[exn:fail:css:parse] with relevant information.
}

@defproc[(tokenize [in input-port?]) (listof token?)]{
Returns a list of tokens produced using @racket[get-next-token] using
all text from the input port.
}


@section{Parsing}
@defmodule[css/parser]

The parser accepts tokens from the tokenizer to produce a stylesheet
as a tree of rules.

@defproc[(parse (listof token?)) list?]{
Returns a list of tokens produced using @racket[get-next-token] using
all text from the input port.
}
