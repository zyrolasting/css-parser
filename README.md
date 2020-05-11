This is a pure-Racket parser of CSS, according to [CSS Syntax Module
Level 3][spec] specification last updated on 16 July 2019.

To deliver sooner, I will assume UTF-8 input only. This is not
reality, but it's good enough for now.

The code is organized such that one module covers either one top-level
section of the spec, or several subsections.  If a module covers
several subsections, then they are typically related (e.g. number
parsing).

To seek out the implementation for some part of the spec, just search
the codebase for its section number. `grep -r '4.3.13' .` will bring
up the "Convert a string to a number" algorithm.

[spec]: https://www.w3.org/TR/css-syntax-3/
