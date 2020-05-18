[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/subscribe.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This project holds pure Racket implementations of W3C's CSS
specifications. It is not complete, but anything that makes it here is
useable. Given enough time and attention that I certainly won't have
alone, one can piece together a CSS engine from the parts in this
repository.

Any non-standard or implementation-specific behaviors are documented
in the Scribble manual(s) for each collection.

## Navigating the Code
The code ships as a multi-collection package. Each collection
corresponds to a W3C specification module (not to be confused with
Racket modules). The Racket modules inside that collection _may_ cover
more than one level (read: edition) of their governing specification.

Section references appear near relevant code, so `grep -r '4.3.13' css-syntax`
will bring up the "Convert a string to a number" algorithm.


## Available collections
This section shows currently available collections, along with the W3C
specification and edition date that collection currently tracks.
While it is possible for a collection to track more than one level,
the latest level has priority for support and development purposes.

* `css-syntax`: _[CSS Syntax Module Level 3][css-syntax]_ (16 July 2019)


## Contributing
Full compliance with W3C specifications is a big job. If you already
have pieces of a CSS engine lying around, consider contributing
here to appear in the credits.

If prudent, I may also include dependencies from other projects and
reprovide their bindings in the right collections. That would depend
on if the license of the outside code is compatible with the license
of this repository, and written consent of the author(s). Those
authors will also appear in the credits.


## Credits
* Sage Gerard: Package author, `css-syntax` implementation, docs, and tests.

[css-syntax]: https://www.w3.org/TR/css-syntax-3
