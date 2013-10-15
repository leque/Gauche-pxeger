# What is this?
This module provides a generator that yields strings which match a given regexp.

# APIs
## [Function] regexp->string-generator re :key (*-max-repeat 8) (char-set-universe char-set:full)
Creates a generator that yields strings
which match a given regular expression RE.

For regular expressions `.` and `pat*`, you would rarely mean truely aribitrary
characters or repetion count.
This function read `.` as characters in CHAR-SET-UNIVERSE,
and `[^...]` as characters in `(char-set-difference CHAR-SET-UNIVERSE #[...])`.
Also, `pat*`, `pat+`, and `pat{n}` are read as `pat{0,*-MAX-REPEAT}`,
`pat{1,*-MAX-REPEAT+1}`, and `pat{n,*-MAX-REPEAT+n}`, respectively.

This function supports subset of Gauche's regular expression.
Unsupported syntaxes are:

- non-greedy patterns -- `*?`, `+?`, `??`, `{n,m}?`
- back reference -- `\n`
- named back reference -- `\k<name>`
- beginning / end of string -- `^`, `$`
- word boundary -- `\b`, `\B`
- look ahead -- `(?=pat)`, `(?!pat)`
- look behind -- `(?<=pat)`, `(?<!pat)`
- atomic patterns -- `(?>pattern)`, `*+`, `++`, `?+`
- conditional matching -- `(?test-pattern then-pattern|else-pattern)`
