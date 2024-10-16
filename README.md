# sml-coexp

[![ci](https://github.com/vikraman/sml-coexp/actions/workflows/ci.yml/badge.svg)](https://github.com/vikraman/sml-coexp)

## Usage

```sh
$ alias smlnj='rlwrap sml'
$ smlnj
- CM.make "coexp.cm" ;
```

### Examples

Coexponentials are implemented using the native continuation type in the Coexp module.
The Examples module contains the multiplication example from the paper.
The Classical module contains the encodings of various classical logic combinators and control operators.

NOTE: There are some typos in the typesetting of the paper draft.
The code samples in this repository are the right definitions which are checked by the computer.

To inspect or run in the SML repl:

``` sh
- open Examples ;
- ex1 Int.toString (INR "0") 0 ;
- ex1 Int.toString (INL 1) 1 ;
```

``` sh
- mult0 [1, 2, 0, 3, 4] ;
- mult1 [1, 2, 0, 3, 4] ;
- mult2 [1, 2, 0, 3, 4] ;
- mult3 [1, 2, 0, 3, 4] ;
- mult4 [1, 2, 0, 3, 4] ;
- mult5 [1, 2, 0, 3, 4] ;
```
