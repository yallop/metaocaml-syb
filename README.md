A port of the [Scrap Your Boilerplate][syb-haskell] library to [BER MetaOCaml][ber] with [modular implicits][modimpl].

Generic programming libraries like Scrap Your Boilerplate make it possible to express a variety of traversals succinctly.  However, the resulting code is often considerably slower (10-20x) than equivalent handwritten code.  This overhead is largely a result of various forms of abstraction and higher-order polymorphism, and can be eliminated by using MetaOCaml's staging facilities to instantiate generic functions to type-specialised code.

The draft paper [Staging Generic Programming][staging-generic-programming] explains the design of this library in detail.

### Installation

```
opam switch 4.02.1+modular-implicits-ber
opam pin add syb https://github.com/yallop/metaocaml-syb.git
```

### Usage

The following examples assume that you have loaded the package and brought the instances into scope:

```ocaml
# #require "syb";;
# open Syb.Instances;;
```

Apply `not` to every `bool` within a larger structure:

```ocaml
# Syb.(everywhere (mkT not)) [[true], 1; [], 2; [true; false], 3];;
- : (bool list * int) list = [([false], 1); ([], 2); ([false; true], 3)]
```

Instantiate a version of the above traversal specialized for `not` and `(bool list * int) list`:

```ocaml
# let f : (bool list * int) list -> (bool list * int) list =
    Syb.(instantiateT (everywhere_ (mkT_ (fun x -> .<not .~x>.))));;
  val f : (bool list * int) list -> (bool list * int) list = <fun>
```

Show the code generated for the type-specialized traversal:

```ocaml
# let f_code : ((bool list * int) list -> (bool list * int) list) code =
    Syb.(generateT (everywhere_ (mkT_ (fun x -> .<not .~x>.))));;
  val f_code : ((bool list * int) list -> (bool list * int) list) code = .<
...
>.
```

[syb-haskell]: http://foswiki.cs.uu.nl/foswiki/GenericProgramming/SYB
[modimpl]: http://www.lpw25.net/ml2014.pdf
[ber]: http://okmij.org/ftp/ML/MetaOCaml.html
[staging-generic-programming]: https://yallop.github.io/metaocaml-syb/staging-generic-programming.pdf