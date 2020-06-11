# generic_regex
OCaml library for generic regular expressions (operating on anything that folds)

A regular expression is a way to define a formal grammar to match a string of characters from an alphabet.

The beauty of that is that the string and the alphabet can be anything you can come up with. They don't have to mean the usual "array of 8-bit integers." It could just as well be a list of UTF-8 characters, an array of floats, or a tree of lists.

All you need is a fold over the data structure and comparison operators for your alphabet (equality for character comparisons, &lt; and &gt; for range matching.) And, as you likely don't want to write regexp AST by hand, a parser for expressions in your alphabet might be helpful.

`regex.ml` contains parsers for regular expressions of chars, ints and floats, and a simple polymorphic NFA-interpreting regexp engine for running the regexps. The `execute_nfa nfa getter str` is the workhorse function for the engine, it takes the parsed regexp `nfa`, a getter function such that `getter i str` returns that `i`th element of `str`, and the data structure `str` to match against.

It works on strings!
```
pat_match "(fo.|bar)+baz$" "afooforbarbarfozbaz"
- : Some (1, 19)
```

It works on arrays of ints!
```
(* int array regexp, _ is the wildcard, ; separates the numbers *)
int_match "[1..10]; _; _; [11..15; 17; 19..25]$" [|17; 4; 0; -1; 17|]
- : Some (1, 5)
```

It works on arrays of floats!
```
(* float array regexp *)
float_match "^2e3;[1.5..2.2]+;17.0" [|2000.0; 1.7; 2.1; 17.0; 8.0|]
- : Some (0, 4)
```

Missing features: no backreferences. No negative ranges (e.g. [^a-z] does not parse.) And it doesn't do greedy matching (e.g. "fo+" matches only the first two characters of "fooooo".) And many more.
