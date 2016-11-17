[![Build Status](https://travis-ci.org/tripokey/elm-fuzzy.svg?branch=master)](https://travis-ci.org/tripokey/elm-fuzzy)

# elm-fuzzy

A library for fuzzy string matching written in Elm.

See demo at: http://tripokey.github.io/elm-fuzzy/

See documentation at: http://package.elm-lang.org/packages/tripokey/elm-fuzzy/latest

## Basic Usage

Sorting a list:

```elm
let
    simpleMatch config separators needle hay =
      match config separators needle hay |> .score
in
    List.sortBy (simpleMatch [] [] "hrdevi") ["screen", "disk", "harddrive", "keyboard", "mouse", "computer"] == ["harddrive","keyboard","disk","screen","computer","mouse"]
```
