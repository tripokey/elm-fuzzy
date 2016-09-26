# elm-fuzzy

A library for fuzzy string matching written in Elm.

See demo at: http://tripokey.github.io/elm-fuzzy/

## Basic Usage

Sorting a list:

```elm
let
    simpleMatch config separators needle hay =
      match config separators needle hay |> .score
in
    List.sortBy (simpleMatch [] [] "hrdevi") ["screen", "disk", "harddrive", "keyboard", "mouse", "computer"] == ["harddrive","keyboard","disk","screen","computer","mouse"]
```
