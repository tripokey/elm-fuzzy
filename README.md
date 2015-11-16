# elm-fuzzy

A library for fuzzy string matching written in Elm.

## Basic Usage

Sorting a list:

```elm
List.sortBy (Fuzzy.match "hrdevi") ["screen", "disk", "harddrive", "keyboard", "mouse", "computer"] == ["harddrive","disk","screen","mouse","keyboard","computer"]
```
