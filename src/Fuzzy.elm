module Fuzzy (match) where

{-| This is library for performing fuzzy string matching.

# Matching
@docs match

-}

import String
import Maybe

type alias Model = List Int

hayPenalty = 1
needlePenalty = 1000
movePenalty = 100

initialModel : Model
initialModel =
    []

{-| Calculate how many moves that is required to order the entries

quickPenalty [5,4,3,2,1] == 4
-}
quickPenalty : List Int -> Int
quickPenalty entries =
  if List.isEmpty entries
  then
    0
  else
    let
        head =
          List.head entries |> Maybe.withDefault 0
        tail =
          List.tail entries |> Maybe.withDefault []
        partition =
          List.partition (\e -> e < head) tail
        smaller =
          fst partition
        larger =
          snd partition
        penalty =
          if List.isEmpty smaller then 0 else 1
    in
        (quickPenalty smaller) + penalty + (quickPenalty larger)


{-| Calculate the fuzzy distance between two Strings.

    distance "test" "test" == 0
    distance "test" "tast" == 1001
-}
distance : String -> String -> Int
distance needle hay =
  let
      accumulate c indexList =
        let
            indexes =
                String.indexes (String.fromChar c) hay
            hayIndex =
                List.filter (\e -> not (List.member e indexList) ) indexes
                  |> List.head
        in
            case hayIndex of
              Just v ->
                indexList ++ [v]

              Nothing ->
                indexList
      accumulated =
        String.foldl accumulate initialModel needle
      mPenalty =
        (accumulated |> quickPenalty) * movePenalty
      hPenalty =
        (String.length hay - (accumulated |> List.length)) * hayPenalty
      nPenalty =
        (String.length needle - (accumulated |> List.length)) * needlePenalty
  in
      mPenalty + hPenalty + nPenalty


{-| Perform fuzzy matching between a List String (needle) and another List String (hay).
The order of the arguments are significant. Lower score is better. Each element in the
Lists represents one "word".

    let
        query = "/usr/lcoa/bin/sh"
        target = "/usr/local/bin/sh"
        sep = "/"
    in
        Fuzzy.match (String.split sep query) (String.split sep hay) == 101

    Fuzzy.match ["test"] ["test]" == 0
    Fuzzy.match ["tst"] ["test"] == 1
    Fuzzy.match ["test"] ["tste"] == 100
    Fuzzy.match ["test"] ["tst"] == 1000
    List.sortBy (\hay -> Fuzzy.match ["hrdevi"] [hay]) ["screen", "disk", "harddrive", "keyboard", "mouse", "computer"] == ["harddrive","keyboard","disk","screen","computer","mouse"]
-}
match : List String -> List String -> Int
match needles hays =
  let
      -- The best score for a needle against a list of hays
      minScore n hs =
        List.foldl (\e prev-> min (distance n e) prev) ((String.length n) * needlePenalty) hs
      -- Sentence logic, reduce hays on left and right side depending on current needle context
      reduceHays ns c hs =
        let
            -- Reduce the left side of hays, the second needle do not need to match the first hay and so on.
            reduceLeft ns c hs =
              List.drop c hs
            -- Reduce the right side of hays, the first needle do not need to match against the last hay if there are other needles and so on.
            reduceRight ns c hs =
              List.take ((List.length hs) - (ns - c - 1)) hs
            -- Pad the hay stack to prevent hay starvation if we have more needles than hays
            padHays ns hs =
              hs ++ (List.repeat (ns - (List.length hs)) "")
        in
            hs |> padHays ns |> reduceRight ns c |> reduceLeft ns c
  in
      fst (List.foldl (\n (prev, num) -> (((minScore n (reduceHays (List.length needles) num hays)) + prev), (num + 1)) ) (0, 0) needles)

