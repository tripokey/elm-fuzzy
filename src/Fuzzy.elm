module Fuzzy (match) where

{-| This is library for performing fuzzy string matching.

# Matching
@docs match

-}

import String


{-| Calculate the Levenshtein distance between two Strings.

    distance "test" "test" == 0
    distance "test" "tast" == 1
-}
distance : String -> String -> Int
distance a b =
  if a == b
  then
    0
  else if String.isEmpty a
  then
    String.length b
  else if String.isEmpty b
  then
    String.length a
  else
    let
        cost =
          if (String.right 1 a) == (String.right 1 b) then 0 else 1
        left =
          (distance (String.dropRight 1 a) b) + 1
        right =
          (distance a (String.dropRight 1 b)) + 1
        both =
          (distance (String.dropRight 1 a) (String.dropRight 1 b)) + cost
    in
        min (min left right) both


{-| For each character in hay remove it if it is not present in needle,
if it is present in needle remove one instance of it from needle.
Return the reduced hay.

    reduce "tts" "testt" == "tst"
-}
reduce : String -> String -> String
reduce needle hay =
  let
      countChar c str =
        String.foldl (\e sum -> if e == c then sum + 1 else sum) 0 str
      filter c sum =
        if countChar c needle > countChar c sum then String.cons c sum else sum
  in
      String.foldl filter "" hay
        |> String.reverse


{-| Perform fuzzy matching between a List String (needle) and another List String (hay).
The order of the arguments are significant. Lower score is better. Each element in the
Lists represents one "word".

    let
        query = "/usr/lcoa/bin/sh"
        target = "/usr/local/bin/sh"
        sep = "/"
    in
        Fuzzy.match (String.split query) (String.split hay) == 2.001

    Fuzzy.match ["test"] ["test]" == 0
    Fuzzy.match ["tst"] ["test"] == 0.001
    List.sortBy (\hay -> Fuzzy.match ["hrdevi"] [hay]) ["screen", "disk", "harddrive", "keyboard", "mouse", "computer"] == ["harddrive","disk","screen","mouse","keyboard","computer"]
-}
match : List String -> List String -> Float
match needles hays =
  let
      -- Score for one needle hay pair
      score n h =
        let
            -- Reduce the hay based on needle, reduce the needle based on the reduced hay.
            reducedHay =
              reduce n h
            -- Reduce the needle based on the reduced hay.
            reducedNeedle =
              reduce reducedHay n
            -- Add the distance between the reduced hay and the reduced needle to the score.
            reducedDistance =
              distance reducedHay reducedNeedle
                |> toFloat
            -- For each character removed from hay add 1/1000 to score. Enables ordering within a match category.
            hayPenalty =
              (String.length h - String.length reducedHay |> toFloat) / 1000
             -- For each character removed from needle add 1 to score.
            needlePenalty =
              String.length n - String.length reducedNeedle |> toFloat
        in
            reducedDistance + needlePenalty + hayPenalty
      -- The best score for a needle against a list of hays
      minScore n hs =
        List.foldl (\e prev-> min (score n e) prev) (toFloat (String.length n)) hs
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

