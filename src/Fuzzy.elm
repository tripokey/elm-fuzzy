module Fuzzy (match) where

{-| This is library for performing fuzzy string matching.

# Matching
@docs match

-}

import String
import Char


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


{-| Perform fuzzy matching between a String (needle) and another String (hay).
The order of the arguments are significant. Lower score is better.

    Fuzzy.match "test" "test" == 0
    Fuzzy.match "tst" "test" == 0.001
    List.sortBy (Fuzzy.match "hrdevi") ["screen", "disk", "harddrive", "keyboard", "mouse", "computer"] == ["harddrive","disk","screen","mouse","keyboard","computer"]
-}
match : String -> String -> Float
match needle hay =
  let
      -- smartCase: If needle does not contain uppercase then do String.toLower on needle and hay.
      shouldLower =
        String.any Char.isUpper needle |> not
      hay' =
        if shouldLower then String.toLower hay else hay
      needle' =
        if shouldLower then String.toLower needle else needle
      -- Reduce the hay based on needle, reduce the needle based on the reduced hay.
      reducedHay =
        reduce needle' hay'
      -- Reduce the needle based on the reduced hay.
      reducedNeedle =
        reduce reducedHay needle'
      -- Add the distance between the reduced hay and the reduced needle to the score.
      reducedDistance =
        distance reducedHay reducedNeedle
          |> toFloat
      -- For each character removed from hay add 1/1000 to score. Enables ordering within a match category.
      hayPenalty =
        (String.length hay' - String.length reducedHay |> toFloat) / 1000
      -- For each character removed from needle add 1 to score.
      needlePenalty =
        String.length needle' - String.length reducedNeedle |> toFloat
  in
      reducedDistance + needlePenalty + hayPenalty

