module Fuzzy exposing (match, addPenalty, removePenalty, movePenalty, Result, Match, Key)

{-| This is library for performing fuzzy string matching.

#Customization
@docs addPenalty, removePenalty, movePenalty

# Matching
@docs match, Result, Match, Key

-}

import String
import Maybe

type Config = AddPenalty Int
  | RemovePenalty Int
  | MovePenalty Int

{-| Represents a matching character in a Match.
-}
type alias Key = Int

{-| Represents a matching word in hay.
score is the score that this Match contributes to the total score in a Result.
offset is the index where this match starts in the hay.
length is the length of the match.
keys is a list of matching indexes within the word. The keys are relative to the offset.
-}
type alias Match = {score: Int, offset: Int, length: Int, keys: List Key}

{-| Represents the result of a match.
score is the total score of the result.
matches is a list of matching words within the hay.
-}
type alias Result = {score: Int, matches: List Match}

{-| Create a penalty configuration that is applied to each additional character in hay.
-}
addPenalty : Int -> Config
addPenalty penalty =
  AddPenalty penalty

{-| Create a penalty configuration that is applied to each additional character in needle.
-}
removePenalty : Int -> Config
removePenalty penalty =
  RemovePenalty penalty

{-| Create a penalty configuration that is applied to each out of order character in hay.
-}
movePenalty : Int -> Config
movePenalty penalty =
  MovePenalty penalty

type alias ConfigModel =
  { addPenalty: Int
  , movePenalty: Int
  , removePenalty: Int
  }


defaultConfig : ConfigModel
defaultConfig =
  ConfigModel 1 100 1000


type alias Model = List Int

initialModel : Model
initialModel =
    []

{-| Sort the entries and calculate how many moves that was required.

quickSort [5,4,3,2,1] == (4, [1,2,3,4,5])
-}
quickSort : List Key -> (Int, List Key)
quickSort entries =
  if List.isEmpty entries
  then
    (0, [])
  else
    let
        head =
          List.head entries |> Maybe.withDefault 0
        tail =
          List.tail entries |> Maybe.withDefault []
        partition =
          List.partition (\e -> e < head) tail
        smaller =
          quickSort (fst partition)
        larger =
          quickSort (snd partition)
        penalty =
          if List.isEmpty (snd smaller) then 0 else 1
    in
        ((fst smaller) + penalty + (fst larger), (snd smaller) ++ [head] ++ (snd larger))


{-| Calculate the fuzzy distance between two Strings.

    (distance config "test" "test").score == 0
    (distance config "test" "tast").score == 1001
-}
distance : ConfigModel -> String -> String -> Match
distance config needle hay =
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
      sorted =
        accumulated |> quickSort
      mPenalty =
        (fst sorted) * config.movePenalty
      hPenalty =
        (String.length hay - (accumulated |> List.length)) * config.addPenalty
      nPenalty =
        (String.length needle - (accumulated |> List.length)) * config.removePenalty
  in
      Match (mPenalty + hPenalty + nPenalty) 0 (String.length hay) (snd sorted)


{-| Split a string based on a list of separators keeping the separators.
-}
dissect : List String -> List String -> List String
dissect separators strings =
  if List.isEmpty separators
  then
    strings
  else
    let
        head =
          List.head separators |> Maybe.withDefault ""
        tail =
          List.tail separators |> Maybe.withDefault []
        dissectEntry entry =
          let
              entryLength =
                String.length entry
              indexes =
                  String.indexes head entry
              separatorLength =
                String.length head
              slice index (prevIndex, sum) =
                let
                    precedingSlice =
                      if prevIndex == index
                      then
                        []
                      else
                        [String.slice prevIndex index entry]
                    separatorSlice =
                      [String.slice index (index + separatorLength) entry]
                in
                    (index+separatorLength, sum ++ precedingSlice ++ separatorSlice)
              result =
                List.foldl slice (0,[]) indexes
              first =
                snd result
              lastIndex =
                fst result
              last =
                if lastIndex == entryLength
                then
                  []
                else
                  [String.slice lastIndex entryLength entry]
          in
              first ++ last
        dissected =
          List.foldl (\e s -> s ++ dissectEntry e) [] strings
    in
        dissect tail dissected


{-| Perform fuzzy matching between a query String (needle) and a target String (hay).
The order of the arguments are significant. Lower score is better. Specifying some
separators will allow for partial matching within a sentence. The default configuration is
movePenalty = 100, addPenalty = 1, removePenalty = 1000.

    let
        simpleMatch config separators needle hay =
          match config separators needle hay |> .score
    in
        simpleMatch [] [] "test" "test" == 0
        simpleMatch [] [] "tst" "test" == 1
        simpleMatch [addPenalty 10000] [] "tst" "test" == 10000
        simpleMatch [] [] "test" "tste" == 100
        simpleMatch [] [] "test" "tst" == 1000
        simpleMatch [] ["/"] "/u/b/s" "/usr/local/bin/sh" == 5
        simpleMatch [] [] "/u/b/s" "/usr/local/bin/sh" == 211
        List.sortBy (simpleMatch [] [] "hrdevi") ["screen", "disk", "harddrive", "keyboard", "mouse", "computer"] == ["harddrive","keyboard","disk","screen","computer","mouse"]
-}
match : List Config -> List String -> String -> String -> Result
match configs separators needle hay =
  let
      accumulateConfig c sum =
        case c of
          AddPenalty val ->
            {sum | addPenalty = val}

          RemovePenalty val ->
            {sum | removePenalty = val}

          MovePenalty val ->
            {sum | movePenalty = val}
      config =
        List.foldl accumulateConfig defaultConfig configs
      needles =
        dissect separators [needle]
      hays =
        dissect separators [hay]
      -- The best score for a needle against a list of hays
      minScore n (offset, hs) =
        let
            initialPenalty =
                ((String.length n) * config.removePenalty) +
                ((String.length n) * config.movePenalty) +
                ((String.length hay) * config.addPenalty)
            initialMatch =
                Match initialPenalty offset 0 []
            accumulateMatch e (prev, prevOffset) =
              let
                  eDistance =
                    distance config n e
                  newOffset =
                    prevOffset + (String.length e)
                  newMatch =
                    if eDistance.score < prev.score
                    then
                      {eDistance | offset = prevOffset}
                    else
                      prev
              in
                  (newMatch, newOffset)
        in
            fst (List.foldl accumulateMatch (initialMatch, offset) hs)
      -- Sentence logic, reduce hays on left and right side depending on current needle context
      reduceHays ns c hs =
        let
            -- Reduce the left side of hays, the second needle do not need to match the first hay and so on.
            reduceLeft ns c hs =
              (List.foldl (\e sum -> (String.length e) + sum) 0 (List.take c hs), List.drop c hs)
            -- Reduce the right side of hays, the first needle do not need to match against the last hay if there are other needles and so on.
            reduceRight ns c hs =
              List.take ((List.length hs) - (ns - c - 1)) hs
            -- Pad the hay stack to prevent hay starvation if we have more needles than hays
            padHays ns hs =
              hs ++ (List.repeat (ns - (List.length hs)) "")
        in
            hs |> padHays ns |> reduceRight ns c |> reduceLeft ns c
      accumulateResult n (prev, num) =
        let
            matchResult =
              minScore n (reduceHays (List.length needles) num hays)
            newResult =
              {prev | score = matchResult.score + prev.score
              , matches = prev.matches ++ [matchResult]}
        in
            (newResult, (num + 1))
      initialResult =
          Result 0 []
  in
      fst (List.foldl accumulateResult (initialResult, 0) needles)

