import StartApp.Simple as StartApp
import Html exposing (Html, input, div, text, button, span)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (placeholder, style)
import Fuzzy
import String

type alias Model =
  { needle: String
  , prototype: String
  , separators: String
  , caseInsensitive: Bool
  , haystack: List String
  }


type Action = Filter String
  | Update String
  | Separate String
  | CaseFlip
  | Add


init : Model
init =
    Model "" "" "" True
      [ "item"
      , "A complete sentance is this."
      , "/var/log/syslog/messages"
      , "evancz/elm-html"
      , "Oil"
      , "Firstname Lastname"
      , "Bread"
      , "if x == 5 then print 11 else print 23;"
      , "for (std::list<std::string>::const_iterator l_it=l_list.begin(); l_it != l_list.end(); ++l_it) {}"
      , "var x = 15"
      ]


update : Action -> Model -> Model
update action model =
  case action of
    Filter val ->
      {model | needle = val }

    Update val ->
      {model | prototype = val }

    Separate val ->
      {model | separators = val }

    CaseFlip ->
      {model | caseInsensitive = not model.caseInsensitive }

    Add ->
      { model
      | prototype = ""
      , haystack = model.prototype :: model.haystack
      }


viewElement : (Fuzzy.Result, String) -> Html
viewElement (result, item) =
  let
      isKey index =
        List.foldl (\e sum -> if not sum then List.member (index-e.offset) e.keys else sum) False result.matches
      isMatch index =
        List.foldl (\e sum -> if not sum then (e.offset <= index && (e.offset + e.length) > index) else sum) False result.matches
      color index =
        if isKey index
        then
          [("color", "red")]
        else
          []
      bgColor index =
        if isMatch index
        then
          [("background-color", "yellow")]
        else
          []
      hStyle index =
        style ((color index) ++ (bgColor index))
      accumulateChar c (sum, index) =
        (sum ++ [span [hStyle index] [c |> String.fromChar |> text]], index + 1)
      highlight =
        String.foldl accumulateChar ([], 0) item
  in
      div []
        [ span
          [ style
            [ ("color", "red")
            ]
          ]
          [ text ((toString result.score) ++ " ") ]
        , span [] (fst highlight)
        ]


viewHayStack : Model -> Html
viewHayStack model =
  let
      processCase item =
        if model.caseInsensitive
        then
          String.toLower item
        else
          item
      separators =
        String.toList (processCase model.separators)
          |> List.map String.fromChar
      needle =
        processCase model.needle
      scoredHays =
        model.haystack
          |> List.map (\hay -> (Fuzzy.match [] separators needle (processCase hay), hay))
      sortedHays =
        List.sortBy (\e -> fst e |> .score) scoredHays
  in
      div []
        (sortedHays
          |> List.map viewElement)


viewFilter : Signal.Address Action -> Model -> Html
viewFilter address model =
  let
      caseText =
        if model.caseInsensitive
        then
          "Case insensitive"
        else
          "Case sensitive"
  in
      div []
        [ input
          [ on "input" targetValue (\e -> Signal.message address (Filter e))
          , placeholder "Filter"
          ] []
        , input
          [ on "input" targetValue (\e -> Signal.message address (Separate e))
          , placeholder "Separators"
          ] []
        , button [ onClick address CaseFlip ] [ text caseText ]
        ]


viewAdd : Signal.Address Action -> Model -> Html
viewAdd address model =
  div []
    [ input
      [ on "input" targetValue (\e -> Signal.message address (Update e))
      , placeholder "Item"
      ] []
    , button [ onClick address Add ] [ text "Add" ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ viewFilter address model 
    , viewHayStack model
    , viewAdd address model
    ]


main =
  StartApp.start { model=init, update=update, view=view}
