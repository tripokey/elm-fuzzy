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


viewElement : (Int, String) -> Html
viewElement (score, item) =
  div []
    [ span
      [ style 
        [ ("color", "red")
        ] 
      ]
      [ text ((toString score) ++ " ") ]
    , text item
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
  in
      div []
        (model.haystack
          |> List.map (\hay -> (Fuzzy.match separators needle (processCase hay), hay))
          |> List.sortBy fst
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
