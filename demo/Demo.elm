module Main exposing (..)

import Html.App as StartApp
import Html exposing (Html, input, div, text, button, span)
import Html.Events exposing (onInput, targetValue, onClick)
import Html.Attributes exposing (placeholder, style)
import Fuzzy
import String


type alias Model =
    { needle : String
    , prototype : String
    , separators : String
    , caseInsensitive : Bool
    , haystack : List String
    }


type Msg
    = Filter String
    | Update String
    | Separate String
    | CaseFlip
    | Add


init : Model
init =
    Model ""
        ""
        ""
        True
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


update : Msg -> Model -> Model
update action model =
    case action of
        Filter val ->
            { model | needle = val }

        Update val ->
            { model | prototype = val }

        Separate val ->
            { model | separators = val }

        CaseFlip ->
            { model | caseInsensitive = not model.caseInsensitive }

        Add ->
            { model
                | prototype = ""
                , haystack = model.prototype :: model.haystack
            }


viewElement : ( Fuzzy.Result, String ) -> Html Msg
viewElement ( result, item ) =
    let
        isKey index =
            List.foldl
                (\e sum ->
                    if not sum then
                        List.member (index - e.offset) e.keys
                    else
                        sum
                )
                False
                result.matches

        isMatch index =
            List.foldl
                (\e sum ->
                    if not sum then
                        (e.offset <= index && (e.offset + e.length) > index)
                    else
                        sum
                )
                False
                result.matches

        color index =
            if isKey index then
                [ ( "color", "red" ) ]
            else
                []

        bgColor index =
            if isMatch index then
                [ ( "background-color", "yellow" ) ]
            else
                []

        hStyle index =
            style ((color index) ++ (bgColor index))

        accumulateChar c ( sum, index ) =
            ( sum ++ [ span [ hStyle index ] [ c |> String.fromChar |> text ] ], index + 1 )

        highlight =
            String.foldl accumulateChar ( [], 0 ) item
    in
        div []
            [ span
                [ style
                    [ ( "color", "red" )
                    ]
                ]
                [ text ((toString result.score) ++ " ") ]
            , span [] (fst highlight)
            ]


viewHayStack : Model -> Html Msg
viewHayStack model =
    let
        processCase item =
            if model.caseInsensitive then
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
                |> List.map (\hay -> ( Fuzzy.match [] separators needle (processCase hay), hay ))

        sortedHays =
            List.sortBy (\e -> fst e |> .score) scoredHays
    in
        div []
            (sortedHays
                |> List.map viewElement
            )


viewFilter : Model -> Html Msg
viewFilter model =
    let
        caseText =
            if model.caseInsensitive then
                "Case insensitive"
            else
                "Case sensitive"
    in
        div []
            [ input
                [ onInput (\e -> Filter e)
                , placeholder "Filter"
                ]
                []
            , input
                [ onInput (\e -> Separate e)
                , placeholder "Separators"
                ]
                []
            , button [ onClick CaseFlip ] [ text caseText ]
            ]


viewAdd : Model -> Html Msg
viewAdd model =
    div []
        [ input
            [ onInput (\e -> Update e)
            , placeholder "Item"
            ]
            []
        , button [ onClick Add ] [ text "Add" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewFilter model
        , viewHayStack model
        , viewAdd model
        ]


main =
    StartApp.beginnerProgram { model = init, update = update, view = view }
