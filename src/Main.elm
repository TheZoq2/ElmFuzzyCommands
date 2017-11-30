module Main exposing (..)

import Html exposing (..)
import Html
import Html.Events exposing (onInput)

import CommandLine

searchOptions : List (String, Int)
searchOptions =
    [ ("removeTag", 1)
    , ("toggleTag", 2)
    , ("toggleGroup", 3)
    , ("removeGroup", 4)
    , ("addTag", 5)
    , ("hideUi", 5)
    ]

type Msg =
    Input String


type alias Model =
    { queryResult: List (String, List Bool)}


init : (Model, Cmd Msg)
init =
    (Model [], Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input query ->
            ({model | 
                queryResult =
                    CommandLine.fuzzyMatch
                        CommandLine.fuzzyScore
                        searchOptions
                        query
            }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        matchRenderer : (String, List Bool) -> Html Msg
        matchRenderer (string, matches) =
            let
                charRenderer (char, match) =
                    ( if match then
                        b []
                      else
                        span []
                    ) [text <| String.fromChar char]
            in
            p
                []
                <| List.map charRenderer
                    <| List.map2 (,) (String.toList string) matches
    in
    div
        []
        [ input [onInput Input] []
        , ul []
            <| List.map matchRenderer model.queryResult
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
