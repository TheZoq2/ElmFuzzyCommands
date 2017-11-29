module Main exposing (..)

import Html exposing (..)
import Html
import Html.Events exposing (onInput)

import CommandLine

searchOptions : List String
searchOptions =
    [ "removeTag"
    , "toggleTag"
    , "toggleGroup"
    , "removeGroup"
    , "addTag"
    ]

type Msg =
    Input String


type alias Model =
    { queryResult: List String}


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
    div
        []
        [ input [onInput Input] []
        , ul []
            <| List.map (\string -> p [] [text string]) model.queryResult
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
