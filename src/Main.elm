module Main exposing (..)

import Html exposing (..)
import Html
import Html.Events exposing (onInput)

import CommandLine
import CommandLine exposing (Command (..))

searchOptions : List String
searchOptions =
    [ "removeTag"
    , "toggleTag"
    , "toggleGroup"
    , "removeGroup"
    , "addTag"
    , "hideUi"
    ]


type CommandTest
    = RemoveTag String
    | ToggleTag String
    | RemoveGroup Int Int
    | HideUi

separateFirstWord : String -> Maybe (String, String)
separateFirstWord query =
    let
        words = String.words query
        first = List.head words
        rest = List.tail words
    in
        case (first, rest) of
            (Just first, Just rest) -> Just (first, String.join " " rest)
            _ -> Nothing


parseFirstWord : (String -> Maybe a) -> String -> Maybe (String, a)
parseFirstWord parser query =
    case separateFirstWord query of
        Just (first, rest) ->
            case parser first of
                Just val ->
                    Just (rest, val)
                Nothing ->
                    Nothing
        Nothing ->
            Nothing


intParser : String -> Maybe Int
intParser word =
    Result.toMaybe <| String.toInt word

intReference : (Int -> CommandTest) -> Command CommandTest
intReference msg =
    NonTerminal []
        (\query ->
            case parseFirstWord intParser query of
                Just (rest, val) ->
                    let
                        _ = Debug.log "rest" rest
                        _ = Debug.log "val" val
                    in
                    Just(rest, Terminal (msg val))
                Nothing ->
                    Nothing
        )

groupReference : (Int -> Int -> CommandTest) -> Command CommandTest
groupReference msg =
    NonTerminal []
        (\query ->
            case parseFirstWord intParser query of
                Just (rest, val) ->
                    Just(rest, intReference (msg val))
                Nothing ->
                    Nothing
        )

topLevelCommand : List String -> Command CommandTest
topLevelCommand tags =
    NonTerminal ["hideUi", "toggleTag", "removeTag", "removeGroup"]
        (\query ->
            let
                tagCommand = NonTerminal tags (\query -> Just ("", Terminal <| ToggleTag query))

                commandParser firstParam =
                    case firstParam of
                        "hideUi" -> Just <| Terminal HideUi
                        "toggleTag" -> Just <| tagCommand
                        "removeGroup" -> Just <| groupReference RemoveGroup
                        _ -> Nothing

            in
                parseFirstWord commandParser query
        )

type Msg
    = Input String


type alias Model =
    { queryResult: List (String, List Bool)
    , commandResult: String
    }


init : (Model, Cmd Msg)
init =
    (Model [] "", Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input query ->
            let
                fuzzyResult =
                    CommandLine.fuzzyMatch
                        CommandLine.fuzzyScore
                        searchOptions
                        query

                parsedCommand = CommandLine.parseCommand
                    query
                    <| topLevelCommand ["yolo", "swag"]

                commandResult =
                    case parsedCommand of
                        Ok HideUi -> 
                            "hideUi"
                        Ok (ToggleTag tag) ->
                            "toggleTag " ++ tag
                        Ok (RemoveGroup id1 id2) ->
                            "RemoveGroup " ++ (toString id1) ++ " " ++ toString id2
                        _ ->
                            ""
            in
                ({model | 
                    queryResult = fuzzyResult,
                    commandResult = commandResult
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
        , p [] [text model.commandResult]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
