module CommandLine exposing (fuzzyScore, fuzzyMatch, Command(..))

import Char

type Command a
    = Terminal a
    | NonTerminal (List String) (String -> Maybe (String, Command a))


type ParseResult a
    = Ok a
    | InvalidParameter
    | MissingParameters (Command a)
    | ExtraParameters

parseCommand : String -> Command a -> ParseResult a
parseCommand query command =
    case command of
        Terminal val ->
            case query of
                "" ->
                    Ok val
                _ ->
                    ExtraParameters
        NonTerminal suggestions parsingFunction ->
            case (parsingFunction query) of
                Just (restQuery, command) ->
                    parseCommand restQuery command
                Nothing ->
                    InvalidParameter



fuzzyScore : String -> String -> (Int, List Bool)
fuzzyScore target input =
    let

        maybeFunction : (Char, String) -> (Char, String) -> (Int, List Bool)
        maybeFunction (t, tRest) (i, iRest) =
            let
                (score, match) =
                    if Char.toLower t == Char.toLower i then
                        (1, True)
                    else
                        (0, False)

                restInput = if match then iRest else input

                (restScore, restMatches) = fuzzyScore tRest restInput
            in
                (score + restScore, match :: restMatches)

    in
        case Maybe.map2 maybeFunction (String.uncons target) (String.uncons input) of
            Just val ->
                val
            Nothing ->
                (0, List.repeat (String.length target) False)



fuzzyMatch :
            (String -> String -> (Int, List Bool))
            -> List String
            -> String
            -> List (String, List Bool)
fuzzyMatch scoringFunction options query =
    let
        withScores =
            List.map2 (\x y -> (x,y)) options
                <| List.map 
                    (\option -> scoringFunction option query) options
    in
        List.map (\(string, (_, matched)) -> (string, matched))
            <| List.sortBy (\(_, (score, _)) -> score) withScores
