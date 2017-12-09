module CommandLine exposing (fuzzyScore, fuzzyMatch, Command(..), parseCommand, ParseError(..), ParamGreed (..), separateFirstWord)

import Char

type Command a
    = Terminal a
    | NonTerminal ParamGreed (List String) (String -> Maybe (Command a))


type ParseError a
    = InvalidParameter
    | MissingParameters (Command a)
    | ExtraParameters


type ParamGreed
    = Word
    | Rest


{-|
  Separates a String into Just (firstWord, rest) if the string
  is non-empty or Nothing if it is empty
-}
separateFirstWord : String -> (String, String, String)
separateFirstWord query =
    let 
        (before, word, after) = 
            String.foldl
                (\char (before, word, after) ->
                    case char of
                        ' ' ->
                            if word /= "" then
                                (before, word, String.cons char after)
                            else
                                (String.cons char before, word, after)
                        char ->
                            if after == "" then
                                (before, String.cons char word, after)
                            else
                                (before, word, String.cons char after)
                )
                ("", "", "")
                query
    in
        (String.reverse before, String.reverse word, String.reverse after)


separateWords : String -> List String


parseCommand : String -> Command a -> Result (ParseError a) a
parseCommand query command =
    case command of
        Terminal val ->
            case query of
                "" ->
                    Ok val
                _ ->
                    Err ExtraParameters
        NonTerminal greed suggestions parsingFunction ->
            let
                (beforeWord, firstWord, restWords) =
                    case greed of
                        Word ->
                            separateFirstWord query
                        Rest ->
                            ("", String.trimLeft query, "")
            in
                case firstWord of
                    "" ->
                        Err (MissingParameters command)
                    param ->
                        case (parsingFunction param) of
                            Just command ->
                                parseCommand restWords command
                            Nothing ->
                                Err InvalidParameter

type FuzzyState
    = MoreParams (List String, List Bool)
    | TooManyParams

fuzzyParseCommand : String -> Command a -> (String, FuzzyState)
fuzzyParseCommand query command =
    let
        inner : String -> Command a -> String -> FuzzyResult a
        inner query command previousQuery =
            let
                splitQuery =
                    case greed of
                        Word -> separateFirstWord query
                        Rest -> (before, query, "")

                fuzzyMatches query suggestions =
                    case suggestions of
                        [] -> 
                            [query, List.repeat (List.length query) True]
                        suggestions ->
                            fuzzyMatch fuzzyScore suggestions query
            in
                case command of
                    NonTerminal suggestions command ->
                        case splltQuery of
                            -- Completely empty query
                            (before, "", "") ->
                            -- No word, but whitespace before
                            (before, "", _) ->
                            -- Word
                            (_, word, rest) ->
                    Terminal _ ->
                        --Something
    in
        inner query command ""


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
