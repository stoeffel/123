module Puzzle exposing
    ( Puzzle
    , Visibility(..)
    , correct
    , generate
    , init
    , viewElements
    )

import Element as E exposing (Element)
import Random
import Random.List


type Puzzle
    = Puzzle (List Bool)


init : Puzzle
init =
    Puzzle []


type Visibility
    = Visible
    | Hidden


viewElements :
    (Visibility -> Int -> Element msg)
    -> Puzzle
    -> List (Element msg)
viewElements f (Puzzle ns) =
    List.indexedMap
        (\i v ->
            if v then
                f Visible (i + 1)

            else
                f Hidden (i + 1)
        )
        ns


generate : Puzzle -> Maybe (Random.Generator Puzzle)
generate p =
    let
        n =
            correctAnswer p
    in
    case List.range 1 9 |> List.filter (\x -> x /= n) of
        [] ->
            Nothing

        x :: xs ->
            Random.uniform x xs
                |> Random.andThen
                    (\y ->
                        List.repeat y True
                            ++ List.repeat (9 - y) False
                            |> Random.List.shuffle
                    )
                |> Random.map Puzzle
                |> Just


correct : Int -> Puzzle -> Bool
correct n p =
    correctAnswer p == n


correctAnswer : Puzzle -> Int
correctAnswer (Puzzle puzzle) =
    List.length (List.filter identity puzzle)
