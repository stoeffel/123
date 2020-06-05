module Puzzle exposing
    ( Puzzle
    , Visibility(..)
    , correct
    , generate
    , init
    , viewElements
    )

import Assets exposing (Asset, Assets)
import Element as E exposing (Element)
import Random
import Random.List


type Puzzle
    = Puzzle (Maybe Asset) (List Bool)


init : Puzzle
init =
    Puzzle Nothing []


type Visibility
    = Visible
    | Hidden


viewElements :
    (Maybe Asset -> Visibility -> Int -> Element msg)
    -> Puzzle
    -> List (Element msg)
viewElements f (Puzzle maybeAsset ns) =
    List.indexedMap
        (\i v ->
            if v then
                f maybeAsset Visible (i + 1)

            else
                f Nothing Hidden (i + 1)
        )
        ns


generate : Assets -> Puzzle -> Maybe (Random.Generator Puzzle)
generate assets p =
    let
        n =
            correctAnswer p
    in
    case List.range 1 9 |> List.filter (\x -> x /= n) of
        [] ->
            Nothing

        x :: xs ->
            Just <|
                Random.map2 Puzzle
                    (Assets.random assets)
                    (Random.uniform x xs
                        |> Random.andThen
                            (\y ->
                                List.repeat y True
                                    ++ List.repeat (9 - y) False
                                    |> Random.List.shuffle
                            )
                    )


correct : Int -> Puzzle -> Bool
correct n p =
    correctAnswer p == n


correctAnswer : Puzzle -> Int
correctAnswer (Puzzle _ puzzle) =
    List.length (List.filter identity puzzle)
