module Puzzle exposing
    ( Puzzle
    , Visibility(..)
    , adjustNum
    , chunked
    , chunks
    , correct
    , generate
    , init
    , maxNum
    , settings
    , viewElements
    )

import Assets exposing (Asset, Assets)
import Color exposing (Color)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Random
import Random.List


type Puzzle
    = Puzzle Int (Maybe Asset) (List Bool)


init : Puzzle
init =
    Puzzle defaultNum Nothing []


adjustNum : Int -> Puzzle -> Puzzle
adjustNum n (Puzzle _ a xs) =
    Puzzle n a xs


type Visibility
    = Visible
    | Hidden


defaultNum : Int
defaultNum =
    9


maxNum : Puzzle -> Int
maxNum (Puzzle n _ _) =
    n


chunked : Puzzle -> List a -> List (List a)
chunked p xs =
    if chunks p <= 0 then
        []

    else if List.length xs > chunks p then
        List.take (chunks p) xs :: chunked p (List.drop (chunks p) xs)

    else
        [ xs ]


viewElements :
    (Maybe Asset -> Visibility -> Int -> Element msg)
    -> Puzzle
    -> List ( String, Element msg )
viewElements f (Puzzle _ maybeAsset ns) =
    List.indexedMap
        (\i v ->
            ( String.fromInt i ++ Assets.name maybeAsset
            , if v then
                f maybeAsset Visible (i + 1)

              else
                f Nothing Hidden (i + 1)
            )
        )
        ns


settings : Int -> (Int -> msg) -> Element msg
settings value toMsg =
    Input.slider
        [ E.height (E.px 30)
        , E.behindContent
            (E.el
                [ E.width (E.px 100)
                , E.height (E.px 2)
                , E.centerY
                , Color.blue
                    |> Color.toRgba
                    |> E.fromRgb
                    |> Background.color
                , Border.rounded 2
                ]
                E.none
            )
        ]
        { onChange = toMsg << ceiling
        , label = Input.labelAbove [] (E.text "Max number")
        , min = 6
        , max = 18
        , step = Just 3
        , value = toFloat value
        , thumb = Input.defaultThumb
        }


chunks : Puzzle -> Int
chunks (Puzzle n _ _) =
    case n of
        6 ->
            2

        9 ->
            3

        12 ->
            4

        15 ->
            3

        18 ->
            6

        _ ->
            3


generate : Assets -> Puzzle -> Maybe (Random.Generator Puzzle)
generate assets p =
    let
        n =
            correctAnswer p

        max =
            maxNum p
    in
    case List.range 1 max |> List.filter (\x -> x /= n) of
        [] ->
            Nothing

        x :: xs ->
            Just <|
                Random.map2 (Puzzle max)
                    (Assets.random assets)
                    (Random.uniform x xs
                        |> Random.andThen
                            (\y ->
                                List.repeat y True
                                    ++ List.repeat (max - y) False
                                    |> Random.List.shuffle
                            )
                    )


correct : Int -> Puzzle -> Bool
correct n p =
    correctAnswer p == n


correctAnswer : Puzzle -> Int
correctAnswer (Puzzle _ _ puzzle) =
    List.length (List.filter identity puzzle)
