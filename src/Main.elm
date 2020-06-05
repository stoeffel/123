module Main exposing (..)

import Animator
import Animator.Inline
import Browser
import Browser.Navigation as Navigation
import Color exposing (Color)
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Json.Decode as D
import Process
import Puzzle exposing (Puzzle)
import Random
import Random.List
import Set exposing (Set)
import Task
import Time
import Url exposing (Url)


type alias Assets =
    { yak : String }


type alias Model =
    { assets : Assets
    , animatePressed : AnimatePressed
    , animateBackground : AnimateState
    , animateState : AnimateState
    , puzzle : Puzzle
    }


type alias AnimatePressed =
    Animator.Timeline Pressed


type Pressed
    = All
    | Attempts (Set Int)
    | Solved Int


type alias AnimateState =
    Animator.Timeline State


type State
    = Idle
    | Correct
    | Wrong


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


type alias Flags =
    { assets : Assets
    }


init : D.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags _ _ =
    let
        defaultModel =
            { assets = { yak = "" }
            , animatePressed = Animator.init All
            , puzzle = Puzzle.init
            , animateBackground = Animator.init Idle
            , animateState = Animator.init Correct
            }
    in
    case D.decodeValue flagsDecoder flags of
        Err _ ->
            ( defaultModel, Cmd.none )

        Ok { assets } ->
            ( { defaultModel | assets = assets }
            , Task.perform (\_ -> GeneratePuzzle 0) (Process.sleep 200)
            )


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map Flags <|
        D.field "assets" <|
            D.map Assets
                (D.field "Yak" D.string)


view : Model -> Browser.Document Msg
view model =
    { title = "123"
    , body =
        [ E.layout
            [ Background.color backgroundColor
            , Font.family
                [ Font.typeface "Patrick Hand"
                , Font.sansSerif
                ]
            , E.paddingXY 5 50
            , E.spacing 8
            , E.clip
            , stateToColor
                |> Animator.Inline.backgroundColor model.animateBackground
                |> E.htmlAttribute
            ]
            (E.column
                [ E.fill
                    |> E.maximum 400
                    |> E.width
                , E.height E.fill
                , E.centerX
                ]
                [ E.el
                    [ E.centerX
                    , Font.size 30
                    ]
                    (E.text "WIÄ VIEL?")
                , E.column
                    [ E.width E.fill
                    , E.height (E.px 440)
                    , E.centerX
                    , E.centerY
                    , E.spaceEvenly
                    , animateXY model.animateState
                    ]
                    (viewImages model.assets model.puzzle)
                , E.column
                    [ E.width E.fill
                    , E.height (E.fillPortion 1)
                    ]
                    (model.puzzle
                        |> Puzzle.viewElements (viewNum model.animatePressed)
                        |> chunksOfLeft 3
                        |> List.map (viewNumPadRow model.animatePressed)
                    )
                ]
            )
        ]
    }


viewImages : Assets -> Puzzle -> List (Element msg)
viewImages assets puzzle =
    Puzzle.viewElements (viewImage assets) puzzle
        |> chunksOfLeft 3
        |> List.map
            (E.row
                [ E.centerX
                , E.centerY
                , E.spacing 8
                , E.padding 8
                ]
            )


viewImage : Assets -> Puzzle.Visibility -> Int -> Element msg
viewImage assets visible _ =
    case visible of
        Puzzle.Visible ->
            E.el
                [ E.htmlAttribute (Attr.style "border-radius" "50%")
                , Border.innerGlow
                    (Color.grey
                        |> Color.toRgba
                        |> E.fromRgb
                    )
                    3
                , Color.lightGrey
                    |> Color.toRgba
                    |> E.fromRgb
                    |> Background.color
                , E.centerX
                , E.padding 2
                , E.px 110
                    |> E.width
                , E.px 110
                    |> E.height
                , E.clip
                ]
            <|
                E.image
                    [ E.centerX
                    , E.centerY
                    , Background.uncropped ""
                    ]
                    { src = assets.yak
                    , description = assets.yak
                    }

        Puzzle.Hidden ->
            E.el
                [ E.padding 2
                , E.px 110
                    |> E.width
                , E.px 110
                    |> E.height
                ]
                E.none


viewNumPadRow : AnimatePressed -> List (Element msg) -> Element msg
viewNumPadRow animatePressed =
    E.row
        [ E.width E.fill
        , E.height E.fill
        , E.spacing 8
        , E.padding 8
        ]


viewNum : AnimatePressed -> Puzzle.Visibility -> Int -> Element Msg
viewNum animatePressed _ n =
    E.el [ E.width E.fill, E.height E.fill ] <|
        Input.button
            [ Font.size 60
            , E.centerX
            , E.centerY
            , E.width E.fill
            , E.height E.fill
            , Border.solid
            , Border.rounded 18
            , Border.width 3
            , (pressedToStyle n >> .lightColor)
                |> Animator.Inline.backgroundColor animatePressed
                |> E.htmlAttribute
            , (pressedToStyle n >> .darkColor)
                |> Animator.Inline.borderColor animatePressed
                |> E.htmlAttribute
            , (pressedToStyle n >> .opacity)
                |> Animator.Inline.opacity animatePressed
                |> E.htmlAttribute
            , (pressedToStyle n >> .xy)
                |> Animator.Inline.xy animatePressed
                |> E.htmlAttribute
            , (pressedToStyle n >> .shadow)
                |> Animator.Inline.style animatePressed
                    "box-shadow"
                    (\float ->
                        let
                            str =
                                String.fromFloat float
                        in
                        "0px " ++ str ++ "px 0px 0px " ++ Color.toCssString Color.blue
                    )
                |> E.htmlAttribute
            ]
            { onPress =
                case Animator.current animatePressed of
                    All ->
                        Nothing

                    Solved x ->
                        Nothing

                    Attempts set ->
                        if Set.member n set then
                            Nothing

                        else
                            Just (NumPressed n)
            , label =
                String.fromInt n
                    |> E.text
                    |> E.el [ E.centerX, E.centerY ]
            }


type Msg
    = NoOp
    | NumPressed Int
    | Tick Time.Posix
    | NewPuzzle Puzzle
    | GeneratePuzzle Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GeneratePuzzle n ->
            ( model
            , case Puzzle.generate model.puzzle of
                Nothing ->
                    Cmd.none

                Just generate ->
                    Random.generate NewPuzzle generate
            )

        NewPuzzle puzzle ->
            ( { model
                | puzzle = puzzle
                , animatePressed =
                    Animator.queue
                        [ Animator.event Animator.quickly <| Attempts Set.empty ]
                        model.animatePressed
                , animateState =
                    Animator.queue [ Animator.event Animator.verySlowly Idle ] model.animateState
              }
            , Cmd.none
            )

        NumPressed n ->
            let
                newState =
                    if Puzzle.correct n model.puzzle then
                        Correct

                    else
                        Wrong

                newPressed =
                    case Animator.current model.animatePressed of
                        All ->
                            All

                        Solved x ->
                            Solved x

                        Attempts set ->
                            case newState of
                                Correct ->
                                    Solved n

                                _ ->
                                    Attempts (Set.insert n set)
            in
            case newState of
                Correct ->
                    ( { model
                        | animatePressed =
                            Animator.interrupt [ Animator.event Animator.quickly newPressed ] model.animatePressed
                        , animateBackground =
                            Animator.interrupt
                                [ Animator.event Animator.verySlowly Correct
                                , Animator.wait <| Animator.millis 3500
                                , Animator.event Animator.verySlowly Idle
                                ]
                                model.animateBackground
                        , animateState =
                            Animator.interrupt
                                [ Animator.wait <| Animator.millis 1500
                                , Animator.event Animator.verySlowly Correct
                                ]
                                model.animateState
                      }
                    , Task.perform (\_ -> GeneratePuzzle n) (Process.sleep 3500)
                    )

                _ ->
                    ( { model
                        | animatePressed =
                            Animator.interrupt [ Animator.event Animator.quickly newPressed ] model.animatePressed
                        , animateBackground =
                            Animator.interrupt
                                [ Animator.event Animator.verySlowly newState
                                , Animator.wait Animator.verySlowly
                                , Animator.event Animator.verySlowly Idle
                                ]
                                model.animateBackground
                      }
                    , Cmd.none
                    )

        Tick newTime ->
            ( Animator.update newTime animator model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animator.toSubscription Tick model animator


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .animateState
            (\newAnimateState model ->
                { model | animateState = newAnimateState }
            )
        |> Animator.watching
            .animateBackground
            (\newAnimateBackground model ->
                { model | animateBackground = newAnimateBackground }
            )
        |> Animator.watching
            .animatePressed
            (\newAnimatePressed model ->
                { model | animatePressed = newAnimatePressed }
            )


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url -> Msg
onUrlChange _ =
    NoOp


backgroundColor : E.Color
backgroundColor =
    E.rgb 0.89 0.89 0.89


chunksOfLeft : Int -> List a -> List (List a)
chunksOfLeft k xs =
    if k <= 0 then
        []

    else if List.length xs > k then
        List.take k xs :: chunksOfLeft k (List.drop k xs)

    else
        [ xs ]


animateXY : AnimateState -> E.Attribute a
animateXY animateState =
    E.htmlAttribute <|
        Animator.Inline.xy animateState <|
            \state ->
                case state of
                    Correct ->
                        { y =
                            Animator.at -570
                                |> Animator.arriveSmoothly 1
                                |> Animator.withWobble 0
                        , x = Animator.at 0
                        }

                    _ ->
                        { y =
                            Animator.at 0
                                |> Animator.arriveSmoothly 1
                                |> Animator.withWobble 0
                        , x = Animator.at 0
                        }


stateToColor : State -> Color
stateToColor state =
    case state of
        Correct ->
            Color.lightGreen
                |> Color.toHsla
                |> (\c -> { c | lightness = 0.8 })
                |> Color.fromHsla

        Wrong ->
            Color.lightPurple
                |> Color.toHsla
                |> (\c -> { c | lightness = 0.8 })
                |> Color.fromHsla

        Idle ->
            backgroundColor
                |> E.toRgb
                |> Color.fromRgba


pressedToStyle :
    Int
    -> Pressed
    ->
        { lightColor : Color
        , darkColor : Color
        , opacity : Animator.Movement
        , xy : { x : Animator.Movement, y : Animator.Movement }
        , shadow : Animator.Movement
        }
pressedToStyle n maybePressed =
    case maybePressed of
        Solved x ->
            if x == n then
                { lightColor = Color.lightGreen
                , darkColor = Color.green
                , opacity = Animator.at 1
                , xy = { x = Animator.at 0, y = Animator.at 8 }
                , shadow = Animator.at 0
                }

            else
                { lightColor = Color.lightPurple
                , darkColor = Color.purple
                , opacity = Animator.at 0.2
                , xy = { x = Animator.at 0, y = Animator.at 8 }
                , shadow = Animator.at 0
                }

        Attempts set ->
            if Set.member n set then
                { lightColor = Color.lightPurple
                , darkColor = Color.purple
                , opacity = Animator.at 0.5
                , xy = { x = Animator.at 0, y = Animator.at 8 }
                , shadow = Animator.at 0
                }

            else
                { lightColor = Color.lightBlue
                , darkColor = Color.blue
                , opacity = Animator.at 1
                , xy = { x = Animator.at 0, y = Animator.at 0 }
                , shadow = Animator.at 8
                }

        All ->
            { lightColor = Color.lightBlue
            , darkColor = Color.blue
            , opacity = Animator.at 0.5
            , xy = { x = Animator.at 0, y = Animator.at 8 }
            , shadow = Animator.at 0
            }
