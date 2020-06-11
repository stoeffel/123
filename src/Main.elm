module Main exposing (..)

import Animator
import Animator.Inline
import Assets exposing (Asset, Assets)
import Browser
import Browser.Navigation as Navigation
import Color exposing (Color)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as EK
import Html.Attributes as Attr
import Json.Decode as D
import Particle exposing (Particle)
import Particle.System as System exposing (System)
import Process
import Puzzle exposing (Puzzle)
import Random exposing (Generator)
import Random.Extra
import Random.Float
import Random.List
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SAttrs
import Task
import Time
import Url exposing (Url)


type alias Model =
    { assets : Assets
    , animatePressed : AnimatePressed
    , animateBackground : AnimateState
    , animateState : AnimateState
    , puzzle : Puzzle
    , showSettings : Bool
    , fireworks : System Firework
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
            { assets = Assets.init
            , animatePressed = Animator.init All
            , puzzle = Puzzle.init
            , animateBackground = Animator.init Idle
            , animateState = Animator.init Correct
            , showSettings = False
            , fireworks = System.init (Random.initialSeed 0)
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
        D.field "assets" Assets.decoder


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
            , E.paddingXY 5 20
            , E.spacing 8
            , E.height E.fill
            , stateToColor
                |> Animator.Inline.backgroundColor model.animateBackground
                |> E.htmlAttribute
            , case Animator.current model.animateBackground of
                Correct ->
                    System.view viewFirework
                        [ Attr.style "width" "100%"
                        , Attr.style "height" "100%"
                        , Attr.style "background-color" "transparent"
                        , SAttrs.viewBox "0 0 300 600"
                        ]
                        model.fireworks
                        |> E.html
                        |> E.inFront

                _ ->
                    E.clip
            ]
            (E.column
                [ E.fill
                    |> E.maximum 400
                    |> E.width
                , E.height E.fill
                , E.centerX
                , E.spacing 8
                ]
                [ E.row
                    [ E.width E.fill
                    , E.paddingXY 40 0
                    , E.centerY
                    ]
                    [ E.el
                        [ E.centerX
                        , Font.size 30
                        ]
                        (E.text "WIÄ VIEL?")
                    , viewSettingsButton model.showSettings (Puzzle.maxNum model.puzzle)
                    ]
                , E.column
                    [ E.width E.fill
                    , E.height (E.fillPortion 4)
                    , E.centerX
                    , E.centerY
                    , animateXY model.animateState
                    ]
                    (viewImages model.puzzle)
                , E.column
                    [ E.width E.fill
                    , E.height E.fill
                    , E.centerX
                    ]
                    (model.puzzle
                        |> Puzzle.viewElements (viewNum model.animatePressed)
                        |> Puzzle.chunked model.puzzle
                        |> List.map (viewNumPadRow model.animatePressed)
                    )
                ]
            )
        ]
    }


viewSettingsButton : Bool -> Int -> Element Msg
viewSettingsButton showSettings value =
    Input.button
        [ E.alignRight
        , E.spacing 10
        , E.below <|
            if showSettings then
                E.el
                    [ E.alignRight
                    , Color.lightBlue
                        |> Color.toRgba
                        |> E.fromRgb
                        |> Background.color
                    , E.padding 8
                    , Border.solid
                    , Border.width 1
                    , Color.blue
                        |> Color.toRgba
                        |> E.fromRgb
                        |> Border.color
                    , Border.rounded 8
                    ]
                    (Puzzle.settings value AdjustValue)

            else
                E.none
        ]
        { onPress = Just ToggleSettings
        , label = E.text "*"
        }


viewImages : Puzzle -> List (Element msg)
viewImages puzzle =
    Puzzle.viewElements (viewImage (Puzzle.chunks puzzle)) puzzle
        |> Puzzle.chunked puzzle
        |> List.map
            (EK.row
                [ E.centerX
                , E.centerY
                , E.padding 8
                , E.height (E.fillPortion 1)
                , E.width E.fill
                ]
            )


viewImage : Int -> Maybe Asset -> Puzzle.Visibility -> Int -> Element msg
viewImage chunks maybeAsset visible _ =
    E.el
        [ E.centerX
        , E.clip
        , E.width E.fill
        , E.height E.fill
        ]
    <|
        case visible of
            Puzzle.Visible ->
                Assets.view maybeAsset

            Puzzle.Hidden ->
                E.none


viewNumPadRow : AnimatePressed -> List ( String, Element msg ) -> Element msg
viewNumPadRow animatePressed =
    EK.row
        [ E.width E.fill
        , E.fill
            |> E.maximum 110
            |> E.height
        , E.spacing 8
        , E.padding 8
        ]


viewNum : AnimatePressed -> Maybe a -> b -> Int -> Element Msg
viewNum animatePressed _ _ n =
    E.el [ E.width E.fill, E.height E.fill ] <|
        Input.button
            [ Font.size 40
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
    | ToggleSettings
    | AdjustValue Int
    | ParticleMsg (System.Msg Firework)
    | TriggerBurst
    | Burst


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GeneratePuzzle n ->
            ( model
            , case Puzzle.generate model.assets model.puzzle of
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
                        , showSettings = False
                      }
                    , Cmd.batch
                        [ Task.perform (\_ -> GeneratePuzzle n) (Process.sleep 3500)
                        , Task.perform (\_ -> TriggerBurst) (Process.sleep 0)
                        ]
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
                        , showSettings = False
                      }
                    , Cmd.none
                    )

        TriggerBurst ->
            ( model
            , Cmd.batch
                [ Task.perform (\_ -> Burst) (Process.sleep 0)
                , Task.perform (\_ -> Burst) (Process.sleep 50)
                , Task.perform (\_ -> Burst) (Process.sleep 100)
                , Task.perform (\_ -> Burst) (Process.sleep 150)
                , Task.perform (\_ -> Burst) (Process.sleep 200)
                , Task.perform (\_ -> Burst) (Process.sleep 250)
                , Task.perform (\_ -> Burst) (Process.sleep 300)
                , Task.perform (\_ -> Burst) (Process.sleep 350)
                , Task.perform (\_ -> Burst) (Process.sleep 400)
                ]
            )

        Burst ->
            ( { model
                | fireworks =
                    System.burst
                        (Random.list 1
                            (Random.Extra.andThen3 fireworkAt
                                (Random.uniform Color.red [ Color.yellow, Color.purple, Color.blue, Color.white ])
                                (Random.float 0 500)
                                (Random.float 0 300)
                            )
                            |> Random.map List.concat
                        )
                        model.fireworks
              }
            , Cmd.none
            )

        ToggleSettings ->
            ( { model | showSettings = not model.showSettings }, Cmd.none )

        AdjustValue n ->
            ( { model | puzzle = Puzzle.adjustNum n model.puzzle }
            , Task.perform (\_ -> GeneratePuzzle 0) (Process.sleep 0)
            )

        Tick newTime ->
            ( Animator.update newTime animator model, Cmd.none )

        ParticleMsg inner ->
            ( { model | fireworks = System.update inner model.fireworks }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animator.toSubscription Tick model animator
        , System.sub [] ParticleMsg model.fireworks
        ]


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


type Firework
    = Fizzler Color


fizzler : Color -> Generator (Particle Firework)
fizzler color =
    Particle.init (Random.constant (Fizzler color))
        |> Particle.withDirection (Random.map degrees (Random.float 0 360))
        |> Particle.withSpeed (Random.map (clamp 0 200) (Random.Float.normal 100 100))
        |> Particle.withLifetime (Random.Float.normal 1.25 0.1)


fireworkAt : Color -> Float -> Float -> Generator (List (Particle Firework))
fireworkAt color x y =
    fizzler color
        |> Particle.withLocation (Random.constant { x = x, y = y })
        |> Particle.withGravity 50
        |> Particle.withDrag
            (\_ ->
                { coefficient = 1
                , density = 0.015
                , area = 2
                }
            )
        |> Random.list 80


viewFirework : Particle Firework -> Svg msg
viewFirework particle =
    case Particle.data particle of
        Fizzler color ->
            let
                length =
                    max 2 (Particle.speed particle / 15)

                { hue, saturation, lightness } =
                    Color.toHsla color

                maxLuminance =
                    0.6

                luminanceDelta =
                    maxLuminance - lightness

                lifetime =
                    Particle.lifetimePercent particle

                opacity =
                    if lifetime < 0.1 then
                        lifetime * 10

                    else
                        1
            in
            Svg.ellipse
                [ -- location within the burst
                  SAttrs.cx (String.fromFloat (length / 2))
                , SAttrs.cy "0"

                -- size, smeared by motion
                , SAttrs.rx (String.fromFloat length)
                , SAttrs.ry "2"
                , SAttrs.transform ("rotate(" ++ String.fromFloat (Particle.directionDegrees particle) ++ ")")

                -- color!
                , SAttrs.opacity (String.fromFloat opacity)
                , maxLuminance
                    - luminanceDelta
                    * (1 - lifetime)
                    |> Color.hsl hue saturation
                    |> Color.toCssString
                    |> SAttrs.fill
                ]
                []


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url -> Msg
onUrlChange _ =
    NoOp


backgroundColor : E.Color
backgroundColor =
    E.rgb 0.89 0.89 0.89


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
