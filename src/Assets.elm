module Assets exposing (Asset, Assets, decoder, init, name, random, view)

import Element as E exposing (Element)
import Element.Background as Background
import Json.Decode as D
import Random


type Assets
    = NotLoaded
    | Loaded LoadedAssets


type alias LoadedAssets =
    { bear : Asset
    , bicycle : Asset
    , clown : Asset
    , dancing : Asset
    , dog : Asset
    , dolphin : Asset
    , elk : Asset
    , fox : Asset
    , giraffe : Asset
    , hedgehog : Asset
    , hunter : Asset
    , karate : Asset
    , monkey : Asset
    , rocket : Asset
    , star : Asset
    , train : Asset
    , uhu : Asset
    , yak : Asset
    }


type Asset
    = Asset String String


maybe : Assets -> Maybe Asset
maybe assets =
    case assets of
        NotLoaded ->
            Nothing

        Loaded a ->
            Just a.hedgehog


random : Assets -> Random.Generator (Maybe Asset)
random assets =
    case assets of
        NotLoaded ->
            Random.constant Nothing

        Loaded a ->
            Random.uniform a.yak
                [ a.bear
                , a.bicycle
                , a.clown
                , a.dancing
                , a.dog
                , a.dolphin
                , a.elk
                , a.fox
                , a.giraffe
                , a.hedgehog
                , a.hunter
                , a.karate
                , a.monkey
                , a.rocket
                , a.star
                , a.train
                , a.uhu
                ]
                |> Random.map Just


view : Maybe Asset -> Element msg
view maybeAsset =
    case maybeAsset of
        Nothing ->
            E.none

        Just (Asset n src) ->
            E.image
                [ E.centerX
                , E.centerY
                , Background.uncropped ""
                ]
                { src = src
                , description = n
                }


name : Maybe Asset -> String
name maybeAsset =
    case maybeAsset of
        Just (Asset n _) ->
            n

        Nothing ->
            ""


init : Assets
init =
    NotLoaded


decoder : D.Decoder Assets
decoder =
    D.succeed LoadedAssets
        |> andMap (assetDecoder "Bear")
        |> andMap (assetDecoder "Bicycle")
        |> andMap (assetDecoder "Clown")
        |> andMap (assetDecoder "Dancing")
        |> andMap (assetDecoder "Dog")
        |> andMap (assetDecoder "Dolphin")
        |> andMap (assetDecoder "Elk")
        |> andMap (assetDecoder "Fox")
        |> andMap (assetDecoder "Giraffe")
        |> andMap (assetDecoder "Hedgehog")
        |> andMap (assetDecoder "Hunter")
        |> andMap (assetDecoder "Karate")
        |> andMap (assetDecoder "Monkey")
        |> andMap (assetDecoder "Rocket")
        |> andMap (assetDecoder "Star")
        |> andMap (assetDecoder "Train")
        |> andMap (assetDecoder "Uhu")
        |> andMap (assetDecoder "Yak")
        |> D.map Loaded


assetDecoder : String -> D.Decoder Asset
assetDecoder n =
    D.field n D.string
        |> D.map (Asset n)


andMap : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
andMap =
    D.map2 (|>)
