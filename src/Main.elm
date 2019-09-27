module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewHand)

import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Task.perform Tick Time.now
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        hour =
            toFloat (Time.toHour model.zone model.time)

        minute =
            toFloat (Time.toMinute model.zone model.time)

        second =
            toFloat (Time.toSecond model.zone model.time)
    in
    svg
        [ viewBox "0 0 400 400"
        , width "400"
        , height "400"
        , Svg.Attributes.style "background-color: #ddd;"
        ]
        [ viewFace (second / 60)
        , viewPortal

        -- , viewHand 6 60 (hour / 12)
        -- , viewHand 6 90 (minute / 60)
        , viewHand 1 120 (second / 60)
        ]


viewPortal : Svg msg
viewPortal =
    circle
        [ cx "200"
        , cy "200"
        , r (String.fromInt smallRadius)
        , fill "#aaa"
        , fillOpacity "0.5"
        ]
        []


viewFace : Float -> Svg msg
viewFace turns =
    let
        t =
            2 * pi * (turns - 0.25)

        length =
            200

        xStr =
            200 + length * cos t |> String.fromFloat

        yStr =
            200 + length * sin t |> String.fromFloat

        color =
            "#fff"

        opacity =
            "0.1"
    in
    g
        [ [ "translate(", xStr, ", ", yStr, ")" ]
            |> List.foldr (++) ""
            |> transform

        -- , fillOpacity opacity
        ]
        [ circle
            [ cx "0"
            , cy "0"
            , r (String.fromInt bigRadius)
            , fill color
            ]
            []
        , viewMarker 25 0
        , viewMarker 10 10
        , viewMarker 10 20
        , viewMarker 25 30
        , viewMarker 10 40
        , viewMarker 10 50
        , viewMarker 25 60
        , viewMarker 10 70
        , viewMarker 10 80
        , viewMarker 25 90
        , viewMarker 10 100
        , viewMarker 10 110
        , viewMarker 25 120
        , viewMarker 10 130
        , viewMarker 10 140
        , viewMarker 25 150
        , viewMarker 10 160
        , viewMarker 10 170
        , viewMarker 25 180
        , viewMarker 10 190
        , viewMarker 10 200
        , viewMarker 25 210
        , viewMarker 10 220
        , viewMarker 10 230
        , viewMarker 25 240
        , viewMarker 10 250
        , viewMarker 10 260
        , viewMarker 25 270
        , viewMarker 10 280
        , viewMarker 10 290
        , viewMarker 25 300
        , viewMarker 10 310
        , viewMarker 10 320
        , viewMarker 25 330
        , viewMarker 10 340
        , viewMarker 10 350
        ]


viewMarker : Int -> Int -> Svg msg
viewMarker length rotation =
    let
        start =
            -260

        end =
            start + length

        rotationStr =
            String.fromInt rotation
    in
    line
        [ x1 "0"
        , y1 (String.fromInt start)
        , x2 "0"
        , y2 (String.fromInt end)
        , stroke "black"
        , strokeWidth "2"
        , [ "rotate(", rotationStr, ", 0, 0)" ]
            |> List.foldr (++) ""
            |> transform
        ]
        []


viewHand : Int -> Float -> Float -> Svg msg
viewHand width length turns =
    let
        t =
            2 * pi * (turns - 0.25)

        x =
            200 + length * cos t

        y =
            200 + length * sin t

        xOne =
            200 - length * cos t

        yOne =
            200 - length * sin t
    in
    line
        [ x1 (String.fromFloat xOne)
        , y1 (String.fromFloat yOne)
        , x2 (String.fromFloat x)
        , y2 (String.fromFloat y)
        , stroke "orange"
        , strokeWidth (String.fromInt width)
        , strokeLinecap "round"
        ]
        []


bigRadius : Int
bigRadius =
    320


smallRadius : Int
smallRadius =
    120


viewportOffset : Int
viewportOffset =
    200
