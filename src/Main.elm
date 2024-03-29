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

        -- , viewNumerals
        -- , viewHand 6 60 (hour / 12)
        -- , viewHand 6 90 (minute / 60)
        , viewHand 1 120 (second / 60)
        ]


viewPortal : Svg msg
viewPortal =
    circle [ cx "200", cy "200", r "120", fill "#fff" ] []


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

        radius =
            "320"

        color =
            "#f00"

        opacity =
            "0.1"
    in
    circle
        [ cx xStr
        , cy yStr
        , r radius
        , fill color
        , fillOpacity opacity
        ]
        []


viewNumerals : Svg msg
viewNumerals =
    text_ [ x "200", y "200" ] [ text "6" ]


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
