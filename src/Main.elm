module Main exposing (main, view)

import Browser
import Browser.Dom exposing (focus)
import Canvas exposing (..)
import Color
import Html exposing (Attribute, Html, button, div, h1, img, input, text)
import Html.Attributes exposing (autofocus, hidden, id, src, style, tabindex)
import Html.Events exposing (keyCode, on, onClick)
import Json.Decode as Json
import List.Extra exposing (last)
import Random
import Task
import Time



---- MODEL ----


type alias Position =
    ( Int, Int )


type alias Coordinates =
    ( Float, Float )


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | KeyDown Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


canvasLength =
    400


worldSize =
    40


positionToRectCoordinates : Position -> Coordinates
positionToRectCoordinates position =
    mapBoth (\x -> toFloat x / worldSize * canvasLength) position


positionToSquareCoordinates : Position -> Coordinates
positionToSquareCoordinates position =
    mapBoth (\x -> (toFloat x / worldSize * canvasLength) + (canvasLength / worldSize / 2)) position


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


view : Model -> Html Msg
view model =
    let
        thingsToRender =
            [ renderBackground ]
    in
    div
        [ id "canvasWrap"
        , tabindex 0
        , onKeyDown KeyDown
        , style "outline" "none"
        , style "height" "100vh"
        ]
        [ Canvas.toHtml
            ( canvasLength, canvasLength )
            [ style "border" "1px solid black" ]
            thingsToRender
        ]


renderSquare color length position =
    shapes [ fill color ]
        [ rect (positionToRectCoordinates position) length length ]


renderCircle color diameter position =
    shapes [ fill color ]
        [ circle (positionToSquareCoordinates position) diameter ]


renderBackground =
    renderSquare Color.gray canvasLength ( 0, 0 )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- UTILS ----


mapBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapBoth f ( x, y ) =
    ( f x, f y )
