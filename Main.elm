module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- MODEL


type Direction
    = North
    | South
    | East
    | West


type Color
    = Black
    | White


type alias Position =
    ( Int, Int )


type alias Ant =
    { position : Position
    , direction : Direction
    }


type alias World =
    Dict Position Color


type alias Model =
    { ant : Ant
    , world : World
    }


model : Model
model =
    { ant =
        { position = ( 25, 25 )
        , direction = North
        }
    , world = Dict.empty
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



-- VIEW


gridUnit : Int
gridUnit =
    10


view : Model -> Svg Msg
view { ant } =
    svg
        [ width "100vw", height "100vh", viewBox "0 0 500 500" ]
        [ renderAnt ant ]


renderAnt : Ant -> Svg a
renderAnt { position } =
    let
        ( xPos, yPos ) =
            position
    in
        rect [ x <| toString <| xPos * gridUnit
             , y <| toString <| yPos * gridUnit
             , width "10"
             , height "10"
             , fill "red"
             ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
