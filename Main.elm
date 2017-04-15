module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (millisecond, Time)


-- MODEL


type Direction
    = North
    | South
    | East
    | West


type Colour
    = Black
    | White
    | Red


type alias Position =
    ( Int, Int )


type alias Ant =
    { position : Position
    , direction : Direction
    }


type alias World =
    Dict Position Colour


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
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( model, Cmd.none )



-- VIEW


gridUnit : Int
gridUnit =
    10


view : Model -> Svg Msg
view { ant, world } =
    svg
        [ width "100vw", height "100vh", viewBox "0 0 500 500" ]
        [ renderWorld world, renderAnt ant ]


renderAnt : Ant -> Svg a
renderAnt { position } =
    renderUnit ( position, Red )


renderWorld : World -> Svg a
renderWorld world =
    g [] (List.map renderUnit <| Dict.toList world)


renderUnit : ( Position, Colour ) -> Svg a
renderUnit ( position, colour ) =
    let
        ( xPos, yPos ) =
            position
    in
        rect
            [ x <| toString <| xPos * gridUnit
            , y <| toString <| yPos * gridUnit
            , width "10"
            , height "10"
            , fill <| colourToString colour
            ]
            []


colourToString : Colour -> String
colourToString colour =
    case colour of
        Red ->
            "red"

        Black ->
            "black"

        White ->
            "white"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (500 * millisecond) Tick



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
