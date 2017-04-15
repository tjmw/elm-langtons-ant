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
            ( moveForward <| flipSquare <| turnAnt <| model, Cmd.none )


flipSquare : Model -> Model
flipSquare model =
    let
        ant =
            model.ant

        world =
            model.world
    in
        { model | world = flipSquareAt ant.position world }


flipSquareAt : Position -> World -> World
flipSquareAt position world =
    let
        currentColour =
            colourAt position world
    in
        Dict.insert position (flipColour currentColour) world


flipColour : Colour -> Colour
flipColour colour =
    case colour of
        White ->
            Black

        Black ->
            White


turnAnt : Model -> Model
turnAnt model =
    let
        ant =
            model.ant

        world =
            model.world
    in
        case colourAt ant.position world of
            White ->
                { model | ant = turnRight ant }

            Black ->
                { model | ant = turnLeft ant }


turnRight : Ant -> Ant
turnRight ant =
    let
        newDirection =
            case ant.direction of
                North ->
                    East

                East ->
                    South

                South ->
                    West

                West ->
                    North
    in
        { ant | direction = newDirection }


turnLeft : Ant -> Ant
turnLeft ant =
    let
        newDirection =
            case ant.direction of
                North ->
                    West

                West ->
                    South

                South ->
                    East

                East ->
                    North
    in
        { ant | direction = newDirection }


moveForward : Model -> Model
moveForward model =
    let
        ant =
            model.ant
    in
        { model | ant = updateAntPosition ant }


updateAntPosition : Ant -> Ant
updateAntPosition ant =
    let
        ( x, y ) =
            ant.position
    in
        case ant.direction of
            North ->
                { ant | position = ( x, wrap <| y - 1 ) }

            East ->
                { ant | position = ( wrap <| x + 1, y ) }

            South ->
                { ant | position = ( x, wrap <| y + 1 ) }

            West ->
                { ant | position = ( wrap <| x - 1, y ) }


wrap : Int -> Int
wrap int =
    if int < 0 then
        gridSize - 1
    else if int > gridSize - 1 then
        0
    else
        int


colourAt : Position -> World -> Colour
colourAt position world =
    case Dict.get position world of
        Just colour ->
            colour

        Nothing ->
            White



-- VIEW


gridUnit : Int
gridUnit =
    10


gridSize : Int
gridSize =
    50


view : Model -> Svg Msg
view { ant, world } =
    svg
        [ width "100vw"
        , height "100vh"
        , viewBox <| "0 0 " ++ viewBoxSize ++ " " ++ viewBoxSize
        ]
        [ renderWorld world, renderAnt ant ]


viewBoxSize : String
viewBoxSize =
    toString <| gridSize * gridUnit


renderAnt : Ant -> Svg a
renderAnt { position } =
    let
        ( xPos, yPos ) =
            position
    in
        renderRect xPos yPos "red"


renderWorld : World -> Svg a
renderWorld world =
    g [] (List.map renderUnit <| Dict.toList world)


renderUnit : ( Position, Colour ) -> Svg a
renderUnit ( position, colour ) =
    let
        ( xPos, yPos ) =
            position

        colourString =
            colourToString colour
    in
        renderRect xPos yPos colourString


renderRect : Int -> Int -> String -> Svg a
renderRect xPos yPos colourString =
    rect
        [ x <| toString <| xPos * gridUnit
        , y <| toString <| yPos * gridUnit
        , width "10"
        , height "10"
        , fill <| colourString
        ]
        []


colourToString : Colour -> String
colourToString colour =
    case colour of
        Black ->
            "black"

        White ->
            "white"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (100 * millisecond) Tick



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
