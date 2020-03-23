module Main exposing (..)

-- import Random

import Array exposing (fromList, get)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (length)
import String


colorToString : Color -> String
colorToString c =
    case c of
        Alien ->
            "👽"

        White ->
            "🤍"

        Wave ->
            "👋"

        Tiger ->
            "🐅"

        Clover ->
            "☘️"

        Red ->
            "🔴"

        Blue ->
            "💙"

        Yellow ->
            "💛"

        Purple ->
            "💜"

        Black ->
            "⬛️"


colors : List Color
colors =
    [ Alien, White, Wave, Tiger, Clover, Red, Blue, Yellow, Purple, Black ]


type Color
    = Alien
    | White
    | Wave
    | Tiger
    | Clover
    | Red
    | Blue
    | Yellow
    | Purple
    | Black

---- MODEL ----

type alias Model =
    { row : Int
    , column : Int
    , selectedColor : Color
    , fillCount : Int
    , moveCount : Int
    , completed : Bool
    , teleportEnabled : Bool
    , grid : List (List Color)
    }


init : ( Model, Cmd Msg )
init =
    ( { row = 0
      , column = 0
      , selectedColor = Alien
      , moveCount = 0
      , fillCount = 0
      , completed = False
      , teleportEnabled = False
      , grid = generateGrid 10
      }
    , Cmd.none
    )

generateGrid : Int -> List (List Color)
generateGrid dimension = 
    generateGridHelper dimension dimension

generateGridHelper : Int -> Int -> List (List Color)
generateGridHelper dimension curr =
    case curr of
        0 ->
            []

        _ ->
            generateList dimension (getEmoji (curr, colors)) :: generateGridHelper dimension (curr - 1)


generateList : Int -> Color -> List Color
generateList size color =
    case size of
        0 ->
            []

        _ ->
            color :: generateList (size - 1) color


getEmoji : ( Int, List Color ) -> Color
getEmoji ( index, options ) =
    case get index (fromList options) of
        Just emoji ->
            emoji

        Nothing ->
            Alien



---- UPDATE ----


type Msg
    = ChangeColor Color
    | Up
    | Down
    | Left
    | Right
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Up ->
            ( { model | row = up model.row }, Cmd.none)

        Down ->
            ( { model | row = down model.row }, Cmd.none)

        Left ->
            ( { model | column = left model.column }, Cmd.none)
         
        Right ->
            ( { model | column = right model.column }, Cmd.none)
        ChangeColor color ->
            ( { model | selectedColor = color }, Cmd.none )


up : Int -> Int
up row =
    if row <= 0 then
        length colors - 1
    else
        row - 1

        
down : Int -> Int
down row =
    if row >= length colors - 1 then
        0
    else
        row + 1

right : Int -> Int
right column =
    if column >= length colors - 1 then
        0
    else
        column + 1

left : Int -> Int
left column =
    if column <= 0 then
        length colors - 1
    else
        column - 1


---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id "grid" ]
        [ h1 [] [ text "Elm Color Fill Game!" ]
        , p [] [ text ("Row: " ++ String.fromInt model.row) ]
        , p [] [ text ("Column: " ++ String.fromInt model.column) ]
        , button [ onClick Up ] [ text "^" ]
        , button [ onClick Down ] [ text "v" ]
        , button [ onClick Left ] [ text "<" ]
        , button [ onClick Right ] [ text ">" ]
        , renderGrid model.grid
        ]


renderGrid : List (List Color) -> Html msg
renderGrid grd =
    grd
        |> List.map (\l -> ul [ class "row" ] [ renderList l ])
        |> ul []


renderList : List Color -> Html msg
renderList lst =
    lst
        |> List.map (\l -> li [] [ text (colorToString l) ])
        |> ul []



---- PROGRAM ----
---- Leave this alone! ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
