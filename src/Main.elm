module Main exposing (..)

-- import Random

import Array exposing (fromList, get)
import Browser
import Browser.Events as Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import List exposing (append, length)
import String


colorToString : Color -> String
colorToString c =
    case c of
        Alien ->
            "👽"

        Unicorn ->
            "\u{1F984}"

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
    [ Alien, Unicorn, Wave, Tiger, Clover, Red, Blue, Yellow, Purple, Black ]


type Color
    = Alien
    | Unicorn
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
            generateList dimension (getEmoji ( curr - 1, colors )) :: generateGridHelper dimension (curr - 1)


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onKeyDown (Decode.map KeyDowns keyDecoder)
        , Events.onKeyUp (Decode.succeed ClearPressed)
        ]


type Msg
    = ChangeColor Color
    | KeyDowns Direction
    | ClearPressed
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeColor color ->
            ( { model | selectedColor = color }, Cmd.none )

        KeyDowns code ->
            ( case code of
                Up ->
                    { model | row = up model.row, moveCount = model.moveCount + 1 }

                Down ->
                    { model | row = down model.row, moveCount = model.moveCount + 1 }

                Left ->
                    { model | column = left model.column, moveCount = model.moveCount + 1 }

                Right ->
                    { model | column = right model.column, moveCount = model.moveCount + 1 }

                Enter ->
                    { model | grid = chomp model.grid, fillCount = model.fillCount + 1 }

                None ->
                    model
            , Cmd.none
            )

        -- Flush the whole model on `keyup`, helps to remove not pressed keys, if focus was lost from the window.
        ClearPressed ->
            ( model, Cmd.none )


chomp : List (List Color) -> List (List Color)
chomp grid =
    case grid of
        [] ->
            []

        x :: xs ->
            append xs [ x ]


type Direction
    = Up
    | Down
    | Left
    | Right
    | Enter
    | None


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "LeftArrow" ->
            Left

        "a" ->
            Left

        "ArrowRight" ->
            Right

        "d" ->
            Right

        "ArrowUp" ->
            Up

        "w" ->
            Up

        "ArrowDown" ->
            Down

        "s" ->
            Down

        "Enter" ->
            Enter

        _ ->
            None


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
        [ h2 [] [ text "Elm Candy Chomper!" ]
        , p [] [ text "Use the arrow keys or WASD to navigate the grid and enter to chomp!" ]
        , p [] [ text ("Location: (" ++ String.fromInt model.row ++ ", " ++ String.fromInt model.column ++ ") Moves: " ++ String.fromInt model.moveCount ++ " Fills: " ++ String.fromInt model.fillCount) ]
        , renderGrid model.grid model.row model.column
        ]


renderGrid : List (List Color) -> Int -> Int -> Html msg
renderGrid grd row col =
    grd
        |> List.indexedMap (\idx l -> ul [ class "row" ] [ renderList l row col idx ])
        |> ul []


renderList : List Color -> Int -> Int -> Int -> Html msg
renderList lst row col currCol =
    lst
        |> List.indexedMap
            (\idx l ->
                if row == idx && col == currCol then
                    li [ class "selected" ] [ text (colorToString l) ]

                else
                    li [] [ text (colorToString l) ]
            )
        |> ul []



---- PROGRAM ----
---- Leave this alone! ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
