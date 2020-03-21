module Main exposing (..)

-- import Random

import Array exposing (fromList, get)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import List exposing (length)
import String


colorToString : Color -> String
colorToString c =
    case c of
        Dog ->
            "🦮"

        Otter ->
            "🦦"

        Waffle ->
            "🧇"

        Planet ->
            "🪐"

        Axe ->
            "🪓"

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

        Orange ->
            "🔶"

        Brown ->
            "📦"


colors : List String
colors =
    [ "🦮", "🦦", "🧇", "🪐", "🪓", "☘️", "🔴", "💙", "💛", "💜", "⬛️", "🔶", "📦" ]


type Color
    = Dog
    | Otter
    | Waffle
    | Planet
    | Axe
    | Clover
    | Red
    | Blue
    | Yellow
    | Purple
    | Black
    | Orange
    | Brown



-- colorOptions : List ( String, Color )
-- colorOptions =
--     [ ( colorToString Dog, Dog )
--     , ( colorToString Otter, Otter )
--     , ( colorToString Waffle, Waffle )
--     , ( colorToString Planet, Planet )
--     , ( colorToString Axe, Axe )
--     , ( colorToString Clover, Clover )
--     , ( colorToString Red, Red )
--     , ( colorToString Blue, Blue )
--     , ( colorToString Yellow, Yellow )
--     , ( colorToString Purple, Purple )
--     , ( colorToString Black, Black )
--     , ( colorToString Orange, Orange )
--     , ( colorToString Brown, Brown )
--     ]
---- MODEL ----


type alias Model =
    { row : Int
    , column : Int
    , selectedColor : Color
    , fillCount : Int
    , moveCount : Int
    , completed : Bool
    , teleportEnabled : Bool
    , grid : List (List String)
    }


init : ( Model, Cmd Msg )
init =
    ( { row = 0
      , column = 0
      , selectedColor = Dog
      , moveCount = 0
      , fillCount = 0
      , completed = False
      , teleportEnabled = False
      , grid = generateGrid 10
      }
    , Cmd.none
    )


generateGrid : Int -> List (List String)
generateGrid dimension =
    case dimension of
        0 ->
            []

        _ ->
            generateList (length colors) :: generateGrid (dimension - 1)


generateList : Int -> List String
generateList size =
    case size of
        0 ->
            []

        _ ->
            getEmoji ( size - 1, colors ) :: generateList (size - 1)


getEmoji : ( Int, List String ) -> String
getEmoji ( index, options ) =
    case get index (fromList options) of
        Just emoji ->
            emoji

        Nothing ->
            "_"



---- UPDATE ----


type Msg
    = ChangeColor Color
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeColor color ->
            ( { model | selectedColor = color }, Cmd.none )



---- VIEW ----




view : Model -> Html Msg
view model =
    div [ id "grid" ]
        [ h1 [] [ text "Elm Color Fill Game!" ]
        , p [] [ text ("Row: " ++ String.fromInt model.row) ]
        , p [] [ text ("Column: " ++ String.fromInt model.column) ]
        , renderGrid model.grid
        ]


renderGrid : List (List String) -> Html msg
renderGrid grd =
    grd
        |> List.map (\l -> ul [ class "row" ] [ renderList l ])
        |> ul []


renderList : List String -> Html msg
renderList lst =
    lst
        |> List.map (\l -> li [] [ text l ])
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
