module Main exposing (main)

import Board exposing (Board, newBoard)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Model =
    { board : Result Board.MkBoardError Board }


initialModel : Model
initialModel =
    { board = newBoard 7 6 }


type Msg
    = NoMsg


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoMsg ->
            model


view : Model -> Html Msg
view model =
    div [ class "btn-group" ]
        []


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
