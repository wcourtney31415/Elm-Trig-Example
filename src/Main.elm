module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Input as Input


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type Msg
    = TextboxChanged String


type alias Model =
    { myNumber : Int
    }


getLastChar str =
    str.reverse.slice 0 1


addZeroAfterDecimal str =
    if getLastChar str == "." then
        str ++ "0"

    else
        str


init =
    { myNumber = 0 }


update msg model =
    case msg of
        TextboxChanged str ->
            case String.toFloat str of
                Nothing ->
                    model

                Just val ->
                    { model | myNumber = val }


view model =
    Element.layout []
        (Input.text []
            { text = String.fromFloat model.myNumber
            , label = Input.labelAbove [] (text "Username")
            , placeholder = Just (Input.placeholder [] (text "Side A"))
            , onChange = \new -> TextboxChanged new
            }
        )
