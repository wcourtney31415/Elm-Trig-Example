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
    = NumTxtBoxChanged String


type alias Model =
    { numTxtBoxValue : ( Float, String )
    }


getLastChar : String -> String
getLastChar str =
    String.slice 0 1 (String.reverse str)


removeLastChar : String -> String
removeLastChar str =
    String.slice 0 (String.length str - 1) str


init =
    { numTxtBoxValue = ( 0, "" ) }


update msg model =
    case msg of
        NumTxtBoxChanged str ->
            let
                ifNumber : String -> Model -> Model
                ifNumber aNumber thisModel =
                    case String.toFloat aNumber of
                        Nothing ->
                            thisModel

                        Just val ->
                            { thisModel | numTxtBoxValue = ( val, str ) }
            in
            if str == "" then
                { model | numTxtBoxValue = ( 0, "" ) }

            else if getLastChar str == "." then
                ifNumber (removeLastChar str) model

            else
                ifNumber str model


view model =
    Element.layout []
        (Element.column []
            [ Input.text []
                { text = Tuple.second model.numTxtBoxValue
                , label = Input.labelAbove [] (text "Side A")
                , placeholder = Just (Input.placeholder [] (text "Side A"))
                , onChange = \new -> NumTxtBoxChanged new
                }
            , Element.el [] (text (String.fromFloat (Tuple.first model.numTxtBoxValue + 1)))
            ]
        )
