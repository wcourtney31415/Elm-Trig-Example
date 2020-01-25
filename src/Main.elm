module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
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
            processNumTextboxChange model str


processNumTextboxChange model str =
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
    Element.layout [ centerX, width fill, padding 40 ]
        (Element.column [ centerX, width fill ]
            [ inputRow model
            ]
        )


inputRow model =
    Element.row [ width fill, spacing 20, centerX ]
        [ sideColumn model
        , angleColumn model
        ]


sideColumn model =
    Element.column [ width fill ]
        [ numTextBox model "Side A" NumTxtBoxChanged
        , numTextBox model "Side B" NumTxtBoxChanged
        , numTextBox model "Side C" NumTxtBoxChanged
        ]


angleColumn model =
    Element.column [ width fill ]
        [ numTextBox model "Angle A" NumTxtBoxChanged
        , numTextBox model "Angle B" NumTxtBoxChanged
        , numTextBox model "Angle C" NumTxtBoxChanged
        ]


numTextBox model label msg =
    Input.text []
        { text = Tuple.second model.numTxtBoxValue
        , label = Input.labelAbove [] (text label)
        , placeholder = Just (Input.placeholder [] (text label))
        , onChange = \new -> msg new
        }
