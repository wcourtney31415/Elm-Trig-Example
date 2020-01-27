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
    = SideAChanged String
    | SideBChanged String
    | AngleAChanged String
    | SideCChanged String
    | AngleBChanged String
    | AngleCChanged String


type alias Model =
    { sideA : ( Float, String )
    , sideB : ( Float, String )
    , sideC : ( Float, String )
    , angleA : ( Float, String )
    , angleB : ( Float, String )
    , angleC : ( Float, String )
    }


getLastChar : String -> String
getLastChar str =
    String.slice 0 1 (String.reverse str)


removeLastChar : String -> String
removeLastChar str =
    String.slice 0 (String.length str - 1) str


init =
    { sideA = ( 0, "" )
    , sideB = ( 0, "" )
    , sideC = ( 0, "" )
    , angleA = ( 0, "" )
    , angleB = ( 0, "" )
    , angleC = ( 0, "" )
    }


update msg model =
    case msg of
        SideAChanged str ->
            processNumTextboxChange model str

        SideBChanged str ->
            processNumTextboxChange model str

        SideCChanged str ->
            processNumTextboxChange model str

        AngleAChanged str ->
            processNumTextboxChange model str

        AngleBChanged str ->
            processNumTextboxChange model str

        AngleCChanged str ->
            processNumTextboxChange model str


processNumTextboxChange model str =
    let
        ifNumber : String -> Model -> Model
        ifNumber aNumber thisModel =
            case String.toFloat aNumber of
                Nothing ->
                    thisModel

                Just val ->
                    { thisModel | sideA = ( val, str ) }
    in
    if str == "" then
        { model | sideA = ( 0, "" ) }

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
    Element.column
        [ width fill
        , spacing 20
        ]
        [ numTextBox model "Side A" SideAChanged
        , numTextBox model "Side B" SideBChanged
        , numTextBox model "Side C" SideCChanged
        ]


angleColumn model =
    Element.column
        [ width fill
        , spacing 20
        ]
        [ numTextBox model "Angle A" AngleAChanged
        , numTextBox model "Angle B" AngleBChanged
        , numTextBox model "Angle C" AngleCChanged
        ]


numTextBox model label msg =
    Input.text []
        { text = Tuple.second model.sideA
        , label = Input.labelAbove [] (text label)
        , placeholder = Just (Input.placeholder [] (text "0.00"))
        , onChange = \new -> msg new
        }
