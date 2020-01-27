module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias NumInputContent =
    ( Float, String )


type Msg
    = SideAChanged String
    | SideBChanged String
    | AngleAChanged String
    | SideCChanged String
    | AngleBChanged String
    | AngleCChanged String


type alias Model =
    { sideA : NumInputContent
    , sideB : NumInputContent
    , sideC : NumInputContent
    , angleA : NumInputContent
    , angleB : NumInputContent
    , angleC : NumInputContent
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
    , angleC = ( 90, "90" )
    }


createNumInputContent : String -> Maybe NumInputContent
createNumInputContent str =
    if str == "" then
        Just ( 0.0, "" )

    else if getLastChar str == "." then
        case String.toFloat (removeLastChar str) of
            Nothing ->
                Nothing

            Just val ->
                Just ( val, str )

    else
        case String.toFloat str of
            Nothing ->
                Nothing

            Just val ->
                Just ( val, str )


update msg model =
    case msg of
        SideAChanged str ->
            case createNumInputContent str of
                Nothing ->
                    model

                Just inputContent ->
                    { model | sideA = inputContent }

        SideBChanged str ->
            case createNumInputContent str of
                Nothing ->
                    model

                Just inputContent ->
                    { model | sideB = inputContent }

        SideCChanged str ->
            case createNumInputContent str of
                Nothing ->
                    model

                Just inputContent ->
                    { model | sideC = inputContent }

        AngleAChanged str ->
            case createNumInputContent str of
                Nothing ->
                    model

                Just inputContent ->
                    { model | angleA = inputContent }

        AngleBChanged str ->
            case createNumInputContent str of
                Nothing ->
                    model

                Just inputContent ->
                    { model | angleB = inputContent }

        AngleCChanged str ->
            model


processpositivefloatTextBoxChange model str =
    if str == "" then
        { model | sideA = ( 0, "" ) }

    else if getLastChar str == "." then
        case String.toFloat (removeLastChar str) of
            Nothing ->
                model

            Just val ->
                { model | sideA = ( val, str ) }

    else
        case String.toFloat str of
            Nothing ->
                model

            Just val ->
                { model | sideA = ( val, str ) }


view model =
    Element.layout [ centerX, width fill, padding 40 ]
        (Element.column [ centerX, width fill ]
            [ title
            , triangle
            , inputRow model
            ]
        )


inputRow model =
    Element.row [ width fill, spacing 20, centerX ]
        [ sideColumn model
        , angleColumn model
        ]


sideColumn model =
    Element.column
        [ width
            (fill
                |> maximum 300
            )
        , spacing 20
        , centerX
        ]
        [ positivefloatTextBox model "Side A" (Tuple.second model.sideA) SideAChanged
        , positivefloatTextBox model "Side B" (Tuple.second model.sideB) SideBChanged
        , positivefloatTextBox model "Side C" (Tuple.second model.sideC) SideCChanged
        ]


angleColumn model =
    Element.column
        [ width
            (fill
                |> maximum 300
            )
        , spacing 20
        , centerX
        ]
        [ positivefloatTextBox model "Angle A" (Tuple.second model.angleA) AngleAChanged
        , positivefloatTextBox model "Angle B" (Tuple.second model.angleB) AngleBChanged
        , positivefloatTextBox model "Angle C" (Tuple.second model.angleC) AngleCChanged
        ]


positivefloatTextBox model label numInputContent msg =
    Input.text []
        { text = numInputContent
        , label = Input.labelAbove [] (text label)
        , placeholder = Just (Input.placeholder [] (text "0.00"))
        , onChange = \new -> msg new
        }


title =
    Element.el [ centerX, Font.size 30, padding 5 ] (text "Right Triangle: Simple Trig")


triangle =
    Element.image
        [ centerX ]
        { src = "/src/Images/Triangle.png"
        , description = "Right Triangle"
        }
