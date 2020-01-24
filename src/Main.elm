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
    { myNumber : ( Float, String )
    }


getLastChar str =
    String.slice 0 1 (String.reverse str)


removeLastChar str =
    String.slice 0 (String.length str - 1) str



{-




   withoutDot str =
       if getLastChar str == "." then
           str

       else
           str

   }
-}


init =
    { myNumber = ( 0, "" ) }



--Special cases "", ending with period,


update msg model =
    case msg of
        TextboxChanged str ->
            let
                ifNumber : String -> Model -> Model
                ifNumber aNumber thisModel =
                    case String.toFloat aNumber of
                        Nothing ->
                            thisModel

                        Just val ->
                            { thisModel | myNumber = ( val, str ) }
            in
            if str == "" then
                { model | myNumber = ( 0, "" ) }

            else if getLastChar str == "." then
                ifNumber (removeLastChar str) model

            else
                ifNumber str model


view model =
    Element.layout []
        (Input.text []
            { text = Tuple.second model.myNumber
            , label = Input.labelAbove [] (text "Side A")
            , placeholder = Just (Input.placeholder [] (text "Side A"))
            , onChange = \new -> TextboxChanged new
            }
        )
