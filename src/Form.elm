module Main exposing (..)

import Browser
import Char exposing (isDigit, isLower, isUpper)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import String


-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    , submit : Bool
    }


init : Model
init =
    Model "" "" "" "" False



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        Age age ->
            { model | age = age }

        Submit ->
            { model | submit = True }

        Reset ->
            { model | name = "", password = "", passwordAgain = "", age = "", submit = False }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "age" "Age" model.age Age
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , if model.submit then
            viewValidation model
          else
            div [] []
        , button [ onClick Submit ] [ text "Submit" ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    div []
        [ if String.isEmpty model.name then
            div [ style "color" "red" ] [ text "Name must be entered!!!" ]
          else
            div [] []
        , if String.isEmpty model.age then
            div [ style "color" "red" ] [ text "Age must be entered!!!" ]
          else
            div [] []
        , if String.filter isDigit model.age /= model.age then
            div [ style "color" "red" ] [ text "Age must be numbers!!!" ]
          else
            div [] []
        , if String.length model.password < 8 then
            div [ style "color" "red" ] [ text "Password must be longer than 8 characters!!!" ]
          else
            div [] []
        , if
            not
                (String.any isUpper model.password
                    && String.any isLower model.password
                    && String.any isDigit model.password
                )
          then
            div [ style "color" "red" ] [ text "Password must contain upper case, lower case, and numeric characters!!!" ]
          else
            div [] []
        , if not (String.isEmpty model.password) && model.password == model.passwordAgain then
            div [ style "color" "green" ] [ text "OK" ]
          else
            div [ style "color" "red" ] [ text "Passwords do not match!" ]
        ]
