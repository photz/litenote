module Login.View exposing (view)

import Html exposing (Html, div, button, input)
import Html.Attributes exposing (class, classList, type_)
import Html.Events exposing (onClick, onInput)

import Login.Model exposing (..)
import Login.Msg exposing (..)
import Session.Model as Session

view : Model -> Html Msg
view model =
    div [ class "login" ]
        [ div [ class "login__layer"
              , onClick Hide
              ]
              []
        , div [ class "login__box" ]
            [ div [ class "login__header" ]
                  [ div [ class "login__headline" ]
                        [ Html.text "Login" ]
                  , div [ class "login__close"
                        , onClick Hide
                        ] []
                  ]
            , div [ class "login__body" ]
                  [ div [ class "login__field" ]
                        [ div [ class "login__label" ]
                              [ Html.text "Email" ]
                        , input [ class "login__input"
                                , type_ "text"
                                , onInput ChangeEmail
                                ]
                            []
                        ]
                  , div [ class "login__field" ]
                        [ div [ class "login__label" ]
                              [ Html.text "Password" ]
                        , input [ class "login__input"
                                , type_ "password"
                                , onInput ChangePassword
                                ]
                              []
                        ]
                  , div [ class "login__button-wrapper" ]
                        [ button
                          [ class "login__button"
                          , onClick (Submit model.email model.password)
                          ]
                              [ Html.text "Login" ]
                        ]
                  ]

            ]
        ]

       
                 
