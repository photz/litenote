module Login.Update exposing (..)

import Login.Model exposing (..)
import Login.Msg exposing (..)
import Html exposing (Html, div, button, input)
import Html.Attributes exposing (class, classList, type_)
import Html.Events exposing (onClick, onInput)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg m = 
    case msg of
        ChangeEmail newEmail ->
            ( { m | email = newEmail }, Cmd.none )
        ChangePassword newPassword ->
            ( { m | password = newPassword }, Cmd.none )
        _ ->
            ( m, Cmd.none )
