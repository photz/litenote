module BlockPicker.Update exposing (update)

import BlockPicker.Msg exposing (..)
import BlockPicker.Model exposing (..)
import WebSocket

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectBlock id ->
            ( { model | selected = Just id }
            , Cmd.none
            )

        _ -> ( model, Cmd.none )
