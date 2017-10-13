module Server exposing (..)


import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Route exposing (Route)
import Page
import Block

type ServerMsg = Routes (List Route)
               | GetPage Page.Model

routeDecoder : Decode.Decoder Route
routeDecoder = decode Route
             |> required "name" Decode.string
             |> required "path" Decode.string
             |> required "page-id" Decode.int

headerAndTextDecoder : Decode.Decoder Block.HeaderAndTextData
headerAndTextDecoder = decode Block.HeaderAndTextData
                     |> required "header" Decode.string
                     |> required "text" Decode.string
                     |> required "inverted" Decode.bool

textOnImageDecoder : Decode.Decoder Block.TextOnImageData
textOnImageDecoder = decode Block.TextOnImageData
                   |> required "text" Decode.string
                   |> required "image" Decode.string

directionDecoder : Decode.Decoder Block.Direction
directionDecoder = Decode.map (\s -> case s of
                                         "row" -> Block.Row
                                         _ -> Block.Column)
                   Decode.string

childrenDecoder : Decode.Decoder (List Block.Model)
childrenDecoder = Decode.list (Decode.lazy (\_ -> blockDecoder))

containerDecoder : Decode.Decoder Block.Model
containerDecoder = decode Block.Container
                 |> required "direction" directionDecoder
                 |> required "children" (Decode.lazy (\_ -> childrenDecoder))

imageDecoder : Decode.Decoder Block.Model
imageDecoder = decode Block.Image
             |> required "image" Decode.string

headerTextLinkDecoder : Decode.Decoder Block.HeaderTextLinkData
headerTextLinkDecoder = decode Block.HeaderTextLinkData
                       |> required "header" Decode.string
                       |> required "text" Decode.string
                       |> required "link" Decode.string

blockMultiplexer : String -> Decode.Decoder Block.Model
blockMultiplexer blockType =
    case blockType of
        "header-and-text" ->
            Decode.map Block.HeaderAndText headerAndTextDecoder
        "text-on-image" ->
            Decode.map Block.TextOnImage textOnImageDecoder
        "container" ->
            containerDecoder
        "image" ->
            imageDecoder
        "header-text-link" ->
            Decode.map Block.HeaderTextLink headerTextLinkDecoder
        _ -> Decode.fail "fail"

blockDecoder : Decode.Decoder Block.Model
blockDecoder = Decode.andThen
               blockMultiplexer
               (Decode.field "block-type" Decode.string)

pageDecoder : Decode.Decoder Page.Model
pageDecoder = decode Page.Model
              |> required "name" Decode.string
              |> required "id" Decode.int
              |> required "content" blockDecoder

multiplexer : String -> Decode.Decoder ServerMsg
multiplexer msgType =
    case msgType of
        "routes" ->
            Decode.field "routes" (Decode.map Routes (Decode.list routeDecoder))
        "page" ->
            Decode.field "page" (Decode.map GetPage pageDecoder)
        _ ->
            Decode.fail ("unknown message type " ++ msgType)

messageDecoder : Decode.Decoder ServerMsg
messageDecoder = Decode.andThen
                 multiplexer
                 (Decode.field "msg-type" Decode.string)
                     


decodeMsg : String -> Result String ServerMsg
decodeMsg s = Decode.decodeString messageDecoder s
