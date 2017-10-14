import Html exposing (Html, div, span)
import Html.Attributes exposing (class, classList, href, style)
import Navigation
import Header
import Page
import Block
import Route exposing (Route)
import Server
import WebSocket
import Routing
import Editor.View as Editor exposing (view)
import Editor.Msg as Editor exposing (Msg)
import Json.Encode as Encode

subscriptions : Model -> Sub Msg
subscriptions = always (Sub.batch [ WebSocket.listen server WsMsg ])

server : String
server = "ws://127.0.0.1:3000"

getRoutes : Cmd Msg
getRoutes = WebSocket.send server "{\"msg-type\":\"get-routes\"}"

getStartPage : Cmd Msg
getStartPage = WebSocket.send server "{\"msg-type\":\"get-page\",\"page-id\":1}"

getPageById : Int -> Cmd Msg
getPageById pageId = WebSocket.send server
                     ("{\"msg-type\":\"get-page\",\"page-id\":"
                          ++ (toString pageId) ++ "}")

saveBlock : Block.Model -> Cmd Msg
saveBlock block =
    let attributes = [ ("msg-type", Encode.string "save-block")
                     , ("block-id", Encode.int block.id)
                     ]
    in

        case block.data of
            Block.HeaderAndText { header, text, inverted } ->
                let attr = List.append attributes
                           [ ("header", Encode.string header)
                           , ("text", Encode.string text)
                           , ("inverted", Encode.bool inverted)
                           ]
                in
                    WebSocket.send server
                        (Encode.object attr |> Encode.encode 0)
            _ -> Cmd.none

init : Navigation.Location -> ( Model, Cmd Msg )
init location = ( { routes = []
                  , editing = Nothing
                  , currentPage = Nothing
                  }, Cmd.batch [ getRoutes, getStartPage ] )

main = Navigation.program OnLocationChange
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }


-- MODEL


type alias Model = { currentPage : Maybe Page.Model
                   , routes : List Route
                   , editing : Maybe Block.Model
                   }

-- UPDATE

type Msg = OnLocationChange Navigation.Location
         | HeaderMsg Header.Msg
         | PageMsg Page.Msg
         | WsMsg String
         | EditorMsg Editor.Msg

updateWs : String -> Model -> ( Model, Cmd Msg )
updateWs content model =
    case Server.decodeMsg content of 
        Err e ->
            let _ = Debug.log "Error while trying to decode" e in
            ( model, Cmd.none )
        Ok msg ->
            case msg of 
                Server.Routes routes ->
                    ( { model | routes = routes }, Cmd.none )
                Server.GetPage page ->
                    ( { model | currentPage = Just page }, Cmd.none )

updateBlock : Block.Model -> String -> String -> Block.Model
updateBlock block name newValue =
    let _ = Debug.log "update block" (block, name, newValue) in
    case block.data of
        Block.HeaderAndText { text, header, inverted } ->
            let data = { text = text
                       , header = header
                       , inverted = inverted
                       }
            in
                (case name of
                     "header" ->
                         { block | data = Block.HeaderAndText { data | header = newValue } }
                     "text" ->
                         { block | data = Block.HeaderAndText { data | text = newValue } }
                     _ -> block)
        _ -> block

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange newLocation ->
            case Routing.parse model.routes newLocation of
                Nothing ->
                    ( { model | currentPage = Nothing }, Cmd.none )
                Just pageId ->
                    ( model, getPageById pageId )
        WsMsg content ->
            updateWs content model
        EditorMsg Editor.Close ->
            ( { model | editing = Nothing }, Cmd.none )
        EditorMsg (Editor.ChangeField { name, newValue })  ->
            (case model.editing of 
                 Nothing -> ( model, Cmd.none )
                 Just block ->
                     let updatedBlock = updateBlock block name newValue in
                     ( { model | editing = Just updatedBlock }, Cmd.none ))
        EditorMsg Editor.SaveBlock ->
            (case model.editing of
                 Nothing -> ( model, Cmd.none )
                 Just block ->
                     ( model, saveBlock block ))
        PageMsg (Page.SelectBlock block) ->
            ( { model | editing = Just block }, Cmd.none )
        x ->
            let _ = Debug.log "unknown message" x in
            ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    case model.currentPage of
        Nothing ->
            div [] [ Html.text "Please wait" ]
        Just page ->
            div []
            [ Header.view page.id model.routes |> Html.map HeaderMsg
            , Page.view page |> Html.map PageMsg
            , case model.editing of
                  Nothing ->
                      div [] []
                  Just someBlock ->
                      Editor.view someBlock |> Html.map EditorMsg
            ]
            
