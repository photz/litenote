import Html exposing (Html, div, span)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Keyboard
import Navigation
import WebSocket

import Block
import Editor.Msg as Editor exposing (Msg)
import Editor.View as Editor exposing (view)
import Header
import Login.Model as Login exposing (..)
import Login.Msg as Login exposing (..)
import Login.Update as Login exposing (..)
import Login.View as Login exposing (view)
import Page
import Route exposing (Route)
import Routing
import Server
import Session.Model as Session exposing (..)


subscriptions : Model -> Sub Msg
subscriptions = always (Sub.batch [ WebSocket.listen server WsMsg
                                  , Keyboard.downs KeyDown
                                  , Keyboard.ups KeyUp
                                  , Test.blockId MouseOverBlock
                                  ])

init : Navigation.Location -> ( Model, Cmd Msg )
init location = ( { routes = []
                  , editing = Nothing
                  , currentPage = Nothing
                  , login = Nothing
                  , controlPressed = False
                  , session = Session.init
                  , editMode = False
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
                   , login : Maybe Login.Model
                   , controlPressed : Bool
                   , session : Session.Model
                   , editMode : Bool
                   }

-- UPDATE

type Msg = OnLocationChange Navigation.Location
         | HeaderMsg Header.Msg
         | PageMsg Page.Msg
         | WsMsg String
         | EditorMsg Editor.Msg
         | LoginMsg Login.Msg
         | KeyUp Int
         | KeyDown Int
         | ToggleMode

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
    let blockAttr = 
        case block.data of
            Block.HeaderAndText { header, text, inverted } ->
                Just [ ("header", Encode.string header)
                     , ("text", Encode.string text)
                     , ("inverted", Encode.bool inverted)
                     ]
            Block.HeaderTextLink { header, text, link } ->
                Just [ ("header", Encode.string header)
                     , ("text", Encode.string text)
                     , ("link", Encode.string link)
                     ]
            Block.TextOnImage { text, image } ->
                Just [ ("text", Encode.string text)
                     , ("image", Encode.string image)
                     ]
            Block.PortraitWithQuote { quote, author, portrait } ->
                Just [ ( "quote", Encode.string quote )
                     , ( "author", Encode.string author )
                     , ( "portrait", Encode.string portrait )
                     ]
            Block.Container _ _ ->
                Nothing
            Block.Image _ ->
                Nothing
    in

    case blockAttr of

        Just a ->
            let allAttr = List.append attributes a in

            WebSocket.send server
                (Encode.object allAttr |> Encode.encode 0)

        Nothing ->
            Cmd.none

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

updateBlockData : Block.Data -> String -> String -> Block.Data
updateBlockData data name newValue =
    case data of
        Block.TextOnImage values ->
            Block.TextOnImage
                (case name of
                     "text" -> { values | text = newValue }
                     _ -> values)

        Block.Image path ->
            Block.Image path

        Block.HeaderTextLink values ->
            Block.HeaderTextLink
                (case name of
                     "header" -> { values | header = newValue }
                     "text" -> { values | text = newValue }
                     _ -> values)

        Block.HeaderAndText values ->
            Block.HeaderAndText
                (case name of
                     "header" ->
                         { values | header = newValue }
                     "text" ->
                         { values | text = newValue }
                     _ ->
                         values)

        Block.PortraitWithQuote values ->
            Block.PortraitWithQuote
                (case name of
                     "quote" ->
                         { values | quote = newValue }
                     "author" ->
                         { values | author = newValue }
                     _ ->
                         values)

        x -> x
            


updateBlock : Block.Model -> String -> String -> Block.Model
updateBlock block name newValue =
    let _ = Debug.log "update block" (block, name, newValue) in
    let newBlock = { block | data = updateBlockData block.data name newValue }
    in
        newBlock

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

        ToggleMode ->
            ( { model | editMode = not model.editMode }
            , Cmd.none
            )

        LoginMsg (Login.Submit email password) ->
            let ( session, login )
                = case ( email, password ) of
                      ( "test", "" ) ->
                          ( Session.LoggedIn, Nothing )
                      _ ->
                          ( Session.Guest
                          , Maybe.map Login.showError model.login
                          )
            in
                ( { model | session = session, login = login }
                , Cmd.none
                )

        LoginMsg Login.Hide ->
            ( { model | login = Nothing }, Cmd.none )

        LoginMsg msg ->
            (case model.login of 
                 Nothing -> ( model, Cmd.none )
                 Just loginModel ->
                     let ( u, c ) = Login.update msg loginModel in
                     ( { model | login = Just u }, Cmd.none )
            )

        PageMsg (Page.SelectBlock block) ->
            ( { model | editing = Just block }, Cmd.none )

        KeyDown keyCode ->
            ( updateKeyDown model keyCode, Cmd.none )

        KeyUp keyCode ->
            ( updateKeyUp model keyCode, Cmd.none )

        x ->
            let _ = Debug.log "unknown message" x in
            ( model, Cmd.none )

updateKeyUp : Model -> Int -> Model
updateKeyUp m k =
    case k of
        17 -> { m | controlPressed = False }
        _ -> m

updateKeyDown : Model -> Int -> Model
updateKeyDown m k =
    case k of
        17 ->
            { m | controlPressed = True }
        27 ->
            { m | login = Nothing
            , editing = Nothing
            }
        77 ->
            if m.controlPressed && (Session.isGuest m.session)
            then { m | login = Just Login.init }
            else m
        _ ->
            m
                         

-- VIEW

renderCornerMenu : Model -> Html Msg
renderCornerMenu m =
    div [ class "corner-menu" ]
        [ div [ classList [ ( "corner-menu__button", True )
                          , ( "corner-menu__button--active", m.editMode )
                          ]
              , onClick ToggleMode
              ]
              []
        ]


appendMaybe : (b -> a ) -> Maybe b -> List a -> List a
appendMaybe f x xs = case x of
                 Just v -> (f v)::xs
                 Nothing -> xs

appendIf : Bool -> a -> List a -> List a
appendIf p x xs = if p
             then x::xs
             else xs

view : Model -> Html Msg
view model =
    case model.currentPage of
        Nothing ->
            div [] [ Html.text "Please wait" ]
        Just page ->
            div []
            ([ div [ classList [ ( "page", True )
                               , ( "page--edit-mode", model.editMode )
                               ]
                   ]
                   [ Header.view page.id model.routes |> Html.map HeaderMsg
                   , Page.view model.session page |> Html.map PageMsg
                   ]
             ]

            |> appendMaybe
                 (\block -> Editor.view block |> Html.map EditorMsg)
                 model.editing

            |> appendMaybe
                 (\b -> Html.map LoginMsg (Login.view b))
                 model.login

            |> appendIf
                 (Session.mayEditBlocks model.session)
                 (renderCornerMenu model))
            
