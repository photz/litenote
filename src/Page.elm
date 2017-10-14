module Page exposing (..)

import Html exposing (Html, div, span, a)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Block

type alias Model = { name : String
                   , id : Int
                   , content : Block.Model
                   }

type Msg = SelectBlock Block.Model

getPath : Model -> String
getPath page = "#" ++ "unknown"


renderHeaderAndText : String -> String -> Bool -> ( List (Html.Attribute Msg), List (Html Msg) )
renderHeaderAndText header text inverted =
    ( [ classList [ ("b-header-and-text", True)
                  , ("b-header-and-text--inverted", inverted)
                  ]
      ]

    , [ div [ class "b-header-and-text__inner" ]
            [ div [ class "b-header-and-text__header" ]
                  [ Html.text header ]
            , div [ class "b-header-and-text__text" ]
                [ Html.text text ]
            ]
      ]
    )


renderTextOnImage text image =
    ( [ class "b-text-on-image"
      , style [ ("background-image", "url(" ++ image ++ ")") ]
      ]
    , [ span [ class "b-text-on-image__text" ]
            [ Html.text text ]
      ]
    )


renderContainer : Block.Direction -> List (Block.Model) -> ( List (Html.Attribute Msg), List (Html Msg) )
renderContainer direction children =
    let modifier = case direction of
                       Block.Row -> "grid__cell--row"
                       Block.Column -> "grid__cell--column"
    in
        ( [ classList [ ("grid__cell", True)
                      , (modifier, True)
                      ]
          ]
        , children |> List.map (renderBlock (List.length children))
        ) 
            
renderImage : String -> ( List (Html.Attribute Msg), List (Html Msg) )
renderImage image =
    let bgImage = "url(" ++ image ++ ")"
    in 
        ( [ classList [ ("b-image", True) ]
          , style [ ("background-image", bgImage) ]
          ]
        , []
        )

renderHeaderTextLink : String -> String -> String -> ( List (Html.Attribute Msg), List (Html Msg) )
renderHeaderTextLink header text link =
    ( [ classList [ ("b-header-text-link", True) ] ]
    , [ div [ class "b-header-text-link__header" ]
            [ Html.text header ]
      , div [ class "b-header-text-link__text" ]
            [ Html.text text ]
      , a [ classList [ ("b-header-text-link__link", True)
                      , ("button", True)
                      ]
          , href ("#" ++ link)
          ]
            [ Html.text "Weiterlesen" ]
      ]
    )

cell : Bool -> Int -> Block.Model -> ( List (Html.Attribute Msg), List (Html Msg)) -> Html Msg
cell editable childrenOfParent block ( attributes, children ) =
    let cellClasslist =
            classList
            [ ("grid__cell", True)
            , ("grid__cell--1-" ++ (toString childrenOfParent), True)
            , ("block", True)
            , ("block--editable", editable)
            ]
    in
        let editButton =
                div [ classList [ ("block__edit-button", True) ]
                    , onClick (SelectBlock block)
                    ]
                []
        in
            Html.node "div" (cellClasslist::attributes) (editButton::children)

renderBlock : Int -> Block.Model -> Html Msg
renderBlock n block =
    case block.data of
        Block.TextOnImage { text, image } ->
            cell True n block (renderTextOnImage text image)
        Block.HeaderAndText { header, text, inverted } ->
            cell True n block (renderHeaderAndText header text inverted)
        Block.Container direction children ->
            cell False n block (renderContainer direction children)
        Block.Image image ->
            cell True n block (renderImage image)
        Block.HeaderTextLink { header, text, link } ->
            cell True n block (renderHeaderTextLink header text link)

        
renderPage : Model -> Html Msg
renderPage page = div
                  [ class "grid" ]
                  [ page.content |> renderBlock 1 ]

view : Model -> Html Msg
view m = renderPage m

         
