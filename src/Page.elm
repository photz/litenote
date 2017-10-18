module Page exposing (..)

import Html exposing (Html, div, span, a)
import Html.Attributes exposing (class, classList, href, style, attribute)
import Html.Events exposing (onClick)
import Block
import Session.Model as Session exposing (..)

type alias Model = { name : String
                   , id : Int
                   , content : Block.Model
                   }

type Msg = SelectBlock Block.Model

getPath : Model -> String
getPath page = "#" ++ "unknown"


renderHeaderAndText : Block.HeaderAndTextData -> ( List (Html.Attribute Msg), List (Html Msg) )
renderHeaderAndText data =
    ( [ classList [ ("b-header-and-text", True)
                  , ("b-header-and-text--inverted", data.inverted)
                  ]
      ]

    , [ div [ class "b-header-and-text__inner" ]
            [ div [ class "b-header-and-text__header" ]
                  [ Html.text data.header ]
            , div [ class "b-header-and-text__text" ]
                [ Html.text data.text ]
            ]
      ]
    )


renderTextOnImage data =
    ( [ class "b-text-on-image"
      , style [ ("background-image", "url(" ++ data.image ++ ")") ]
      ]
    , [ span [ class "b-text-on-image__text" ]
            [ Html.text data.text ]
      ]
    )


renderContainer : Session.Model -> Block.Direction -> List (Block.Model) -> ( List (Html.Attribute Msg), List (Html Msg) )
renderContainer session direction children =
    let modifier = case direction of
                       Block.Row -> "grid__cell--row"
                       Block.Column -> "grid__cell--column"
    in
        ( [ classList [ ("grid__cell", True)
                      , (modifier, True)
                      ]
          ]
        , children |> List.map (renderBlock session (List.length children))
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

renderHeaderTextLink : Block.HeaderTextLinkData -> ( List (Html.Attribute Msg), List (Html Msg) )
renderHeaderTextLink data =
    ( [ classList [ ("b-header-text-link", True) ] ]
    , [ div [ class "b-header-text-link__header" ]
            [ Html.text data.header ]
      , div [ class "b-header-text-link__text" ]
            [ Html.text data.text ]
      , a [ classList [ ("b-header-text-link__link", True)
                      , ("button", True)
                      ]
          , href ("#" ++ data.link)
          ]
            [ Html.text "Weiterlesen" ]
      ]
    )

renderPortraitWithQuote : Block.PortraitWithQuoteData -> ( List (Html.Attribute Msg), List (Html Msg) )
renderPortraitWithQuote data =
    let bem el = "b-portrait-with-quote__" ++ el in
    let bemEl name = class (bem name) in
    let bgImg = "url(" ++ data.portrait ++ ")" in
    let bemElMod el mod =
            classList [ ( bem el, True )
                      , ( bem el ++ "--" ++ mod, True )
                      ]
    in
        ( [ classList [ ( "b-portrait-with-quote", True ) ] ]
        , [ div [ bemEl "inner" ]
                [ div [ bemEl "portrait"
                      , style [ ( "background-image", bgImg ) ]
                      ]
                      [ div [ bemEl "aspect-ratio-filler" ]
                            []
                      ]
                , div [ bemEl "quote-and-author" ]
                      [ div [ bemEl "quote" ]
                            [ div [ bemElMod "quotation-mark" "open" ]
                              []
                            , Html.text data.quote
                            , div [ bemElMod "quotation-mark" "close" ]
                                []
                            ]
                      , div [ bemEl "author" ]
                            [ span [ bemEl "bar" ]
                              [ Html.text "───" ]
                            , Html.text data.author
                            ]
                      ]
                ]
          ]

    )
        
        
cell : Session.Model -> Bool -> Int -> Block.Model -> ( List (Html.Attribute Msg), List (Html Msg)) -> Html Msg
cell session editable childrenOfParent block ( attributes, children ) =
    let cellClasslist =
            classList
            [ ("grid__cell", True)
            , ("grid__cell--1-" ++ (toString childrenOfParent), True)
            , ("block", True)
            , ("block--editable", Session.mayEditBlocks session && editable)
            ]
    in
        let editButton =
                div [ classList [ ("block__edit-button", True) ]
                    , onClick (SelectBlock block)
                    ]
                []
        in
            Html.node "div" (dataAttr::cellClasslist::attributes) (editButton::children)

renderBlock : Session.Model -> Int -> Block.Model -> Html Msg
renderBlock session n block =
    case block.data of
        Block.TextOnImage data ->
            cell session True n block (renderTextOnImage data)
        Block.HeaderAndText data ->
            cell session True n block (renderHeaderAndText data)
        Block.Container direction children ->
            cell session False n block (renderContainer session direction children)
        Block.Image image ->
            cell session True n block (renderImage image)
        Block.HeaderTextLink data ->
            cell session True n block (renderHeaderTextLink data)
        Block.PortraitWithQuote data ->
            cell session True n block (renderPortraitWithQuote data)

        
view : Session.Model -> Model -> Html Msg
view session page = div
                  [ class "grid" ]
                  [ page.content |> renderBlock session 1 ]

         
