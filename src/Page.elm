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
            Html.node "div" (cellClasslist::attributes) (editButton::children)

renderBlock : Session.Model -> Int -> Block.Model -> Html Msg
renderBlock session n block =
    case block.data of
        Block.TextOnImage { text, image } ->
            cell session True n block (renderTextOnImage text image)
        Block.HeaderAndText { header, text, inverted } ->
            cell session True n block (renderHeaderAndText header text inverted)
        Block.Container direction children ->
            cell session False n block (renderContainer session direction children)
        Block.Image image ->
            cell session True n block (renderImage image)
        Block.HeaderTextLink { header, text, link } ->
            cell session True n block (renderHeaderTextLink header text link)
        Block.PortraitWithQuote data ->
            cell session True n block (renderPortraitWithQuote data)

        
view : Session.Model -> Model -> Html Msg
view session page = div
                  [ class "grid" ]
                  [ page.content |> renderBlock session 1 ]

         
