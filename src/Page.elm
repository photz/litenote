module Page exposing (Model, getPath, view, Msg)

import Html exposing (Html, div, span, a)
import Html.Attributes exposing (class, classList, href, style)
import Block

type alias Model = { name : String
                   , id : Int
                   , content : Block.Model
                   }

type Msg = Booom

getPath : Model -> String
getPath page = "#" ++ "unknown"


renderHeaderAndText : String -> String -> Bool -> Html Msg
renderHeaderAndText header text inverted =
    div [ classList [ ("b-header-and-text", True)
                    , ("b-header-and-text--inverted", inverted)
                    ]
        ]
        [ div [ class "b-header-and-text__inner" ]
              [ div [ class "b-header-and-text__header" ]
                    [ Html.text header ]
              , div [ class "b-header-and-text__text" ]
                    [ Html.text text ]
              ]
        ]

renderTextOnImage text image =
    div [ class "b-text-on-image"
        , style [ ("background-image", "url(" ++ image ++ ")") ]
        ]
        [ span [ class "b-text-on-image__text" ]
              [ Html.text text ]
        ]


renderContainer : Block.Direction -> List (Block.Model) -> Html Msg
renderContainer direction children =
    let modifier = case direction of
                       Block.Row -> "grid__cell--row"
                       Block.Column -> "grid__cell--column"
    in
        div [ classList [ ("grid__cell", True)
                        , (modifier, True)
                        ]
            ]
            (children |> List.map renderBlock)
            
renderImage image =
    let bgImage = "url(" ++ image ++ ")"
    in 
        div
        [ class "b-image"
        , style [ ("background-image", bgImage) ]
        ]
        []

renderHeaderTextLink : String -> String -> String -> Html Msg
renderHeaderTextLink header text link =
    div [ class "b-header-text-link" ]
        [ div [ class "b-header-text-link__header" ]
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

renderBlock : Block.Model -> Html Msg
renderBlock block =
    case block of
        Block.TextOnImage { text, image } ->
            renderTextOnImage text image
        Block.HeaderAndText { header, text, inverted } ->
            renderHeaderAndText header text inverted
        Block.Container direction children ->
            renderContainer direction children
        Block.Image image ->
            renderImage image
        Block.HeaderTextLink { header, text, link } ->
            renderHeaderTextLink header text link
        
renderPage : Model -> Html Msg
renderPage page = div
                  [ class "grid" ]
                  [ page.content |> renderBlock ]

view : Model -> Html Msg
view m = renderPage m
