module BlockPicker.View exposing (view)

import Html exposing (Html, div, button)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onMouseOver, onClick)

import BlockPicker.Model exposing (..)
import BlockPicker.Msg exposing (..)

renderBlock : { id : String, name : String, icon : String, desc : String } -> Html Msg
renderBlock { name, icon, id } =
    div [ class "block-picker__block"
        , onClick (SelectBlock id)
        ]
        [ div [ class "block-picker__icon" 
              , style [ ( "background-image", "url(" ++ icon ++ ")" ) ]
              ]
              [ div [ class "block-picker__aspect-ratio-filler" ]
                    []
              ]
        , div [ class "block-picker__caption" ]
              [ Html.text name ]
        ]


blocks = [ { name = "Header and Text"
           , icon = "header-and-text.svg"
           , desc = "A block for a headline and a paragraph of text. The colors can be inverted."
           , id = "header-and-text"
           }
         , { name = "Text on Image"
           , icon = "text-on-image.svg"
           , desc = ""
           , id = "text-on-image"
           }
         , { name = "Image"
           , icon = "image.svg"
           , desc = "A block for a simple image"
           , id = "image"
           }
         , { name = "Portrait with Quote"
           , icon = "portrait-with-quote.svg"
           , desc = ""
           , id = "portrait-with-quote"
           }
         , { name = "Header, Text, Link"
           , icon = "header-text-link.svg"
           , desc = ""
           , id = "header-text-link"
           }
         ]

selected : Model -> Maybe { name : String, icon : String, desc : String, id : String }
selected model =
    model.selected
        |> Maybe.andThen (\sel -> Just (List.filter (\b -> b.id == sel) blocks))
        |> Maybe.andThen List.head



renderSelection : Model -> Html Msg
renderSelection model =
    div [ class "block-picker__selection" ]
        (case selected model of
            Nothing ->
                [ Html.text "Please click on a block" ]
            Just selectedBlock ->
                [ div [ class "block-picker__selected-name" ]
                      [ Html.text selectedBlock.name ]
                , div [ class "block-picker__selected-desc" ]
                      [ Html.text selectedBlock.desc ]
                , button [ class "block-picker__select-button"
                         , onClick (AppendBlockToPage selectedBlock.id)
                         ]
                      [ Html.text "Add to page" ]
                ])



view : Model -> Html Msg
view model =
    div
    [ class "block-picker" ]
    [ div [ class "block-picker__layer" ] []
    , div [ class "block-picker__box" ]
        [ div [ class "block-picker__header" ]
              [ Html.text "Choose the kind of block you want to insert" ]
              
        , div [ class "block-picker__body" ]
            
            [ div [ class "block-picker__available-blocks" ]
                  (blocks |> List.map renderBlock)
                  
            , renderSelection model
            ]
        ]
    ]
    
    
    
    
