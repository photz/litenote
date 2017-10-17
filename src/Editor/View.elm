module Editor.View exposing (view)

import Html exposing (Html, div, button, textarea)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick, onInput)
import Editor.Msg exposing (..)
import Block

textinput : String -> String -> String -> Html Msg
textinput name label content =
    div [ class "field" ]
        [ div [ class "field__label" ]
              [ Html.text label ]
        , textarea
              [ class "field__input"
              , onInput (\v -> ChangeField { name = name, newValue = v })
              ]
              [ Html.text content ]
        ]

logo : Html Msg
logo = div
       [ class "editor__logo" ]
       [ Html.text "Litenote" ]

closeButton : Html Msg
closeButton = div
              [ onClick Close
              , class "editor__close"
              ]
              []

header : Html Msg
header = div [ class "editor__header" ]
         [ logo, closeButton ]

save : Html Msg
save = button [ class "editor__save"
              , onClick SaveBlock
              ]
       [ Html.text "Save" ]

type Field = ImageField { name : String
                        , label : String
                        , currentValue : String
                        }
           | TextField { name : String
                       , label : String
                       , currentValue : String
                       }
           | BoolField { name : String
                       , label : String
                       , currentValue : Bool
                       }

getFields : Block.Model -> List Field
getFields block =
    case block.data of
        Block.HeaderAndText data ->
            [ TextField { name = "header"
                        , label = "Header"
                        , currentValue = data.header
                        }
            , TextField { name = "text"
                        , label = "Text"
                        , currentValue = data.text
                        }
            ]
        Block.TextOnImage data ->
            [ TextField { name = "text"
                        , label = "Text"
                        , currentValue = data.text
                        }
            ]
        Block.HeaderTextLink data ->
            [ TextField { name = "header"
                        , label = "Header"
                        , currentValue = data.header
                        }
            , TextField { name = "text"
                        , label = "Text"
                        , currentValue = data.text
                        }
            ]
        Block.Image path -> []
        Block.PortraitWithQuote data ->
            [ TextField { name = "quote"
                        , label = "Quote"
                        , currentValue = data.quote
                        }
            , TextField { name = "author"
                        , label = "Author"
                        , currentValue = data.author
                        }
            ]
        _ -> []

renderField : Field -> Html Msg
renderField field =
    case field of
        TextField { name, label, currentValue } ->
            textinput name label currentValue
        _ -> div [] []

view : Block.Model -> Html Msg
view block = div
             [ class "editor" ]
             [ header
             , div [ class "editor__fields" ]
                 (block
                 |> getFields
                 |> List.map renderField)
             , save
             ]

