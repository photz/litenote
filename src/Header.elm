module Header exposing (view, Msg)

import Html exposing (Html, div, a)
import Html.Attributes exposing (class, classList, href)
import Page
import Route exposing (..)

-- UPDATE

type Msg = Fooo

-- VIEW

renderNavItem : Int -> Route -> Html Msg
renderNavItem currentPageId route =
    a [ classList [ ("navigation__item", True)
                  , ("navigation__item--active", currentPageId == route.pageId)
                  ]
      , href ("#" ++ route.path)
      ]
    [ route.name |> Html.text ]


renderNav : Int -> List Route -> Html Msg
renderNav currentPageId routes =
    div [ classList [ ("header__nav", True)
                    , ("navigation", True)
                    ]
        ]
        (routes |> List.map (renderNavItem currentPageId))
                  


renderHeader : Int -> List Route -> Html Msg
renderHeader currentPageId routes =
    div [ class "header" ]
        [ div [ class "header__inner" ]
              [ div [ class "header__logo" ] []
              , renderNav currentPageId routes
              ]
        ]

                           

view : Int -> List Route -> Html Msg
view currentPageId routes = renderHeader currentPageId routes
