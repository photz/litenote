module Routing exposing (parse)

import Navigation
import UrlParser exposing ((</>))
import Route exposing (Route)

parse : List Route -> Navigation.Location -> Maybe Int
parse routes loc =
    let matchers = UrlParser.oneOf
                   (routes
                   |> List.map
                        (\r ->
                             UrlParser.map
                                 r.pageId (UrlParser.s r.path)))

    in
        UrlParser.parseHash matchers loc
