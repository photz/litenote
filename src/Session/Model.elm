module Session.Model exposing (..)

type Model = Guest
           | LoggedIn

init = Guest

isGuest : Model -> Bool
isGuest m = case m of
                Guest -> True
                _ -> False

isLoggedIn : Model -> Bool
isLoggedIn m = case m of
                   LoggedIn -> True
                   _ -> False

mayEditBlocks : Model -> Bool
mayEditBlocks m = isLoggedIn m
