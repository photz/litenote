module Login.Model exposing (..)

type alias Model = { email : String
                   , password : String
                   , error : Bool
                   }


init : Model
init = { email = ""
       , password = ""
       , error = False
       }

showError : Model -> Model
showError m = { m | error = True }
