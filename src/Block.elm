module Block exposing (..)

type Direction = Row | Column

type alias HeaderAndTextData = { header : String
                               , text : String
                               , inverted: Bool
                               }

type alias HeaderTextLinkData = { header : String
                                , text : String
                                , link : String
                                } 

type alias TextOnImageData = { text : String
                             , image : String
                             }

type Data = HeaderAndText HeaderAndTextData
          | TextOnImage TextOnImageData
          | Container Direction (List Model)
          | Image String
          | HeaderTextLink HeaderTextLinkData
            
type alias Model = { id : Int
                   , data : Data
                   }
