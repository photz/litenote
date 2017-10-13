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

type Model = HeaderAndText HeaderAndTextData
           | TextOnImage TextOnImageData
           | Container Direction (List Model)
           | Image String
           | HeaderTextLink HeaderTextLinkData
