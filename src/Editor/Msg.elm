module Editor.Msg exposing (..)

type Msg = Close
         | ChangeField { name : String, newValue : String }
         | SaveBlock
