module FileShow

open Fable.Helpers.React
open Fulma

let fileShowField targetValue =
    Column.column [ Column.Width(Screen.All, Column.Is6) ]
        [ Column.column []
              [ Notification.notification [ Notification.Color Color.IsLight ]
                    [ Field.div [ Field.IsGrouped ]
                         [ Field.label [ Field.Label.IsNormal ] 
                            [ Label.label [ Label.For "filename" ] [ str "Archivo:" ] ]
                           Control.p [ Control.IsExpanded ] [ Input.text [ Input.Id "filename"
                                                                           Input.IsReadOnly true
                                                                           Input.IsStatic true
                                                                           Input.Value targetValue ] ] ] ] ] ]


