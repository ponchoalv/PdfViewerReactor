module FileSearch

open Fable.Helpers.React
open Fulma

open Utils

let fileSearchField targetValue onChangeAction onEnterKeyAction onClickAction =
    Column.column []
        [ Column.column []
              [ Notification.notification [ Notification.Color Color.IsLight ]
                    [ Field.div [ Field.IsGroupedCentered ]
                          [ Control.p [ Control.IsExpanded ]
                                [ Input.text
                                      [ Input.Placeholder "Escribir nombre de archivo a cargar..."
                                        Input.OnChange onChangeAction
                                        Input.Props
                                            [ onKeyDown KeyCode.enter
                                                  onEnterKeyAction ]
                                        Input.Value targetValue ] ]
                            Button.button [ Button.Color Color.IsPrimary
                                            Button.OnClick onClickAction ]
                                [ span [] [ str "Cargar Archivo" ] ] ] ] ] ]

