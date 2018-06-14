module FileSearch

open Fable.Helpers.React
open Fulma

open Utils

let fileSearchField (targetValue: string option) onChangeAction (loadFileAction: string -> unit) =  
      let loadFile = 
            (fun _ -> match targetValue with
                        | Some filename when filename.Trim ' ' = "" -> ()
                        | Some filename -> loadFileAction filename
                        | None -> ())

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
                                                    loadFile ]
                                          Input.Value (match targetValue with
                                                            | Some filename -> filename
                                                            | None -> "" ) ] ]
                              Button.button [ Button.Color Color.IsPrimary
                                              Button.OnClick loadFile ]
                                  [ span [] [ str "Cargar Archivo" ] ] ] ] ] ]
