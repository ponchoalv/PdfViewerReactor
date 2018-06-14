module ZoomField

open Fable.Helpers.React
open Fulma
open Fulma.Extensions

open EditField
open Utils

let zoomField targetValue onChangeAction onKeyDownAction onBlurAction editStateTarget editToggleAction scaleValueTarget incrementAction decrementAction = 
    Notification.notification [ Notification.Color Color.IsLight 
                                Notification.Props [ Tooltip.dataTooltip "Doble-Click para editar el zoom del documento" ]
                                Notification.CustomClass Tooltip.ClassName ] 
        [ Field.div [ Field.IsGroupedCentered ]
                    [ editableField [ Input.Value targetValue
                                      Input.OnChange onChangeAction
                                      Input.Props 
                                        [ onKeyDown KeyCode.enter
                                            onKeyDownAction
                                          Props.OnBlur
                                            onBlurAction
                                          Props.AutoFocus true ]
                                      Input.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                            editStateTarget
                            [ Label.Props [ Props.OnDoubleClick editToggleAction ] ]
                            [ str "Zoom "
                              str (sprintf "%3.0f%%" (scaleValueTarget * 100.)) ]  
                      Control.p [ ]
                        [ Button.a
                            [ Button.Color IsInfo
                              Button.OnClick incrementAction]
                            [ str "+" ] ]
                      Control.p [ ]
                        [ Button.a
                            [ Button.Color IsDanger
                              Button.OnClick decrementAction ]
                            [ str "-" ] ] ] ]