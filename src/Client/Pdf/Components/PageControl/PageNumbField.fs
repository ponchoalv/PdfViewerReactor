module PageNumbField

open Fable.Helpers.React
open Fulma
open Fulma.Extensions

open EditField
open Utils

let pageNumberField valueTarget totalPages onChangeAction onKeyDownAction onBlurAction onDoubleClickAction editStateTarget = 
    (match totalPages with 
        | Some pages -> 
            Notification.notification [ Notification.Color Color.IsLight
                                        Notification.Props [ Tooltip.dataTooltip "Doble-Click para editar el número de pagina" ]
                                        Notification.CustomClass Tooltip.ClassName ]
                [ editableField [ Input.OnChange onChangeAction
                                  Input.Props 
                                    [ onKeyDown KeyCode.enter
                                        onKeyDownAction
                                      Props.OnBlur
                                        onBlurAction
                                      Props.AutoFocus true ]
                                  Input.Value (sprintf "%i" valueTarget)
                                  Input.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] 
                                editStateTarget
                                [   Label.Modifiers [ ] 
                                    Label.Props [ Props.OnDoubleClick onDoubleClickAction ] ]
                                [   str "Pagina "
                                    str (sprintf "%i " valueTarget) 
                                    str (sprintf "de %i" pages) ] ]
        | None -> 
             span [] [str "No se Cargó ningún documento"])