module WidthAdjust

open Fable.Helpers.React
open Fulma
open Fulma.Extensions

let widthAdjustTile  = 
    Notification.notification [ Notification.Color Color.IsLight ] 
        [ Field.div [ Field.IsGroupedCentered ]
                    [ Switch.switch [ Switch.Checked true
                                      Switch.Size IsLarge  ] [ strong [] [str  "< >" ] ] ] ]