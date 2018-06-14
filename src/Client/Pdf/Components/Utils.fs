module Utils
open Fable.Helpers.React.Props

module KeyCode =
    let enter = 13.
    let upArrow = 38.
    let downArrow = 40.

let onKeyDown keyCode action =
    OnKeyDown(fun (ev : Fable.Import.React.KeyboardEvent) ->
        if ev.keyCode = keyCode then
            ev.preventDefault()
            action ev)
