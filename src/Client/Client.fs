module Client

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma

type Model = ReactPDF.Model

type Msg = ReactPDF.Msg

let init() : Model * Cmd<Msg> = ReactPDF.init()
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> = ReactPDF.update msg model

let safeComponents =
    let intersperse sep ls =
        List.foldBack (fun x ->
            function
            | [] -> [ x ]
            | xs -> x :: sep :: xs) ls []

    let components =
        [ "Saturn", "https://saturnframework.github.io/docs/"
          "Fable", "http://fable.io"
          "Elmish", "https://elmish.github.io/elmish/"
          "Fulma", "https://mangelmaxime.github.io/Fulma"
          "Bulma\u00A0Templates", "https://dansup.github.io/bulma-templates/" ]
        |> List.map (fun (desc, link) -> a [ Href link ] [ str desc ])
        |> intersperse (str ", ")
        |> span []

    p [] [ strong [] [ str "SAFE Template" ]
           str " powered by: "
           components ]

let show =
    function
    | Some x -> string x
    | None -> "Loading..."

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ] [ Navbar.Item.div [] [ Heading.h1 [Heading.Modifiers [Modifier.TextColor Color.IsWhite]] [ str "Pdf Viewer!" ] ] ]
          Container.container [] [ yield ReactPDF.view model dispatch ]
          br []
          Footer.footer []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif

|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif

|> Program.run
