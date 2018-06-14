module ReactPDF

open Elmish
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.PowerPack
open Fulma

open ZoomField
open PageNumbField
open FileShow
open FileSearch
open Paginator
open PdfComponent

type Msg =
    | SetNumPages of int
    | SetPage of int
    | UpdateFieldFileName of string
    | IncrementCurrentPage
    | DecrementCurrentPage
    | NoChange
    | FileLoaded of Fable.Import.Browser.Blob
    | ErrorMsg of exn
    | LoadFile of string
    | UpdatePageField of int
    | UpdatePageEditingStatus
    | IncrementPageNumberFieldState
    | DecrementPageNumberFieldState
    | IncrementZoom
    | DecrementZoom
    | SetZoom
    | UpdateZoomEditingStatus
    | UpdateZoomFieldValue of string

type Model =
    { CurrentPage : int
      NumPages : int option
      File : Fable.Import.Browser.Blob option
      MaxItems : int
      PageScale : float
      FileName : string
      FieldFileName : string option
      IsPageNumberDisabled : bool
      PageNumberFieldState : int
      IsZoomFieldEditingDisabled: bool
      ZoomFieldState : string
      ErrorMsg: string option
      PageWidth: float }


let onDocumentLoad (dispatch : Msg -> unit) (pdf : Pdf) =
    // Browser.console.log (sprintf "Number of pages %i" pdf.pdfInfo.numPages)
    dispatch (SetNumPages pdf.pdfInfo.numPages)

let getFile filename =
    promise
        {
        return! Fetch.fetch (sprintf "http://localhost:8080/%s" filename) []
                |> Promise.bind (fun fetched -> fetched.blob()) }

let init() : Model * Cmd<Msg> =
    { CurrentPage = 1
      NumPages = None
      File = None
      MaxItems = 10
      PageScale = 1.0
      PageWidth = 600.
      FileName = "analyzing-visualizing-data-f-sharp.pdf"
      FieldFileName = None
      IsPageNumberDisabled = true
      PageNumberFieldState = 1
      IsZoomFieldEditingDisabled = false
      ZoomFieldState = "100" 
      ErrorMsg = None }, Cmd.ofPromise getFile "analyzing-visualizing-data-f-sharp.pdf" FileLoaded ErrorMsg

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match model, msg with
    | _, SetNumPages numPages -> { model with NumPages = Some numPages
                                              IsPageNumberDisabled = true }, Cmd.none
    | _, IncrementCurrentPage -> match model.CurrentPage with
                                    | page when page >= model.NumPages.Value -> model, Cmd.none
                                    | _ -> { model with CurrentPage = (model.CurrentPage + 1) },  Cmd.ofMsg IncrementPageNumberFieldState                                                        
    |_ , IncrementPageNumberFieldState -> { model with PageNumberFieldState =  (model.PageNumberFieldState + 1) }, Cmd.none
    | _, DecrementCurrentPage -> match model.CurrentPage with
                                    | page when page <= 0 -> model, Cmd.none
                                    | _ -> { model with CurrentPage = (model.CurrentPage - 1) },  Cmd.ofMsg DecrementPageNumberFieldState
    |_ , DecrementPageNumberFieldState -> { model with PageNumberFieldState = (model.PageNumberFieldState - 1) }, Cmd.none  
    | _, SetPage pageNum -> { model with CurrentPage = pageNum
                                         IsPageNumberDisabled = true
                                         PageNumberFieldState = pageNum }, Cmd.none
    | _, NoChange -> model, Cmd.none
    | _, ErrorMsg exp -> { model with ErrorMsg = Some exp.Message
                                      FileName = "" }, Cmd.none
    | _, FileLoaded file ->
        { model with File = Some file
                     CurrentPage = 1
                     FieldFileName = None
                     PageNumberFieldState = 1 }, Cmd.none
    | _, LoadFile fileName -> { model with FileName = fileName
                                           File = None
                                           ErrorMsg = None }, Cmd.ofPromise getFile fileName FileLoaded ErrorMsg
    | _, UpdateFieldFileName filename -> { model with FieldFileName = Some filename }, Cmd.none
    | _, UpdatePageField page -> { model with PageNumberFieldState = page }, Cmd.none
    | _, UpdatePageEditingStatus -> { model with IsPageNumberDisabled = not model.IsPageNumberDisabled }, Cmd.none
    | _, IncrementZoom -> { model with PageScale = model.PageScale + 0.1
                                       ZoomFieldState = (sprintf "%3.0f" ((model.PageScale + 0.1) * 100.)) }, Cmd.none
    | _, DecrementZoom -> { model with PageScale = model.PageScale - 0.1
                                       ZoomFieldState = (sprintf "%3.0f" ((model.PageScale - 0.1) * 100.)) }, Cmd.none
    | _, SetZoom -> { model with PageScale = (float model.ZoomFieldState) / 100.}, Cmd.ofMsg UpdateZoomEditingStatus
    | _, UpdateZoomFieldValue value -> { model with ZoomFieldState = value }, Cmd.none 
    | _, UpdateZoomEditingStatus -> { model with IsZoomFieldEditingDisabled = not model.IsZoomFieldEditingDisabled }, Cmd.none

let private paginationView model dispatch =
    paginationTile 
        model.NumPages
        model.CurrentPage
        (fun _ -> dispatch DecrementCurrentPage)
        (fun _ -> dispatch IncrementCurrentPage)
        (fun page -> (fun _ -> dispatch (SetPage page)))
        model.MaxItems 
let private fileShowFieldsView model =
    fileShowField model.FileName

let private fileSearchFieldsView model dispatch =
    let loadFileAction filename = dispatch (LoadFile filename)
    
    fileSearchField model.FieldFileName
        (fun e -> dispatch (UpdateFieldFileName !!e.Value))
        loadFileAction

let private headerTile model dispatch =
    Tile.child [] [ Box.box' [] [ Columns.columns [] [ fileShowFieldsView model
                                                       fileSearchFieldsView model dispatch ] ] ]

// Vista que muestra y maneja los numeros de pagina.
let private pageNumberFieldView model dispatch =
    let setPageFunc = (fun _ -> dispatch (SetPage model.PageNumberFieldState))

    pageNumberField model.PageNumberFieldState 
        model.NumPages
        (fun e -> dispatch (UpdatePageField !!e.Value))
        setPageFunc
        setPageFunc
        (fun _ -> dispatch UpdatePageEditingStatus)
        (not model.IsPageNumberDisabled)

// Vista que maneja el zoom de la pagina
let private zoomFieldView model dispatch =
    let setZoomFunc = (fun _ -> dispatch SetZoom)

    zoomField model.ZoomFieldState 
        (fun e -> dispatch (UpdateZoomFieldValue !!e.Value))
        setZoomFunc
        setZoomFunc
        model.IsZoomFieldEditingDisabled
        (fun _ -> dispatch UpdateZoomEditingStatus)
        model.PageScale
        (fun _ -> dispatch IncrementZoom)
        (fun _ -> dispatch DecrementZoom)

let private pageControlView model dispatch = 
     Tile.child [ ] 
        [ Box.box' [ ] [
                    Columns.columns [] [
                        Column.column [ Column.Width (Screen.All, Column.Is4) ] 
                            [ zoomFieldView model dispatch ]
                        Column.column [] []
                        Column.column [Column.Width (Screen.All, Column.Is4) ] 
                            [ pageNumberFieldView model dispatch ] ] ] ] 

let view (model : Model) (dispatch : Msg -> unit) =
    Tile.ancestor []
        [ Tile.parent [ Tile.IsVertical
                        Tile.Size Tile.Is10 ]
              [ headerTile model dispatch

                Tile.child [ Tile.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ]
                     [ (match model.ErrorMsg with
                                | Some error -> Field.div [] 
                                                    [ Tag.list [ Tag.List.HasAddons; Tag.List.IsCentered ] 
                                                          [ Tag.tag [ Tag.Color Color.IsDanger
                                                                      Tag.Size IsMedium ] [ str error ] ] ]
                                | None -> Box.box' []
                                            (match model.File with
                                                | Some file ->
                                                    [ Box.box' [] [ yield pdfReact [  File file
                                                                                      OnLoadSuccess(onDocumentLoad dispatch) ]
                                                                           [ yield pdfPage [ PageNumber model.CurrentPage
                                                                                             Width (model.PageWidth * model.PageScale) ] [] ] ] 
                                                      pageControlView model dispatch
                                                      br [] 
                                                      paginationView model dispatch ]
                                                | None -> [ str "No hay archivos Cargados" ] ) ) ] ] ]
