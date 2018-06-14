module PdfComponent

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Import.React


[<Pojo>]
type PdfInfo =
    { numPages : int }

[<Pojo>]
type Pdf =
    { pdfInfo : PdfInfo }

type PdfProps =
    | File of Fable.Import.Browser.Blob  
    | OnLoadSuccess of (Pdf -> unit)
    | ClassName of string

type PdfPageProps =
    | PageNumber of int
    | PageIndex of int
    | Width of float
    | Scale of float
    | ClassName of string

let inline pdfReact (props : PdfProps list) (elems : ReactElement list) : ReactElement =
    ofImport "Document" "react-pdf/dist/entry.webpack" (keyValueList CaseRules.LowerFirst props) elems
let inline pdfPage (props : PdfPageProps list) (elems : ReactElement list) : ReactElement =
    ofImport "Page" "react-pdf/dist/entry.webpack" (keyValueList CaseRules.LowerFirst props) elems
