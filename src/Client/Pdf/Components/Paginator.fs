module Paginator

open Fable.Helpers.React
open Fulma

let paginationTile totalPages currentPage onPreviousClick onNextClick onPageNumberClick maxItems =
    Tile.child [] 
        [ Box.box' [] 
            [ (match totalPages with
                | Some pages ->
                    Pagination.pagination [ Pagination.IsCentered ]
                        [ Pagination.previous [ Props [ Props.OnClick
                                                            (match currentPage with
                                                             | 1 -> ignore
                                                             | _ -> onPreviousClick )
                                                        Props.Disabled(currentPage = 1) ] ]
                              [ str "Anterior" ]
                          Pagination.next [ Props [ Props.OnClick
                                                        (match currentPage with
                                                            | _ when currentPage = pages -> ignore
                                                            | _ -> onNextClick)
                                                    Props.Disabled(currentPage = pages) ] ]
                              [ str "Siguiente" ]
                          Pagination.list []
                              [ let pagesItems =
                                    seq {
                                        for page in 1..pages do
                                            yield Pagination.link
                                                      [ Pagination.Link.Current
                                                            (page = currentPage)
                                                        Pagination.Link.Props
                                                            [ Props.OnClick
                                                                  (match currentPage with
                                                                    | _ when currentPage = page -> ignore
                                                                    | _ -> onPageNumberClick page) ] ]
                                                      [ str (sprintf "%i" page) ]}
                                let showedPagesItems items position =
                                    match items with
                                    | list when pages < maxItems -> list |> Seq.toList
                                    | list ->
                                        match position with
                                        | position when position < (pages - maxItems) ->
                                            list
                                            |> Seq.skip ( position - (position % maxItems))
                                            |> Seq.truncate maxItems
                                            |> Seq.toList
                                        | _ ->
                                            list
                                            |> Seq.skip (pages - maxItems)
                                            |> Seq.truncate maxItems
                                            |> Seq.toList
                                yield! showedPagesItems pagesItems (currentPage - 1) ] ]
                | _ -> str "No hay un documento cargado") ] ]